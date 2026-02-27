import fs from "fs";
import os from "os";
import path from "path";
import process from "process";
import tmp from "tmp";
import chalk from "chalk";
import spawn from "cross-spawn";
import express from "express";
import serveIndex from "serve-index";
import expressWs from "express-ws";
import ws from "ws";
import { SpawnSyncReturns } from "child_process";
import { glob, globSync } from "glob";
import chokidar from "chokidar";
import open from "open";
import { fileURLToPath } from "url";
import { version } from "./version.js";
import { diffLines } from "diff";

import util from "util";
const readFileAsync = util.promisify(fs.readFile);
const statAsync = util.promisify(fs.stat);

tmp.setGracefulCleanup();

express.static.mime.define({ "text/plain; charset=UTF-8": ["elm"] });
express.static.mime.define({ "text/plain; charset=UTF-8": ["md"] });

interface Options {
  address: string;
  debug: boolean;
  dir: string;
  port: number;
  browser: boolean;
  reload: boolean;
}

interface Manifest {
  type: string;
  name?: string;
  summary?: string;
  license?: string;
  version?: string;
  "exposed-modules": string[] | Record<string, string[]>;
  "elm-version": string;
  dependencies: Record<string, string> | Record<string, Record<string, string>>;
  "test-dependencies":
    | Record<string, string>
    | Record<string, Record<string, string>>;
  "source-directories"?: string[];

  // dynamically added for convenience
  timestamp: number;
}

interface Package {
  name: string;
  summary: string;
  license: string;
  versions: string[];
}

type Release = Record<string, number>;
type Elm = (args: string[], cwd?: string) => SpawnSyncReturns<Buffer>;
type Output = object | object[];

function info(...args: any[]) {
  console.log(...args);
}

function warning(...args: any[]) {
  console.log(chalk.yellow(...args));
}

function error(...args: any[]) {
  console.log(chalk.red(...args));
}

function fatal(...args: any[]) {
  error(...args);
  chalk.red("Exiting...");
  process.exit(1);
}

function elmErrors(error: any) {
  if (error.type === "compile-errors") {
    console.log(elmErrorWithColor(error.errors));
  }
}

type Error = {
  path: string;
  problems: Problem[];
};

type Problem = {
  title: string;
  message: (Message | string)[];
};

type Message = {
  bold: boolean;
  underline: boolean;
  color: string;
  string: string;
};

const elmErrorWithColor = (errors: Error[]) => {
  const repeat = (str: string, num: number, min = 3) =>
    [...Array(num < 0 ? min : num)].map((_) => str).join("");

  const errorToString = (error: Error): string => {
    const problemToString = (problem: Problem): string => {
      // Removing the elm-stuff/generatedFolderName from the beginning of the filepath
      let errorPath = error.path
        .substring(process.cwd().length + 1)
        .split(path.sep);
      errorPath.shift();
      errorPath.shift();

      const errorFilePath = errorPath.join(path.sep);

      return [
        chalk.cyan(
          `-- ${problem.title} ${repeat(
            "-",
            63 - problem.title.length - errorFilePath.length
          )} ${errorFilePath}`
        ),
        problem.message.map(messageToString).join(""),
      ].join("\n\n");
    };

    const messageToString = (line: Message | string) => {
      if (typeof line === "string") {
        return line;
      } else {
        let message = line.string;
        if (line.bold) {
          message = chalk.bold(message);
        }
        if (line.underline) {
          message = chalk.underline(message);
        }
        switch (line.color) {
          case "green":
            message = chalk.green(message);
            break;

          case "yellow":
            message = chalk.yellow(message);
            break;

          case "cyan":
            message = chalk.cyan(message);
            break;

          case "RED":
            message = chalk.red(message);
            break;

          default:
            break;
        }

        return message;
      }
    };

    return error.problems.map(problemToString).join("\n\n");
  };
  return errors.map(errorToString).join("\n\n\n");
};

/*
 * Find and check Elm executable
 */
function getElm(): [Elm, string] {
  let elm: Elm = (args, cwd = ".") =>
    spawn.sync("npx", ["--no-install", "elm"].concat(args), { cwd });

  let exec = elm(["--version"]);
  if (exec.error || exec.status !== 0 || exec.stderr.toString().length > 0) {
    elm = (args, cwd = ".") => spawn.sync("elm", args, { cwd });
    exec = elm(["--version"]);
  }

  if (exec.error) {
    fatal(`cannot run 'elm --version' (${exec.error})`);
  } else if (exec.status !== 0) {
    error(`cannot run 'elm --version':`);
    process.stderr.write(exec.stderr);
    process.exit(exec.status ? exec.status : 1);
  }

  const version = exec.stdout.toString().trim();
  if (!version.startsWith("0.19")) {
    fatal(`unsupported Elm version ${version}`);
  }

  return [elm, version];
}

function getElmCache(elmVersion: string) {
  const dir = os.platform() === "win32" ? "AppData/Roaming/elm" : ".elm";
  const home = process.env.ELM_HOME || path.join(os.homedir(), dir);
  const packages = elmVersion === "0.19.0" ? "package" : "packages";
  const cache = path.join(home, elmVersion, packages);
  return cache;
}

async function getManifest(manifestPath: string): Promise<Manifest> {
  return readFileAsync(manifestPath, "utf8")
    .then(async (json) => {
      let manifest = JSON.parse(json);
      let stat = await statAsync(manifestPath);
      manifest["timestamp"] = Math.round(stat.mtime.getTime() / 1000);
      manifest = completeApplication(manifestPath, manifest);
      return manifest;
    })
    .catch((err) => error(err));
}

function getManifestSync(manifestPath: string): Manifest | null {
  try {
    const json = fs.readFileSync(manifestPath, "utf8");
    let manifest = JSON.parse(json);
    let stat = fs.statSync(manifestPath);
    manifest["timestamp"] = Math.round(stat.mtime.getTime() / 1000);
    return completeApplication(manifestPath, manifest);
  } catch (err) {
    return null;
  }
}

function completeApplication(
  manifestPath: string,
  manifest: Manifest
): Manifest {
  if (manifest.type !== "application") {
    return manifest;
  }
  try {
    const elmAppPath = path.resolve(
      path.dirname(manifestPath),
      "elm-application.json"
    );
    if (fs.existsSync(elmAppPath)) {
      const elmApp = JSON.parse(fs.readFileSync(elmAppPath).toString());
      Object.assign(manifest, elmApp);
    }
  } catch (err) {
    error(err);
  }
  if (!("name" in manifest)) {
    manifest.name = "my/application";
  }
  if (!("version" in manifest)) {
    manifest.version = "1.0.0";
  }
  if (!("summary" in manifest)) {
    manifest.summary = "Elm application";
  }
  if (!("license" in manifest)) {
    manifest.license = "Fair";
  }
  return manifest;
}

function fullname(manifest: Manifest): string {
  return `${manifest.name}/${manifest.version}`;
}

async function searchPackages(
  pattern: string
): Promise<Record<string, Package>> {
  let paths = await glob(pattern + "/elm.json", { realpath: true });
  const manifests = await Promise.all(
    paths.map((path: string) => getManifest(path))
  );
  let packages = manifests.reduce((acc, pkg: Manifest) => {
    if (pkg.name && pkg.name in acc && pkg.version) {
      acc[pkg.name].versions.push(pkg.version);
      return acc;
    } else if (pkg.name && pkg.version) {
      acc[pkg.name] = {
        name: pkg.name,
        summary: pkg.summary || "",
        license: pkg.license || "Fair",
        versions: [pkg.version],
      };
      return acc;
    } else {
      warning("invalid elm.json", pkg);
      return acc;
    }
  }, {} as Record<string, Package>);
  return packages;
}

async function packageReleases(pattern: string): Promise<Release> {
  const paths = await glob(pattern + "/elm.json", {
    realpath: true,
  });
  const manifests = await Promise.all(
    paths.map((path: string) => getManifest(path))
  );
  let releases = manifests.reduce((releases, pkg: Manifest) => {
    if (pkg.version && pkg.timestamp) {
      releases[pkg.version] = pkg.timestamp;
      return releases;
    } else {
      return releases;
    }
  }, {} as Release);
  return releases;
}

function merge(objects: object[]): object {
  return objects.reduce((acc, obj) => Object.assign(acc, obj));
}

function buildDocs(
  manifest: Manifest,
  dir: string,
  elm: Elm,
  clean: boolean = true,
  verbose: boolean = false
): Output {
  info(`  |> building ${path.resolve(dir)} documentation`);
  try {
    if (manifest.type == "package") {
      return buildPackageDocs(dir, elm, clean, verbose);
    } else if (manifest.type == "application") {
      return buildApplicationDocs(manifest, dir, elm, clean, verbose);
    }
  } catch (err) {
    error(err);
  }
  return {};
}

// Return a docs.json or a json error report
function buildPackageDocs(
  dir: string,
  elm: Elm,
  clean: boolean,
  verbose: boolean
): Output {
  const tmpFile = tmp.fileSync({ prefix: "elm-docs", postfix: ".json" });
  const buildDir = path.resolve(dir);
  if (!clean) {
    info(`  |> generating ${tmpFile.name} documentation`);
  }
  const build = elm(
    ["make", `--docs=${tmpFile.name}`, "--report=json"],
    buildDir
  );
  if (build.error) {
    error(`cannot build documentation (${build.error})`);
  } else if (build.stderr.toString().length > 0 && verbose) {
    elmErrors(JSON.parse(build.stderr.toString()));
  }
  let docs;
  try {
    docs = JSON.parse(fs.readFileSync(tmpFile.name).toString());
  } catch (err) {
    try {
      // Return Errors JSON report
      docs = JSON.parse(build.stderr.toString());
      if (docs.type === "compile-errors") {
        docs.errors.forEach((error: any) => {
          error.path = error.path.substring(buildDir.length + 1);
        });
      }
    } catch (err) {
      docs = {};
    }
  }
  if (clean) {
    tmpFile.removeCallback();
  }
  return docs;
}

function buildApplicationDocs(
  manifest: Manifest,
  dir: string,
  elm: Elm,
  clean: boolean,
  verbose: boolean
): Output {
  // Build package from application manifest
  const elmStuff = path.resolve(dir, "elm-stuff");
  if (!fs.existsSync(elmStuff)) {
    fs.mkdirSync(elmStuff);
  }
  const tmpDir = tmp.dirSync({
    tmpdir: elmStuff,
    prefix: "elm-application-",
    unsafeCleanup: true,
  });
  const tmpDirSrc = path.resolve(tmpDir.name, "src");
  fs.mkdirSync(tmpDirSrc);

  if (!clean) {
    info(`  |> generating ${tmpDir.name} package`);
  }

  // Build package manifest
  let pkg: Manifest = {
    type: "package",
    name: manifest.name,
    summary: manifest.summary,
    license: manifest.license,
    version: manifest.version,
    "exposed-modules": manifest["exposed-modules"],
    "elm-version": versionToConstraint(manifest["elm-version"]),
    dependencies: {},
    "test-dependencies": {},
    timestamp: manifest.timestamp,
  };

  // Add dependencies constraints
  if (manifest.dependencies.direct) {
    for (const [name, version] of Object.entries(
      manifest.dependencies.direct
    )) {
      pkg.dependencies[name] = versionToConstraint(version);
    }
  }

  // Add source directories exposed-modules
  let exposedModules: string[] = getExposedModules(pkg["exposed-modules"]);

  if (manifest["source-directories"]) {
    manifest["source-directories"].forEach((src) => {
      const srcDir = path.resolve(src);
      importModules(srcDir, tmpDirSrc);
      const elmJsonPath = path.resolve(src, "../elm.json");

      if (fs.existsSync(elmJsonPath)) {
        try {
          const srcManifest = getManifestSync(elmJsonPath);
          if (srcManifest && srcManifest.type === "package") {
            const srcModules = getExposedModules(
              srcManifest["exposed-modules"]
            );
            exposedModules = exposedModules.concat(srcModules);
          }
        } catch (err) {
          error(err);
        }
      }
    });
  }
  pkg["exposed-modules"] = exposedModules;

  // Write elm.json and generate package documentation
  const elmJson = JSON.stringify(pkg);
  fs.writeFileSync(tmpDir.name + "/elm.json", elmJson, "utf8");
  const docs = buildPackageDocs(tmpDir.name, elm, clean, verbose);

  // remove temporary directory
  if (clean) {
    tmpDir.removeCallback();
  }
  return docs;
}

function getExposedModules(
  manifestExposedModules: string[] | Record<string, string[]> | null
): string[] {
  let exposedModules: string[] = [];

  if (manifestExposedModules) {
    if (Array.isArray(manifestExposedModules)) {
      exposedModules = manifestExposedModules;
    } else if (typeof manifestExposedModules === "object") {
      Object.values(manifestExposedModules).forEach((modules) => {
        exposedModules = exposedModules.concat(modules);
      });
    }
  }
  return exposedModules;
}

function importModules(srcDir: string, dstDir: string) {
  globSync("**/*.elm", { cwd: srcDir }).forEach((elm) => {
    try {
      const dir = path.resolve(dstDir, path.dirname(elm));
      mkdirSyncRecursive(path.resolve(dstDir, dir));
      const srcModulePath = path.resolve(srcDir, elm);
      const dstModulePath = path.resolve(dstDir, elm);
      let module = fs.readFileSync(srcModulePath).toString();
      if (module.match(/^port +module /) !== null) {
        // Stub ports by subscriptions and commands that do nothing
        info(`  |> stubbing ${elm} ports`);
        module = module.replace(
          /^port +([^ :]+)([^\n]+)$/gm,
          (match, name, decl, _off, _str) => {
            if (name === "module") {
              return ["module", decl].join(" ");
            } else if (decl.includes("Sub")) {
              info("  |> stubbing incoming port", name);
              return name + " " + decl + "\n" + name + " = always Sub.none\n";
            } else if (decl.includes("Cmd")) {
              info("  |> stubbing outgoing port", name);
              return name + " " + decl + "\n" + name + " = always Cmd.none\n";
            } else {
              warning("unmatched", match);
            }
            return match;
          }
        );
        fs.writeFileSync(dstModulePath, module);
      } else {
        linkModule(srcModulePath, dstModulePath);
      }
    } catch (err) {
      error(err);
    }
  });
}

function mkdirSyncRecursive(dir: string) {
  const absoluteDir = path.resolve(dir);
  const sep = path.sep;
  absoluteDir.split(sep).reduce((parent, child) => {
    const d = path.resolve(parent, child);
    try {
      if (!fs.existsSync(d)) {
        fs.mkdirSync(d);
      }
    } catch (err) {
      error(err);
    }
    return d;
  }, "/");
}

function linkModule(linked: string, link: string) {
  try {
    if (!fs.existsSync(link)) {
      if (os.platform() === "win32") {
        // Windows requires to be admin to create symlinks
        fs.copyFileSync(linked, link);
      } else {
        fs.symlinkSync(linked, link);
      }
    }
  } catch (err) {
    error(err);
  }
}

function versionToConstraint(version: string): string {
  const [major, minor, patch] = version.split(".", 3);
  const nextPatch = parseInt(patch) + 1;
  return `${major}.${minor}.${patch} <= v < ${major}.${minor}.${nextPatch}`;
}

interface ApiDiff {
  magnitude: string;
  addedModules: string[];
  removedModules: string[];
  changedModules: ApiModuleChanges[];
}

interface ApiModuleChanges {
  name: string;
  added: string[];
  changed: string[];
  removed: string[];
}

function parseDiffOutput(output: string): ApiDiff | null {
  const lines = output.split("\n");
  if (lines.length === 0) {
    return null;
  }

  // First line: "This is a MAJOR/MINOR/PATCH change."
  const magnitudeMatch = lines[0].match(/This is a (MAJOR|MINOR|PATCH) change/);
  if (!magnitudeMatch) {
    return null;
  }

  const diff: ApiDiff = {
    magnitude: magnitudeMatch[1],
    addedModules: [],
    removedModules: [],
    changedModules: [],
  };

  let i = 1;
  while (i < lines.length) {
    const line = lines[i];

    if (line.match(/^-+ ADDED MODULES/)) {
      i++;
      while (i < lines.length && lines[i].match(/^\s+\S/)) {
        diff.addedModules.push(lines[i].trim());
        i++;
      }
    } else if (line.match(/^-+ REMOVED MODULES/)) {
      i++;
      while (i < lines.length && lines[i].match(/^\s+\S/)) {
        diff.removedModules.push(lines[i].trim());
        i++;
      }
    } else if (line.match(/^-{4,} .+ - (MAJOR|MINOR|PATCH) -{4,}$/)) {
      const moduleMatch = line.match(/^-{4,} (.+?) - (?:MAJOR|MINOR|PATCH) -{4,}$/);
      if (moduleMatch) {
        const mc: ApiModuleChanges = {
          name: moduleMatch[1],
          added: [],
          changed: [],
          removed: [],
        };
        i++;
        let section: "added" | "changed" | "removed" | null = null;
        while (i < lines.length && !lines[i].match(/^-{4,}/)) {
          const sectionLine = lines[i].trim();
          if (sectionLine === "Added:") {
            section = "added";
          } else if (sectionLine === "Changed:") {
            section = "changed";
          } else if (sectionLine === "Removed:") {
            section = "removed";
          } else if (section && sectionLine.length > 0) {
            // Extract item name from definition lines like "newFunc : String -> Int"
            // or "- oldFunc : Int -> String" / "+ oldFunc : Int -> Int -> String"
            // Skip continuation lines (indented type signatures, standalone symbols)
            const cleanLine = sectionLine.replace(/^[+-]\s*/, "");
            const nameMatch = cleanLine.match(/^([a-zA-Z][a-zA-Z0-9_]*)\s+:/);
            if (nameMatch) {
              const itemName = nameMatch[1];
              // For changed section, each item appears twice (- and +), only add once
              if (section === "changed") {
                if (!mc.changed.includes(itemName)) {
                  mc.changed.push(itemName);
                }
              } else {
                if (!mc[section].includes(itemName)) {
                  mc[section].push(itemName);
                }
              }
            }
          }
          i++;
        }
        diff.changedModules.push(mc);
      } else {
        i++;
      }
    } else {
      i++;
    }
  }

  return diff;
}

function buildDiff(elm: Elm): ApiDiff | null {
  try {
    const result = elm(["diff"]);
    if (result.error || result.status !== 0) {
      return null;
    }
    return parseDiffOutput(result.stdout.toString());
  } catch (err) {
    return null;
  }
}


// -- Content diff (doc comments + README) --

interface DocsModule {
  name: string;
  comment: string;
  unions: Array<{ name: string; comment: string; args: string[]; cases: Array<[string, string[]]> }>;
  aliases: Array<{ name: string; comment: string; args: string[]; type: string }>;
  values: Array<{ name: string; comment: string; type: string }>;
  binops: Array<{ name: string; comment: string; type: string; associativity: string; precedence: number }>;
}

type DiffLine = [number, string];

interface ItemContentDiff {
  commentDiff?: DiffLine[];
  oldAnnotation?: string;
}

interface ModuleContentDiff {
  items: Record<string, ItemContentDiff>;
}

interface ContentDiff {
  modules: Record<string, ModuleContentDiff>;
  readmeDiff?: DiffLine[];
}

function compareVersionsLocal(a: string, b: string): number {
  const pa = a.split(".").map(Number);
  const pb = b.split(".").map(Number);
  for (let i = 0; i < 3; i++) {
    if (pa[i] < pb[i]) return -1;
    if (pa[i] > pb[i]) return 1;
  }
  return 0;
}

function getLatestPublishedVersionFromCache(elmCache: string, packageName: string): string | null {
  try {
    const [author, project] = packageName.split("/", 2);
    const pkgDir = path.join(elmCache, author, project);
    if (!fs.existsSync(pkgDir)) return null;

    const versions = fs.readdirSync(pkgDir).filter((v) => /^\d+\.\d+\.\d+$/.test(v));
    if (versions.length === 0) return null;

    return versions.sort(compareVersionsLocal).pop()!;
  } catch {
    return null;
  }
}

async function getLatestPublishedVersion(elmCache: string, packageName: string): Promise<string | null> {
  // Try local cache first
  const cached = getLatestPublishedVersionFromCache(elmCache, packageName);
  if (cached) return cached;

  // Fall back to fetching from the Elm package registry
  try {
    const res = await fetch(
      `https://package.elm-lang.org/packages/${packageName}/releases.json`,
      { headers: { "User-Agent": "elm-docs-server" } }
    );
    if (!res.ok) return null;
    const releases = (await res.json()) as Record<string, number>;
    const versions = Object.keys(releases);
    if (versions.length === 0) return null;
    return versions.sort(compareVersionsLocal).pop()!;
  } catch {
    return null;
  }
}

function loadPublishedDocsFromCache(elmCache: string, name: string, version: string): DocsModule[] | null {
  try {
    const docsPath = path.join(elmCache, name, version, "docs.json");
    if (!fs.existsSync(docsPath)) return null;
    return JSON.parse(fs.readFileSync(docsPath, "utf-8")) as DocsModule[];
  } catch {
    return null;
  }
}

async function fetchPublishedDocs(name: string, version: string): Promise<DocsModule[] | null> {
  try {
    const res = await fetch(
      `https://package.elm-lang.org/packages/${name}/${version}/docs.json`,
      { headers: { "User-Agent": "elm-docs-server" } }
    );
    if (!res.ok) return null;
    return (await res.json()) as DocsModule[];
  } catch {
    return null;
  }
}

async function loadPublishedDocs(elmCache: string, name: string, version: string): Promise<DocsModule[] | null> {
  return loadPublishedDocsFromCache(elmCache, name, version)
    ?? await fetchPublishedDocs(name, version);
}

function loadPublishedReadmeFromCache(elmCache: string, name: string, version: string): string | null {
  try {
    const readmePath = path.join(elmCache, name, version, "README.md");
    if (!fs.existsSync(readmePath)) return null;
    return fs.readFileSync(readmePath, "utf-8");
  } catch {
    return null;
  }
}

async function fetchPublishedReadme(name: string, version: string): Promise<string | null> {
  try {
    const res = await fetch(
      `https://package.elm-lang.org/packages/${name}/${version}/README.md`,
      { headers: { "User-Agent": "elm-docs-server" } }
    );
    if (!res.ok) return null;
    return await res.text();
  } catch {
    return null;
  }
}

async function loadPublishedReadme(elmCache: string, name: string, version: string): Promise<string | null> {
  return loadPublishedReadmeFromCache(elmCache, name, version)
    ?? await fetchPublishedReadme(name, version);
}

function computeLineDiff(oldText: string, newText: string): DiffLine[] | null {
  const changes = diffLines(oldText, newText);
  const hasChanges = changes.some((c) => c.added || c.removed);
  if (!hasChanges) return null;

  const result: DiffLine[] = [];
  for (const change of changes) {
    const status = change.added ? 1 : change.removed ? -1 : 0;
    const lines = change.value.replace(/\n$/, "").split("\n");
    for (const line of lines) {
      result.push([status, line]);
    }
  }
  return result;
}

function formatAnnotation(item: { name: string; type: string }): string {
  return `${item.name} : ${item.type}`;
}

function formatAliasAnnotation(alias: { name: string; args: string[]; type: string }): string {
  const argsStr = alias.args.length > 0 ? " " + alias.args.join(" ") : "";
  return `type alias ${alias.name}${argsStr} = ${alias.type}`;
}

function formatUnionAnnotation(union: { name: string; args: string[]; cases: Array<[string, string[]]> }): string {
  const argsStr = union.args.length > 0 ? " " + union.args.join(" ") : "";
  const ctors = union.cases.map(([name, types]) => {
    if (types.length === 0) return name;
    return name + " " + types.join(" ");
  }).join(" | ");
  return `type ${union.name}${argsStr} = ${ctors}`;
}

function computeContentDiff(
  publishedDocs: DocsModule[] | null,
  currentDocs: DocsModule[] | null,
  publishedReadme: string | null,
  currentReadme: string | null
): ContentDiff | null {
  if (!publishedDocs || !currentDocs) return null;

  const publishedByName = new Map<string, DocsModule>();
  for (const mod of publishedDocs) {
    publishedByName.set(mod.name, mod);
  }

  const diff: ContentDiff = { modules: {} };
  let hasAnyDiff = false;

  for (const currentMod of currentDocs) {
    const publishedMod = publishedByName.get(currentMod.name);
    if (!publishedMod) continue;

    const moduleDiff: ModuleContentDiff = { items: {} };
    let moduleHasDiff = false;

    for (const currentVal of currentMod.values) {
      const publishedVal = publishedMod.values.find((v) => v.name === currentVal.name);
      if (!publishedVal) continue;

      const itemDiff: ItemContentDiff = {};
      let itemHasDiff = false;

      if (publishedVal.comment !== currentVal.comment) {
        const commentDiff = computeLineDiff(publishedVal.comment, currentVal.comment);
        if (commentDiff) {
          itemDiff.commentDiff = commentDiff;
          itemHasDiff = true;
        }
      }

      if (publishedVal.type !== currentVal.type) {
        itemDiff.oldAnnotation = formatAnnotation(publishedVal);
        itemHasDiff = true;
      }

      if (itemHasDiff) {
        moduleDiff.items[currentVal.name] = itemDiff;
        moduleHasDiff = true;
      }
    }

    for (const currentBinop of currentMod.binops) {
      const publishedBinop = publishedMod.binops.find((b) => b.name === currentBinop.name);
      if (!publishedBinop) continue;

      const itemDiff: ItemContentDiff = {};
      let itemHasDiff = false;

      if (publishedBinop.comment !== currentBinop.comment) {
        const commentDiff = computeLineDiff(publishedBinop.comment, currentBinop.comment);
        if (commentDiff) {
          itemDiff.commentDiff = commentDiff;
          itemHasDiff = true;
        }
      }

      if (publishedBinop.type !== currentBinop.type) {
        itemDiff.oldAnnotation = formatAnnotation(publishedBinop);
        itemHasDiff = true;
      }

      if (itemHasDiff) {
        moduleDiff.items[currentBinop.name] = itemDiff;
        moduleHasDiff = true;
      }
    }

    for (const currentAlias of currentMod.aliases) {
      const publishedAlias = publishedMod.aliases.find((a) => a.name === currentAlias.name);
      if (!publishedAlias) continue;

      const itemDiff: ItemContentDiff = {};
      let itemHasDiff = false;

      if (publishedAlias.comment !== currentAlias.comment) {
        const commentDiff = computeLineDiff(publishedAlias.comment, currentAlias.comment);
        if (commentDiff) {
          itemDiff.commentDiff = commentDiff;
          itemHasDiff = true;
        }
      }

      if (publishedAlias.type !== currentAlias.type || JSON.stringify(publishedAlias.args) !== JSON.stringify(currentAlias.args)) {
        itemDiff.oldAnnotation = formatAliasAnnotation(publishedAlias);
        itemHasDiff = true;
      }

      if (itemHasDiff) {
        moduleDiff.items[currentAlias.name] = itemDiff;
        moduleHasDiff = true;
      }
    }

    for (const currentUnion of currentMod.unions) {
      const publishedUnion = publishedMod.unions.find((u) => u.name === currentUnion.name);
      if (!publishedUnion) continue;

      const itemDiff: ItemContentDiff = {};
      let itemHasDiff = false;

      if (publishedUnion.comment !== currentUnion.comment) {
        const commentDiff = computeLineDiff(publishedUnion.comment, currentUnion.comment);
        if (commentDiff) {
          itemDiff.commentDiff = commentDiff;
          itemHasDiff = true;
        }
      }

      if (JSON.stringify(publishedUnion.cases) !== JSON.stringify(currentUnion.cases) || JSON.stringify(publishedUnion.args) !== JSON.stringify(currentUnion.args)) {
        itemDiff.oldAnnotation = formatUnionAnnotation(publishedUnion);
        itemHasDiff = true;
      }

      if (itemHasDiff) {
        moduleDiff.items[currentUnion.name] = itemDiff;
        moduleHasDiff = true;
      }
    }

    if (moduleHasDiff) {
      diff.modules[currentMod.name] = moduleDiff;
      hasAnyDiff = true;
    }
  }

  if (publishedReadme != null && currentReadme != null && publishedReadme !== currentReadme) {
    const readmeDiff = computeLineDiff(publishedReadme, currentReadme);
    if (readmeDiff) {
      diff.readmeDiff = readmeDiff;
      hasAnyDiff = true;
    }
  }

  return hasAnyDiff ? diff : null;
}

class DocServer {
  options: Options;
  private elm: Elm;
  private elmVersion: string;
  private elmCache: string;
  private app: expressWs.Application;
  private ws: expressWs.Instance;
  private wss: ws.Server;
  private manifest: Manifest | null;

  constructor(options?: Options) {
    const {
      address = "127.0.0.1",
      dir = ".",
      port = 8000,
      browser = true,
      reload = true,
      debug = false,
    } = options || {};
    this.options = {
      address,
      browser,
      debug,
      dir: fs.lstatSync(dir).isFile() ? path.dirname(dir) : path.resolve(dir),
      port,
      reload,
    };

    try {
      process.chdir(this.options.dir);
    } catch (err) {
      error(err);
    }

    [this.elm, this.elmVersion] = getElm();
    this.elmCache = getElmCache(this.elmVersion);
    let app = express();
    this.ws = expressWs(app);
    this.app = this.ws.app;
    this.wss = this.ws.getWss();
    this.manifest = getManifestSync("elm.json");

    info(
      chalk.bold(`elm-doc-preview ${version}`),
      `using elm ${this.elmVersion}`
    );
    if (this.manifest && this.manifest.name && this.manifest.version) {
      info(
        "Previewing",
        chalk.magenta(`${this.manifest.name} ${this.manifest.version}`),
        `from ${this.options.dir}`
      );
    } else {
      info(
        `No package or application found in ${this.options.dir},`,
        "running documentation server only"
      );
    }

    this.setupWebServer();
    if (this.manifest && this.options.reload) {
      this.setupFilesWatcher();
    }
  }

  private setupWebServer() {
    const filename = fileURLToPath(import.meta.url);
    const dirname = path.dirname(filename);

    this.app.use(
      "/",
      express.static(path.join(dirname, "../static"), {
        index: "../static/index.html",
      })
    );

    // websockets
    this.app.ws("/", (socket, req) => {
      info(`  |> ${req.socket.remoteAddress} connected`);
      this.sendDiff();
      socket.on("close", () => {
        info("  |> client disconnected");
      });
    });

    // preview
    this.app.get("/preview", (_req, res) => {
      if (this.manifest) {
        res.json(this.manifest);
      }
    });
    // search.json
    this.app.get("/search.json", (_req, res) => {
      Promise.all(
        [`${this.elmCache}/*/*/*`, "."].map((pattern) =>
          searchPackages(pattern)
        )
      )
        .then((packagesArray) => {
          // add/overwrite cache with project
          res.json(Object.values(merge(packagesArray)));
        })
        .catch((err) => error(err));
    });

    // releases.json
    this.app.get("/packages/:author/:project/releases.json", (req, res) => {
      const p = req.params;
      const name = `${p.author}/${p.project}`;
      let dirs = [`${this.elmCache}/${name}/*`];
      if (this.manifest && this.manifest.name === name) {
        dirs.push(".");
      }
      Promise.all(dirs.map((dir) => packageReleases(dir)))
        .then((releasesArray) => {
          // add/overwrite cache with project
          res.json(merge(releasesArray));
        })
        .catch((err) => error(err));
    });

    // docs.json
    this.app.get(
      "/packages/:author/:project/:version/docs.json",
      (req, res) => {
        const p = req.params;
        const name = `${p.author}/${p.project}/${p.version}`;
        if (this.manifest && fullname(this.manifest) === name) {
          res.json(
            buildDocs(this.manifest, ".", this.elm, !this.options.debug)
          );
        } else {
          res.sendFile(path.resolve(this.elmCache, name, "docs.json"));
        }
      }
    );

    // Serve README.md files
    this.app.get(
      "/packages/:author/:project/:version/README.md",
      (req, res) => {
        const p = req.params;
        const name = [p.author, p.project, p.version].join("/");
        if (this.manifest && fullname(this.manifest) === name) {
          res.sendFile(path.resolve(".", "README.md"));
        } else {
          res.sendFile(path.resolve(this.elmCache, name, "README.md"));
        }
      }
    );

    // Serve elm.json files
    this.app.get("/packages/:author/:project/:version/elm.json", (req, res) => {
      const p = req.params;
      const name = `${p.author}/${p.project}/${p.version}`;
      if (this.manifest && fullname(this.manifest) === name) {
        const manifest = getManifestSync("elm.json");
        res.json(manifest);
      } else {
        res.sendFile(path.resolve(this.elmCache, name, "elm.json"));
      }
    });

    let setHeaders = (res: express.Response, path: string, _stat: any) => {
      if (path.endsWith("/LICENSE")) {
        res.setHeader("Content-Type", "text/plain; charset=UTF-8");
      }
    };
    // Serve project source
    if (this.manifest) {
      this.app.use(
        `/source/${fullname(this.manifest)}`,
        express.static(".", { setHeaders: setHeaders }),
        serveIndex(".", { icons: true })
      );
    }
    // Serve cached packages source
    this.app.use(
      "/source",
      express.static(this.elmCache, { setHeaders: setHeaders }),
      serveIndex(this.elmCache, { icons: true })
    );

    // default route
    this.app.get("*", (_req, res) => {
      res.sendFile(path.join(dirname, "../static/index.html"));
    });
  }

  private setupFilesWatcher() {
    // We use glob patterns to avoid https://github.com/paulmillr/chokidar/issues/237.
    // We want to watch ["elm.json", "elm-application.json", "README.md"].
    const glob = ["elm*.json", "README*.md"];
    if (this.manifest && this.manifest["source-directories"]) {
      this.manifest["source-directories"].forEach((src) => {
        glob.push(src + "/**/*.elm");
        glob.push(path.normalize(src + "/../elm.json"));
      });
    } else if (this.manifest) {
      glob.push("src/**/*.elm");
    }
    const watcher = chokidar.watch(glob, {
      ignored: ["**/node_modules", "**/elm-stuff", "**/.git"],
      ignoreInitial: true,
      atomic: true,
    });

    watcher
      .on("all", (_event, filepath) => this.onChange(filepath))
      .on("error", (err) => error(err))
      .on("ready", () => {
        if (this.manifest && this.manifest.type === "package") {
          info(`  |> watching package`);
        } else if (this.manifest && this.manifest.type === "application") {
          info(`  |> watching application`);
        }
        if (this.options.debug) {
          info(watcher.getWatched());
        }
      });
  }

  private onChange(filepath: string) {
    info("  |>", "detected", filepath, "modification");
    if (filepath == "README.md") {
      this.sendReadme();
      this.sendDiff();
    } else if (filepath.endsWith(".json")) {
      this.manifest = getManifestSync("elm.json");
      this.sendManifest();
      this.sendDocs();
      this.sendDiff();
    } else {
      this.sendDocs();
      this.sendDiff();
    }
  }

  private sendReadme() {
    const readme = path.join(this.options.dir, "README.md");
    if (
      this.manifest &&
      this.manifest.name &&
      this.manifest.name.includes("/")
    ) {
      const [author, project] = this.manifest.name.split("/", 2);
      try {
        info("  |>", "sending README");
        this.broadcast({
          type: "readme",
          data: {
            author: author,
            project: project,
            version: this.manifest.version,
            readme: fs.readFileSync(readme).toString(),
          },
        });
      } catch (err) {
        error(err);
      }
    }
  }

  private sendManifest() {
    if (
      this.manifest &&
      this.manifest.name &&
      this.manifest.name.includes("/")
    ) {
      const [author, project] = this.manifest.name.split("/", 2);
      info("  |>", "sending Manifest");
      this.broadcast({
        type: "manifest",
        data: {
          author: author,
          project: project,
          version: this.manifest.version,
          manifest: this.manifest,
        },
      });
    }
  }

  private sendDocs() {
    if (
      this.manifest &&
      this.manifest.name &&
      this.manifest.name.includes("/")
    ) {
      const docs = buildDocs(
        this.manifest,
        this.options.dir,
        this.elm,
        !this.options.debug
      );
      const [author, project] = this.manifest.name.split("/", 2);
      info("  |>", "sending Docs");
      this.broadcast({
        type: "docs",
        data: {
          author: author,
          project: project,
          version: this.manifest.version,
          time: this.manifest.timestamp,
          docs: docs,
        },
      });
    }
  }

  private async sendDiff() {
    if (
      this.manifest &&
      this.manifest.type === "package" &&
      this.manifest.name &&
      this.manifest.name.includes("/")
    ) {
      const [author, project] = this.manifest.name.split("/", 2);
      info("  |>", "running elm diff");
      const diff = buildDiff(this.elm);

      // Compute content diff (doc comments + README)
      let contentDiff: ContentDiff | null = null;
      if (this.manifest.name) {
        const latestVersion = await getLatestPublishedVersion(this.elmCache, this.manifest.name);
        if (latestVersion) {
          info("  |>", "computing content diff against", this.manifest.name, latestVersion);
          const [publishedDocs, publishedReadme] = await Promise.all([
            loadPublishedDocs(this.elmCache, this.manifest.name, latestVersion),
            loadPublishedReadme(this.elmCache, this.manifest.name, latestVersion),
          ]);
          const currentDocs = buildDocs(this.manifest, this.options.dir, this.elm, true);
          let currentReadme: string | null = null;
          try {
            currentReadme = fs.readFileSync(path.join(this.options.dir, "README.md"), "utf-8");
          } catch {}
          contentDiff = computeContentDiff(
            publishedDocs,
            Array.isArray(currentDocs) ? currentDocs as DocsModule[] : null,
            publishedReadme,
            currentReadme
          );
          info("  |>", "content diff:", contentDiff ? "found changes" : "no changes");
        } else {
          info("  |>", "no published version found for content diff");
        }
      }

      info("  |>", "sending Diff");
      this.broadcast({
        type: "diff",
        data: {
          author: author,
          project: project,
          version: this.manifest.version,
          diff: diff,
          contentDiff: contentDiff,
        },
      });
    }
  }

  listen() {
    return this.app.listen(this.options.port, this.options.address, () => {
      if (
        this.options.browser &&
        this.manifest &&
        this.manifest.name &&
        this.manifest.version
      ) {
        open(
          `http://localhost:${this.options.port}/packages/${this.manifest.name}/${this.manifest.version}/`
        );
      } else if (this.options.browser) {
        open(`http://localhost:${this.options.port}`);
      }
      info(
        chalk.blue("Browse"),
        chalk.bold.green(`http://localhost:${this.options.port.toString()}`),
        chalk.blue("to see your documentation")
      );
    });
  }

  make(filename: string) {
    if (this.manifest) {
      const docs = buildDocs(
        this.manifest,
        this.options.dir,
        this.elm,
        !this.options.debug,
        true
      );
      info(`  |> writing documentation into ${filename}`);
      if (Array.isArray(docs) && docs.length > 0) {
        if (filename !== "/dev/null") {
          try {
            fs.writeFileSync(filename, JSON.stringify(docs), "utf8");
          } catch (err) {
            fatal(err);
          }
        }
        process.exit(0);
      } else {
        fatal("failed to build project documentation");
      }
    }
  }

  private broadcast(obj: object) {
    this.wss.clients.forEach((client) => {
      if (client.readyState === ws.OPEN) {
        client.send(JSON.stringify(obj));
      }
    });
  }
}

export default DocServer;
