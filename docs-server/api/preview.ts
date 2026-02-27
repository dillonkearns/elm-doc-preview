import type { VercelRequest, VercelResponse } from "@vercel/node";
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import os from "os";
import tar from "tar";
import { diffLines } from "diff";

const TMP_DIR = os.tmpdir();
const ELM_HOME = path.join(TMP_DIR, ".elm-home");

function elmBinPath(): string {
  // The elm npm package puts the binary here
  const local = path.join(process.cwd(), "node_modules", "elm", "bin", "elm");
  if (fs.existsSync(local)) return local;

  // Fallback: check if elm is on PATH
  const which = spawnSync("which", ["elm"]);
  if (which.status === 0) return which.stdout.toString().trim();

  throw new Error("Elm binary not found");
}

async function resolveRef(
  owner: string,
  repo: string,
  ref: string
): Promise<string> {
  const res = await fetch(
    `https://api.github.com/repos/${owner}/${repo}/commits/${ref}`,
    {
      headers: {
        Accept: "application/vnd.github.v3+json",
        "User-Agent": "elm-docs-server",
      },
    }
  );
  if (!res.ok) {
    throw new Error(
      `Failed to resolve ref '${ref}': GitHub API returned ${res.status}`
    );
  }
  const data = (await res.json()) as { sha: string };
  return data.sha;
}

async function downloadAndExtract(
  owner: string,
  repo: string,
  sha: string
): Promise<string> {
  const workDir = path.join(TMP_DIR, `elm-docs-${sha}`);

  // Already extracted from a previous invocation on this warm container
  if (fs.existsSync(path.join(workDir, "elm.json"))) {
    return workDir;
  }

  const tarballUrl = `https://github.com/${owner}/${repo}/archive/${sha}.tar.gz`;
  const tarPath = path.join(TMP_DIR, `${sha}.tar.gz`);

  const res = await fetch(tarballUrl, {
    headers: { "User-Agent": "elm-docs-server" },
    redirect: "follow",
  });

  if (!res.ok) {
    throw new Error(`Failed to download tarball: ${res.status}`);
  }

  const arrayBuf = await res.arrayBuffer();
  fs.writeFileSync(tarPath, new Uint8Array(arrayBuf));

  fs.mkdirSync(workDir, { recursive: true });
  await tar.extract({ file: tarPath, cwd: workDir, strip: 1 });
  fs.unlinkSync(tarPath);

  return workDir;
}

function buildDocs(workDir: string): object | null {
  const elm = elmBinPath();
  const docsPath = path.join(workDir, "docs.json");

  // Ensure ELM_HOME exists
  fs.mkdirSync(ELM_HOME, { recursive: true });

  const result = spawnSync(elm, ["make", `--docs=${docsPath}`], {
    cwd: workDir,
    env: { ...process.env, ELM_HOME },
    timeout: 55000,
  });

  if (fs.existsSync(docsPath)) {
    return JSON.parse(fs.readFileSync(docsPath, "utf-8"));
  }

  // If docs.json wasn't created, try to get error info
  const stderr = result.stderr?.toString() || "";
  const stdout = result.stdout?.toString() || "";
  throw new Error(
    `elm make failed (exit ${result.status}): ${stderr || stdout}`
  );
}

// -- Diff parsing (ported from lib/elm-doc-server.ts) --

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
            const cleanLine = sectionLine.replace(/^[+-]\s*/, "");
            const nameMatch = cleanLine.match(/^([a-zA-Z][a-zA-Z0-9_]*)\s+:/);
            if (nameMatch) {
              const itemName = nameMatch[1];
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

function compareVersions(a: string, b: string): number {
  const pa = a.split(".").map(Number);
  const pb = b.split(".").map(Number);
  for (let i = 0; i < 3; i++) {
    if (pa[i] < pb[i]) return -1;
    if (pa[i] > pb[i]) return 1;
  }
  return 0;
}

async function getLatestPublishedVersion(packageName: string): Promise<string | null> {
  try {
    const res = await fetch(
      `https://package.elm-lang.org/packages/${packageName}/releases.json`,
      { headers: { "User-Agent": "elm-docs-server" } }
    );

    if (!res.ok) return null;

    const releases = (await res.json()) as Record<string, number>;
    const publishedVersions = Object.keys(releases);

    if (publishedVersions.length === 0) return null;

    return publishedVersions.sort(compareVersions).pop()!;
  } catch {
    return null;
  }
}

async function shouldRunDiff(workDir: string): Promise<boolean> {
  try {
    const elmJson = JSON.parse(
      fs.readFileSync(path.join(workDir, "elm.json"), "utf-8")
    );

    // Applications don't have published versions — elm diff will fail on its own
    if (elmJson.type !== "package") return true;

    const name: string = elmJson.name;
    const version: string = elmJson.version;

    const latest = await getLatestPublishedVersion(name);
    if (!latest) return true;

    // If elm.json version is behind the latest published version,
    // this is a historical commit — the diff would compare against a
    // newer release and produce confusing "reverse" results.
    return compareVersions(version, latest) >= 0;
  } catch {
    // If anything fails, let elm diff run and handle errors itself
    return true;
  }
}

async function lookupPullRequest(
  owner: string,
  repo: string,
  ref: string
): Promise<string | null> {
  try {
    const res = await fetch(
      `https://api.github.com/repos/${owner}/${repo}/pulls?head=${owner}:${ref}&state=open&per_page=1`,
      {
        headers: {
          Accept: "application/vnd.github.v3+json",
          "User-Agent": "elm-docs-server",
        },
      }
    );
    if (!res.ok) return null;
    const pulls = (await res.json()) as Array<{ html_url: string }>;
    return pulls.length > 0 ? pulls[0].html_url : null;
  } catch {
    return null;
  }
}

function buildDiff(workDir: string): ApiDiff | null {
  try {
    const elm = elmBinPath();

    fs.mkdirSync(ELM_HOME, { recursive: true });

    const result = spawnSync(elm, ["diff"], {
      cwd: workDir,
      env: { ...process.env, ELM_HOME },
      input: "",
      timeout: 15000,
    });

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

type DiffLine = [number, string]; // [status, text] where -1=removed, 0=context, 1=added

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

function computeLineDiff(oldText: string, newText: string): DiffLine[] | null {
  const changes = diffLines(oldText, newText);
  const hasChanges = changes.some((c) => c.added || c.removed);
  if (!hasChanges) return null;

  const result: DiffLine[] = [];
  for (const change of changes) {
    const status = change.added ? 1 : change.removed ? -1 : 0;
    // Split into individual lines, removing trailing empty line from split
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

    // Compare values
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

    // Compare binops
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

    // Compare aliases
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

    // Compare unions
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

  // Compare README
  if (publishedReadme != null && currentReadme != null && publishedReadme !== currentReadme) {
    const readmeDiff = computeLineDiff(publishedReadme, currentReadme);
    if (readmeDiff) {
      diff.readmeDiff = readmeDiff;
      hasAnyDiff = true;
    }
  }

  return hasAnyDiff ? diff : null;
}

export default async function handler(
  req: VercelRequest,
  res: VercelResponse
) {
  if (req.method === "OPTIONS") {
    return res.status(200).end();
  }

  const { owner, repo, ref } = req.query as {
    owner: string;
    repo: string;
    ref: string;
  };

  if (!owner || !repo || !ref) {
    return res.status(400).json({
      error: "Missing parameters. URL format: /repos/{owner}/{repo}/{ref}/preview.json",
    });
  }

  console.log(`Building preview for ${owner}/${repo}@${ref}`);

  try {
    // Step 1: Resolve ref to commit SHA
    const sha = await resolveRef(owner, repo, ref);
    console.log(`Resolved ${ref} -> ${sha}`);

    // Step 2: Download and extract source
    const workDir = await downloadAndExtract(owner, repo, sha);
    console.log(`Source ready at ${workDir}`);

    // Step 3: Build docs + look up PR + get package info in parallel
    const docs = buildDocs(workDir);
    console.log(`Docs built successfully`);

    // Read elm.json to get package name for content diff
    let packageName: string | null = null;
    let currentReadme: string | null = null;
    try {
      const elmJson = JSON.parse(fs.readFileSync(path.join(workDir, "elm.json"), "utf-8"));
      if (elmJson.type === "package") {
        packageName = elmJson.name;
      }
    } catch {}
    try {
      currentReadme = fs.readFileSync(path.join(workDir, "README.md"), "utf-8");
    } catch {}

    // Step 4: Build diff (best-effort, null on failure) + PR lookup + content diff
    // Skip diff for historical commits where a newer version is already published,
    // since elm diff compares against the latest published version and would
    // produce confusing reverse results.
    const [runDiff, pullRequestUrl, latestVersion] = await Promise.all([
      shouldRunDiff(workDir),
      lookupPullRequest(owner, repo, ref),
      packageName ? getLatestPublishedVersion(packageName) : Promise.resolve(null),
    ]);
    const diff = runDiff ? buildDiff(workDir) : null;
    console.log(`Diff: ${runDiff ? (diff ? diff.magnitude : "no changes") : "skipped (historical commit)"}`);
    console.log(`PR: ${pullRequestUrl || "none"}`);

    // Step 5: Compute content diff (doc comments + README)
    let contentDiff: ContentDiff | null = null;
    if (packageName && latestVersion) {
      const [publishedDocs, publishedReadme] = await Promise.all([
        fetchPublishedDocs(packageName, latestVersion),
        fetchPublishedReadme(packageName, latestVersion),
      ]);
      contentDiff = computeContentDiff(
        publishedDocs,
        Array.isArray(docs) ? docs as DocsModule[] : null,
        publishedReadme,
        currentReadme
      );
      console.log(`Content diff: ${contentDiff ? "found changes" : "no changes"}`);
    }

    // If the ref is a full commit SHA, cache forever (immutable).
    // Otherwise (branch/tag), use a short TTL so new pushes are picked up.
    const isFullSha = /^[0-9a-f]{40}$/i.test(ref);
    if (isFullSha) {
      res.setHeader("Cache-Control", "s-maxage=31536000, immutable");
    } else {
      res.setHeader("Cache-Control", "s-maxage=60, stale-while-revalidate=300");
    }

    return res.status(200).json({ docs, diff, pullRequestUrl, contentDiff });
  } catch (err: unknown) {
    const message = err instanceof Error ? err.message : String(err);
    console.error(`Error: ${message}`);
    return res.status(500).json({ error: message });
  }
}
