import type { VercelRequest, VercelResponse } from "@vercel/node";
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import os from "os";
import tar from "tar";

const TMP_DIR = os.tmpdir();
const ELM_HOME = path.join(TMP_DIR, ".elm-home");
const SEED_DIR = path.join(__dirname, "..", "elm-home-seed");

// -- Shared infrastructure (same as preview.ts) --

function restoreSeed(): void {
  if (fs.existsSync(path.join(ELM_HOME, "0.19.1", "packages", "registry.dat"))) {
    return;
  }
  if (!fs.existsSync(SEED_DIR)) {
    return;
  }
  fs.cpSync(SEED_DIR, ELM_HOME, { recursive: true });
}

function elmBinPath(): string {
  const candidates = [
    path.join(__dirname, "..", "node_modules", "elm", "bin", "elm"),
    path.join(process.cwd(), "node_modules", "elm", "bin", "elm"),
  ];

  for (const p of candidates) {
    if (fs.existsSync(p)) return p;
  }

  const which = spawnSync("which", ["elm"]);
  if (which.status === 0) return which.stdout.toString().trim();

  throw new Error(
    `Elm binary not found. Searched: ${candidates.join(", ")}; cwd=${process.cwd()}, __dirname=${__dirname}`
  );
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

async function fetchSource(
  owner: string,
  repo: string,
  ref: string
): Promise<{ sha: string; workDir: string }> {
  const isFullSha = /^[0-9a-f]{40}$/i.test(ref);

  if (isFullSha) {
    const workDir = path.join(TMP_DIR, `elm-docs-${ref}`);
    if (fs.existsSync(path.join(workDir, "elm.json"))) {
      return { sha: ref, workDir };
    }
  }

  const tarballUrl = `https://codeload.github.com/${owner}/${repo}/tar.gz/${ref}`;
  const [sha, tarballRes] = await Promise.all([
    isFullSha ? ref : resolveRef(owner, repo, ref),
    fetch(tarballUrl, { headers: { "User-Agent": "elm-docs-server" } }),
  ]);

  const workDir = path.join(TMP_DIR, `elm-docs-${sha}`);

  if (fs.existsSync(path.join(workDir, "elm.json"))) {
    return { sha, workDir };
  }

  if (!tarballRes.ok) {
    throw new Error(`Failed to download tarball for '${ref}': ${tarballRes.status}`);
  }

  const tarPath = path.join(TMP_DIR, `${sha}.tar.gz`);
  const arrayBuf = await tarballRes.arrayBuffer();
  fs.writeFileSync(tarPath, new Uint8Array(arrayBuf));

  fs.mkdirSync(workDir, { recursive: true });
  await tar.extract({ file: tarPath, cwd: workDir, strip: 1 });
  fs.unlinkSync(tarPath);

  return { sha, workDir };
}

function buildDocs(workDir: string): object | null {
  const elm = elmBinPath();
  const docsPath = path.join(workDir, "docs.json");

  restoreSeed();

  const result = spawnSync(elm, ["make", `--docs=${docsPath}`], {
    cwd: workDir,
    env: { ...process.env, ELM_HOME },
    timeout: 55000,
  });

  if (fs.existsSync(docsPath)) {
    return JSON.parse(fs.readFileSync(docsPath, "utf-8"));
  }

  const stderr = result.stderr?.toString() || "";
  const stdout = result.stdout?.toString() || "";
  throw new Error(
    `elm make failed (exit ${result.status}): ${stderr || stdout}`
  );
}

// -- Diff computation --

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

interface DocsModule {
  name: string;
  unions: { name: string; args: unknown; cases: unknown }[];
  aliases: { name: string; args: unknown; type: unknown }[];
  values: { name: string; type: unknown }[];
  binops: { name: string; type: unknown; associativity: string; precedence: number }[];
}

function computeDiff(
  baseDocs: DocsModule[],
  headDocs: DocsModule[]
): ApiDiff | null {
  const baseByName = new Map(baseDocs.map((m) => [m.name, m]));
  const headByName = new Map(headDocs.map((m) => [m.name, m]));

  const addedModules = headDocs
    .filter((m) => !baseByName.has(m.name))
    .map((m) => m.name);

  const removedModules = baseDocs
    .filter((m) => !headByName.has(m.name))
    .map((m) => m.name);

  const changedModules: ApiModuleChanges[] = [];

  for (const headMod of headDocs) {
    const baseMod = baseByName.get(headMod.name);
    if (!baseMod) continue;

    const changes = diffModule(baseMod, headMod);
    if (changes) {
      changedModules.push(changes);
    }
  }

  if (
    addedModules.length === 0 &&
    removedModules.length === 0 &&
    changedModules.length === 0
  ) {
    return null;
  }

  const hasRemovals =
    removedModules.length > 0 ||
    changedModules.some((mc) => mc.removed.length > 0);

  const hasAdditions =
    addedModules.length > 0 ||
    changedModules.some((mc) => mc.added.length > 0);

  const magnitude = hasRemovals ? "MAJOR" : hasAdditions ? "MINOR" : "PATCH";

  return { magnitude, addedModules, removedModules, changedModules };
}

function diffModule(
  baseMod: DocsModule,
  headMod: DocsModule
): ApiModuleChanges | null {
  const added: string[] = [];
  const changed: string[] = [];
  const removed: string[] = [];

  diffItems(baseMod.unions, headMod.unions, (u) => u.name,
    (a, b) => jsonEq(a.args, b.args) && jsonEq(a.cases, b.cases),
    added, changed, removed);

  diffItems(baseMod.aliases, headMod.aliases, (a) => a.name,
    (a, b) => jsonEq(a.args, b.args) && jsonEq(a.type, b.type),
    added, changed, removed);

  diffItems(baseMod.values, headMod.values, (v) => v.name,
    (a, b) => jsonEq(a.type, b.type),
    added, changed, removed);

  diffItems(baseMod.binops, headMod.binops, (b) => b.name,
    (a, b) => jsonEq(a.type, b.type) && a.associativity === b.associativity && a.precedence === b.precedence,
    added, changed, removed);

  if (added.length === 0 && changed.length === 0 && removed.length === 0) {
    return null;
  }

  return { name: baseMod.name, added, changed, removed };
}

function diffItems<T>(
  baseItems: T[], headItems: T[],
  getName: (item: T) => string,
  isEqual: (a: T, b: T) => boolean,
  added: string[], changed: string[], removed: string[]
): void {
  const baseByName = new Map(baseItems.map((item) => [getName(item), item]));
  const headByName = new Map(headItems.map((item) => [getName(item), item]));

  for (const headItem of headItems) {
    const name = getName(headItem);
    const baseItem = baseByName.get(name);
    if (!baseItem) {
      added.push(name);
    } else if (!isEqual(baseItem, headItem)) {
      changed.push(name);
    }
  }

  for (const baseItem of baseItems) {
    if (!headByName.has(getName(baseItem))) {
      removed.push(getName(baseItem));
    }
  }
}

function jsonEq(a: unknown, b: unknown): boolean {
  return JSON.stringify(a) === JSON.stringify(b);
}

// -- Handler --

export default async function handler(
  req: VercelRequest,
  res: VercelResponse
) {
  if (req.method === "OPTIONS") {
    return res.status(200).end();
  }

  const { owner, repo, base, head } = req.query as {
    owner: string;
    repo: string;
    base: string;
    head: string;
  };

  if (!owner || !repo || !base || !head) {
    return res.status(400).json({
      error: "Missing parameters. URL format: /:owner/:repo/compare/:base/:head/compare.json",
    });
  }

  console.log(`Comparing ${owner}/${repo}: ${base}...${head}`);

  try {
    // Fetch both refs in parallel
    const [baseSource, headSource] = await Promise.all([
      fetchSource(owner, repo, base),
      fetchSource(owner, repo, head),
    ]);

    console.log(`Sources ready: base=${baseSource.sha.slice(0, 8)}, head=${headSource.sha.slice(0, 8)}`);

    // Build docs for both (sequential since they share ELM_HOME)
    const baseDocs = buildDocs(baseSource.workDir);
    console.log(`Base docs built`);

    const headDocs = buildDocs(headSource.workDir);
    console.log(`Head docs built`);

    // Read head README
    const readmePath = path.join(headSource.workDir, "README.md");
    const readme = fs.existsSync(readmePath)
      ? fs.readFileSync(readmePath, "utf-8")
      : null;

    // Compute diff — always return a diff object (never null) for compare
    const diff = computeDiff(baseDocs as DocsModule[], headDocs as DocsModule[]) ?? {
      magnitude: "PATCH",
      addedModules: [],
      removedModules: [],
      changedModules: [],
    };
    console.log(`Diff: ${diff.magnitude} (added=${diff.addedModules.length}, removed=${diff.removedModules.length}, changed=${diff.changedModules.length})`);

    // Cache based on whether both refs are full SHAs
    const bothFullSha =
      /^[0-9a-f]{40}$/i.test(base) && /^[0-9a-f]{40}$/i.test(head);
    if (bothFullSha) {
      res.setHeader("Cache-Control", "s-maxage=31536000, immutable");
    } else {
      res.setHeader("Cache-Control", "s-maxage=60, stale-while-revalidate=300");
    }

    return res.status(200).json({ docs: headDocs, diff, pullRequestUrl: null, readme });
  } catch (err: unknown) {
    const message = err instanceof Error ? err.message : String(err);
    console.error(`Error: ${message}`);
    return res.status(500).json({ error: message });
  }
}
