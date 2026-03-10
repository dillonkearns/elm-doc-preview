import type { VercelRequest, VercelResponse } from "@vercel/node";
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import os from "os";
import tar from "tar";

const TMP_DIR = os.tmpdir();
const ELM_HOME = path.join(TMP_DIR, ".elm-home");
const SEED_DIR = path.join(__dirname, "..", "elm-home-seed");

function restoreSeed(): void {
  // Skip if ELM_HOME already populated (warm container)
  if (fs.existsSync(path.join(ELM_HOME, "0.19.1", "packages", "registry.dat"))) {
    return;
  }
  // Skip if no bundled seed (local dev without build step)
  if (!fs.existsSync(SEED_DIR)) {
    return;
  }
  fs.cpSync(SEED_DIR, ELM_HOME, { recursive: true });
}

function elmBinPath(): string {
  const candidates = [
    // Relative to function file (Vercel bundles includeFiles here)
    path.join(__dirname, "..", "node_modules", "elm", "bin", "elm"),
    // Relative to cwd
    path.join(process.cwd(), "node_modules", "elm", "bin", "elm"),
  ];

  for (const p of candidates) {
    if (fs.existsSync(p)) return p;
  }

  // Fallback: check if elm is on PATH
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

  // If ref is already a full SHA, check warm cache immediately (zero network)
  if (isFullSha) {
    const workDir = path.join(TMP_DIR, `elm-docs-${ref}`);
    if (fs.existsSync(path.join(workDir, "elm.json"))) {
      return { sha: ref, workDir };
    }
  }

  // Fire resolve and download in parallel.
  // Use codeload.github.com directly to skip the 302 redirect from github.com/archive/.
  const tarballUrl = `https://codeload.github.com/${owner}/${repo}/tar.gz/${ref}`;
  const [sha, tarballRes] = await Promise.all([
    isFullSha ? ref : resolveRef(owner, repo, ref),
    fetch(tarballUrl, { headers: { "User-Agent": "elm-docs-server" } }),
  ]);

  const workDir = path.join(TMP_DIR, `elm-docs-${sha}`);

  // Check warm cache now that we know the SHA
  if (fs.existsSync(path.join(workDir, "elm.json"))) {
    return { sha, workDir };
  }

  if (!tarballRes.ok) {
    throw new Error(`Failed to download tarball: ${tarballRes.status}`);
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

  // If docs.json wasn't created, try to get error info
  const stderr = result.stderr?.toString() || "";
  const stdout = result.stdout?.toString() || "";
  throw new Error(
    `elm make failed (exit ${result.status}): ${stderr || stdout}`
  );
}

// -- Diff computation (compare two docs.json arrays) --

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

function compareVersions(a: string, b: string): number {
  const pa = a.split(".").map(Number);
  const pb = b.split(".").map(Number);
  for (let i = 0; i < 3; i++) {
    if (pa[i] < pb[i]) return -1;
    if (pa[i] > pb[i]) return 1;
  }
  return 0;
}

// Fetch published docs.json from the Elm registry and compute diff against current docs.
// Returns null if the package isn't published, this is a historical commit, or fetching fails.
async function computeDiffFromRegistry(
  workDir: string,
  currentDocs: DocsModule[]
): Promise<ApiDiff | null> {
  try {
    const elmJson = JSON.parse(
      fs.readFileSync(path.join(workDir, "elm.json"), "utf-8")
    );

    // Applications don't have published versions
    if (elmJson.type !== "package") return null;

    const name: string = elmJson.name;
    const version: string = elmJson.version;

    // Fetch published releases from the Elm package registry
    const releasesRes = await fetch(
      `https://package.elm-lang.org/packages/${name}/releases.json`,
      { headers: { "User-Agent": "elm-docs-server" } }
    );

    if (!releasesRes.ok) {
      // Package never published
      return null;
    }

    const releases = (await releasesRes.json()) as Record<string, number>;
    const publishedVersions = Object.keys(releases);

    if (publishedVersions.length === 0) return null;

    const latest = publishedVersions.sort(compareVersions).pop()!;

    // If elm.json version is behind the latest published version,
    // this is a historical commit — skip diff to avoid confusing reverse results.
    if (compareVersions(version, latest) < 0) {
      return null;
    }

    // Fetch the published docs.json for the latest version
    const docsRes = await fetch(
      `https://package.elm-lang.org/packages/${name}/${latest}/docs.json`,
      { headers: { "User-Agent": "elm-docs-server" } }
    );

    if (!docsRes.ok) {
      return null;
    }

    const publishedDocs = (await docsRes.json()) as DocsModule[];
    return computeDiff(publishedDocs, currentDocs);
  } catch {
    return null;
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
    // Step 1: Fire PR lookup immediately — response arrives during later blocking work
    const prPromise = lookupPullRequest(owner, repo, ref);

    // Step 2: Resolve ref + download source
    const { sha, workDir } = await fetchSource(owner, repo, ref);
    console.log(`Source ready for ${owner}/${repo}@${sha}`);

    // Step 3: Build docs (spawnSync blocks event loop ~400-800ms)
    const docs = buildDocs(workDir);
    console.log(`Docs built successfully`);

    // Step 4: Read README.md from the repo (best-effort)
    const readmePath = path.join(workDir, "README.md");
    const readme = fs.existsSync(readmePath)
      ? fs.readFileSync(readmePath, "utf-8")
      : null;

    // Step 5: Compute diff by fetching published docs.json from the Elm registry
    // and comparing against the just-built docs.
    const diff = await computeDiffFromRegistry(workDir, docs as DocsModule[]);
    console.log(`Diff: ${diff ? diff.magnitude : "none"}`);

    // Step 6: Await PR lookup
    const pullRequestUrl = await prPromise;
    console.log(`PR: ${pullRequestUrl || "none"}`);

    // If the ref is a full commit SHA, cache forever (immutable).
    // Otherwise (branch/tag), use a short TTL so new pushes are picked up.
    const isFullSha = /^[0-9a-f]{40}$/i.test(ref);
    if (isFullSha) {
      res.setHeader("Cache-Control", "s-maxage=31536000, immutable");
    } else {
      res.setHeader("Cache-Control", "s-maxage=60, stale-while-revalidate=300");
    }

    return res.status(200).json({ docs, diff, pullRequestUrl, readme });
  } catch (err: unknown) {
    const message = err instanceof Error ? err.message : String(err);
    console.error(`Error: ${message}`);
    return res.status(500).json({ error: message });
  }
}
