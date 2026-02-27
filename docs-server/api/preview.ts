import type { VercelRequest, VercelResponse } from "@vercel/node";
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import os from "os";
import tar from "tar";

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
      while (i < lines.length && !lines[i].match(/^-{4,}/)) {
        if (lines[i].match(/^\s+\S/)) {
          diff.addedModules.push(lines[i].trim());
        }
        i++;
      }
    } else if (line.match(/^-+ REMOVED MODULES/)) {
      i++;
      while (i < lines.length && !lines[i].match(/^-{4,}/)) {
        if (lines[i].match(/^\s+\S/)) {
          diff.removedModules.push(lines[i].trim());
        }
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

async function shouldRunDiff(workDir: string): Promise<boolean> {
  try {
    const elmJson = JSON.parse(
      fs.readFileSync(path.join(workDir, "elm.json"), "utf-8")
    );

    // Applications don't have published versions — elm diff will fail on its own
    if (elmJson.type !== "package") return true;

    const name: string = elmJson.name;
    const version: string = elmJson.version;

    // Fetch published releases from the Elm package registry
    const res = await fetch(
      `https://package.elm-lang.org/packages/${name}/releases.json`,
      { headers: { "User-Agent": "elm-docs-server" } }
    );

    if (!res.ok) {
      // Package never published — diff is meaningful
      return true;
    }

    const releases = (await res.json()) as Record<string, number>;
    const publishedVersions = Object.keys(releases);

    if (publishedVersions.length === 0) return true;

    const latest = publishedVersions.sort(compareVersions).pop()!;

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

    // Step 3: Build docs + look up PR in parallel
    const docs = buildDocs(workDir);
    console.log(`Docs built successfully`);

    // Step 4: Read README.md from the repo (best-effort)
    const readmePath = path.join(workDir, "README.md");
    const readme = fs.existsSync(readmePath)
      ? fs.readFileSync(readmePath, "utf-8")
      : null;

    // Step 5: Build diff (best-effort, null on failure) + PR lookup
    // Skip diff for historical commits where a newer version is already published,
    // since elm diff compares against the latest published version and would
    // produce confusing reverse results.
    const [runDiff, pullRequestUrl] = await Promise.all([
      shouldRunDiff(workDir),
      lookupPullRequest(owner, repo, ref),
    ]);
    const diff = runDiff ? buildDiff(workDir) : null;
    console.log(`Diff: ${runDiff ? (diff ? diff.magnitude : "no changes") : "skipped (historical commit)"}`);
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
