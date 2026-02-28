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

  const seedStart = Date.now();
  restoreSeed();
  console.log(`restoreSeed: ${Date.now() - seedStart}ms`);

  const makeStart = Date.now();
  const result = spawnSync(elm, ["make", `--docs=${docsPath}`], {
    cwd: workDir,
    env: { ...process.env, ELM_HOME },
    timeout: 55000,
  });

  console.log(`elm make: ${Date.now() - makeStart}ms`);

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
      error: "Missing parameters. URL format: /repos/{owner}/{repo}/{ref}/docs.json",
    });
  }

  console.log(`Building docs for ${owner}/${repo}@${ref}`);

  try {
    const t0 = Date.now();

    // Step 1: Resolve ref to commit SHA
    const sha = await resolveRef(owner, repo, ref);
    console.log(`Resolved ${ref} -> ${sha} (${Date.now() - t0}ms)`);

    // Step 2: Download and extract source
    const t1 = Date.now();
    const workDir = await downloadAndExtract(owner, repo, sha);
    console.log(`Source ready at ${workDir} (${Date.now() - t1}ms)`);

    // Step 3: Build docs
    const t2 = Date.now();
    const docs = buildDocs(workDir);
    console.log(`Docs built successfully (${Date.now() - t2}ms, total: ${Date.now() - t0}ms)`);

    // If the ref is a full commit SHA, cache forever (immutable).
    // Otherwise (branch/tag), use a short TTL so new pushes are picked up.
    const isFullSha = /^[0-9a-f]{40}$/i.test(ref);
    if (isFullSha) {
      res.setHeader("Cache-Control", "s-maxage=31536000, immutable");
    } else {
      res.setHeader("Cache-Control", "s-maxage=60, stale-while-revalidate=300");
    }

    res.setHeader("Server-Timing", `resolve;dur=${t1 - t0}, download;dur=${t2 - t1}, build;dur=${Date.now() - t2}, total;dur=${Date.now() - t0}`);
    return res.status(200).json(docs);
  } catch (err: unknown) {
    const message = err instanceof Error ? err.message : String(err);
    console.error(`Error: ${message}`);
    return res.status(500).json({ error: message });
  }
}
