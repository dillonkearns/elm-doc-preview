import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { execSync } from "child_process";
import { computeDiff as computeDiffLib } from "../lib/compute-diff.js";

/**
 * Build docs.json for an Elm application by creating a temporary package project.
 * Handles port module stubbing and incomplete documentation gracefully.
 */
export async function buildApplicationDocs(
  rawInput: unknown,
  _context: unknown
): Promise<string> {
  const input = rawInput as {
    sourceDirectories: string[];
    directDeps: Record<string, string>;
    projectDir: string;
  };
  const { sourceDirectories, directDeps, projectDir } = input;

  // Create temp directory
  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "elm-doc-tui-"));
  const tmpSrcDir = path.join(tmpDir, "src");
  fs.mkdirSync(tmpSrcDir);

  // Find and copy/stub all Elm files
  const exposedModules: string[] = [];

  for (const srcDir of sourceDirectories) {
    const resolvedSrcDir = path.resolve(projectDir, srcDir);
    const elmFiles = findElmFiles(resolvedSrcDir);

    for (const relPath of elmFiles) {
      const srcPath = path.join(resolvedSrcDir, relPath);
      const dstPath = path.join(tmpSrcDir, relPath);

      // Create directories
      fs.mkdirSync(path.dirname(dstPath), { recursive: true });

      let contents = fs.readFileSync(srcPath, "utf8");

      // Stub port modules
      if (/^port\s+module\s/m.test(contents)) {
        contents = contents.replace(
          /^port\s+([^\s:]+)([^\n]+)$/gm,
          (match, name, decl) => {
            if (name === "module") {
              return `module${decl}`;
            } else if (decl.includes("Sub")) {
              return `${name}${decl}\n${name} = always Sub.none\n`;
            } else if (decl.includes("Cmd")) {
              return `${name}${decl}\n${name} = always Cmd.none\n`;
            }
            return match;
          }
        );
        fs.writeFileSync(dstPath, contents);
      } else {
        // Symlink non-port modules
        try {
          fs.symlinkSync(srcPath, dstPath);
        } catch {
          fs.copyFileSync(srcPath, dstPath);
        }
      }

      // Add to exposed modules if it has documentation
      const moduleName = relPath
        .replace(/\.elm$/, "")
        .replace(/\//g, ".")
        .replace(/\\/g, ".");

      if (hasModuleDoc(contents)) {
        exposedModules.push(moduleName);
      }
    }
  }

  if (exposedModules.length === 0) {
    throw new Error("No documented modules found in source directories.");
  }

  // Build package elm.json
  const deps: Record<string, string> = {};
  for (const [name, version] of Object.entries(directDeps)) {
    const major = parseInt(version.split(".")[0]);
    deps[name] = `${version} <= v < ${major + 1}.0.0`;
  }

  const packageElmJson = {
    type: "package",
    name: "author/project",
    summary: "Temporary package for docs generation",
    license: "BSD-3-Clause",
    version: "1.0.0",
    "exposed-modules": exposedModules,
    "elm-version": "0.19.0 <= v < 0.20.0",
    dependencies: deps,
    "test-dependencies": {},
  };

  fs.writeFileSync(
    path.join(tmpDir, "elm.json"),
    JSON.stringify(packageElmJson, null, 4)
  );

  // Run elm make --docs
  const docsPath = path.join(tmpDir, "docs.json");
  try {
    execSync(`elm make --docs=${docsPath}`, {
      cwd: tmpDir,
      stdio: "pipe",
    });
  } catch (e: any) {
    // Try again with only modules that don't cause errors
    // Parse error output and remove problematic modules
    const stderr = e.stderr?.toString() || "";
    const errorModules = new Set<string>();

    // Match "NO DOCS" errors and extract module file paths
    const noDocsPattern = /-- NO DOCS -+ (src\/\S+\.elm)/g;
    let match;
    while ((match = noDocsPattern.exec(stderr)) !== null) {
      const modPath = match[1]
        .replace(/^src\//, "")
        .replace(/\.elm$/, "")
        .replace(/\//g, ".");
      errorModules.add(modPath);
    }

    if (errorModules.size > 0) {
      const filteredModules = exposedModules.filter(
        (m) => !errorModules.has(m)
      );
      if (filteredModules.length === 0) {
        throw new Error(
          "No modules could be compiled for documentation. All modules have documentation errors."
        );
      }

      packageElmJson["exposed-modules"] = filteredModules;
      fs.writeFileSync(
        path.join(tmpDir, "elm.json"),
        JSON.stringify(packageElmJson, null, 4)
      );

      execSync(`elm make --docs=${docsPath}`, {
        cwd: tmpDir,
        stdio: "pipe",
      });
    } else {
      throw new Error(`elm make failed: ${stderr}`);
    }
  }

  const docs = fs.readFileSync(docsPath, "utf8");

  // Clean up temp directory
  fs.rmSync(tmpDir, { recursive: true, force: true });

  return docs;
}

function findElmFiles(dir: string, prefix: string = ""): string[] {
  const files: string[] = [];
  try {
    for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
      const relPath = prefix ? `${prefix}/${entry.name}` : entry.name;
      if (entry.isDirectory()) {
        files.push(...findElmFiles(path.join(dir, entry.name), relPath));
      } else if (entry.name.endsWith(".elm")) {
        files.push(relPath);
      }
    }
  } catch {
    // Directory doesn't exist or can't be read
  }
  return files;
}

function hasModuleDoc(contents: string): boolean {
  // Check if the file has a module-level doc comment {-| ... -}
  // Module doc comments appear right after the module declaration
  const lines = contents.split("\n");
  let inModuleDecl = false;

  for (const line of lines) {
    const trimmed = line.trim();
    if (
      trimmed.startsWith("module ") ||
      trimmed.startsWith("port module ") ||
      trimmed.startsWith("effect module ")
    ) {
      inModuleDecl = true;
    }
    if (inModuleDecl && trimmed.includes(")")) {
      inModuleDecl = false;
      continue;
    }
    if (!inModuleDecl && trimmed === "{-|") {
      return true;
    }
    if (!inModuleDecl && trimmed.startsWith("{-|")) {
      return true;
    }
    if (
      !inModuleDecl &&
      trimmed !== "" &&
      !trimmed.startsWith("--") &&
      !trimmed.startsWith("{-")
    ) {
      // Hit actual code before a doc comment
      return false;
    }
  }
  return false;
}

/**
 * Compute an API diff between two docs.json strings.
 * Extends the base computeDiff with comment diffs for the TUI.
 */
export async function computeDiff(
  rawInput: unknown,
  _context: unknown
): Promise<unknown> {
  const input = rawInput as {
    baseDocs: string;
    headDocs: string;
    baseReadme?: string;
    headReadme?: string;
  };

  const baseDocs = JSON.parse(input.baseDocs);
  const headDocs = JSON.parse(input.headDocs);
  const apiDiff = computeDiffLib(baseDocs, headDocs);
  if (!apiDiff) return null;

  // Extend with comment diffs
  const baseByName = new Map(
    (baseDocs as DocsModule[]).map((m) => [m.name, m])
  );
  const headByName = new Map(
    (headDocs as DocsModule[]).map((m) => [m.name, m])
  );

  const commentDiffs: Record<string, ItemCommentDiffs> = {};
  // Old type signatures for changed items (JSON-encoded type values)
  const oldTypes: Record<string, Record<string, unknown>> = {};

  for (const mc of apiDiff.changedModules) {
    const baseMod = baseByName.get(mc.name);
    const headMod = headByName.get(mc.name);
    if (!baseMod || !headMod) continue;

    const modDiffs: ItemCommentDiffs = {};

    // Collect old types for changed items
    for (const itemName of mc.changed) {
      const baseType = findItemType(baseMod, itemName);
      if (baseType !== null) {
        if (!oldTypes[mc.name]) oldTypes[mc.name] = {};
        oldTypes[mc.name][itemName] = baseType;
      }
    }

    // Module-level comment diff
    if (baseMod.comment !== headMod.comment) {
      modDiffs["__module__"] = unifiedDiff(baseMod.comment, headMod.comment);
    }

    // Diff comments for changed items
    for (const itemName of mc.changed) {
      const baseComment = findItemComment(baseMod, itemName);
      const headComment = findItemComment(headMod, itemName);
      if (baseComment !== null && headComment !== null && baseComment !== headComment) {
        modDiffs[itemName] = unifiedDiff(baseComment, headComment);
      }
    }

    if (Object.keys(modDiffs).length > 0) {
      commentDiffs[mc.name] = modDiffs;
    }
  }

  // Also check for comment-only changes (items not in changed/added/removed)
  for (const headMod of headDocs as DocsModule[]) {
    const baseMod = baseByName.get(headMod.name);
    if (!baseMod) continue;
    // Skip if module is already tracked as changed
    if (apiDiff.changedModules.some((mc) => mc.name === headMod.name)) {
      // Check for items with only comment changes (not in changed list)
      const mc = apiDiff.changedModules.find((c) => c.name === headMod.name)!;
      const allTracked = new Set([...mc.added, ...mc.changed, ...mc.removed]);

      for (const category of ["values", "unions", "aliases", "binops"] as const) {
        for (const headItem of headMod[category] as { name: string; comment: string }[]) {
          if (allTracked.has(headItem.name)) continue;
          const baseItem = (baseMod[category] as { name: string; comment: string }[])
            .find((b) => b.name === headItem.name);
          if (baseItem && baseItem.comment !== headItem.comment) {
            if (!commentDiffs[headMod.name]) commentDiffs[headMod.name] = {};
            commentDiffs[headMod.name][headItem.name] = unifiedDiff(baseItem.comment, headItem.comment);
          }
        }
      }

      continue;
    }
    // Module not in changedModules — check for comment-only changes
    if (baseMod.comment !== headMod.comment) {
      if (!commentDiffs[headMod.name]) commentDiffs[headMod.name] = {};
      commentDiffs[headMod.name]["__module__"] = unifiedDiff(baseMod.comment, headMod.comment);
    }
    for (const category of ["values", "unions", "aliases", "binops"] as const) {
      for (const headItem of headMod[category] as { name: string; comment: string }[]) {
        const baseItem = (baseMod[category] as { name: string; comment: string }[])
          .find((b) => b.name === headItem.name);
        if (baseItem && baseItem.comment !== headItem.comment) {
          if (!commentDiffs[headMod.name]) commentDiffs[headMod.name] = {};
          commentDiffs[headMod.name][headItem.name] = unifiedDiff(baseItem.comment, headItem.comment);
        }
      }
    }
  }

  // README diff
  let readmeDiff: string | null = null;
  if (input.baseReadme !== undefined && input.headReadme !== undefined) {
    if (input.baseReadme !== input.headReadme) {
      readmeDiff = unifiedDiff(input.baseReadme || "", input.headReadme || "");
    }
  }

  return {
    ...apiDiff,
    commentDiffs: Object.keys(commentDiffs).length > 0 ? commentDiffs : undefined,
    readmeDiff: readmeDiff,
    oldTypes: Object.keys(oldTypes).length > 0 ? oldTypes : undefined,
  };
}

interface DocsModule {
  name: string;
  comment: string;
  values: { name: string; comment: string; type: unknown }[];
  unions: { name: string; comment: string; args: string[]; cases: unknown[] }[];
  aliases: { name: string; comment: string; args: string[]; type: unknown }[];
  binops: { name: string; comment: string; type: unknown; associativity: string; precedence: number }[];
}

type ItemCommentDiffs = Record<string, string>;

function findItemComment(mod: DocsModule, itemName: string): string | null {
  for (const v of mod.values) if (v.name === itemName) return v.comment;
  for (const u of mod.unions) if (u.name === itemName) return u.comment;
  for (const a of mod.aliases) if (a.name === itemName) return a.comment;
  for (const b of mod.binops) if (b.name === itemName) return b.comment;
  return null;
}

function findItemType(mod: DocsModule, itemName: string): unknown | null {
  for (const v of mod.values) if (v.name === itemName) return v.type;
  for (const b of mod.binops) if (b.name === itemName) return b.type;
  // Unions and aliases don't have a simple type string — skip for now
  return null;
}

/**
 * Line-based unified diff with limited context (3 lines around changes).
 * Returns a string with +/- prefixed lines, context lines, and ... separators.
 */
function unifiedDiff(
  oldText: string,
  newText: string,
  contextLines: number = 3
): string {
  const oldLines = oldText.split("\n");
  const newLines = newText.split("\n");

  // Build full diff with line types
  const lcs = longestCommonSubsequence(oldLines, newLines);
  const fullDiff: { type: "add" | "remove" | "context"; text: string }[] = [];

  let oldIdx = 0;
  let newIdx = 0;

  for (const [lcsOldIdx, lcsNewIdx] of lcs) {
    while (oldIdx < lcsOldIdx) {
      fullDiff.push({ type: "remove", text: oldLines[oldIdx] });
      oldIdx++;
    }
    while (newIdx < lcsNewIdx) {
      fullDiff.push({ type: "add", text: newLines[newIdx] });
      newIdx++;
    }
    fullDiff.push({ type: "context", text: oldLines[oldIdx] });
    oldIdx++;
    newIdx++;
  }
  while (oldIdx < oldLines.length) {
    fullDiff.push({ type: "remove", text: oldLines[oldIdx] });
    oldIdx++;
  }
  while (newIdx < newLines.length) {
    fullDiff.push({ type: "add", text: newLines[newIdx] });
    newIdx++;
  }

  // Find which lines are "changed" (add or remove)
  const changedIndices = new Set<number>();
  fullDiff.forEach((line, i) => {
    if (line.type !== "context") changedIndices.add(i);
  });

  // Mark which lines to include (changed lines + context around them)
  const includeIndices = new Set<number>();
  changedIndices.forEach((idx) => {
    for (
      let i = Math.max(0, idx - contextLines);
      i <= Math.min(fullDiff.length - 1, idx + contextLines);
      i++
    ) {
      includeIndices.add(i);
    }
  });

  // Build output with ... separators for gaps
  const result: string[] = [];
  let lastIncluded = -2;

  for (let i = 0; i < fullDiff.length; i++) {
    if (!includeIndices.has(i)) continue;

    if (lastIncluded >= 0 && i > lastIncluded + 1) {
      result.push("  ...");
    }
    lastIncluded = i;

    const line = fullDiff[i];
    if (line.type === "add") {
      result.push("+ " + line.text);
    } else if (line.type === "remove") {
      result.push("- " + line.text);
    } else {
      result.push("  " + line.text);
    }
  }

  return result.join("\n");
}

function longestCommonSubsequence(
  a: string[],
  b: string[]
): [number, number][] {
  const m = a.length;
  const n = b.length;
  const dp: number[][] = Array.from({ length: m + 1 }, () =>
    Array(n + 1).fill(0)
  );

  for (let i = 1; i <= m; i++) {
    for (let j = 1; j <= n; j++) {
      if (a[i - 1] === b[j - 1]) {
        dp[i][j] = dp[i - 1][j - 1] + 1;
      } else {
        dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }

  // Backtrack to find the actual LCS indices
  const result: [number, number][] = [];
  let i = m;
  let j = n;
  while (i > 0 && j > 0) {
    if (a[i - 1] === b[j - 1]) {
      result.push([i - 1, j - 1]);
      i--;
      j--;
    } else if (dp[i - 1][j] > dp[i][j - 1]) {
      i--;
    } else {
      j--;
    }
  }

  return result.reverse();
}
