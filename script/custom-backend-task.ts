import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { execSync } from "child_process";

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
