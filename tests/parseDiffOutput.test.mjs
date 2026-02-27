import { describe, it } from "node:test";
import assert from "node:assert/strict";

// parseDiffOutput extracted from preview.ts / elm-doc-server.ts
// If these ever diverge, this test should be split or the function shared.
function parseDiffOutput(output) {
  const lines = output.split("\n");
  if (lines.length === 0) return null;

  const magnitudeMatch = lines[0].match(
    /This is a (MAJOR|MINOR|PATCH) change/
  );
  if (!magnitudeMatch) return null;

  const diff = {
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
      const moduleMatch = line.match(
        /^-{4,} (.+?) - (?:MAJOR|MINOR|PATCH) -{4,}$/
      );
      if (moduleMatch) {
        const mc = {
          name: moduleMatch[1],
          added: [],
          changed: [],
          removed: [],
        };
        i++;
        let section = null;
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

// -- Tests --

describe("parseDiffOutput", () => {
  it("returns null for empty input", () => {
    assert.equal(parseDiffOutput(""), null);
  });

  it("returns null for non-diff input", () => {
    assert.equal(parseDiffOutput("some random text"), null);
  });

  it("parses added modules with blank lines (the bug scenario)", () => {
    // Real elm diff output has blank lines between headers and content
    const output = [
      "This is a MINOR change.",
      "",
      "---- ADDED MODULES - MINOR ----",
      "",
      "    FilePath",
      "    NewModule",
      "",
    ].join("\n");

    const diff = parseDiffOutput(output);
    assert.notEqual(diff, null);
    assert.equal(diff.magnitude, "MINOR");
    assert.deepEqual(diff.addedModules, ["FilePath", "NewModule"]);
  });

  it("parses removed modules with blank lines", () => {
    const output = [
      "This is a MAJOR change.",
      "",
      "---- REMOVED MODULES - MAJOR ----",
      "",
      "    OldModule",
      "    DeprecatedModule",
      "",
    ].join("\n");

    const diff = parseDiffOutput(output);
    assert.notEqual(diff, null);
    assert.deepEqual(diff.removedModules, ["OldModule", "DeprecatedModule"]);
  });

  it("parses added modules without blank lines", () => {
    const output = [
      "This is a MINOR change.",
      "",
      "---- ADDED MODULES - MINOR ----",
      "    FilePath",
      "    NewModule",
    ].join("\n");

    const diff = parseDiffOutput(output);
    assert.notEqual(diff, null);
    assert.deepEqual(diff.addedModules, ["FilePath", "NewModule"]);
  });

  it("parses changed modules with added/changed/removed items", () => {
    const output = [
      "This is a MAJOR change.",
      "",
      "---- SomeModule - MAJOR ----",
      "",
      "  Added:",
      "    newFunc : String -> Int",
      "",
      "  Changed:",
      "    - oldFunc : Int -> String",
      "    + oldFunc : Int -> Int -> String",
      "",
      "  Removed:",
      "    - removedFunc : String",
      "",
    ].join("\n");

    const diff = parseDiffOutput(output);
    assert.notEqual(diff, null);
    assert.equal(diff.changedModules.length, 1);
    const mc = diff.changedModules[0];
    assert.equal(mc.name, "SomeModule");
    assert.deepEqual(mc.added, ["newFunc"]);
    assert.deepEqual(mc.changed, ["oldFunc"]);
    assert.deepEqual(mc.removed, ["removedFunc"]);
  });

  it("parses a full diff with added modules, removed modules, and changed modules", () => {
    const output = [
      "This is a MAJOR change.",
      "",
      "---- ADDED MODULES - MINOR ----",
      "",
      "    FilePath",
      "",
      "---- REMOVED MODULES - MAJOR ----",
      "",
      "    OldModule",
      "",
      "---- Api - MINOR ----",
      "",
      "  Added:",
      "    newEndpoint : Request -> Response",
      "",
    ].join("\n");

    const diff = parseDiffOutput(output);
    assert.notEqual(diff, null);
    assert.equal(diff.magnitude, "MAJOR");
    assert.deepEqual(diff.addedModules, ["FilePath"]);
    assert.deepEqual(diff.removedModules, ["OldModule"]);
    assert.equal(diff.changedModules.length, 1);
    assert.equal(diff.changedModules[0].name, "Api");
    assert.deepEqual(diff.changedModules[0].added, ["newEndpoint"]);
  });

  it("deduplicates changed items (before/after lines)", () => {
    const output = [
      "This is a MINOR change.",
      "",
      "---- Mod - MINOR ----",
      "",
      "  Changed:",
      "    - foo : Int",
      "    + foo : String",
      "    - foo : Int",
      "",
    ].join("\n");

    const diff = parseDiffOutput(output);
    assert.notEqual(diff, null);
    assert.deepEqual(diff.changedModules[0].changed, ["foo"]);
  });
});
