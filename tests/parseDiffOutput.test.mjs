import { describe, it } from "node:test";
import assert from "node:assert/strict";

// OLD buggy version — stops at blank lines in ADDED/REMOVED sections
function parseDiffOutputOld(output) {
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

// NEW fixed version — skips blank lines in ADDED/REMOVED sections
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

// Real `elm diff` output from dillonkearns/elm-pages path-type branch
const REAL_ELM_DIFF_OUTPUT = `This is a MAJOR change.

---- ADDED MODULES - MINOR ----

    FilePath


---- BackendTask - MINOR ----

    Added:
        and :
            BackendTask.BackendTask error b
            -> BackendTask.BackendTask error a
            -> BackendTask.BackendTask error b
        finally :
            BackendTask.BackendTask cleanupError ()
            -> BackendTask.BackendTask error a
            -> BackendTask.BackendTask error a


---- BackendTask.Do - MAJOR ----

    Changed:
      - glob :
            String
            -> (List String -> BackendTask FatalError a)
            -> BackendTask FatalError a
      + glob :
            String.String
            -> (
            List.List FilePath.FilePath
            -> BackendTask.BackendTask FatalError.FatalError a
            )
            -> BackendTask.BackendTask FatalError.FatalError a



---- BackendTask.File - MAJOR ----

    Added:
        exists : FilePath.FilePath -> BackendTask.BackendTask error Basics.Bool
        optional :
            BackendTask.BackendTask
                { fatal : FatalError.FatalError
                , recoverable : BackendTask.File.FileReadError decoderError
                }
                a
            -> BackendTask.BackendTask FatalError.FatalError (Maybe.Maybe a)

    Changed:
      - rawFile :
            String
            -> BackendTask
                   { fatal : FatalError
                   , recoverable : FileReadError decoderError
                   }
                   String
      + rawFile :
            FilePath.FilePath
            -> BackendTask.BackendTask
                   { fatal : FatalError.FatalError
                   , recoverable : BackendTask.File.FileReadError decoderError
                   }
                   String.String



---- Pages.Script - MAJOR ----

    Added:
        copyFile :
            { to : FilePath.FilePath }
            -> FilePath.FilePath
            -> BackendTask.BackendTask FatalError.FatalError FilePath.FilePath
        deleteFile :
            FilePath.FilePath
            -> BackendTask.BackendTask FatalError.FatalError ()

    Changed:
      - writeFile :
            { path : String, body : String }
            -> BackendTask { fatal : FatalError, recoverable : Error } ()
      + writeFile :
            { body : String.String }
            -> FilePath.FilePath
            -> BackendTask.BackendTask
                   { fatal : FatalError.FatalError
                   , recoverable : Pages.Script.Error
                   }
                   FilePath.FilePath
    `;

// -- Tests --

describe("parseDiffOutput — old (buggy) version", () => {
  it("FAILS: drops FilePath from addedModules due to blank line after header", () => {
    const diff = parseDiffOutputOld(REAL_ELM_DIFF_OUTPUT);
    // The old parser produces an EMPTY addedModules — this is the bug
    assert.deepEqual(diff.addedModules, []);
  });
});

describe("parseDiffOutput — fixed version", () => {
  it("parses FilePath as an added module from real elm diff output", () => {
    const diff = parseDiffOutput(REAL_ELM_DIFF_OUTPUT);
    assert.notEqual(diff, null);
    assert.deepEqual(diff.addedModules, ["FilePath"]);
  });

  it("parses all changed modules from real elm diff output", () => {
    const diff = parseDiffOutput(REAL_ELM_DIFF_OUTPUT);
    const names = diff.changedModules.map((m) => m.name);
    assert.deepEqual(names, [
      "BackendTask",
      "BackendTask.Do",
      "BackendTask.File",
      "Pages.Script",
    ]);
  });

  it("parses added items in changed modules from real elm diff output", () => {
    const diff = parseDiffOutput(REAL_ELM_DIFF_OUTPUT);
    const bt = diff.changedModules.find((m) => m.name === "BackendTask");
    assert.deepEqual(bt.added, ["and", "finally"]);

    const file = diff.changedModules.find(
      (m) => m.name === "BackendTask.File"
    );
    assert.deepEqual(file.added, ["exists", "optional"]);
  });

  it("parses changed items in changed modules from real elm diff output", () => {
    const diff = parseDiffOutput(REAL_ELM_DIFF_OUTPUT);
    const doMod = diff.changedModules.find(
      (m) => m.name === "BackendTask.Do"
    );
    assert.deepEqual(doMod.changed, ["glob"]);

    const file = diff.changedModules.find(
      (m) => m.name === "BackendTask.File"
    );
    assert.deepEqual(file.changed, ["rawFile"]);
  });

  it("parses Pages.Script added and changed items", () => {
    const diff = parseDiffOutput(REAL_ELM_DIFF_OUTPUT);
    const ps = diff.changedModules.find((m) => m.name === "Pages.Script");
    assert.deepEqual(ps.added, ["copyFile", "deleteFile"]);
    assert.deepEqual(ps.changed, ["writeFile"]);
  });

  it("returns null for empty input", () => {
    assert.equal(parseDiffOutput(""), null);
  });

  it("returns null for non-diff input", () => {
    assert.equal(parseDiffOutput("some random text"), null);
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
    assert.deepEqual(diff.changedModules[0].changed, ["foo"]);
  });
});
