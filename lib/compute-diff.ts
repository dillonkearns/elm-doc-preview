/**
 * Compute an API diff between two docs.json structures.
 *
 * This is used both by the local dev server and the Vercel serverless function
 * to diff between two arbitrary git refs (instead of relying on `elm diff`
 * which only compares against the latest published version).
 */

export interface ApiDiff {
  magnitude: string;
  addedModules: string[];
  removedModules: string[];
  changedModules: ApiModuleChanges[];
}

export interface ApiModuleChanges {
  name: string;
  added: string[];
  changed: string[];
  removed: string[];
}

interface DocsModule {
  name: string;
  comment: string;
  unions: DocsUnion[];
  aliases: DocsAlias[];
  values: DocsValue[];
  binops: DocsBinop[];
}

interface DocsUnion {
  name: string;
  comment: string;
  args: string[];
  cases: [string, unknown[]][];
}

interface DocsAlias {
  name: string;
  comment: string;
  args: string[];
  type: unknown;
}

interface DocsValue {
  name: string;
  comment: string;
  type: unknown;
}

interface DocsBinop {
  name: string;
  comment: string;
  type: unknown;
  associativity: string;
  precedence: number;
}

/**
 * Compute the API diff between base and head docs.json arrays.
 * Returns null if there are no changes.
 */
export function computeDiff(
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
    if (!baseMod) continue; // added module, handled above

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

  // Diff unions
  diffItems(
    baseMod.unions,
    headMod.unions,
    (u) => u.name,
    (a, b) =>
      jsonEqual(a.args, b.args) && jsonEqual(a.cases, b.cases),
    added,
    changed,
    removed
  );

  // Diff aliases
  diffItems(
    baseMod.aliases,
    headMod.aliases,
    (a) => a.name,
    (a, b) =>
      jsonEqual(a.args, b.args) && jsonEqual(a.type, b.type),
    added,
    changed,
    removed
  );

  // Diff values
  diffItems(
    baseMod.values,
    headMod.values,
    (v) => v.name,
    (a, b) => jsonEqual(a.type, b.type),
    added,
    changed,
    removed
  );

  // Diff binops
  diffItems(
    baseMod.binops,
    headMod.binops,
    (b) => b.name,
    (a, b) =>
      jsonEqual(a.type, b.type) &&
      a.associativity === b.associativity &&
      a.precedence === b.precedence,
    added,
    changed,
    removed
  );

  if (added.length === 0 && changed.length === 0 && removed.length === 0) {
    return null;
  }

  return { name: baseMod.name, added, changed, removed };
}

function diffItems<T>(
  baseItems: T[],
  headItems: T[],
  getName: (item: T) => string,
  isEqual: (a: T, b: T) => boolean,
  added: string[],
  changed: string[],
  removed: string[]
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
    const name = getName(baseItem);
    if (!headByName.has(name)) {
      removed.push(name);
    }
  }
}

function jsonEqual(a: unknown, b: unknown): boolean {
  return JSON.stringify(a) === JSON.stringify(b);
}
