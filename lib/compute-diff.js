/**
 * Compute an API diff between two docs.json structures.
 *
 * This is used both by the local dev server and the Vercel serverless function
 * to diff between two arbitrary git refs (instead of relying on `elm diff`
 * which only compares against the latest published version).
 */
/**
 * Compute the API diff between base and head docs.json arrays.
 * Returns null if there are no changes.
 */
export function computeDiff(baseDocs, headDocs) {
    const baseByName = new Map(baseDocs.map((m) => [m.name, m]));
    const headByName = new Map(headDocs.map((m) => [m.name, m]));
    const addedModules = headDocs
        .filter((m) => !baseByName.has(m.name))
        .map((m) => m.name);
    const removedModules = baseDocs
        .filter((m) => !headByName.has(m.name))
        .map((m) => m.name);
    const changedModules = [];
    for (const headMod of headDocs) {
        const baseMod = baseByName.get(headMod.name);
        if (!baseMod)
            continue; // added module, handled above
        const changes = diffModule(baseMod, headMod);
        if (changes) {
            changedModules.push(changes);
        }
    }
    if (addedModules.length === 0 &&
        removedModules.length === 0 &&
        changedModules.length === 0) {
        return null;
    }
    const hasRemovals = removedModules.length > 0 ||
        changedModules.some((mc) => mc.removed.length > 0);
    const hasAdditions = addedModules.length > 0 ||
        changedModules.some((mc) => mc.added.length > 0);
    const magnitude = hasRemovals ? "MAJOR" : hasAdditions ? "MINOR" : "PATCH";
    return { magnitude, addedModules, removedModules, changedModules };
}
function diffModule(baseMod, headMod) {
    const added = [];
    const changed = [];
    const removed = [];
    // Diff unions
    diffItems(baseMod.unions, headMod.unions, (u) => u.name, (a, b) => jsonEqual(a.args, b.args) && jsonEqual(a.cases, b.cases), added, changed, removed);
    // Diff aliases
    diffItems(baseMod.aliases, headMod.aliases, (a) => a.name, (a, b) => jsonEqual(a.args, b.args) && jsonEqual(a.type, b.type), added, changed, removed);
    // Diff values
    diffItems(baseMod.values, headMod.values, (v) => v.name, (a, b) => jsonEqual(a.type, b.type), added, changed, removed);
    // Diff binops
    diffItems(baseMod.binops, headMod.binops, (b) => b.name, (a, b) => jsonEqual(a.type, b.type) &&
        a.associativity === b.associativity &&
        a.precedence === b.precedence, added, changed, removed);
    if (added.length === 0 && changed.length === 0 && removed.length === 0) {
        return null;
    }
    return { name: baseMod.name, added, changed, removed };
}
function diffItems(baseItems, headItems, getName, isEqual, added, changed, removed) {
    const baseByName = new Map(baseItems.map((item) => [getName(item), item]));
    const headByName = new Map(headItems.map((item) => [getName(item), item]));
    for (const headItem of headItems) {
        const name = getName(headItem);
        const baseItem = baseByName.get(name);
        if (!baseItem) {
            added.push(name);
        }
        else if (!isEqual(baseItem, headItem)) {
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
function jsonEqual(a, b) {
    return JSON.stringify(a) === JSON.stringify(b);
}
