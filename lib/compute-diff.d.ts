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
export declare function computeDiff(baseDocs: DocsModule[], headDocs: DocsModule[]): ApiDiff | null;
export {};
