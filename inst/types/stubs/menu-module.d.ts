export interface MenuModuleTest {
    readonly name: string;
    readonly run: () => Promise<void>;
}
export declare function menuModuleTests(exports: unknown): readonly MenuModuleTest[];
