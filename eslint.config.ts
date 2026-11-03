import eslint from "@eslint/js";
import { defineConfig, globalIgnores } from "eslint/config";
import tseslint from "typescript-eslint";

export default defineConfig(
  globalIgnores(["inst/", "docs/"]),
  eslint.configs.recommended,
  tseslint.configs.strictTypeChecked,
  tseslint.configs.stylisticTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        projectService: true,
        tsconfigRootDir: import.meta.dirname,
      },
    },
  },
  {
    // Each directory of srcts is a module, and its index.ts names what it
    // offers. Other modules import from that file alone. The directories types
    // and testing are exempt, since they hold only declarations and test
    // helpers.
    files: ["srcts/**/*.ts"],
    rules: {
      "no-restricted-imports": [
        "error",
        {
          patterns: [
            {
              regex: String.raw`^(\.\./)+(?!\.|(types|testing)/)[^/]+/(?!index\.ts$)`,
              message: "Import another module through its index.ts.",
            },
          ],
        },
      ],
    },
  },
);
