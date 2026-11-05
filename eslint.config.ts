import eslint from "@eslint/js";
import { defineConfig, globalIgnores } from "eslint/config";
import reactHooks from "eslint-plugin-react-hooks";
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
    // The rules of React's hooks, for the components and hooks of core's
    // client.
    files: ["srcts/**/*.{ts,tsx}"],
    extends: [reactHooks.configs.flat["recommended-latest"]],
  },
  {
    // Each directory of srcts is a module, and its index.ts names what it
    // offers. Every file outside the module, a file directly in srcts
    // included, imports from that file alone. The directories types and
    // testing are exempt, since they hold only declarations and test helpers.
    files: ["srcts/**/*.{ts,tsx}"],
    rules: {
      "no-restricted-imports": [
        "error",
        {
          patterns: [
            {
              regex: String.raw`^(\.\.?/)+(?!\.|(types|testing)/)[^/]+/(?!index\.ts$)`,
              message: "Import another module through its index.ts.",
            },
          ],
        },
      ],
    },
  },
);
