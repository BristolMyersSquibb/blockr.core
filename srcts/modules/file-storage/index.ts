// The module of core's storage plugin, of the plugin class
// blockr.core-file_storage_plugin, whose default export is the class without
// its id: it saves a board as a download and opens an upload, as Storage has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#storage).
// An app runs it unless another of its plugins is a storage plugin. It keeps
// no state, so the board holds nothing for it, and it names no board to open as
// the page starts. The storage reaches for the page's document only as a
// frontend calls it.

import { coreStorage } from "../../storage/index.ts";
import type { PluginClass } from "../../store/index.ts";

export const api = 1;

export default {
  storage: {
    save: (board) => coreStorage().save(board),
    open: () => coreStorage().open(),
  },
} satisfies Omit<PluginClass, "id">;
