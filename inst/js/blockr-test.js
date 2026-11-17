// srcts/modules/blockr-test/index.ts
function blockrTest(core) {
  const open = () => {
    const now = core();
    return now?.store.hasBoard() === true ? now : void 0;
  };
  return {
    board() {
      const now = open();
      return now === void 0 ? null : structuredClone(now.store.board());
    },
    statuses() {
      const now = open();
      if (now === void 0) return {};
      return Object.fromEntries(
        Object.keys(now.store.board().blocks).flatMap((id) => {
          const status = now.scheduler.status(id);
          return status === void 0 ? [] : [[id, structuredClone(status)]];
        })
      );
    },
    focus() {
      return open()?.focus.get() ?? null;
    },
    onScreen() {
      const shown = open()?.onScreen.get() ?? null;
      return shown === null ? null : structuredClone(shown);
    },
    notifications() {
      const now = open();
      return now === void 0 ? [] : structuredClone(now.notifications.get());
    },
    async show(object) {
      const now = open();
      if (now === void 0) {
        throw new Error("No board is open yet, so the frontend has nothing to show.");
      }
      await now.show(object);
    }
  };
}
export {
  blockrTest
};
