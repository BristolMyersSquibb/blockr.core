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
    }
  };
}
export {
  blockrTest
};
