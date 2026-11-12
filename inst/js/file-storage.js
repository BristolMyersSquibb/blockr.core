// srcts/util/util.ts
function asError(error) {
  return error instanceof Error ? error : new Error(String(error));
}
function isObject(x) {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}

// srcts/storage/storage.ts
function coreStorage(doc = document) {
  return {
    save(board) {
      try {
        download(board, fileName({ id: boardIdFromText(board) }), doc);
        return Promise.resolve();
      } catch (error) {
        return Promise.reject(asError(error));
      }
    },
    open: () => pickFile(doc)
  };
}
function fileName(board, now = /* @__PURE__ */ new Date()) {
  const two = (n) => String(n).padStart(2, "0");
  const date = `${String(now.getFullYear())}-${two(now.getMonth() + 1)}-${two(now.getDate())}`;
  const time = `${two(now.getHours())}-${two(now.getMinutes())}-${two(now.getSeconds())}`;
  return `${board.id}_${date}_${time}.json`;
}
function download(text, name, doc = document) {
  const url = URL.createObjectURL(new Blob([text], { type: "application/json" }));
  const link = doc.createElement("a");
  link.href = url;
  link.download = name;
  link.hidden = true;
  doc.body.append(link);
  link.click();
  link.remove();
  setTimeout(() => {
    URL.revokeObjectURL(url);
  }, 0);
}
function pickFile(doc = document) {
  return new Promise((resolve, reject) => {
    const input = doc.createElement("input");
    input.type = "file";
    input.accept = ".json,application/json";
    input.hidden = true;
    input.addEventListener(
      "change",
      () => {
        const file = input.files?.[0];
        input.remove();
        if (file === void 0) resolve(null);
        else file.text().then(resolve, reject);
      },
      { once: true }
    );
    input.addEventListener(
      "cancel",
      () => {
        input.remove();
        resolve(null);
      },
      { once: true }
    );
    doc.body.append(input);
    input.click();
  });
}
function boardIdFromText(text) {
  let json;
  try {
    json = JSON.parse(text);
  } catch {
    return "board";
  }
  const board = isObject(json) ? json.board : void 0;
  return isObject(board) && typeof board.id === "string" && board.id !== "" ? board.id : "board";
}

// srcts/modules/file-storage/index.ts
var api = 1;
var file_storage_default = {
  storage: {
    save: (board) => coreStorage().save(board),
    open: () => coreStorage().open()
  }
};
export {
  api,
  file_storage_default as default
};
