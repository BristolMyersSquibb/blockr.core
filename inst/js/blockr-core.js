// srcts/bridge/shiny.ts
var name = "blockr";
function shinyChannel(options = {}) {
  const page = window;
  const shiny = options.shiny ?? page.Shiny;
  if (shiny === void 0) {
    throw new Error("The Shiny bridge needs Shiny's client, window.Shiny.");
  }
  const fetcher = options.fetch ?? window.fetch.bind(window);
  const onDisconnected = options.onDisconnected ?? disconnected(page.jQuery);
  let upload = null;
  let session = 0;
  return {
    async send(message, bodies) {
      const at = session;
      for (const [id, body] of bodies) {
        await post(fetcher, upload, id, body);
      }
      if (at !== session) {
        throw new Error("The session ended before its message was sent.");
      }
      shiny.setInputValue(name, JSON.stringify(message), {
        priority: "event"
      });
    },
    listen(listener) {
      shiny.addCustomMessageHandler(name, (message) => {
        if (message instanceof ArrayBuffer) {
          for (const [item, body] of framed(message)) {
            listener.received(item, body);
          }
        } else if (Array.isArray(message)) {
          for (const item of message) {
            if (typeof item !== "object" || item === null) continue;
            const [carried, body] = received(item, fetcher);
            listener.received(carried, body);
          }
        } else if (isSessionStart(message)) {
          session += 1;
          upload = message;
          listener.started();
        }
      });
      onDisconnected(() => {
        session += 1;
        upload = null;
        listener.ended();
      });
    }
  };
}
function disconnected(jquery) {
  if (jquery === void 0) {
    throw new Error("The Shiny bridge needs the jQuery that Shiny puts on the page.");
  }
  return (fn) => {
    jquery(document).on("shiny:disconnected", fn);
  };
}
function isSessionStart(message) {
  return typeof message === "object" && message !== null && typeof message.upload === "string";
}
function framed(frame) {
  const length = new DataView(frame).getUint32(0, true);
  const json = new TextDecoder().decode(new Uint8Array(frame, 4, length));
  const bytes = frame.slice(4 + length);
  return JSON.parse(json).flatMap((item) => {
    if (typeof item !== "object" || item === null) return [];
    const announced = "status" in item && "body" in item ? item.body : null;
    if (typeof announced !== "object" || announced === null) {
      return [[item, null]];
    }
    const body = Promise.resolve(blob([bytes], announced));
    return [[item, body]];
  });
}
function received(item, fetcher) {
  const announced = "status" in item && "body" in item ? item.body : null;
  if (typeof announced !== "object" || announced === null) return [item, null];
  const { data, url, ...body } = announced;
  const reply = { ...item, body };
  let bytes;
  if (data !== void 0) {
    bytes = new Promise((resolve) => {
      resolve(blob([fromBase64(data)], body));
    });
  } else if (url !== void 0) {
    bytes = fetchBody(fetcher, url, body);
  } else {
    bytes = Promise.reject(
      new Error(`R sent no bytes of the body of its reply to ${reply.id}.`)
    );
  }
  bytes.catch(() => void 0);
  return [reply, bytes];
}
async function fetchBody(fetcher, url, body) {
  const response = await fetcher(url);
  if (!response.ok) {
    throw new Error(
      `R served no body at ${url}: ${String(response.status)} ${await response.text()}`
    );
  }
  return blob([await response.blob()], body);
}
async function post(fetcher, upload, id, body) {
  if (upload === null) {
    throw new Error("No session runs that a body could be posted to.");
  }
  const max = upload.max_request_size;
  if (max !== void 0 && body.size > max) {
    throw new Error(
      `The body of request ${id}, of ${String(body.size)} bytes, is larger than the ${String(max)} bytes that R takes.`
    );
  }
  const url = `${upload.upload}${upload.upload.includes("?") ? "&" : "?"}id=${encodeURIComponent(id)}`;
  const response = await fetcher(url, {
    method: "POST",
    body,
    headers: {
      "Content-Type": body.type === "" ? "application/octet-stream" : body.type
    }
  });
  if (!response.ok) {
    throw new Error(
      `R refused the body of request ${id}: ${await response.text()}`
    );
  }
}
function blob(parts, body) {
  return body.name === void 0 ? new Blob(parts, { type: body.type }) : new File(parts, body.name, { type: body.type });
}
function fromBase64(data) {
  const text = atob(data);
  const bytes = new Uint8Array(text.length);
  for (let i = 0; i < text.length; i += 1) {
    bytes[i] = text.charCodeAt(i);
  }
  return bytes;
}

// srcts/protocol/transport.ts
var SessionEndedError = class extends Error {
  constructor(id) {
    super(`The session ended before R replied to request ${id}.`);
    this.name = "SessionEndedError";
  }
};
function createTransport(channel, options) {
  let next = 0;
  let state = "none";
  let shared = /* @__PURE__ */ new Set();
  const waiting = /* @__PURE__ */ new Map();
  let held = [];
  function mint() {
    next += 1;
    return String(next);
  }
  function dispatch(outbound) {
    const plain = outbound.filter((entry) => entry.call?.body === void 0);
    if (plain.length > 0) {
      transmit(plain, /* @__PURE__ */ new Map());
    }
    for (const entry of outbound) {
      if (entry.call?.body !== void 0) {
        transmit([entry], /* @__PURE__ */ new Map([[entry.call.id, entry.call.body]]));
      }
    }
  }
  function transmit(outbound, bodies) {
    const calls = outbound.flatMap((entry) => entry.call ?? []);
    for (const call of calls) {
      waiting.set(call.id, call);
    }
    channel.send(
      outbound.map((entry) => entry.message),
      bodies
    ).then(
      () => {
        for (const call of calls) {
          call.sent = true;
          if (call.aborted) cancel(call);
        }
      },
      (error) => {
        for (const call of calls) {
          if (waiting.get(call.id) === call) {
            waiting.delete(call.id);
            call.settle({ error: asError(error) });
          }
        }
      }
    );
  }
  function cancel(call) {
    if (waiting.get(call.id) !== call || !shared.has("cancel")) return;
    quietly(channel.send([{ kind: "cancel", request: call.id }], /* @__PURE__ */ new Map()));
  }
  function abort(entry, call) {
    const index = held.indexOf(entry);
    if (index >= 0) {
      held.splice(index, 1);
      call.settle({ answer: cancelled(call.id) });
    } else if (call.sent) {
      cancel(call);
    } else {
      call.aborted = true;
    }
  }
  function queue(entry) {
    if (state === "ready") {
      dispatch([entry]);
    } else {
      held.push(entry);
    }
  }
  function end() {
    state = "none";
    shared = /* @__PURE__ */ new Set();
    const lost = [...waiting.values()];
    waiting.clear();
    for (const call of lost) {
      call.settle({ error: new SessionEndedError(call.id) });
    }
  }
  function hello() {
    const named = options.hello();
    const id = mint();
    const call = {
      id,
      body: void 0,
      progress: void 0,
      sent: false,
      aborted: false,
      settle(result) {
        if (!("answer" in result)) {
          if (!(result.error instanceof SessionEndedError)) report(result.error);
          return;
        }
        const { reply } = result.answer;
        if (reply.status === "ok" && "capabilities" in reply) {
          const theirs = new Set(reply.capabilities);
          shared = new Set(named.capabilities.filter((c) => theirs.has(c)));
          state = "ready";
          const outbound = held;
          held = [];
          dispatch(outbound);
        }
        guard(() => {
          options.session(result.answer);
        });
      }
    };
    transmit([{ message: { ...named, id, kind: "hello" }, call }], /* @__PURE__ */ new Map());
  }
  channel.listen({
    started() {
      end();
      state = "started";
      hello();
    },
    ended() {
      end();
    },
    received(item, body) {
      const call = waiting.get(item.id);
      if (call === void 0) return;
      if (!("status" in item)) {
        const note = item.progress;
        guard(() => {
          call.progress?.(note);
        });
        return;
      }
      waiting.delete(item.id);
      if (body === null) {
        call.settle({ answer: { reply: item, body: null } });
      } else {
        body.then(
          (blob2) => {
            call.settle({ answer: { reply: item, body: blob2 } });
          },
          (error) => {
            call.settle({ error: asError(error) });
          }
        );
      }
    }
  });
  return {
    request(request, { body, progress, signal } = {}) {
      const id = mint();
      const message = {
        ...request,
        id,
        ...body === void 0 ? {} : { body: announce(body) }
      };
      return new Promise((resolve, reject) => {
        const onAbort = () => {
          abort(entry, call);
        };
        const call = {
          id,
          body,
          progress,
          sent: false,
          aborted: false,
          settle(result) {
            signal?.removeEventListener("abort", onAbort);
            if ("answer" in result) {
              resolve(result.answer);
            } else {
              reject(result.error);
            }
          }
        };
        const entry = { message, call };
        if (signal?.aborted === true) {
          call.settle({ answer: cancelled(id) });
          return;
        }
        signal?.addEventListener("abort", onAbort, { once: true });
        queue(entry);
      });
    },
    forget(blocks) {
      queue({ message: { kind: "forget", blocks } });
    }
  };
}
function announce(body) {
  const type = body.type === "" ? "application/octet-stream" : body.type;
  return body instanceof File ? { type, name: body.name } : { type };
}
function cancelled(id) {
  return { reply: { id, status: "cancelled" }, body: null };
}
function guard(fn) {
  try {
    fn();
  } catch (error) {
    report(error);
  }
}
function asError(error) {
  return error instanceof Error ? error : new Error(String(error));
}
function report(error) {
  queueMicrotask(() => {
    throw error;
  });
}
function quietly(sent) {
  sent.catch(() => void 0);
}
export {
  SessionEndedError,
  createTransport,
  shinyChannel
};
