# A page and a session that the Shiny transport connects, for its browser test
# (test-shiny-transport-browser.R). The page holds core's client over the
# transport, from the blockr.core installed, and R's handler answers the
# requests of a plugin that the test sends: an echo of its args, progress,
# a body of a given size, the size of a body it receives, and the end of the
# session.
# The page resolves the imports of core's client, of React and of core's React
# SDK among them, through the import map that R writes into its head.

options(shiny.maxRequestSize = 1.5 * 2^20)

shiny::addResourcePath("blockr-js", system.file("js", package = "blockr.core"))

# The bytes 1, 2, ..., 250, 0, 1, ..., as the page makes them too.
pattern <- function(n) as.raw(seq_len(n) %% 251L)

coalesce <- function(x, y) if (is.null(x)) y else x

answer <- function(id, value) {
  list(id = id, status = "ok", value = blockr.core:::encode_answer(value))
}

handler <- function(session) {

  function(message, send) {

    for (req in message) {

      id <- req[["id"]]

      reply <- switch(
        req[["kind"]],
        hello = list(
          id = id,
          status = "ok",
          protocol = "1.0",
          capabilities = I(c("cancel", "progress"))
        ),
        request = switch(
          req[["name"]],
          echo = answer(id, req[["args"]]),
          progress = {
            note <- list(value = 0.5, message = "halfway")
            send(list(list(id = id, progress = note)))
            answer(id, TRUE)
          },
          download = list(
            id = id,
            status = "ok",
            body = blockr.core::new_reply_body(
              pattern(req[["args"]][["size"]]),
              "application/octet-stream",
              name = req[["args"]][["name"]]
            )
          ),
          upload = {
            body <- req[["body"]]
            bytes <- readBin(body[["datapath"]], "raw", body[["size"]])
            answer(
              id,
              list(
                size = body[["size"]],
                sum = sum(as.integer(bytes)),
                match = identical(bytes, pattern(body[["size"]])),
                type = body[["type"]],
                name = coalesce(body[["name"]], character())
              )
            )
          },
          close = session$close()
        )
      )

      if (is.list(reply)) {
        send(list(reply))
      }
    }
  }
}

page <- "
import { createClient, shinyTransport } from './blockr-js/blockr-core.js';
import { TextInput } from '@blockr/core/react';
import { createRoot } from 'react-dom/client';
import { jsx } from 'react/jsx-runtime';

const sessions = [];
let started = () => undefined;

// Counts the binary frames that reach the page, as Shiny announces each
// message.
let frames = 0;
window.jQuery(document).on('shiny:message', (event) => {
  const custom = event.message.custom;
  if (custom && custom.blockr instanceof ArrayBuffer) frames++;
});

const client = createClient(shinyTransport(), {
  hello: () => ({ protocol: '1.0', capabilities: ['cancel', 'progress'] }),
  session: (reply) => {
    sessions.push(reply);
    started();
  },
});

function pattern(n) {
  const bytes = new Uint8Array(n);
  for (let i = 0; i < n; i++) bytes[i] = (i + 1) % 251;
  return bytes;
}

async function summary(blob) {
  if (blob === null) return null;
  const bytes = new Uint8Array(await blob.arrayBuffer());
  let sum = 0;
  for (const b of bytes) sum += b;
  const name = blob instanceof File ? blob.name : null;
  return { type: blob.type, name, size: bytes.length, sum };
}

async function ask(name, args = {}, body = undefined) {
  const notes = [];
  try {
    const request = {
      kind: 'request',
      type: 'plugin',
      plugin: 'blockr.test-transport_plugin',
      name,
      args,
      reads: [],
    };
    const replied = await client.request(request, {
      body,
      progress: (note) => notes.push(note),
    });
    return { reply: replied.reply, body: await summary(replied.body), notes };
  } catch (error) {
    return { error: error.name, message: error.message };
  }
}

window.app = {
  sessions,
  ask,
  // Draws the SDK's TextInput with react-dom/client and gives the value it
  // shows, which its hook gives only where both read the same React.
  sdkInput: async () => {
    const el = document.createElement('div');
    document.body.append(el);
    const bound = {
      value: 'hello', error: null, get: () => 'hello',
      commit: async () => true, type() {}, flush() {},
    };
    const root = createRoot(el);
    root.render(jsx(TextInput, { bound, label: 'Name' }));
    await new Promise((done) => setTimeout(done, 100));
    const value = el.querySelector('input')?.value ?? null;
    root.unmount();
    el.remove();
    return value;
  },
  frames: () => frames,
  // Resolves once n sessions have started.
  sessionsStarted: (n) =>
    new Promise((resolve) => {
      const check = () => {
        if (sessions.length >= n) resolve(sessions.length);
      };
      started = check;
      check();
    }),
  upload: (size, name) =>
    ask(
      'upload',
      {},
      name === undefined
        ? new Blob([pattern(size)])
        : new File([pattern(size)], name, { type: 'text/csv' }),
    ),
};
"

shiny::shinyApp(
  ui = shiny::tagList(
    shiny::tags$head(blockr.core:::import_map()),
    shiny::tags$script(type = "module", shiny::HTML(page))
  ),
  server = function(input, output, session) {
    blockr.core:::shiny_transport(handler(session), session)
    # Locally, only a forced reconnect starts a new session.
    session$allowReconnect("force")
  }
)
