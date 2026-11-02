# A page and a session that the Shiny bridge connects, for the bridge's browser
# test (test-shiny-bridge-browser.R). The page holds core's transport over the
# bridge, from the blockr.core installed, and R's handler answers the requests
# of an extension that the test sends: an echo of its args, progress, a body of
# a given size, the size of a body it receives, and the end of the session.

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
            body = list(
              x = pattern(req[["args"]][["size"]]),
              type = "application/octet-stream",
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
import { createTransport, shinyChannel } from './blockr-js/blockr-core.js';

const sessions = [];
let started = () => undefined;

// Counts the binary frames that reach the page, as Shiny announces each
// message.
let frames = 0;
window.jQuery(document).on('shiny:message', (event) => {
  const custom = event.message.custom;
  if (custom && custom.blockr instanceof ArrayBuffer) frames++;
});

const transport = createTransport(shinyChannel(), {
  hello: () => ({ protocol: '1.0', capabilities: ['cancel', 'progress'] }),
  session: (answer) => {
    sessions.push(answer.reply);
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
      extension: 'blockr.test-bridge',
      name,
      args,
      reads: [],
    };
    const answer = await transport.request(request, {
      body,
      progress: (note) => notes.push(note),
    });
    return { reply: answer.reply, body: await summary(answer.body), notes };
  } catch (error) {
    return { error: error.name, message: error.message };
  }
}

window.bridge = {
  sessions,
  ask,
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
  ui = shiny::tags$script(type = "module", shiny::HTML(page)),
  server = function(input, output, session) {
    blockr.core:::shiny_bridge(handler(session), session)
    # Locally, only a forced reconnect starts a new session.
    session$allowReconnect("force")
  }
)
