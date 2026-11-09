// Reading blockr options: once per page, the names read at once in one
// request, with the value as R sent it.

import { describe, expect, test } from "vitest";
import { SessionEndedError } from "../protocol/index.ts";
import type { Client, Reply } from "../protocol/index.ts";
import { reports } from "../testing/reports.ts";
import type { Json } from "../types/json.ts";
import { createBlockrOptions } from "./index.ts";

// A client whose requests the test answers, in the order they were sent.
function asked() {
  const requests: { names: string[]; settle: (result: { reply: Reply } | { error: Error }) => void }[] = [];
  const client: Pick<Client, "request"> = {
    request(request) {
      return new Promise((resolve, reject) => {
        if (request.kind !== "blockr_options") throw new Error("The test answers blockr options only.");
        requests.push({
          names: request.names,
          settle: (result) => {
            if ("error" in result) reject(result.error);
            else resolve({ reply: result.reply, body: null });
          },
        });
      });
    },
  };
  return {
    client,
    requests,
    answer(i: number, values: Readonly<Record<string, Json>>) {
      requests[i]?.settle({ reply: { id: String(i), status: "ok", values } });
    },
  };
}

const flush = () => new Promise((resolve) => setTimeout(resolve, 0));

describe("reading a blockr option", () => {
  test("asks R for the names read at once in one request, and for each name once per page", async () => {
    const r = asked();
    const options = createBlockrOptions(r.client);
    const reads = [options.read("evaluation_queue"), options.read("gate_visibility"), options.read("evaluation_queue")];
    await flush();
    expect(r.requests.map(({ names }) => names)).toEqual([["evaluation_queue", "gate_visibility"]]);

    r.answer(0, { evaluation_queue: 3 });
    await expect(Promise.all(reads)).resolves.toEqual([3, undefined, 3]);

    await expect(options.read("evaluation_queue")).resolves.toBe(3);
    expect(r.requests).toHaveLength(1);
  });

  test("gives the value as R sent it, a string that an environment variable sets among them, and undefined where nothing sets it", async () => {
    const r = asked();
    const options = createBlockrOptions(r.client);
    const reads = Promise.all(["delay", "queue", "gate", "missing", "theme", "severities", "unset"].map((name) => options.read(name)));
    await flush();
    r.answer(0, { delay: "30", queue: "-1", gate: "FALSE", missing: null, theme: true, severities: ["error"] });

    await expect(reads).resolves.toEqual(["30", "-1", "FALSE", null, true, ["error"], undefined]);
  });

  test("asks the next session where the session ends before R replies", async () => {
    const r = asked();
    const options = createBlockrOptions(r.client);
    const read = options.read("queue");
    await flush();
    r.requests[0]?.settle({ error: new SessionEndedError("2") });
    await flush();
    expect(r.requests.map(({ names }) => names)).toEqual([["queue"], ["queue"]]);
    r.answer(1, { queue: 5 });
    await expect(read).resolves.toBe(5);
  });

  test("fails every read of a request that R replies error to, with R's message", async () => {
    const r = asked();
    const options = createBlockrOptions(r.client);
    const reads = [options.read("queue"), options.read("gate")];
    await flush();
    r.requests[0]?.settle({
      reply: {
        id: "0",
        status: "error",
        conditions: [
          {
            severity: "error",
            class: ["conflicing_blockr_option", "blockr_error", "rlang_error", "error", "condition"],
            message: "Conflicting options set for queue: check environment variable BLOCKR_QUEUE and option blockr.queue.",
          },
        ],
      },
    });

    for (const read of reads) {
      await expect(read).rejects.toThrow(
        'R sent no values of the blockr options "queue" and "gate": R replied error. Conflicting options set for queue: check environment variable BLOCKR_QUEUE and option blockr.queue.',
      );
    }
  });

  test("fails the reads where R cannot answer, and asks again the next time they are read", async () => {
    const r = asked();
    const options = createBlockrOptions(r.client);
    const read = options.read("queue");
    await flush();
    r.requests[0]?.settle({ error: new Error("The transport is closed.") });
    await expect(read).rejects.toThrow('R sent no values of the blockr options "queue": The transport is closed.');

    const again = options.read("queue");
    await flush();
    expect(r.requests).toHaveLength(2);
    r.answer(1, { queue: 7 });
    await expect(again).resolves.toBe(7);
  });

  test("fails a read that R cannot answer once core has stopped, and reports nothing itself", async () => {
    const r = asked();
    const options = createBlockrOptions(r.client);
    const read = options.read("queue");
    await flush();
    const reported = reports();
    r.requests[0]?.settle({ error: new Error("The client sends R nothing.") });
    await expect(read).rejects.toThrow('R sent no values of the blockr options "queue": The client sends R nothing.');
    reported.restore();
    expect(reported.errors()).toEqual([]);
  });

  test("refuses a name that is no non-empty string", async () => {
    const options = createBlockrOptions(asked().client);
    await expect(options.read("")).rejects.toThrow("A blockr option is read by its name");
  });
});
