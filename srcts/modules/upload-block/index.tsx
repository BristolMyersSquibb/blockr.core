// The UI of the upload block: a file input,
// whose file the module sends to R through the action upload,
// which keeps the file and answers with its path,
// which the module then submits as the state arg file_path.

import { useEffect, useRef, useState } from "react";
import type { ReactElement } from "react";
import { strings } from "../../binding/index.ts";
import { Field, reactModule, Stack, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import { errorMessage } from "../../util/index.ts";
import type { State } from "./state.ts";

export const api = 1;

function UploadBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  const path = useValue(host, "file_path");
  // The name of the file on its way to R, while it is,
  // and why the last upload failed.
  const [uploading, setUploading] = useState<string | null>(null);
  const [failed, setFailed] = useState<string | null>(null);
  // An upload gives up as the module unmounts.
  const sending = useRef<AbortController | null>(null);
  useEffect(
    () => () => {
      sending.current?.abort();
    },
    [],
  );

  async function upload(file: File): Promise<void> {
    const controller = new AbortController();
    sending.current = controller;
    setUploading(file.name);
    setFailed(null);
    try {
      const [kept] = strings(await host.request("upload", {}, { file, signal: controller.signal }));
      if (kept === undefined) throw new Error(`R kept ${file.name} but gave no path for it.`);
      await path.commit(kept);
    } catch (error) {
      if (!controller.signal.aborted) setFailed(errorMessage(error));
    } finally {
      setUploading(null);
    }
  }

  const held = typeof path.value === "string" && path.value !== "" ? path.value : undefined;
  const error = failed ?? path.error;
  // The line below the input names the file on its way or the file uploaded.
  // Before then, the block's status says what is missing.
  const said = uploading !== null ? `Uploading ${uploading}…` : held === undefined ? null : `Uploaded ${basename(held)}`;
  return (
    <Stack>
      <Field label="Upload data">
        <input
          type="file"
          className={styles.box}
          disabled={uploading !== null}
          onChange={(event) => {
            const file = event.target.files?.[0];
            // The input forgets the file,
            // so that picking the same file again uploads it again.
            event.target.value = "";
            if (file !== undefined) void upload(file);
          }}
        />
      </Field>
      {said !== null && <p className={styles.hint}>{said}</p>}
      {error !== null && (
        <p role="alert" className={styles.alert}>
          {error}
        </p>
      )}
    </Stack>
  );
}

export default reactModule(UploadBlock);

function basename(path: string): string {
  return path.replace(/[\\/]+$/, "").split(/[\\/]/).pop() ?? path;
}
