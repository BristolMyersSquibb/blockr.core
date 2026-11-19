// Puts text on the clipboard, as Copy block ID does, after the card of the
// dock since blockr.dock#602
// (https://github.com/BristolMyersSquibb/blockr.dock/pull/602).
// A page that is not served over HTTPS or from the local machine has no
// clipboard API, as an app that R serves over HTTP on another machine, and a
// browser may refuse the API's write.
// So the text then goes on the clipboard through the document's copy command,
// which works only within the user's click or key press, and which the copy
// therefore runs before it waits for anything.

// Puts `text` on the clipboard, and resolves whether it is there.
export async function copyText(doc: Document, text: string): Promise<boolean> {
  const { clipboard } = (doc.defaultView?.navigator ?? {}) as Partial<Pick<Navigator, "clipboard">>;
  if (clipboard === undefined) return copyCommand(doc, text);
  try {
    await clipboard.writeText(text);
    return true;
  } catch {
    return copyCommand(doc, text);
  }
}

// Puts `text` on the clipboard through the document's copy command, from a
// field that it selects for the command and removes again, and gives the focus
// back to what had it.
function copyCommand(doc: Document, text: string): boolean {
  const before = doc.activeElement;
  const field = doc.createElement("textarea");
  field.value = text;
  field.setAttribute("readonly", "");
  field.style.cssText = "position: fixed; inset-block-start: 0; opacity: 0;";
  doc.body.append(field);
  field.select();
  try {
    // The copy command is the last resort where the page has no clipboard
    // API, which a page served over HTTP lacks.
    // eslint-disable-next-line @typescript-eslint/no-deprecated
    return doc.execCommand("copy");
  } catch {
    return false;
  } finally {
    field.remove();
    if (before instanceof HTMLElement) before.focus({ preventScroll: true });
  }
}
