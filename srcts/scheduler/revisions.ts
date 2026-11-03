// Revisions come from one counter for the page, as an auto-increment hands out
// ids. A revision is then unique across blocks too, and a block that comes back
// takes one above any its id had
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#revisions).

// The revisions after `last`, one at a time. They stop at 2^53 - 1, the last
// whole number that a double holds exactly. Past it, adding 1 would no longer
// change a revision, and revisions would repeat.
export function counter(last = 0): () => number {
  return () => {
    if (last >= Number.MAX_SAFE_INTEGER) {
      throw new Error(
        "The page has used up its revisions, 2^53 - 1 of them, so the scheduler has stopped. Reload the page to start again.",
      );
    }
    last += 1;
    return last;
  };
}
