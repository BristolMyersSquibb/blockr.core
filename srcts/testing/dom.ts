// What makes happy-dom, the DOM that the tests of core and of packages run
// on, do as a browser does where core's basic frontend needs it, once it
// loads, as the test setup of core loads it (./setup.ts), and the stubs for
// the tests of packages (../stubs/index.ts).
// In happy-dom, an IntersectionObserver never reports. So core's basic
// frontend would count no card in view, and core would ask R for the output
// of no card, which waits until its block is on screen.
// So an IntersectionObserver reports each element that it observes as in
// view, on the microtask after observe(), as on a page that shows them all.
// This matches the stubs, which measure every output's element as a panel of
// 640 by 400 pixels (../stubs/core.ts).
// A test that checks what is in view gives an IntersectionObserver of its own.
// Outside happy-dom, this changes nothing.
// What blockr.ui's components need of happy-dom, `@blockr/ui/stubs` does as
// it loads, which core's stubs load (../stubs/index.ts).

if (typeof navigator !== "undefined" && navigator.userAgent.includes("HappyDOM")) {
  globalThis.IntersectionObserver = class InView implements IntersectionObserver {
    readonly root = null;
    readonly rootMargin = "0px";
    readonly scrollMargin = "0px";
    readonly thresholds: readonly number[] = [0];
    readonly #callback: IntersectionObserverCallback;
    readonly #observed = new Set<Element>();

    constructor(callback: IntersectionObserverCallback) {
      this.#callback = callback;
    }

    observe(target: Element): void {
      this.#observed.add(target);
      queueMicrotask(() => {
        if (!this.#observed.has(target)) return;
        const rect = target.getBoundingClientRect();
        const entry = {
          target,
          isIntersecting: true,
          intersectionRatio: 1,
          boundingClientRect: rect,
          intersectionRect: rect,
          rootBounds: null,
          time: performance.now(),
        } satisfies IntersectionObserverEntry;
        this.#callback([entry], this);
      });
    }

    unobserve(target: Element): void {
      this.#observed.delete(target);
    }

    disconnect(): void {
      this.#observed.clear();
    }

    takeRecords(): IntersectionObserverEntry[] {
      return [];
    }
  };
}
