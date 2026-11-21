// What makes happy-dom, the DOM that the tests of core and of packages run
// on, do as a browser does where the components of core's React SDK need it,
// once it loads, as the test setup of core loads it (./setup.ts), and the
// stubs for the tests of packages (../stubs/index.ts):
// - In happy-dom, every element measures 0 by 0 pixels, through getters of
//   clientWidth and clientHeight on the prototype of HTMLElement, where a
//   browser's sit on the prototype of Element.
//   React Aria's Virtualizer takes getters on the prototype of HTMLElement
//   for those of a test that sizes the page, and then draws the rows that fit
//   0 pixels, which are none.
//   So the getters move to the prototype of Element, and the Virtualizer
//   draws every row in a test, as React Aria does for a DOM that sizes
//   nothing.
//   A test that checks which rows the Virtualizer draws defines the getters
//   on the prototype of HTMLElement again, with the sizes it wants.
// - In happy-dom, an empty min, max or step of a number input reads as 0,
//   where a browser reads none, and React Aria's NumberField empties them on
//   the number input that it checks a number with.
//   So setting one of them to "" removes the attribute.
//   This is happy-dom's bug #2484
//   (https://github.com/capricorn86/happy-dom/issues/2484), which its PR
//   #2516 fixes, and this part goes once a release of happy-dom holds the
//   fix.
// - In happy-dom, an IntersectionObserver never reports. So core's basic
//   frontend would count no card in view, and core would ask R for the output
//   of no card, which waits until its block is on screen.
//   So an IntersectionObserver reports each element that it observes as in
//   view, on the microtask after observe(), as on a page that shows them all.
//   This matches the stubs, which measure every output's element as a panel of
//   640 by 400 pixels (../stubs/core.ts).
//   A test that checks what is in view gives an IntersectionObserver of its
//   own.
// Outside happy-dom, none of them changes what the DOM does.

for (const name of ["clientWidth", "clientHeight"]) {
  if (typeof HTMLElement === "undefined") break;
  const getter = Object.getOwnPropertyDescriptor(HTMLElement.prototype, name);
  if (getter === undefined) continue;
  Reflect.deleteProperty(HTMLElement.prototype, name);
  if (!Object.hasOwn(Element.prototype, name)) Object.defineProperty(Element.prototype, name, getter);
}

for (const name of ["min", "max", "step"]) {
  if (typeof HTMLInputElement === "undefined") break;
  const property = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, name);
  if (property?.set === undefined) continue;
  Object.defineProperty(HTMLInputElement.prototype, name, {
    ...property,
    set(this: HTMLInputElement, value: string) {
      if (value === "") this.removeAttribute(name);
      else property.set?.call(this, value);
    },
  });
}

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
