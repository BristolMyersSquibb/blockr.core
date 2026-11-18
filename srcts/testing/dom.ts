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
// Outside happy-dom, neither changes what the DOM does.

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
