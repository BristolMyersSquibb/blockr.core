// node_modules/@preact/signals-core/dist/signals-core.module.js
var i = /* @__PURE__ */ Symbol.for("preact-signals");
function t() {
  if (!(v > 1)) {
    var i2, t2 = false;
    !(function() {
      var i3 = c;
      c = void 0;
      while (void 0 !== i3) {
        var t3 = i3.S;
        if (t3.v === i3.v) {
          for (var r3 = t3.t; void 0 !== r3; r3 = r3.x) if (r3.i === i3.i) r3.i = t3.i;
        }
        i3 = i3.o;
      }
    })();
    while (void 0 !== h) {
      var r2 = h;
      h = void 0;
      s++;
      while (void 0 !== r2) {
        var n2 = r2.u;
        r2.u = void 0;
        r2.f &= -3;
        if (!(8 & r2.f) && w(r2)) try {
          r2.c();
        } catch (r3) {
          if (!t2) {
            i2 = r3;
            t2 = true;
          }
        }
        r2 = n2;
      }
    }
    s = 0;
    v--;
    if (t2) throw i2;
  } else v--;
}
function r(i2) {
  if (v > 0) return i2();
  u = ++e;
  v++;
  try {
    return i2();
  } finally {
    t();
  }
}
var n;
var o = void 0;
function f(i2) {
  var t2 = o, r2 = n;
  o = void 0;
  n = void 0;
  try {
    return i2();
  } finally {
    o = t2;
    n = r2;
  }
}
var h = void 0;
var v = 0;
var s = 0;
var e = 0;
var u = 0;
var c = void 0;
var a = 0;
function d(i2) {
  if (void 0 !== o) {
    var t2 = i2.n;
    if (void 0 === t2 || t2.t !== o) {
      t2 = { i: 0, S: i2, p: o.s, n: void 0, t: o, e: void 0, x: void 0, r: t2 };
      if (void 0 !== o.s) o.s.n = t2;
      o.s = t2;
      i2.n = t2;
      if (32 & o.f) i2.S(t2);
      return t2;
    } else if (-1 === t2.i) {
      t2.i = 0;
      if (void 0 !== t2.n) {
        t2.n.p = t2.p;
        if (void 0 !== t2.p) t2.p.n = t2.n;
        t2.p = o.s;
        t2.n = void 0;
        o.s.n = t2;
        o.s = t2;
      }
      return t2;
    }
  }
}
function l(i2, t2) {
  this.v = i2;
  this.i = 0;
  this.n = void 0;
  this.t = void 0;
  this.l = 0;
  this.W = null == t2 ? void 0 : t2.watched;
  this.Z = null == t2 ? void 0 : t2.unwatched;
  this.name = null == t2 ? void 0 : t2.name;
}
l.prototype.brand = i;
l.prototype.h = function() {
  return true;
};
l.prototype.S = function(i2) {
  var t2 = this, r2 = this.t;
  if (r2 !== i2 && void 0 === i2.e) {
    i2.x = r2;
    this.t = i2;
    if (void 0 !== r2) r2.e = i2;
    else f(function() {
      var i3;
      null == (i3 = t2.W) || i3.call(t2);
    });
  }
};
l.prototype.U = function(i2) {
  var t2 = this;
  if (void 0 !== this.t) {
    var r2 = i2.e, n2 = i2.x;
    if (void 0 !== r2) {
      r2.x = n2;
      i2.e = void 0;
    }
    if (void 0 !== n2) {
      n2.e = r2;
      i2.x = void 0;
    }
    if (i2 === this.t) {
      this.t = n2;
      if (void 0 === n2) f(function() {
        var i3;
        null == (i3 = t2.Z) || i3.call(t2);
      });
    }
  }
};
l.prototype.subscribe = function(i2) {
  var t2 = this;
  return x(function() {
    var r2 = t2.value;
    f(function() {
      return i2(r2);
    });
  }, { name: "sub" });
};
l.prototype.valueOf = function() {
  return this.value;
};
l.prototype.toString = function() {
  return this.value + "";
};
l.prototype.toJSON = function() {
  return this.value;
};
l.prototype.peek = function() {
  var i2 = this;
  return f(function() {
    return i2.value;
  });
};
Object.defineProperty(l.prototype, "value", { get: function() {
  var i2 = d(this);
  if (void 0 !== i2) i2.i = this.i;
  return this.v;
}, set: function(i2) {
  if (i2 !== this.v) {
    if (s > 100) throw new Error("Cycle detected");
    !(function(i3) {
      if (0 !== v && 0 === s) {
        if (i3.l !== u) {
          i3.l = u;
          c = { S: i3, v: i3.v, i: i3.i, o: c };
        }
      }
    })(this);
    this.v = i2;
    this.i++;
    a++;
    v++;
    try {
      for (var r2 = this.t; void 0 !== r2; r2 = r2.x) r2.t.N();
    } finally {
      t();
    }
  }
} });
function y(i2, t2) {
  return new l(i2, t2);
}
function w(i2) {
  for (var t2 = i2.s; void 0 !== t2; t2 = t2.n) if (t2.S.i !== t2.i || !t2.S.h() || t2.S.i !== t2.i) return true;
  return false;
}
function b(i2) {
  for (var t2 = i2.s; void 0 !== t2; t2 = t2.n) {
    var r2 = t2.S.n;
    if (void 0 !== r2) t2.r = r2;
    t2.S.n = t2;
    t2.i = -1;
    if (void 0 === t2.n) {
      i2.s = t2;
      break;
    }
  }
}
function _(i2) {
  var t2 = i2.s, r2 = void 0;
  while (void 0 !== t2) {
    var n2 = t2.p;
    if (-1 === t2.i) {
      t2.S.U(t2);
      if (void 0 !== n2) n2.n = t2.n;
      if (void 0 !== t2.n) t2.n.p = n2;
    } else r2 = t2;
    t2.S.n = t2.r;
    if (void 0 !== t2.r) t2.r = void 0;
    t2 = n2;
  }
  i2.s = r2;
}
function j(i2, t2) {
  l.call(this, void 0, t2);
  this.x = i2;
  this.s = void 0;
  this.g = a - 1;
  this.f = 4;
}
j.prototype = new l();
j.prototype.h = function() {
  this.f &= -3;
  if (1 & this.f) return false;
  if (32 == (36 & this.f)) return true;
  this.f &= -5;
  if (this.g === a) return true;
  this.g = a;
  this.f |= 1;
  if (this.i > 0 && !w(this)) {
    this.f &= -2;
    return true;
  }
  var i2 = o;
  try {
    b(this);
    o = this;
    var t2 = this.x();
    if (16 & this.f || this.v !== t2 || 0 === this.i) {
      this.v = t2;
      this.f &= -17;
      this.i++;
    }
  } catch (i3) {
    this.v = i3;
    this.f |= 16;
    this.i++;
  }
  o = i2;
  _(this);
  this.f &= -2;
  return true;
};
j.prototype.S = function(i2) {
  if (void 0 === this.t) {
    this.f |= 36;
    for (var t2 = this.s; void 0 !== t2; t2 = t2.n) t2.S.S(t2);
  }
  l.prototype.S.call(this, i2);
};
j.prototype.U = function(i2) {
  if (void 0 !== this.t) {
    l.prototype.U.call(this, i2);
    if (void 0 === this.t) {
      this.f &= -33;
      for (var t2 = this.s; void 0 !== t2; t2 = t2.n) t2.S.U(t2);
    }
  }
};
j.prototype.N = function() {
  if (!(2 & this.f)) {
    this.f |= 6;
    for (var i2 = this.t; void 0 !== i2; i2 = i2.x) i2.t.N();
  }
};
Object.defineProperty(j.prototype, "value", { get: function() {
  if (1 & this.f) throw new Error("Cycle detected");
  var i2 = d(this);
  this.h();
  if (void 0 !== i2) i2.i = this.i;
  if (16 & this.f) throw this.v;
  return this.v;
} });
function O(i2, t2) {
  return new j(i2, t2);
}
function p(i2) {
  var r2 = i2.j;
  i2.j = void 0;
  if ("function" == typeof r2) {
    v++;
    var n2 = o;
    o = void 0;
    try {
      r2();
    } catch (t2) {
      i2.f &= -2;
      i2.f |= 8;
      g(i2);
      throw t2;
    } finally {
      o = n2;
      t();
    }
  }
}
function g(i2) {
  for (var t2 = i2.s; void 0 !== t2; t2 = t2.n) t2.S.U(t2);
  i2.x = void 0;
  i2.s = void 0;
  p(i2);
}
function S(i2) {
  if (o !== this) throw new Error("Out-of-order effect");
  _(this);
  o = i2;
  this.f &= -2;
  try {
    if (8 & this.f) g(this);
  } finally {
    t();
  }
}
function m(i2, t2) {
  this.x = i2;
  this.j = void 0;
  this.s = void 0;
  this.u = void 0;
  this.f = 32;
  this.name = null == t2 ? void 0 : t2.name;
  if (n) n.push(this);
}
m.prototype.c = function() {
  var i2 = this.S();
  try {
    if (8 & this.f) return;
    if (void 0 === this.x) return;
    var t2 = this.x();
    if ("function" == typeof t2) this.j = t2;
  } finally {
    i2();
  }
};
m.prototype.S = function() {
  if (1 & this.f) throw new Error("Cycle detected");
  this.f |= 1;
  this.f &= -9;
  p(this);
  b(this);
  v++;
  var i2 = o;
  o = this;
  return S.bind(this, i2);
};
m.prototype.N = function() {
  if (!(2 & this.f)) {
    this.f |= 2;
    this.u = h;
    h = this;
  }
};
m.prototype.d = function() {
  this.f |= 8;
  if (!(1 & this.f)) g(this);
};
m.prototype.dispose = function() {
  this.d();
};
function x(i2, t2) {
  var r2 = new m(i2, t2);
  try {
    r2.c();
  } catch (i3) {
    r2.d();
    throw i3;
  }
  var n2 = r2.d.bind(r2);
  n2[Symbol.dispose] = n2;
  return n2;
}
function E(i2) {
  return function() {
    var t2 = arguments, n2 = this;
    return r(function() {
      return f(function() {
        return i2.apply(n2, [].slice.call(t2));
      });
    });
  };
}

// srcts/graph/graph.ts
function graphOf(board) {
  const blocks = Object.keys(board.blocks);
  const into = /* @__PURE__ */ new Map();
  const sources = /* @__PURE__ */ new Map();
  const targets = /* @__PURE__ */ new Map();
  for (const id2 of blocks) {
    into.set(id2, []);
    sources.set(id2, []);
    targets.set(id2, []);
  }
  for (const [id2, link] of Object.entries(board.links)) {
    const linksIn = into.get(link.to);
    const from = sources.get(link.to);
    const to = targets.get(link.from);
    if (linksIn === void 0 || from === void 0 || to === void 0) continue;
    linksIn.push(id2);
    from.push(link.from);
    to.push(link.to);
  }
  const upstream = once(sources);
  const downstream = once(targets);
  const order = topological(blocks, sources, targets);
  let cycle;
  return {
    order,
    linksInto: (id2) => into.get(id2) ?? [],
    upstream: (id2) => upstream.get(id2) ?? [],
    downstream: (id2) => downstream.get(id2) ?? [],
    withUpstream: (ids2) => closure(ids2, upstream),
    withDownstream: (ids2) => closure(ids2, downstream),
    cycle() {
      cycle ??= { found: findCycle(board, targets) };
      return cycle.found;
    }
  };
}
function once(ends) {
  return new Map([...ends].map(([id2, blocks]) => [id2, [...new Set(blocks)]]));
}
function closure(ids2, steps) {
  const reached = /* @__PURE__ */ new Set();
  const todo = [...ids2].filter((id2) => steps.has(id2));
  for (const id2 of todo) reached.add(id2);
  for (const id2 of todo) {
    for (const next of steps.get(id2) ?? []) {
      if (reached.has(next)) continue;
      reached.add(next);
      todo.push(next);
    }
  }
  return reached;
}
function topological(blocks, sources, targets) {
  const left2 = new Map(blocks.map((id2) => [id2, sources.get(id2)?.length ?? 0]));
  const order = blocks.filter((id2) => left2.get(id2) === 0);
  for (const id2 of order) {
    for (const next of targets.get(id2) ?? []) {
      const count = (left2.get(next) ?? 0) - 1;
      left2.set(next, count);
      if (count === 0) order.push(next);
    }
  }
  const placed = new Set(order);
  return [...order, ...blocks.filter((id2) => !placed.has(id2))];
}
function findCycle(board, targets) {
  const done = /* @__PURE__ */ new Set();
  const path = [];
  const onPath = /* @__PURE__ */ new Set();
  function visit(block) {
    if (onPath.has(block)) return path.slice(path.indexOf(block));
    if (done.has(block)) return void 0;
    path.push(block);
    onPath.add(block);
    for (const next of targets.get(block) ?? []) {
      const cycle = visit(next);
      if (cycle !== void 0) return cycle;
    }
    path.pop();
    onPath.delete(block);
    done.add(block);
    return void 0;
  }
  const starts = new Set(Object.values(board.links).map((link) => link.from));
  for (const block of starts) {
    if (!targets.has(block)) continue;
    const cycle = visit(block);
    if (cycle !== void 0) return cycle;
  }
  return void 0;
}

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function messageOf(error) {
  return error instanceof Error ? error.message : String(error);
}
function asError(error) {
  return error instanceof Error ? error : new Error(String(error));
}
function report(error) {
  queueMicrotask(() => {
    throw error;
  });
}
function isObject(x2) {
  return typeof x2 === "object" && x2 !== null && !Array.isArray(x2);
}
function jsonEqual(a2, b2) {
  if (a2 === b2) return true;
  if (Array.isArray(a2)) {
    return Array.isArray(b2) && a2.length === b2.length && a2.every((x2, i2) => jsonEqual(x2, b2[i2]));
  }
  if (isObject(a2) && isObject(b2)) {
    const keys = Object.keys(a2);
    return keys.length === Object.keys(b2).length && keys.every((key) => Object.hasOwn(b2, key) && jsonEqual(a2[key], b2[key]));
  }
  return false;
}
function without(record, keys) {
  if (!keys.some((key) => Object.hasOwn(record, key))) return record;
  return Object.fromEntries(
    Object.entries(record).filter(([key]) => !keys.includes(key))
  );
}
function frozenCopy(x2) {
  return freeze(structuredClone(x2));
}
function freeze(x2) {
  if (typeof x2 === "object" && x2 !== null) {
    for (const value2 of Object.values(x2)) freeze(value2);
    Object.freeze(x2);
  }
  return x2;
}
function listing(items) {
  if (items.length <= 1) return items.join("");
  return `${items.slice(0, -1).join(", ")} and ${items.at(-1) ?? ""}`;
}
function quoted(items) {
  return listing(items.map((item) => `"${item}"`));
}
function packageOf(id2) {
  const at = id2.indexOf("-");
  if (at < 0) return null;
  const pkg = id2.slice(0, at);
  return isPackageName(pkg) ? pkg : null;
}
function isPackageName(name3) {
  return typeof name3 === "string" && /^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$/.test(name3);
}
function numberOf(text) {
  const trimmed = text.trim();
  if (/^[+-]?inf(inity)?$/i.test(trimmed)) return trimmed.startsWith("-") ? -Infinity : Infinity;
  return trimmed === "" ? NaN : Number(trimmed);
}
function logicalOf(text) {
  if (["TRUE", "true", "True", "T"].includes(text)) return true;
  if (["FALSE", "false", "False", "F"].includes(text)) return false;
  return void 0;
}

// node_modules/@cfworker/json-schema/dist/esm/deep-compare-strict.js
function deepCompareStrict(a2, b2) {
  const typeofa = typeof a2;
  if (typeofa !== typeof b2) {
    return false;
  }
  const aIsArray = Array.isArray(a2);
  const bIsArray = Array.isArray(b2);
  if (aIsArray !== bIsArray) {
    return false;
  }
  if (aIsArray) {
    const length = a2.length;
    if (length !== b2.length) {
      return false;
    }
    for (let i2 = 0; i2 < length; i2++) {
      if (!deepCompareStrict(a2[i2], b2[i2])) {
        return false;
      }
    }
    return true;
  }
  if (typeofa === "object") {
    if (!a2 || !b2) {
      return a2 === b2;
    }
    const aKeys = Object.keys(a2);
    const bKeys = Object.keys(b2);
    const length = aKeys.length;
    if (length !== bKeys.length) {
      return false;
    }
    for (const k of aKeys) {
      if (!Object.prototype.hasOwnProperty.call(b2, k)) {
        return false;
      }
      if (!deepCompareStrict(a2[k], b2[k])) {
        return false;
      }
    }
    return true;
  }
  return a2 === b2;
}

// node_modules/@cfworker/json-schema/dist/esm/pointer.js
function encodePointer(p2) {
  return encodeURI(escapePointer(p2));
}
function escapePointer(p2) {
  return p2.replace(/~/g, "~0").replace(/\//g, "~1");
}

// node_modules/@cfworker/json-schema/dist/esm/dereference.js
var schemaArrayKeyword = {
  prefixItems: true,
  items: true,
  allOf: true,
  anyOf: true,
  oneOf: true
};
var schemaMapKeyword = {
  $defs: true,
  definitions: true,
  properties: true,
  patternProperties: true,
  dependentSchemas: true
};
var ignoredKeyword = {
  id: true,
  $id: true,
  $ref: true,
  $schema: true,
  $anchor: true,
  $vocabulary: true,
  $comment: true,
  default: true,
  enum: true,
  const: true,
  required: true,
  dependentRequired: true,
  type: true,
  maximum: true,
  minimum: true,
  exclusiveMaximum: true,
  exclusiveMinimum: true,
  multipleOf: true,
  maxLength: true,
  minLength: true,
  pattern: true,
  format: true,
  maxItems: true,
  minItems: true,
  uniqueItems: true,
  maxProperties: true,
  minProperties: true
};
var initialBaseURI = typeof self !== "undefined" && self.location && self.location.origin !== "null" ? new URL(self.location.origin + self.location.pathname + location.search) : new URL("https://github.com/cfworker");
function dereference(schema, lookup = /* @__PURE__ */ Object.create(null), baseURI = initialBaseURI, basePointer = "") {
  if (schema && typeof schema === "object" && !Array.isArray(schema)) {
    const id2 = schema.$id || schema["id"];
    if (id2) {
      const url = new URL(id2, baseURI.href);
      if (url.hash.length > 1) {
        lookup[url.href] = schema;
      } else {
        url.hash = "";
        if (basePointer === "") {
          baseURI = url;
        } else {
          dereference(schema, lookup, baseURI);
        }
      }
    }
  } else if (schema !== true && schema !== false) {
    return lookup;
  }
  const schemaURI = baseURI.href + (basePointer ? "#" + basePointer : "");
  if (lookup[schemaURI] !== void 0 && lookup[schemaURI] !== schema) {
    throw new Error(`Duplicate schema URI "${schemaURI}".`);
  }
  lookup[schemaURI] = schema;
  if (schema === true || schema === false) {
    return lookup;
  }
  if (schema.__absolute_uri__ === void 0) {
    Object.defineProperty(schema, "__absolute_uri__", {
      enumerable: false,
      value: schemaURI
    });
  }
  if (schema.$ref && schema.__absolute_ref__ === void 0) {
    const url = new URL(schema.$ref, baseURI.href);
    url.hash = url.hash;
    Object.defineProperty(schema, "__absolute_ref__", {
      enumerable: false,
      value: url.href
    });
  }
  if (schema.$recursiveRef && schema.__absolute_recursive_ref__ === void 0) {
    const url = new URL(schema.$recursiveRef, baseURI.href);
    url.hash = url.hash;
    Object.defineProperty(schema, "__absolute_recursive_ref__", {
      enumerable: false,
      value: url.href
    });
  }
  if (schema.$anchor) {
    const url = new URL("#" + schema.$anchor, baseURI.href);
    lookup[url.href] = schema;
  }
  for (let key in schema) {
    if (ignoredKeyword[key]) {
      continue;
    }
    const keyBase = `${basePointer}/${encodePointer(key)}`;
    const subSchema = schema[key];
    if (Array.isArray(subSchema)) {
      if (schemaArrayKeyword[key]) {
        const length = subSchema.length;
        for (let i2 = 0; i2 < length; i2++) {
          dereference(subSchema[i2], lookup, baseURI, `${keyBase}/${i2}`);
        }
      }
    } else if (schemaMapKeyword[key]) {
      for (let subKey in subSchema) {
        dereference(subSchema[subKey], lookup, baseURI, `${keyBase}/${encodePointer(subKey)}`);
      }
    } else {
      dereference(subSchema, lookup, baseURI, keyBase);
    }
  }
  return lookup;
}

// node_modules/@cfworker/json-schema/dist/esm/format.js
var DATE = /^(\d\d\d\d)-(\d\d)-(\d\d)$/;
var DAYS = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
var TIME = /^(\d\d):(\d\d):(\d\d)(\.\d+)?(z|[+-]\d\d(?::?\d\d)?)?$/i;
var HOSTNAME = /^(?=.{1,253}\.?$)[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\.[a-z0-9](?:[-0-9a-z]{0,61}[0-9a-z])?)*\.?$/i;
var URIREF = /^(?:[a-z][a-z0-9+\-.]*:)?(?:\/?\/(?:(?:[a-z0-9\-._~!$&'()*+,;=:]|%[0-9a-f]{2})*@)?(?:\[(?:(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:){0,1}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)|[Vv][0-9a-f]+\.[a-z0-9\-._~!$&'()*+,;=:]+)\]|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)|(?:[a-z0-9\-._~!$&'"()*+,;=]|%[0-9a-f]{2})*)(?::\d*)?(?:\/(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})*)*|\/(?:(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})*)*)?|(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})*)*)?(?:\?(?:[a-z0-9\-._~!$&'"()*+,;=:@/?]|%[0-9a-f]{2})*)?(?:#(?:[a-z0-9\-._~!$&'"()*+,;=:@/?]|%[0-9a-f]{2})*)?$/i;
var URITEMPLATE = /^(?:(?:[^\x00-\x20"'<>%\\^`{|}]|%[0-9a-f]{2})|\{[+#./;?&=,!@|]?(?:[a-z0-9_]|%[0-9a-f]{2})+(?::[1-9][0-9]{0,3}|\*)?(?:,(?:[a-z0-9_]|%[0-9a-f]{2})+(?::[1-9][0-9]{0,3}|\*)?)*\})*$/i;
var URL_ = /^(?:(?:https?|ftp):\/\/)(?:\S+(?::\S*)?@)?(?:(?!10(?:\.\d{1,3}){3})(?!127(?:\.\d{1,3}){3})(?!169\.254(?:\.\d{1,3}){2})(?!192\.168(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z\u{00a1}-\u{ffff}0-9]+-?)*[a-z\u{00a1}-\u{ffff}0-9]+)(?:\.(?:[a-z\u{00a1}-\u{ffff}0-9]+-?)*[a-z\u{00a1}-\u{ffff}0-9]+)*(?:\.(?:[a-z\u{00a1}-\u{ffff}]{2,})))(?::\d{2,5})?(?:\/[^\s]*)?$/iu;
var UUID = /^(?:urn:uuid:)?[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/i;
var JSON_POINTER = /^(?:\/(?:[^~/]|~0|~1)*)*$/;
var JSON_POINTER_URI_FRAGMENT = /^#(?:\/(?:[a-z0-9_\-.!$&'()*+,;:=@]|%[0-9a-f]{2}|~0|~1)*)*$/i;
var RELATIVE_JSON_POINTER = /^(?:0|[1-9][0-9]*)(?:#|(?:\/(?:[^~/]|~0|~1)*)*)$/;
var EMAIL = (input2) => {
  if (input2[0] === '"')
    return false;
  const [name3, host, ...rest] = input2.split("@");
  if (!name3 || !host || rest.length !== 0 || name3.length > 64 || host.length > 253)
    return false;
  if (name3[0] === "." || name3.endsWith(".") || name3.includes(".."))
    return false;
  if (!/^[a-z0-9.-]+$/i.test(host) || !/^[a-z0-9.!#$%&'*+/=?^_`{|}~-]+$/i.test(name3))
    return false;
  return host.split(".").every((part) => /^[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$/i.test(part));
};
var IPV4 = /^(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)$/;
var IPV6 = /^((([0-9a-f]{1,4}:){7}([0-9a-f]{1,4}|:))|(([0-9a-f]{1,4}:){6}(:[0-9a-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){5}(((:[0-9a-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){4}(((:[0-9a-f]{1,4}){1,3})|((:[0-9a-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){3}(((:[0-9a-f]{1,4}){1,4})|((:[0-9a-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){2}(((:[0-9a-f]{1,4}){1,5})|((:[0-9a-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){1}(((:[0-9a-f]{1,4}){1,6})|((:[0-9a-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9a-f]{1,4}){1,7})|((:[0-9a-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))$/i;
var DURATION = (input2) => input2.length > 1 && input2.length < 80 && (/^P\d+([.,]\d+)?W$/.test(input2) || /^P[\dYMDTHS]*(\d[.,]\d+)?[YMDHS]$/.test(input2) && /^P([.,\d]+Y)?([.,\d]+M)?([.,\d]+D)?(T([.,\d]+H)?([.,\d]+M)?([.,\d]+S)?)?$/.test(input2));
function bind(r2) {
  return r2.test.bind(r2);
}
var format = {
  date,
  time: time.bind(void 0, false),
  "date-time": date_time,
  duration: DURATION,
  uri,
  "uri-reference": bind(URIREF),
  "uri-template": bind(URITEMPLATE),
  url: bind(URL_),
  email: EMAIL,
  hostname: bind(HOSTNAME),
  ipv4: bind(IPV4),
  ipv6: bind(IPV6),
  regex,
  uuid: bind(UUID),
  "json-pointer": bind(JSON_POINTER),
  "json-pointer-uri-fragment": bind(JSON_POINTER_URI_FRAGMENT),
  "relative-json-pointer": bind(RELATIVE_JSON_POINTER)
};
function isLeapYear(year) {
  return year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0);
}
function date(str) {
  const matches = str.match(DATE);
  if (!matches)
    return false;
  const yearPart = matches[1];
  const monthPart = matches[2];
  const dayPart = matches[3];
  if (!yearPart || !monthPart || !dayPart)
    return false;
  const year = +yearPart;
  const month = +monthPart;
  const day = +dayPart;
  const maxDay = month == 2 && isLeapYear(year) ? 29 : DAYS[month];
  return month >= 1 && month <= 12 && day >= 1 && maxDay !== void 0 && day <= maxDay;
}
function time(full, str) {
  const matches = str.match(TIME);
  if (!matches)
    return false;
  const hourPart = matches[1];
  const minutePart = matches[2];
  const secondPart = matches[3];
  if (!hourPart || !minutePart || !secondPart)
    return false;
  const hour = +hourPart;
  const minute = +minutePart;
  const second = +secondPart;
  const timeZone = !!matches[5];
  return (hour <= 23 && minute <= 59 && second <= 59 || hour == 23 && minute == 59 && second == 60) && (!full || timeZone);
}
var DATE_TIME_SEPARATOR = /t|\s/i;
function date_time(str) {
  const [datePart, timePart, extra] = str.split(DATE_TIME_SEPARATOR);
  return datePart !== void 0 && timePart !== void 0 && extra === void 0 && date(datePart) && time(true, timePart);
}
var NOT_URI_FRAGMENT = /\/|:/;
var URI_PATTERN = /^(?:[a-z][a-z0-9+\-.]*:)(?:\/?\/(?:(?:[a-z0-9\-._~!$&'()*+,;=:]|%[0-9a-f]{2})*@)?(?:\[(?:(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:){0,1}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)|[Vv][0-9a-f]+\.[a-z0-9\-._~!$&'()*+,;=:]+)\]|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)|(?:[a-z0-9\-._~!$&'()*+,;=]|%[0-9a-f]{2})*)(?::\d*)?(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*|\/(?:(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)?|(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)(?:\?(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?(?:#(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?$/i;
function uri(str) {
  return NOT_URI_FRAGMENT.test(str) && URI_PATTERN.test(str);
}
var Z_ANCHOR = /[^\\]\\Z/;
function regex(str) {
  if (Z_ANCHOR.test(str))
    return false;
  try {
    new RegExp(str, "u");
    return true;
  } catch (e2) {
    return false;
  }
}

// node_modules/@cfworker/json-schema/dist/esm/ucs2-length.js
function ucs2length(s2) {
  let result = 0;
  let length = s2.length;
  let index = 0;
  let charCode;
  while (index < length) {
    result++;
    charCode = s2.charCodeAt(index++);
    if (charCode >= 55296 && charCode <= 56319 && index < length) {
      charCode = s2.charCodeAt(index);
      if ((charCode & 64512) == 56320) {
        index++;
      }
    }
  }
  return result;
}

// node_modules/@cfworker/json-schema/dist/esm/validate.js
function hasOwnProperty(instance, key) {
  return Object.prototype.hasOwnProperty.call(instance, key);
}
function decimalInteger(value2) {
  const coefficientAndExponent = Math.abs(value2).toString().split("e");
  const coefficient = coefficientAndExponent[0];
  const exponent = coefficientAndExponent[1] ?? "0";
  const point = coefficient.indexOf(".");
  const fractionLength = point === -1 ? 0 : coefficient.length - point - 1;
  return [
    BigInt(coefficient.replace(".", "")),
    Number(exponent) - fractionLength
  ];
}
function isMultipleOf(instance, divisor) {
  if (!Number.isFinite(instance) || !Number.isFinite(divisor) || divisor <= 0) {
    return false;
  }
  const [instanceCoefficient, instanceExponent] = decimalInteger(instance);
  const [divisorCoefficient, divisorExponent] = decimalInteger(divisor);
  const exponentDifference = instanceExponent - divisorExponent;
  if (exponentDifference >= 0) {
    return instanceCoefficient * 10n ** BigInt(exponentDifference) % divisorCoefficient === 0n;
  }
  return instanceCoefficient % (divisorCoefficient * 10n ** BigInt(-exponentDifference)) === 0n;
}
function validate(instance, schema, draft = "2019-09", lookup = dereference(schema), shortCircuit = true, recursiveAnchor = null, instanceLocation = "#", schemaLocation = "#", evaluated = /* @__PURE__ */ Object.create(null)) {
  if (schema === true) {
    return { valid: true, errors: [] };
  }
  if (schema === false) {
    return {
      valid: false,
      errors: [
        {
          instanceLocation,
          keyword: "false",
          keywordLocation: instanceLocation,
          error: "False boolean schema."
        }
      ]
    };
  }
  const rawInstanceType = typeof instance;
  let instanceType;
  switch (rawInstanceType) {
    case "boolean":
    case "number":
    case "string":
      instanceType = rawInstanceType;
      break;
    case "object":
      if (instance === null) {
        instanceType = "null";
      } else if (Array.isArray(instance)) {
        instanceType = "array";
      } else {
        instanceType = "object";
      }
      break;
    default:
      throw new Error(`Instances of "${rawInstanceType}" type are not supported.`);
  }
  const { $ref, $recursiveRef, $recursiveAnchor, type: $type, const: $const, enum: $enum, required: $required, not: $not, anyOf: $anyOf, allOf: $allOf, oneOf: $oneOf, if: $if, then: $then, else: $else, format: $format, properties: $properties, patternProperties: $patternProperties, additionalProperties: $additionalProperties, unevaluatedProperties: $unevaluatedProperties, minProperties: $minProperties, maxProperties: $maxProperties, propertyNames: $propertyNames, dependentRequired: $dependentRequired, dependentSchemas: $dependentSchemas, dependencies: $dependencies, prefixItems: $prefixItems, items: $items, additionalItems: $additionalItems, unevaluatedItems: $unevaluatedItems, contains: $contains, minContains: $minContains, maxContains: $maxContains, minItems: $minItems, maxItems: $maxItems, uniqueItems: $uniqueItems, minimum: $minimum, maximum: $maximum, exclusiveMinimum: $exclusiveMinimum, exclusiveMaximum: $exclusiveMaximum, multipleOf: $multipleOf, minLength: $minLength, maxLength: $maxLength, pattern: $pattern, __absolute_ref__, __absolute_recursive_ref__ } = schema;
  const errors = [];
  if ($recursiveAnchor === true && recursiveAnchor === null) {
    recursiveAnchor = schema;
  }
  if ($recursiveRef === "#") {
    const refSchema = recursiveAnchor === null ? lookup[__absolute_recursive_ref__] : recursiveAnchor;
    const keywordLocation = `${schemaLocation}/$recursiveRef`;
    const result = validate(instance, recursiveAnchor === null ? schema : recursiveAnchor, draft, lookup, shortCircuit, refSchema, instanceLocation, keywordLocation, evaluated);
    if (!result.valid) {
      errors.push({
        instanceLocation,
        keyword: "$recursiveRef",
        keywordLocation,
        error: "A subschema had errors."
      }, ...result.errors);
    }
  }
  if ($ref !== void 0) {
    const uri2 = __absolute_ref__ || $ref;
    const refSchema = lookup[uri2];
    if (refSchema === void 0) {
      let message = `Unresolved $ref "${$ref}".`;
      if (__absolute_ref__ && __absolute_ref__ !== $ref) {
        message += `  Absolute URI "${__absolute_ref__}".`;
      }
      message += `
Known schemas:
- ${Object.keys(lookup).join("\n- ")}`;
      throw new Error(message);
    }
    const keywordLocation = `${schemaLocation}/$ref`;
    const result = validate(instance, refSchema, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, keywordLocation, evaluated);
    if (!result.valid) {
      errors.push({
        instanceLocation,
        keyword: "$ref",
        keywordLocation,
        error: "A subschema had errors."
      }, ...result.errors);
    }
    if (draft === "4" || draft === "7") {
      return { valid: errors.length === 0, errors };
    }
  }
  if (Array.isArray($type)) {
    let length = $type.length;
    let valid = false;
    for (let i2 = 0; i2 < length; i2++) {
      if (instanceType === $type[i2] || $type[i2] === "integer" && instanceType === "number" && instance % 1 === 0 && instance === instance) {
        valid = true;
        break;
      }
    }
    if (!valid) {
      errors.push({
        instanceLocation,
        keyword: "type",
        keywordLocation: `${schemaLocation}/type`,
        error: `Instance type "${instanceType}" is invalid. Expected "${$type.join('", "')}".`
      });
    }
  } else if ($type === "integer") {
    if (instanceType !== "number" || instance % 1 || instance !== instance) {
      errors.push({
        instanceLocation,
        keyword: "type",
        keywordLocation: `${schemaLocation}/type`,
        error: `Instance type "${instanceType}" is invalid. Expected "${$type}".`
      });
    }
  } else if ($type !== void 0 && instanceType !== $type) {
    errors.push({
      instanceLocation,
      keyword: "type",
      keywordLocation: `${schemaLocation}/type`,
      error: `Instance type "${instanceType}" is invalid. Expected "${$type}".`
    });
  }
  if ($const !== void 0) {
    if (instanceType === "object" || instanceType === "array") {
      if (!deepCompareStrict(instance, $const)) {
        errors.push({
          instanceLocation,
          keyword: "const",
          keywordLocation: `${schemaLocation}/const`,
          error: `Instance does not match ${JSON.stringify($const)}.`
        });
      }
    } else if (instance !== $const) {
      errors.push({
        instanceLocation,
        keyword: "const",
        keywordLocation: `${schemaLocation}/const`,
        error: `Instance does not match ${JSON.stringify($const)}.`
      });
    }
  }
  if ($enum !== void 0) {
    if (instanceType === "object" || instanceType === "array") {
      if (!$enum.some((value2) => deepCompareStrict(instance, value2))) {
        errors.push({
          instanceLocation,
          keyword: "enum",
          keywordLocation: `${schemaLocation}/enum`,
          error: `Instance does not match any of ${JSON.stringify($enum)}.`
        });
      }
    } else if (!$enum.some((value2) => instance === value2)) {
      errors.push({
        instanceLocation,
        keyword: "enum",
        keywordLocation: `${schemaLocation}/enum`,
        error: `Instance does not match any of ${JSON.stringify($enum)}.`
      });
    }
  }
  if ($not !== void 0) {
    const keywordLocation = `${schemaLocation}/not`;
    const result = validate(instance, $not, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, keywordLocation);
    if (result.valid) {
      errors.push({
        instanceLocation,
        keyword: "not",
        keywordLocation,
        error: 'Instance matched "not" schema.'
      });
    }
  }
  let subEvaluateds = [];
  if ($anyOf !== void 0) {
    const keywordLocation = `${schemaLocation}/anyOf`;
    const errorsLength = errors.length;
    let anyValid = false;
    for (const [i2, subSchema] of $anyOf.entries()) {
      const subEvaluated = Object.create(evaluated);
      const result = validate(instance, subSchema, draft, lookup, shortCircuit, $recursiveAnchor === true ? recursiveAnchor : null, instanceLocation, `${keywordLocation}/${i2}`, subEvaluated);
      errors.push(...result.errors);
      anyValid = anyValid || result.valid;
      if (result.valid) {
        subEvaluateds.push(subEvaluated);
      }
    }
    if (anyValid) {
      errors.length = errorsLength;
    } else {
      errors.splice(errorsLength, 0, {
        instanceLocation,
        keyword: "anyOf",
        keywordLocation,
        error: "Instance does not match any subschemas."
      });
    }
  }
  if ($allOf !== void 0) {
    const keywordLocation = `${schemaLocation}/allOf`;
    const errorsLength = errors.length;
    let allValid = true;
    for (const [i2, subSchema] of $allOf.entries()) {
      const subEvaluated = Object.create(evaluated);
      const result = validate(instance, subSchema, draft, lookup, shortCircuit, $recursiveAnchor === true ? recursiveAnchor : null, instanceLocation, `${keywordLocation}/${i2}`, subEvaluated);
      errors.push(...result.errors);
      allValid = allValid && result.valid;
      if (result.valid) {
        subEvaluateds.push(subEvaluated);
      }
    }
    if (allValid) {
      errors.length = errorsLength;
    } else {
      errors.splice(errorsLength, 0, {
        instanceLocation,
        keyword: "allOf",
        keywordLocation,
        error: `Instance does not match every subschema.`
      });
    }
  }
  if ($oneOf !== void 0) {
    const keywordLocation = `${schemaLocation}/oneOf`;
    const errorsLength = errors.length;
    const matches = $oneOf.filter((subSchema, i2) => {
      const subEvaluated = Object.create(evaluated);
      const result = validate(instance, subSchema, draft, lookup, shortCircuit, $recursiveAnchor === true ? recursiveAnchor : null, instanceLocation, `${keywordLocation}/${i2}`, subEvaluated);
      errors.push(...result.errors);
      if (result.valid) {
        subEvaluateds.push(subEvaluated);
      }
      return result.valid;
    }).length;
    if (matches === 1) {
      errors.length = errorsLength;
    } else {
      errors.splice(errorsLength, 0, {
        instanceLocation,
        keyword: "oneOf",
        keywordLocation,
        error: `Instance does not match exactly one subschema (${matches} matches).`
      });
    }
  }
  if (instanceType === "object" || instanceType === "array") {
    Object.assign(evaluated, ...subEvaluateds);
  }
  if ($if !== void 0) {
    const keywordLocation = `${schemaLocation}/if`;
    const conditionResult = validate(instance, $if, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, keywordLocation, evaluated).valid;
    if (conditionResult) {
      if ($then !== void 0) {
        const thenResult = validate(instance, $then, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, `${schemaLocation}/then`, evaluated);
        if (!thenResult.valid) {
          errors.push({
            instanceLocation,
            keyword: "if",
            keywordLocation,
            error: `Instance does not match "then" schema.`
          }, ...thenResult.errors);
        }
      }
    } else if ($else !== void 0) {
      const elseResult = validate(instance, $else, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, `${schemaLocation}/else`, evaluated);
      if (!elseResult.valid) {
        errors.push({
          instanceLocation,
          keyword: "if",
          keywordLocation,
          error: `Instance does not match "else" schema.`
        }, ...elseResult.errors);
      }
    }
  }
  if (instanceType === "object") {
    if ($required !== void 0) {
      for (const key of $required) {
        if (!hasOwnProperty(instance, key)) {
          errors.push({
            instanceLocation,
            keyword: "required",
            keywordLocation: `${schemaLocation}/required`,
            error: `Instance does not have required property "${key}".`
          });
        }
      }
    }
    const keys = Object.keys(instance);
    if ($minProperties !== void 0 && keys.length < $minProperties) {
      errors.push({
        instanceLocation,
        keyword: "minProperties",
        keywordLocation: `${schemaLocation}/minProperties`,
        error: `Instance does not have at least ${$minProperties} properties.`
      });
    }
    if ($maxProperties !== void 0 && keys.length > $maxProperties) {
      errors.push({
        instanceLocation,
        keyword: "maxProperties",
        keywordLocation: `${schemaLocation}/maxProperties`,
        error: `Instance does not have at least ${$maxProperties} properties.`
      });
    }
    if ($propertyNames !== void 0) {
      const keywordLocation = `${schemaLocation}/propertyNames`;
      for (const key in instance) {
        const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
        const result = validate(key, $propertyNames, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, keywordLocation);
        if (!result.valid) {
          errors.push({
            instanceLocation,
            keyword: "propertyNames",
            keywordLocation,
            error: `Property name "${key}" does not match schema.`
          }, ...result.errors);
        }
      }
    }
    if ($dependentRequired !== void 0) {
      const keywordLocation = `${schemaLocation}/dependantRequired`;
      for (const key in $dependentRequired) {
        if (hasOwnProperty(instance, key)) {
          const required2 = $dependentRequired[key];
          for (const dependantKey of required2) {
            if (!hasOwnProperty(instance, dependantKey)) {
              errors.push({
                instanceLocation,
                keyword: "dependentRequired",
                keywordLocation,
                error: `Instance has "${key}" but does not have "${dependantKey}".`
              });
            }
          }
        }
      }
    }
    if ($dependentSchemas !== void 0) {
      for (const [key, dependentSchema] of Object.entries($dependentSchemas)) {
        const keywordLocation = `${schemaLocation}/dependentSchemas`;
        if (hasOwnProperty(instance, key)) {
          const result = validate(instance, dependentSchema, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, `${keywordLocation}/${encodePointer(key)}`, evaluated);
          if (!result.valid) {
            errors.push({
              instanceLocation,
              keyword: "dependentSchemas",
              keywordLocation,
              error: `Instance has "${key}" but does not match dependant schema.`
            }, ...result.errors);
          }
        }
      }
    }
    if ($dependencies !== void 0) {
      const keywordLocation = `${schemaLocation}/dependencies`;
      for (const key in $dependencies) {
        if (hasOwnProperty(instance, key)) {
          const propsOrSchema = $dependencies[key];
          if (Array.isArray(propsOrSchema)) {
            for (const dependantKey of propsOrSchema) {
              if (!hasOwnProperty(instance, dependantKey)) {
                errors.push({
                  instanceLocation,
                  keyword: "dependencies",
                  keywordLocation,
                  error: `Instance has "${key}" but does not have "${dependantKey}".`
                });
              }
            }
          } else {
            const result = validate(instance, propsOrSchema, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, `${keywordLocation}/${encodePointer(key)}`);
            if (!result.valid) {
              errors.push({
                instanceLocation,
                keyword: "dependencies",
                keywordLocation,
                error: `Instance has "${key}" but does not match dependant schema.`
              }, ...result.errors);
            }
          }
        }
      }
    }
    const thisEvaluated = /* @__PURE__ */ Object.create(null);
    let stop = false;
    if ($properties !== void 0) {
      const keywordLocation = `${schemaLocation}/properties`;
      for (const [key, propertySchema] of Object.entries($properties)) {
        if (!hasOwnProperty(instance, key)) {
          continue;
        }
        const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
        const result = validate(instance[key], propertySchema, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, `${keywordLocation}/${encodePointer(key)}`);
        thisEvaluated[key] = true;
        if (result.valid) {
          evaluated[key] = true;
        } else {
          stop = shortCircuit;
          errors.push({
            instanceLocation,
            keyword: "properties",
            keywordLocation,
            error: `Property "${key}" does not match schema.`
          }, ...result.errors);
          if (stop)
            break;
        }
      }
    }
    if (!stop && $patternProperties !== void 0) {
      const keywordLocation = `${schemaLocation}/patternProperties`;
      for (const [pattern, subSchema] of Object.entries($patternProperties)) {
        const regex2 = new RegExp(pattern, "u");
        for (const key in instance) {
          if (!regex2.test(key)) {
            continue;
          }
          const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
          const result = validate(instance[key], subSchema, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, `${keywordLocation}/${encodePointer(pattern)}`);
          thisEvaluated[key] = true;
          if (result.valid) {
            evaluated[key] = true;
          } else {
            stop = shortCircuit;
            errors.push({
              instanceLocation,
              keyword: "patternProperties",
              keywordLocation,
              error: `Property "${key}" matches pattern "${pattern}" but does not match associated schema.`
            }, ...result.errors);
          }
        }
      }
    }
    if (!stop && $additionalProperties !== void 0) {
      const keywordLocation = `${schemaLocation}/additionalProperties`;
      for (const key in instance) {
        if (thisEvaluated[key]) {
          continue;
        }
        const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
        const result = validate(instance[key], $additionalProperties, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, keywordLocation);
        if (result.valid) {
          evaluated[key] = true;
        } else {
          stop = shortCircuit;
          errors.push({
            instanceLocation,
            keyword: "additionalProperties",
            keywordLocation,
            error: `Property "${key}" does not match additional properties schema.`
          }, ...result.errors);
        }
      }
    } else if (!stop && $unevaluatedProperties !== void 0) {
      const keywordLocation = `${schemaLocation}/unevaluatedProperties`;
      for (const key in instance) {
        if (!evaluated[key]) {
          const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
          const result = validate(instance[key], $unevaluatedProperties, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, keywordLocation);
          if (result.valid) {
            evaluated[key] = true;
          } else {
            errors.push({
              instanceLocation,
              keyword: "unevaluatedProperties",
              keywordLocation,
              error: `Property "${key}" does not match unevaluated properties schema.`
            }, ...result.errors);
          }
        }
      }
    }
  } else if (instanceType === "array") {
    if ($maxItems !== void 0 && instance.length > $maxItems) {
      errors.push({
        instanceLocation,
        keyword: "maxItems",
        keywordLocation: `${schemaLocation}/maxItems`,
        error: `Array has too many items (${instance.length} > ${$maxItems}).`
      });
    }
    if ($minItems !== void 0 && instance.length < $minItems) {
      errors.push({
        instanceLocation,
        keyword: "minItems",
        keywordLocation: `${schemaLocation}/minItems`,
        error: `Array has too few items (${instance.length} < ${$minItems}).`
      });
    }
    const length = instance.length;
    let i2 = 0;
    let stop = false;
    if ($prefixItems !== void 0) {
      const keywordLocation = `${schemaLocation}/prefixItems`;
      const length2 = Math.min($prefixItems.length, length);
      for (; i2 < length2; i2++) {
        const result = validate(instance[i2], $prefixItems[i2], draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, `${keywordLocation}/${i2}`);
        evaluated[i2] = true;
        if (!result.valid) {
          stop = shortCircuit;
          errors.push({
            instanceLocation,
            keyword: "prefixItems",
            keywordLocation,
            error: `Items did not match schema.`
          }, ...result.errors);
          if (stop)
            break;
        }
      }
    }
    if ($items !== void 0) {
      const keywordLocation = `${schemaLocation}/items`;
      if (Array.isArray($items)) {
        const length2 = Math.min($items.length, length);
        for (; i2 < length2; i2++) {
          const result = validate(instance[i2], $items[i2], draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, `${keywordLocation}/${i2}`);
          evaluated[i2] = true;
          if (!result.valid) {
            stop = shortCircuit;
            errors.push({
              instanceLocation,
              keyword: "items",
              keywordLocation,
              error: `Items did not match schema.`
            }, ...result.errors);
            if (stop)
              break;
          }
        }
      } else {
        for (; i2 < length; i2++) {
          const result = validate(instance[i2], $items, draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, keywordLocation);
          evaluated[i2] = true;
          if (!result.valid) {
            stop = shortCircuit;
            errors.push({
              instanceLocation,
              keyword: "items",
              keywordLocation,
              error: `Items did not match schema.`
            }, ...result.errors);
            if (stop)
              break;
          }
        }
      }
      if (!stop && $additionalItems !== void 0) {
        const keywordLocation2 = `${schemaLocation}/additionalItems`;
        for (; i2 < length; i2++) {
          const result = validate(instance[i2], $additionalItems, draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, keywordLocation2);
          evaluated[i2] = true;
          if (!result.valid) {
            stop = shortCircuit;
            errors.push({
              instanceLocation,
              keyword: "additionalItems",
              keywordLocation: keywordLocation2,
              error: `Items did not match additional items schema.`
            }, ...result.errors);
          }
        }
      }
    }
    if ($contains !== void 0) {
      if (length === 0 && $minContains === void 0) {
        errors.push({
          instanceLocation,
          keyword: "contains",
          keywordLocation: `${schemaLocation}/contains`,
          error: `Array is empty. It must contain at least one item matching the schema.`
        });
      } else if ($minContains !== void 0 && length < $minContains) {
        errors.push({
          instanceLocation,
          keyword: "minContains",
          keywordLocation: `${schemaLocation}/minContains`,
          error: `Array has less items (${length}) than minContains (${$minContains}).`
        });
      } else {
        const keywordLocation = `${schemaLocation}/contains`;
        const errorsLength = errors.length;
        let contained = 0;
        for (let j2 = 0; j2 < length; j2++) {
          const result = validate(instance[j2], $contains, draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${j2}`, keywordLocation);
          if (result.valid) {
            evaluated[j2] = true;
            contained++;
          } else {
            errors.push(...result.errors);
          }
        }
        if (contained >= ($minContains || 0)) {
          errors.length = errorsLength;
        }
        if ($minContains === void 0 && contained === 0) {
          errors.splice(errorsLength, 0, {
            instanceLocation,
            keyword: "contains",
            keywordLocation,
            error: `Array does not contain item matching schema.`
          });
        } else if ($minContains !== void 0 && contained < $minContains) {
          errors.push({
            instanceLocation,
            keyword: "minContains",
            keywordLocation: `${schemaLocation}/minContains`,
            error: `Array must contain at least ${$minContains} items matching schema. Only ${contained} items were found.`
          });
        } else if ($maxContains !== void 0 && contained > $maxContains) {
          errors.push({
            instanceLocation,
            keyword: "maxContains",
            keywordLocation: `${schemaLocation}/maxContains`,
            error: `Array may contain at most ${$maxContains} items matching schema. ${contained} items were found.`
          });
        }
      }
    }
    if (!stop && $unevaluatedItems !== void 0) {
      const keywordLocation = `${schemaLocation}/unevaluatedItems`;
      for (i2; i2 < length; i2++) {
        if (evaluated[i2]) {
          continue;
        }
        const result = validate(instance[i2], $unevaluatedItems, draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, keywordLocation);
        evaluated[i2] = true;
        if (!result.valid) {
          errors.push({
            instanceLocation,
            keyword: "unevaluatedItems",
            keywordLocation,
            error: `Items did not match unevaluated items schema.`
          }, ...result.errors);
        }
      }
    }
    if ($uniqueItems) {
      for (let j2 = 0; j2 < length; j2++) {
        const a2 = instance[j2];
        const ao = typeof a2 === "object" && a2 !== null;
        for (let k = 0; k < length; k++) {
          if (j2 === k) {
            continue;
          }
          const b2 = instance[k];
          const bo = typeof b2 === "object" && b2 !== null;
          if (a2 === b2 || ao && bo && deepCompareStrict(a2, b2)) {
            errors.push({
              instanceLocation,
              keyword: "uniqueItems",
              keywordLocation: `${schemaLocation}/uniqueItems`,
              error: `Duplicate items at indexes ${j2} and ${k}.`
            });
            j2 = Number.MAX_SAFE_INTEGER;
            k = Number.MAX_SAFE_INTEGER;
          }
        }
      }
    }
  } else if (instanceType === "number") {
    if (draft === "4") {
      if ($minimum !== void 0 && ($exclusiveMinimum === true && instance <= $minimum || instance < $minimum)) {
        errors.push({
          instanceLocation,
          keyword: "minimum",
          keywordLocation: `${schemaLocation}/minimum`,
          error: `${instance} is less than ${$exclusiveMinimum ? "or equal to " : ""} ${$minimum}.`
        });
      }
      if ($maximum !== void 0 && ($exclusiveMaximum === true && instance >= $maximum || instance > $maximum)) {
        errors.push({
          instanceLocation,
          keyword: "maximum",
          keywordLocation: `${schemaLocation}/maximum`,
          error: `${instance} is greater than ${$exclusiveMaximum ? "or equal to " : ""} ${$maximum}.`
        });
      }
    } else {
      if ($minimum !== void 0 && instance < $minimum) {
        errors.push({
          instanceLocation,
          keyword: "minimum",
          keywordLocation: `${schemaLocation}/minimum`,
          error: `${instance} is less than ${$minimum}.`
        });
      }
      if ($maximum !== void 0 && instance > $maximum) {
        errors.push({
          instanceLocation,
          keyword: "maximum",
          keywordLocation: `${schemaLocation}/maximum`,
          error: `${instance} is greater than ${$maximum}.`
        });
      }
      if ($exclusiveMinimum !== void 0 && instance <= $exclusiveMinimum) {
        errors.push({
          instanceLocation,
          keyword: "exclusiveMinimum",
          keywordLocation: `${schemaLocation}/exclusiveMinimum`,
          error: `${instance} is less than ${$exclusiveMinimum}.`
        });
      }
      if ($exclusiveMaximum !== void 0 && instance >= $exclusiveMaximum) {
        errors.push({
          instanceLocation,
          keyword: "exclusiveMaximum",
          keywordLocation: `${schemaLocation}/exclusiveMaximum`,
          error: `${instance} is greater than or equal to ${$exclusiveMaximum}.`
        });
      }
    }
    if ($multipleOf !== void 0) {
      if (!isMultipleOf(instance, $multipleOf)) {
        errors.push({
          instanceLocation,
          keyword: "multipleOf",
          keywordLocation: `${schemaLocation}/multipleOf`,
          error: `${instance} is not a multiple of ${$multipleOf}.`
        });
      }
    }
  } else if (instanceType === "string") {
    const length = $minLength === void 0 && $maxLength === void 0 ? 0 : ucs2length(instance);
    if ($minLength !== void 0 && length < $minLength) {
      errors.push({
        instanceLocation,
        keyword: "minLength",
        keywordLocation: `${schemaLocation}/minLength`,
        error: `String is too short (${length} < ${$minLength}).`
      });
    }
    if ($maxLength !== void 0 && length > $maxLength) {
      errors.push({
        instanceLocation,
        keyword: "maxLength",
        keywordLocation: `${schemaLocation}/maxLength`,
        error: `String is too long (${length} > ${$maxLength}).`
      });
    }
    if ($pattern !== void 0 && !new RegExp($pattern, "u").test(instance)) {
      errors.push({
        instanceLocation,
        keyword: "pattern",
        keywordLocation: `${schemaLocation}/pattern`,
        error: `String does not match pattern.`
      });
    }
    if ($format !== void 0 && format[$format] && !format[$format](instance)) {
      errors.push({
        instanceLocation,
        keyword: "format",
        keywordLocation: `${schemaLocation}/format`,
        error: `String does not match format "${$format}".`
      });
    }
  }
  return { valid: errors.length === 0, errors };
}

// node_modules/@cfworker/json-schema/dist/esm/validator.js
var Validator = class {
  schema;
  draft;
  shortCircuit;
  lookup;
  constructor(schema, draft = "2019-09", shortCircuit = true) {
    this.schema = schema;
    this.draft = draft;
    this.shortCircuit = shortCircuit;
    this.lookup = dereference(schema);
  }
  validate(instance) {
    return validate(instance, this.schema, this.draft, this.lookup, this.shortCircuit);
  }
  addSchema(schema, id2) {
    if (id2) {
      schema = { ...schema, $id: id2 };
    }
    dereference(schema, this.lookup);
  }
};

// srcts/util/validate.ts
function compile(what, schema, fails = "fails its schema") {
  let validator;
  try {
    validator = new Validator(schema, draftOf(schema), false);
  } catch (error) {
    throw new Error(`The schema of ${what} cannot be read: ${messageOf(error)}`, {
      cause: error
    });
  }
  return (value2) => {
    const result = validator.validate(value2);
    return result.valid ? [] : describe(result.errors, fails);
  };
}
function draftOf(schema) {
  const declared = typeof schema.$schema === "string" ? schema.$schema : "";
  if (declared.includes("draft-04")) return "4";
  if (declared.includes("draft-07")) return "7";
  if (declared.includes("2019-09")) return "2019-09";
  return "2020-12";
}
var wrappers = /* @__PURE__ */ new Set([
  "properties",
  "items",
  "prefixItems",
  "additionalItems",
  "$ref",
  "$dynamicRef",
  "$recursiveRef",
  "allOf",
  "false"
]);
function describe(errors, fails) {
  return errors.filter((error) => !wrappers.has(error.keyword)).map((error) => {
    const at = error.instanceLocation.replace(/^#/, "");
    return `${fails}${at === "" ? "" : ` at ${at}`}: ${error.error}`;
  });
}

// srcts/protocol/client.ts
var SessionEndedError = class extends Error {
  constructor(id2) {
    super(`The session ended before R replied to request ${id2}.`);
    this.name = "SessionEndedError";
  }
};
function createClient(transport, options) {
  let next = 0;
  let state2 = "none";
  let shared = /* @__PURE__ */ new Set();
  const waiting2 = /* @__PURE__ */ new Map();
  let held = [];
  const sending = /* @__PURE__ */ new Set();
  let ends = 0;
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
      waiting2.set(call.id, call);
    }
    const sent2 = transport.send(
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
          if (waiting2.get(call.id) === call) {
            waiting2.delete(call.id);
            call.settle({ error: asError(error) });
          }
        }
      }
    );
    if (bodies.size > 0) {
      sending.add(sent2);
      void sent2.finally(() => sending.delete(sent2));
    }
  }
  function cancel(call) {
    if (waiting2.get(call.id) !== call || !shared.has("cancel")) return;
    quietly(transport.send([{ kind: "cancel", request: call.id }], /* @__PURE__ */ new Map()));
  }
  function abort(entry, call) {
    const index = held.indexOf(entry);
    if (index >= 0) {
      held.splice(index, 1);
      call.settle(cancelled(call.id));
    } else if (call.sent) {
      cancel(call);
    } else {
      call.aborted = true;
    }
  }
  function queue(entry) {
    if (state2 === "ready") {
      dispatch([entry]);
    } else {
      held.push(entry);
    }
  }
  function end() {
    state2 = "none";
    shared = /* @__PURE__ */ new Set();
    ends += 1;
    const lost = [...waiting2.values()];
    waiting2.clear();
    for (const call of lost) {
      call.settle({ error: new SessionEndedError(call.id) });
    }
  }
  function sendHello() {
    const named2 = options.hello();
    const id2 = mint();
    const call = {
      id: id2,
      body: void 0,
      progress: void 0,
      sent: false,
      aborted: false,
      settle(result) {
        if ("error" in result) {
          if (!(result.error instanceof SessionEndedError)) report(result.error);
          return;
        }
        const { reply } = result;
        if (reply.status === "ok" && "capabilities" in reply) {
          const theirs = new Set(reply.capabilities);
          shared = new Set(named2.capabilities.filter((c2) => theirs.has(c2)));
          state2 = "ready";
          const outbound = held;
          held = [];
          dispatch(outbound);
        }
        guard(() => {
          options.session(reply);
        });
      }
    };
    transmit([{ message: { ...named2, id: id2, kind: "hello" }, call }], /* @__PURE__ */ new Map());
  }
  transport.listen({
    started() {
      end();
      state2 = "started";
      sendHello();
    },
    ended() {
      end();
    },
    received(item, body) {
      const call = waiting2.get(item.id);
      if (call === void 0) return;
      if (!("status" in item)) {
        const note = item.progress;
        guard(() => {
          call.progress?.(note);
        });
        return;
      }
      waiting2.delete(item.id);
      if (body === null) {
        call.settle({ reply: item, body: null });
      } else {
        body.then(
          (blob2) => {
            call.settle({ reply: item, body: blob2 });
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
      const id2 = mint();
      const message = {
        ...request,
        id: id2,
        ...body === void 0 ? {} : { body: announce(body) }
      };
      return new Promise((resolve, reject) => {
        const onAbort2 = () => {
          abort(entry, call);
        };
        const call = {
          id: id2,
          body,
          progress,
          sent: false,
          aborted: false,
          settle(result) {
            signal?.removeEventListener("abort", onAbort2);
            if ("error" in result) {
              reject(result.error);
            } else {
              resolve(result);
            }
          }
        };
        const entry = { message, call };
        if (signal?.aborted === true) {
          call.settle(cancelled(id2));
          return;
        }
        signal?.addEventListener("abort", onAbort2, { once: true });
        queue(entry);
      });
    },
    forget(blocks) {
      queue({ message: { kind: "forget", blocks } });
    },
    capabilities: () => shared,
    hello() {
      const runs = state2 !== "none";
      const lost = held;
      held = [];
      end();
      for (const { call } of lost) {
        call?.settle({ error: new SessionEndedError(call.id) });
      }
      if (!runs) return;
      state2 = "started";
      if (sending.size === 0) {
        sendHello();
        return;
      }
      const at = ends;
      void Promise.allSettled([...sending]).then(() => {
        if (ends === at) sendHello();
      });
    }
  };
}
function announce(body) {
  const type = body.type === "" ? "application/octet-stream" : body.type;
  return body instanceof File ? { type, name: body.name } : { type };
}
function cancelled(id2) {
  return { reply: { id: id2, status: "cancelled" }, body: null };
}
function guard(fn) {
  try {
    fn();
  } catch (error) {
    report(error);
  }
}
function quietly(sent2) {
  sent2.catch(() => void 0);
}

// srcts/protocol/messages.ts
var PROTOCOL_VERSION = "1.0";

// srcts/scheduler/status.ts
var NONE = Object.freeze({ warning: 0, message: 0 });
var NO_CONDITIONS = Object.freeze([]);
function outcomeOf(reply, revision) {
  if (reply.status === "cancelled" || reply.status === "missing") return void 0;
  const evaluated = reply;
  return {
    status: reply.status,
    revision,
    summary: evaluated.summary ?? null,
    conditions: evaluated.conditions ?? NO_CONDITIONS,
    omitted: evaluated.omitted ?? NONE,
    stdout: evaluated.stdout ?? "",
    stderr: evaluated.stderr ?? ""
  };
}
function unsent(revision, message) {
  return {
    status: "error",
    revision,
    summary: null,
    conditions: [
      {
        severity: "error",
        class: ["blockr_error", "error", "condition"],
        message: `The evaluation could not be sent to R: ${message}`
      }
    ],
    omitted: NONE,
    stdout: "",
    stderr: ""
  };
}
function unevaluated(revision, running2) {
  return {
    status: "unevaluated",
    revision,
    evaluated: null,
    running: running2,
    conditions: NO_CONDITIONS,
    omitted: NONE,
    stdout: "",
    stderr: "",
    summary: null
  };
}
function replied(outcome, revision, running2) {
  return {
    status: outcome.revision !== revision ? "stale" : outcome.status === "ok" ? "ready" : outcome.status === "error" ? "failed" : "invalid",
    revision,
    evaluated: outcome.revision,
    running: running2,
    conditions: outcome.conditions,
    omitted: outcome.omitted,
    stdout: outcome.stdout,
    stderr: outcome.stderr,
    summary: outcome.summary
  };
}
function waiting(why, revision, evaluated, running2) {
  return {
    status: "waiting",
    revision,
    evaluated,
    running: running2,
    conditions: [why],
    omitted: NONE,
    stdout: "",
    stderr: "",
    summary: null
  };
}
function waitingFor(what) {
  let message;
  if ("upstream" in what) {
    const blocks = what.upstream.length === 1 ? "block" : "blocks";
    message = `This block is waiting for ${blocks} ${quoted(what.upstream)}.`;
  } else if (what.unlinked.includes("...")) {
    message = "This block is waiting for a block to be linked to it.";
  } else {
    const inputs = what.unlinked.length === 1 ? "input" : "inputs";
    message = `This block is waiting for its ${inputs} ${quoted(what.unlinked)} to be linked.`;
  }
  return {
    severity: "message",
    class: ["blockr_message", "message", "condition"],
    phase: "status",
    message
  };
}
function sameStatus(a2, b2) {
  return a2.status === b2.status && a2.revision === b2.revision && a2.evaluated === b2.evaluated && a2.running === b2.running && a2.omitted === b2.omitted && a2.stdout === b2.stdout && a2.stderr === b2.stderr && a2.summary === b2.summary && a2.conditions.length === b2.conditions.length && a2.conditions.every(
    (condition, i2) => condition === b2.conditions[i2] || condition.phase === "status" && condition.message === b2.conditions[i2]?.message
  );
}
function statusesOf(input2) {
  const statuses = /* @__PURE__ */ new Map();
  const changed = [];
  for (const id2 of input2.graph.order) {
    const revision = input2.revisions.get(id2);
    if (revision === void 0) continue;
    const outcome = input2.outcomes.get(id2);
    const running2 = input2.running.has(id2);
    const reason = why(id2);
    const now = reason !== void 0 ? waiting(reason, revision, outcome?.revision ?? null, running2) : outcome === void 0 ? unevaluated(revision, running2) : replied(outcome, revision, running2);
    const before = input2.before.get(id2);
    if (before !== void 0 && sameStatus(before, now)) {
      statuses.set(id2, before);
    } else {
      statuses.set(id2, Object.freeze(now));
      changed.push(id2);
    }
  }
  for (const id2 of input2.before.keys()) {
    if (!statuses.has(id2)) changed.push(id2);
  }
  return { statuses, changed };
  function why(id2) {
    const inputs = input2.inputs.get(id2);
    const unlinked = inputs?.unlinked ?? [];
    if (unlinked.length > 0) return waitingFor({ unlinked });
    const reads = (inputs?.recipe ?? []).flatMap((read) => read.block ?? []);
    const blocked = [...new Set(reads)].filter((up) => {
      const status = statuses.get(up)?.status;
      return status === "failed" || status === "invalid" || status === "waiting";
    });
    return blocked.length > 0 ? waitingFor({ upstream: blocked }) : void 0;
  }
}
function waitingError(id2, status) {
  const why = status.conditions[0]?.message ?? "This block is waiting.";
  return new Error(
    `${why.replace(/^This block/, `Block "${id2}"`)} Its evaluation with parameters is not sent.`
  );
}

// srcts/scheduler/evaluations.ts
function oneTimeOf(blocks, params, priority, statuses) {
  if (params === void 0) {
    const onBoard = new Set(blocks.filter((id2) => statuses.has(id2)));
    if (onBoard.size === 0) {
      return new Error(`The board holds none of the blocks ${quoted(blocks)} to evaluate.`);
    }
    return { blocks: onBoard, priority };
  }
  if (blocks.length === 0) return new Error("An evaluation with parameters names no block.");
  const absent = blocks.filter((id2) => !statuses.has(id2));
  if (absent.length > 0) {
    const named2 = absent.length === 1 ? "block" : "blocks";
    return new Error(`The board holds no ${named2} ${quoted(absent)} to evaluate.`);
  }
  const parts = /* @__PURE__ */ new Map();
  for (const id2 of blocks) {
    const status = statuses.get(id2);
    if (status?.status === "waiting") return waitingError(id2, status);
    parts.set(id2, { recipe: void 0, sent: void 0, result: void 0 });
  }
  return { priority, params, parts };
}
function settle(oneTimes, statuses) {
  const settled = [];
  for (const [id2, oneTime] of oneTimes) {
    if ("params" in oneTime) continue;
    const blocks = [...oneTime.blocks];
    if (blocks.some((block) => !current(statuses.get(block)))) continue;
    const result = {};
    for (const block of blocks) {
      const status = statuses.get(block);
      if (status !== void 0) result[block] = status;
    }
    settled.push([id2, { statuses: result }]);
  }
  return { oneTimes: without2(oneTimes, settled.map(([id2]) => id2)), settled };
}
function left(oneTimes, gone) {
  const kept = /* @__PURE__ */ new Map();
  const settled = [];
  for (const [id2, oneTime] of oneTimes) {
    if (!("params" in oneTime)) {
      const blocks = new Set([...oneTime.blocks].filter((block) => !gone.has(block)));
      if (blocks.size === 0) {
        settled.push([id2, { error: new Error("Every block of the one-time evaluation left the board.") }]);
      } else {
        kept.set(id2, blocks.size === oneTime.blocks.size ? oneTime : { ...oneTime, blocks });
      }
      continue;
    }
    const early = [...oneTime.parts].find(
      ([block, part]) => gone.has(block) && part.sent === void 0 && part.result === void 0
    );
    if (early === void 0) {
      kept.set(id2, oneTime);
    } else {
      const error = new Error(
        `Block "${early[0]}" left the board before its evaluation with parameters was sent.`
      );
      settled.push([id2, { error }]);
    }
  }
  return { oneTimes: kept, settled };
}
function sent(oneTimes, id2, block, evaluation) {
  return withPart(oneTimes, id2, block, { recipe: evaluation.recipe, sent: evaluation.id });
}
function answered(oneTimes, id2, block, reply, outcome, revision) {
  const oneTime = oneTimes.get(id2);
  if (oneTime === void 0 || !("params" in oneTime)) return { oneTimes, settled: [] };
  const part = oneTime.parts.get(block);
  if (part === void 0) return { oneTimes, settled: [] };
  if (outcome !== void 0) {
    const result = Object.freeze(replied(outcome, revision, false));
    const parts = new Map(oneTime.parts).set(block, { ...part, result });
    const results = [...parts].flatMap(
      ([at, { result: status }]) => status === void 0 ? [] : [[at, status]]
    );
    if (results.length < parts.size) {
      return { oneTimes: new Map(oneTimes).set(id2, { ...oneTime, parts }), settled: [] };
    }
    return {
      oneTimes: without2(oneTimes, [id2]),
      settled: [[id2, { statuses: Object.fromEntries(results) }]]
    };
  }
  if (reply.status === "missing") {
    return { oneTimes: withPart(oneTimes, id2, block, { recipe: void 0, sent: void 0 }), settled: [] };
  }
  const error = new Error(
    `R dropped the evaluation of block "${block}" with parameters, as it does for a block that leaves the board.`
  );
  return { oneTimes: without2(oneTimes, [id2]), settled: [[id2, { error }]] };
}
function neededBy(oneTimes, graph) {
  const needs = [];
  for (const oneTime of oneTimes.values()) {
    if (!("params" in oneTime)) {
      needs.push({ blocks: oneTime.blocks, priority: oneTime.priority });
      continue;
    }
    for (const [block, part] of oneTime.parts) {
      if (part.result !== void 0) continue;
      const reads = part.recipe === void 0 ? graph.upstream(block) : part.recipe.inputs.flatMap((input2) => input2.block ?? []);
      needs.push({ blocks: reads, priority: oneTime.priority });
    }
  }
  return needs;
}
function pendingOf(oneTimes) {
  const pending = [];
  for (const [id2, oneTime] of oneTimes) {
    if (!("params" in oneTime)) continue;
    for (const [block, part] of oneTime.parts) {
      if (part.sent === void 0 && part.result === void 0) {
        pending.push({ oneTime: id2, block, priority: oneTime.priority });
      }
    }
  }
  return pending;
}
function current(status) {
  return status?.status !== "stale" && status?.status !== "unevaluated";
}
function withPart(oneTimes, id2, block, change) {
  const oneTime = oneTimes.get(id2);
  if (oneTime === void 0 || !("params" in oneTime)) return oneTimes;
  const part = oneTime.parts.get(block);
  if (part === void 0) return oneTimes;
  const parts = new Map(oneTime.parts).set(block, { ...part, ...change });
  return new Map(oneTimes).set(id2, { ...oneTime, parts });
}
function without2(oneTimes, ids2) {
  if (ids2.length === 0) return oneTimes;
  const kept = new Map(oneTimes);
  for (const id2 of ids2) kept.delete(id2);
  return kept;
}

// srcts/scheduler/needs.ts
function needsOf(graph, needs) {
  const onBoard = new Set(graph.order);
  const found = /* @__PURE__ */ new Map();
  const raise = (id2, priority) => {
    if (!onBoard.has(id2)) return;
    const was = found.get(id2);
    if (was === void 0 || priority > was) found.set(id2, priority);
  };
  for (const need of needs) {
    for (const id2 of need.blocks) raise(id2, need.priority);
  }
  for (const id2 of [...graph.order].reverse()) {
    const priority = found.get(id2);
    if (priority === void 0) continue;
    for (const up of graph.upstream(id2)) raise(up, priority);
  }
  return found;
}

// srcts/scheduler/plan.ts
var GIVE_WAY = 5e3;
function planOf(input2) {
  const { graph, revisions, statuses, needs, now } = input2;
  const sent2 = [...input2.sent];
  const asked = [...input2.asked];
  const pending = [...input2.pending];
  const index = new Map(graph.order.map((id2, i2) => [id2, i2]));
  const deadlines = [];
  const cancel = [];
  const cancelled2 = /* @__PURE__ */ new Set();
  const planned = /* @__PURE__ */ new Set();
  const priorityOf = (evaluation) => evaluation.oneTime !== void 0 ? evaluation.oneTime.priority : needs.get(evaluation.block) ?? (evaluation.background ? -1 : void 0);
  const inFlight = (id2, at) => planned.has(id2) || sent2.some(
    (evaluation) => evaluation.block === id2 && evaluation.revision === at && !evaluation.cancelled && !cancelled2.has(evaluation.id)
  );
  const needsEvaluation = (id2) => {
    const status = statuses.get(id2)?.status;
    return status === "stale" || status === "unevaluated";
  };
  const available = (id2) => {
    const at = revisions.get(id2);
    return at !== void 0 && (statuses.get(id2)?.status === "ready" || inFlight(id2, at));
  };
  const waitedSince = (evaluation, priority) => {
    let since;
    for (const other of asked) {
      if (other.id > evaluation.id) since = Math.min(since ?? other.at, other.at);
    }
    for (const other of sent2) {
      if (other.id <= evaluation.id) continue;
      if ((priorityOf(other) ?? -Infinity) > priority) {
        since = Math.min(since ?? other.at, other.at);
      }
    }
    return since;
  };
  const paramsFor = (id2, at) => pending.some((item) => item.block === id2) || sent2.some(
    (evaluation) => evaluation.oneTime !== void 0 && evaluation.block === id2 && evaluation.revision === at
  );
  let oldest = Infinity;
  for (const item of [...sent2, ...asked]) oldest = Math.min(oldest, item.id);
  const drop = (evaluation) => {
    cancel.push(evaluation.id);
    cancelled2.add(evaluation.id);
  };
  for (const evaluation of sent2) {
    if (evaluation.cancelled || evaluation.oneTime !== void 0) continue;
    const at = revisions.get(evaluation.block);
    if (at === void 0) continue;
    if (at !== evaluation.revision) {
      drop(evaluation);
      continue;
    }
    const priority = priorityOf(evaluation);
    if (priority === void 0) {
      if (evaluation.id !== oldest) drop(evaluation);
      continue;
    }
    if (!input2.interrupt || priority >= 0) continue;
    const since = waitedSince(evaluation, priority);
    if (since === void 0) continue;
    if (now - since >= GIVE_WAY) drop(evaluation);
    else deadlines.push(since + GIVE_WAY);
  }
  const queued = [];
  for (const [id2, priority] of needs) {
    const at = revisions.get(id2);
    if (at === void 0 || !needsEvaluation(id2)) continue;
    if (inFlight(id2, at) || paramsFor(id2, at)) continue;
    queued.push({ block: id2, priority });
  }
  for (const item of pending) {
    queued.push({ block: item.block, priority: item.priority, oneTime: item.oneTime });
  }
  queued.sort(
    (a2, b2) => (a2.priority === b2.priority ? 0 : a2.priority > b2.priority ? -1 : 1) || (index.get(a2.block) ?? 0) - (index.get(b2.block) ?? 0)
  );
  let slots = input2.queue + 1 - sent2.length;
  const send = [];
  const reject = [];
  const rejected = /* @__PURE__ */ new Set();
  if (!input2.holding) {
    for (const item of queued) {
      if (item.oneTime !== void 0) {
        if (rejected.has(item.oneTime)) continue;
        const status = statuses.get(item.block);
        if (status?.status === "waiting") {
          reject.push({ oneTime: item.oneTime, error: waitingError(item.block, status) });
          rejected.add(item.oneTime);
          continue;
        }
      }
      if (slots <= 0 || !graph.upstream(item.block).every(available)) continue;
      send.push(
        item.oneTime === void 0 ? { block: item.block } : { block: item.block, oneTime: item.oneTime }
      );
      planned.add(item.block);
      slots -= 1;
    }
  }
  const busy = asked.length > 0 || sent2.some((evaluation) => (priorityOf(evaluation) ?? 0) >= 0) || queued.some((item) => item.priority >= 0);
  let idleSince = input2.idleSince;
  if (busy || input2.holding || !Number.isFinite(input2.delay)) {
    idleSince = void 0;
  } else {
    idleSince ??= now;
    const due = idleSince + input2.delay * 1e3;
    if (now < due) {
      deadlines.push(due);
    } else if (slots > 0 && !sent2.some((evaluation) => evaluation.background)) {
      const block = inBackground();
      if (block !== void 0) send.push({ block, background: true });
    }
  }
  return {
    cancel,
    send,
    reject,
    idleSince,
    wakeAt: deadlines.length > 0 ? Math.min(...deadlines) : void 0
  };
  function inBackground() {
    for (const id2 of graph.order) {
      if (needs.has(id2) || !needsEvaluation(id2)) continue;
      const at = revisions.get(id2);
      if (at === void 0 || inFlight(id2, at)) continue;
      if (graph.upstream(id2).every((up) => statuses.get(up)?.status === "ready")) return id2;
    }
    return void 0;
  }
}

// srcts/scheduler/recipes.ts
function inputsOf(board, graph, registry, bound) {
  const inputs = /* @__PURE__ */ new Map();
  for (const [id2, block] of Object.entries(board.blocks)) {
    const declared = own(registry, block.registry_id)?.inputs ?? [];
    const links = graph.linksInto(id2).flatMap((link) => own(board.links, link) ?? []);
    inputs.set(
      id2,
      declared.includes("...") ? variadic(declared, links, bound) : fixed(declared, links, bound)
    );
  }
  return inputs;
}
function fixed(declared, links, bound) {
  const recipe = [];
  const unlinked = [];
  for (const name3 of declared) {
    const link = links.find((candidate) => candidate.input === name3);
    if (link !== void 0) recipe.push({ input: name3, block: link.from });
    else if (bound.has(name3)) recipe.push({ input: name3 });
    else unlinked.push(name3);
  }
  return { recipe, unlinked };
}
function variadic(declared, links, bound) {
  const recipe = links.map((link) => ({ input: link.input, block: link.from }));
  for (const name3 of bound) {
    if (!links.some((link) => link.input === name3)) recipe.push({ input: name3 });
  }
  const unlinked = recipe.length === 0 ? ["..."] : [];
  for (const name3 of declared) {
    if (name3 === "...") continue;
    if (!links.some((link) => link.input === name3) && !bound.has(name3)) unlinked.push(name3);
  }
  return { recipe, unlinked };
}
function sameInputs(a2, b2) {
  return a2.length === b2.length && a2.every((input2, i2) => input2.input === b2[i2]?.input && input2.block === b2[i2].block);
}

// srcts/scheduler/revisions.ts
function counter(last = 0) {
  return () => {
    if (last >= Number.MAX_SAFE_INTEGER) {
      throw new Error(
        "The page has used up its revisions, 2^53 - 1 of them, so the scheduler has stopped. Reload the page to start again."
      );
    }
    last += 1;
    return last;
  };
}
function opening(graph, next) {
  return new Map(graph.order.map((id2) => [id2, next()]));
}
function revise(revisions, before, after, removed, next) {
  const gone = [...revisions.keys()].filter(
    (id2) => !Object.hasOwn(after.board.blocks, id2) || removed.has(id2)
  );
  const left2 = new Set(gone);
  const moved = /* @__PURE__ */ new Set();
  for (const id2 of after.graph.order) {
    const was = left2.has(id2) ? void 0 : own(before.board.blocks, id2);
    const now = own(after.board.blocks, id2);
    if (was === void 0 || now === void 0) {
      moved.add(id2);
      continue;
    }
    const same2 = was.registry_id === now.registry_id && (was.state === now.state || jsonEqual(was.state, now.state)) && sameInputs(before.inputs.get(id2)?.recipe ?? [], after.inputs.get(id2)?.recipe ?? []);
    if (!same2) moved.add(id2);
  }
  const kept = new Map([...revisions].filter(([id2]) => !left2.has(id2)));
  return { revisions: bump(after.graph, kept, moved, next), gone };
}
function bump(graph, revisions, roots, next) {
  const hit = graph.withDownstream(roots);
  const result = /* @__PURE__ */ new Map();
  for (const id2 of graph.order) {
    const at = revisions.get(id2);
    result.set(id2, at === void 0 || hit.has(id2) ? next() : at);
  }
  return result;
}

// srcts/scheduler/scheduler.ts
var NO_BOARD = { blocks: {}, links: {} };
function createScheduler(options) {
  const { store, client } = options;
  const queue = setting(options.evaluationQueue, 2, "evaluation_queue", true);
  const delay = setting(options.backgroundEvaluationDelay, Infinity, "background_evaluation_delay");
  const gated = flag(options.gateVisibility, true, "gate_visibility");
  const board = y(void 0);
  const registry = y({});
  const bound = y(new Set(options.bound ?? []));
  const revisions = y(/* @__PURE__ */ new Map());
  const outcomes = y(/* @__PURE__ */ new Map());
  const sent2 = y(/* @__PURE__ */ new Map());
  const asked = y(/* @__PURE__ */ new Map());
  const demands = y(/* @__PURE__ */ new Map());
  const oneTimes = y(/* @__PURE__ */ new Map());
  const holding = y(true);
  const graph = O(() => graphOf(board.value ?? NO_BOARD));
  const inputs = O(
    () => inputsOf(board.value ?? NO_BOARD, graph.value, registry.value, bound.value)
  );
  let last = /* @__PURE__ */ new Map();
  const statuses = O(() => {
    last = statusesOf({
      graph: graph.value,
      revisions: revisions.value,
      outcomes: outcomes.value,
      inputs: inputs.value,
      running: running(sent2.value),
      before: last
    }).statuses;
    return last;
  });
  const needs = O(
    () => needsOf(graph.value, [
      ...[...demands.value.values()].map((demand) => ({
        blocks: demand.blocks.value,
        priority: demand.priority
      })),
      ...neededBy(oneTimes.value, graph.value)
    ])
  );
  const state2 = O(() => [
    board.value,
    registry.value,
    bound.value,
    revisions.value,
    outcomes.value,
    sent2.value,
    asked.value,
    demands.value,
    oneTimes.value,
    holding.value
  ]);
  const controllers = /* @__PURE__ */ new Map();
  const settles = /* @__PURE__ */ new Map();
  const subscribers = /* @__PURE__ */ new Set();
  let ids2 = 0;
  const count = options.nextRevision ?? counter();
  let stopped;
  const ended = () => options.signal?.aborted === true;
  const endedError = () => new Error("Another board opened.");
  let idleSince;
  let planned;
  let passTimer;
  let wakeTimer;
  function revision() {
    try {
      return count();
    } catch (error) {
      stopped = error;
      throw error;
    }
  }
  const opened = E((next) => {
    registry.value = store.blockRegistry();
    board.value = next;
    revisions.value = opening(graph.value, revision);
  });
  const applied = E((delta, next) => {
    const before = { board: board.value ?? next, inputs: inputs.value };
    board.value = next;
    const revised = revise(
      revisions.value,
      before,
      { board: next, inputs: inputs.value, graph: graph.value },
      new Set(delta.blocks?.rm ?? []),
      revision
    );
    revisions.value = revised.revisions;
    if (revised.gone.length === 0) return;
    const gone = new Set(revised.gone);
    outcomes.value = new Map([...outcomes.value].filter(([id2]) => !gone.has(id2)));
    update(left(oneTimes.value, gone));
    client.forget(revised.gone);
  });
  const lacking = E((missing) => {
    const lost = /* @__PURE__ */ new Set();
    for (const { block, revision: at } of missing) {
      if (revisions.value.get(block) === at && statuses.value.get(block)?.status === "ready") {
        lost.add(block);
      }
    }
    if (lost.size > 0) revisions.value = bump(graph.value, revisions.value, lost, revision);
  });
  function update(next) {
    oneTimes.value = next.oneTimes;
    for (const [id2, result] of next.settled) {
      const settle2 = settles.get(id2);
      settles.delete(id2);
      settle2?.(result);
    }
  }
  const end = E((id2, result) => {
    if (!oneTimes.value.has(id2)) return;
    const rest = new Map(oneTimes.value);
    rest.delete(id2);
    update({ oneTimes: rest, settled: [[id2, result]] });
  });
  function recipeOf(id2) {
    const block = board.value === void 0 ? void 0 : own(board.value.blocks, id2);
    const recipe = inputs.value.get(id2)?.recipe;
    if (block === void 0 || recipe === void 0) return void 0;
    return {
      registry_id: block.registry_id,
      state: { ...block.state },
      inputs: recipe.map(
        (input2) => input2.block === void 0 ? { input: input2.input } : { input: input2.input, block: input2.block, revision: revisions.value.get(input2.block) ?? 0 }
      )
    };
  }
  function send(item) {
    const recipe = recipeOf(item.block);
    const at = revisions.value.get(item.block);
    if (recipe === void 0 || at === void 0) return;
    let oneTime;
    if (item.oneTime !== void 0) {
      const found = oneTimes.value.get(item.oneTime);
      if (found === void 0 || !("params" in found)) return;
      oneTime = { id: item.oneTime, priority: found.priority, params: found.params };
    }
    const id2 = ++ids2;
    sent2.value = new Map(sent2.value).set(id2, {
      id: id2,
      at: Date.now(),
      block: item.block,
      revision: at,
      oneTime: oneTime === void 0 ? void 0 : { id: oneTime.id, priority: oneTime.priority },
      background: item.background === true,
      cancelled: false
    });
    if (oneTime !== void 0) {
      oneTimes.value = sent(oneTimes.value, oneTime.id, item.block, { id: id2, recipe });
    }
    const controller = new AbortController();
    controllers.set(id2, controller);
    client.request(
      {
        kind: "evaluate",
        block: item.block,
        revision: at,
        recipe,
        ...oneTime === void 0 ? {} : { params: oneTime.params }
      },
      { signal: controller.signal }
    ).then(
      ({ reply }) => {
        answered2(id2, reply);
      },
      (error) => {
        unanswered(id2, error);
      }
    );
  }
  function cancel(id2) {
    const evaluation = sent2.value.get(id2);
    if (evaluation === void 0) return;
    sent2.value = new Map(sent2.value).set(id2, { ...evaluation, cancelled: true });
    controllers.get(id2)?.abort();
  }
  function done(id2) {
    const evaluation = sent2.value.get(id2);
    if (evaluation === void 0) return void 0;
    const rest = new Map(sent2.value);
    rest.delete(id2);
    sent2.value = rest;
    controllers.delete(id2);
    return evaluation;
  }
  const answered2 = E((id2, reply) => {
    const evaluation = done(id2);
    if (evaluation === void 0 || ended()) return;
    if (reply.status === "missing") lacking(reply.missing);
    const outcome = outcomeOf(reply, evaluation.revision);
    if (outcome !== void 0 && evaluation.revision === revisions.value.get(evaluation.block) && statuses.value.get(evaluation.block)?.status !== "waiting") {
      outcomes.value = new Map(outcomes.value).set(evaluation.block, outcome);
    }
    if (evaluation.oneTime !== void 0) {
      update(
        answered(
          oneTimes.value,
          evaluation.oneTime.id,
          evaluation.block,
          reply,
          outcome,
          evaluation.revision
        )
      );
    }
  });
  const unanswered = E((id2, error) => {
    const evaluation = done(id2);
    if (evaluation === void 0 || ended()) return;
    if (evaluation.oneTime !== void 0) {
      end(evaluation.oneTime.id, { error });
    } else if (!(error instanceof SessionEndedError) && revisions.value.get(evaluation.block) === evaluation.revision) {
      const outcome = unsent(evaluation.revision, messageOf(error));
      outcomes.value = new Map(outcomes.value).set(evaluation.block, outcome);
    }
  });
  function schedule() {
    passTimer ??= setTimeout(pass, 0);
  }
  function wake(at) {
    if (wakeTimer !== void 0) clearTimeout(wakeTimer);
    wakeTimer = void 0;
    if (at === void 0) return;
    wakeTimer = setTimeout(
      () => {
        wakeTimer = void 0;
        pass();
      },
      Math.max(0, at - Date.now())
    );
  }
  function pass() {
    passTimer = void 0;
    r(() => {
      if (stopped === void 0 && board.value !== void 0) {
        const plan = planOf({
          graph: graph.value,
          revisions: revisions.value,
          statuses: statuses.value,
          needs: needs.value,
          sent: sent2.value.values(),
          asked: asked.value.values(),
          pending: pendingOf(oneTimes.value),
          queue,
          interrupt: client.capabilities().has("interrupt"),
          holding: holding.value,
          now: Date.now(),
          idleSince,
          delay
        });
        idleSince = plan.idleSince;
        for (const id2 of plan.cancel) cancel(id2);
        for (const { oneTime, error } of plan.reject) end(oneTime, { error });
        for (const item of plan.send) send(item);
        wake(plan.wakeAt);
      }
      planned = state2.value;
    });
  }
  function resendable(request) {
    if (request.kind === "output" || request.output !== void 0) return true;
    if (request.registry_id === void 0) return false;
    const entry = own(store.blockRegistry(), request.registry_id);
    return entry === void 0 || own(entry.requests, request.name)?.action !== true;
  }
  function demanding(id2, owner, blocks, priority) {
    let demanded;
    if (typeof blocks === "function") {
      demanded = O(() => {
        const of = board.value;
        if (of === void 0) return /* @__PURE__ */ new Set();
        return f(() => {
          try {
            return new Set(blocks(of));
          } catch (error) {
            report(
              new Error(
                `The demand of ${owner} threw, so it demands nothing until it returns: ${messageOf(error)}`,
                { cause: error }
              )
            );
            return /* @__PURE__ */ new Set();
          }
        });
      });
    } else {
      demanded = y(new Set(blocks));
    }
    demands.value = new Map(demands.value).set(id2, { owner, priority, blocks: demanded });
  }
  if (!gated) {
    demanding(++ids2, "blockr.core", (all) => Object.keys(all.blocks), 0);
  }
  if (store.hasBoard()) opened(store.board());
  let heard = /* @__PURE__ */ new Map();
  const dispose = x(() => {
    const now = statuses.value;
    const settled = settle(oneTimes.value, now);
    if (settled.settled.length > 0) {
      f(() => {
        update(settled);
      });
    }
    const changed = changedSince(heard, now);
    heard = now;
    if (changed.length > 0) {
      f(() => {
        for (const fn of [...subscribers]) {
          try {
            fn(changed);
          } catch (error) {
            report(error);
          }
        }
      });
    }
    if (state2.value !== planned) schedule();
  });
  const offStore = store.subscribe((change, next) => {
    if ("opened" in change || board.peek() === void 0) opened(next);
    else applied(change.delta, next);
  });
  function stop() {
    stopped = endedError();
    if (passTimer !== void 0) clearTimeout(passTimer);
    passTimer = void 0;
    wake(void 0);
    dispose();
    offStore();
    subscribers.clear();
    const error = endedError();
    update({ oneTimes: /* @__PURE__ */ new Map(), settled: [...oneTimes.peek().keys()].map((id2) => [id2, { error }]) });
  }
  if (ended()) stop();
  else options.signal?.addEventListener("abort", stop, { once: true });
  return {
    demand: E((owner, blocks, { priority } = {}) => {
      const id2 = ++ids2;
      demanding(id2, owner, blocks, checked(priority));
      return {
        set: E((next, { priority: given } = {}) => {
          const demand = demands.value.get(id2);
          if (demand === void 0) {
            throw new Error(`The demand of ${owner} was released, so it can no longer be set.`);
          }
          demanding(id2, owner, next, given === void 0 ? demand.priority : checked(given));
        }),
        release: E(() => {
          if (!demands.value.has(id2)) return;
          const rest = new Map(demands.value);
          rest.delete(id2);
          demands.value = rest;
        })
      };
    }),
    evaluate: E((blocks, params, { priority, signal: abort } = {}) => {
      if (ended()) return Promise.reject(endedError());
      const at = priority ?? 0;
      const invalid = priorityError(at);
      if (invalid !== void 0) return Promise.reject(invalid);
      if (abort?.aborted === true) return Promise.reject(asError(abort.reason));
      return new Promise((resolve, reject) => {
        const id2 = ++ids2;
        const onAbort2 = () => {
          end(id2, { error: abort?.reason });
        };
        abort?.addEventListener("abort", onAbort2, { once: true });
        settles.set(id2, (result) => {
          abort?.removeEventListener("abort", onAbort2);
          if ("error" in result) reject(asError(result.error));
          else resolve(result.statuses);
        });
        const oneTime = oneTimeOf([...new Set(blocks)], params, at, statuses.value);
        if (oneTime instanceof Error) {
          update({ oneTimes: oneTimes.value, settled: [[id2, { error: oneTime }]] });
        } else {
          oneTimes.value = new Map(oneTimes.value).set(id2, oneTime);
        }
      });
    }),
    invalidate: E((id2) => {
      if (!revisions.value.has(id2)) return;
      revisions.value = bump(graph.value, revisions.value, /* @__PURE__ */ new Set([id2]), revision);
    }),
    status: (id2) => statuses.peek().get(id2),
    subscribe(fn) {
      const wrapped = (changed) => {
        fn(changed);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },
    async request(request, requestOptions = {}) {
      for (; ; ) {
        if (ended()) throw endedError();
        const id2 = ++ids2;
        asked.value = new Map(asked.peek()).set(id2, { id: id2, at: Date.now() });
        try {
          const result = await client.request(request, requestOptions);
          if (result.reply.status === "missing") lacking(result.reply.missing);
          return result;
        } catch (error) {
          if (error instanceof SessionEndedError && resendable(request) && !ended()) continue;
          throw error;
        } finally {
          const rest = new Map(asked.peek());
          rest.delete(id2);
          asked.value = rest;
        }
      }
    },
    inputs: (id2) => inputs.peek().get(id2),
    start: E(() => {
      holding.value = false;
    }),
    session: E(() => {
      revisions.value = bump(graph.value, revisions.value, new Set(revisions.value.keys()), revision);
    })
  };
}
function running(sent2) {
  return new Set([...sent2.values()].map((evaluation) => evaluation.block));
}
function changedSince(before, now) {
  const changed = [];
  for (const [id2, status] of now) {
    if (before.get(id2) !== status) changed.push(id2);
  }
  for (const id2 of before.keys()) {
    if (!now.has(id2)) changed.push(id2);
  }
  return changed;
}
function setting(value2, fallback, name3, counts = false) {
  if (value2 === void 0) return fallback;
  const number = typeof value2 === "string" ? numberOf(value2) : value2;
  if (typeof number === "number" && number >= 0 && (!counts || Number.isInteger(number) || number === Infinity)) {
    return number;
  }
  throw new RangeError(
    `The blockr option ${name3} is ${shown(value2)}, where it takes ${counts ? "a whole number" : "a number"} of 0 or more.`
  );
}
function flag(value2, fallback, name3) {
  if (value2 === void 0) return fallback;
  const logical = typeof value2 === "string" ? logicalOf(value2) : value2;
  if (typeof logical === "boolean") return logical;
  throw new TypeError(`The blockr option ${name3} is ${shown(value2)}, where it takes TRUE or FALSE.`);
}
function shown(value2) {
  return typeof value2 === "number" ? String(value2) : JSON.stringify(value2);
}
function checked(priority) {
  const at = priority ?? 0;
  const invalid = priorityError(at);
  if (invalid !== void 0) throw invalid;
  return at;
}
function priorityError(priority) {
  return typeof priority !== "number" || Number.isNaN(priority) ? new TypeError(`An evaluation priority is a number, not ${String(priority)}.`) : void 0;
}

// srcts/mount/answers.ts
function createAnswers(scheduler, signal) {
  const kept = /* @__PURE__ */ new Map();
  const flights = /* @__PURE__ */ new Map();
  signal?.addEventListener(
    "abort",
    () => {
      kept.clear();
      const given = [...flights.values()];
      flights.clear();
      for (const flight of given) flight.controller.abort();
    },
    { once: true }
  );
  scheduler.subscribe(() => {
    sweep();
  });
  function current2(request) {
    return readsOf(request).every(
      (read) => read.block === void 0 || scheduler.status(read.block)?.revision === read.revision
    );
  }
  function sweep() {
    for (const [key, { request }] of kept) {
      if (!current2(request)) kept.delete(key);
    }
  }
  function start2(ask, key) {
    const controller = new AbortController();
    const progress = /* @__PURE__ */ new Set();
    const done = scheduler.request(ask.request, {
      ...ask.body === void 0 ? {} : { body: ask.body },
      signal: controller.signal,
      progress(note) {
        for (const caller of [...progress]) {
          try {
            caller.fn(note);
          } catch (error) {
            report(error);
          }
        }
      }
    }).then((answer) => {
      if (key !== void 0 && ask.share === "keep" && answer.reply.status === "ok" && !("volatile" in answer.reply && answer.reply.volatile === true) && current2(ask.request)) {
        kept.set(key, { answer, request: ask.request });
      }
      return answer;
    }).finally(() => {
      if (key !== void 0 && flights.get(key) === flight) flights.delete(key);
    });
    done.catch(() => void 0);
    const flight = { done, controller, progress, callers: 0 };
    if (key !== void 0) flights.set(key, flight);
    return flight;
  }
  function join(flight, key, signal2, progress) {
    flight.callers += 1;
    const listener = progress === void 0 ? void 0 : { fn: progress };
    if (listener !== void 0) flight.progress.add(listener);
    return new Promise((resolve, reject) => {
      let left2 = false;
      const leave2 = () => {
        left2 = true;
        flight.callers -= 1;
        if (listener !== void 0) flight.progress.delete(listener);
        signal2?.removeEventListener("abort", abort);
      };
      const abort = () => {
        leave2();
        reject(asError(signal2?.reason));
        if (flight.callers === 0) {
          if (key !== void 0 && flights.get(key) === flight) flights.delete(key);
          flight.controller.abort();
        }
      };
      signal2?.addEventListener("abort", abort, { once: true });
      flight.done.then(
        (answer) => {
          if (left2) return;
          leave2();
          resolve(answer);
        },
        (error) => {
          if (left2) return;
          leave2();
          reject(asError(error));
        }
      );
    });
  }
  return {
    ask(ask, { signal: signal2, progress } = {}) {
      if (signal2?.aborted === true) return Promise.reject(asError(signal2.reason));
      const key = ask.share === "once" ? void 0 : keyOf(ask.request);
      const answer = key === void 0 ? void 0 : kept.get(key)?.answer;
      if (answer !== void 0) return Promise.resolve(answer);
      const flight = (key === void 0 ? void 0 : flights.get(key)) ?? start2(ask, key);
      return join(flight, key, signal2, progress);
    },
    size: () => kept.size
  };
}
function keyOf(request) {
  if (request.type === "block_class") {
    return JSON.stringify(
      sorted([
        request.type,
        request.registry_id,
        request.name,
        request.args,
        request.options ?? {},
        request.reads
      ])
    );
  }
  return JSON.stringify(
    sorted([
      request.type,
      request.type === "renderer" ? request.renderer : null,
      request.output,
      request.name,
      request.args,
      request.options,
      request.params
    ])
  );
}
function readsOf(request) {
  return request.type === "block_class" ? request.reads : [request.output];
}
function sorted(x2) {
  if (Array.isArray(x2)) return x2.map(sorted);
  if (isObject(x2)) {
    const object2 = x2;
    return Object.fromEntries(
      Object.keys(object2).sort().map((name3) => [name3, sorted(object2[name3] ?? null)])
    );
  }
  return x2;
}

// srcts/mount/load.ts
var API = 1;
var ModuleError = class extends Error {
  module;
  package;
  constructor(module, pkg, what, options) {
    const name3 = module.replace(/[?#].*$/, "");
    super(`The module ${name3} ${pkg === null ? "from no package" : `of ${pkg}`} ${what}`, options);
    this.name = "ModuleError";
    this.module = name3;
    this.package = pkg;
  }
};
function createLoader(options = {}) {
  const importer = options.importer ?? importModule;
  const stylesheet = options.stylesheet ?? linkStylesheet;
  const modules = options.modules ?? (() => ({}));
  const imported = /* @__PURE__ */ new Map();
  const sheets = /* @__PURE__ */ new Map();
  function once2(cache, url, load2) {
    let loading = cache.get(url);
    if (loading === void 0) {
      loading = load2(url);
      cache.set(url, loading);
    }
    return loading;
  }
  async function load(source, pkg) {
    const sheet = source.css === void 0 ? Promise.resolve() : once2(sheets, source.css, stylesheet).catch((error) => {
      report(new ModuleError(source.module, pkg, `has a stylesheet that failed to load: ${messageOf(error)}`, { cause: error }));
    });
    const exports = once2(imported, source.module, importer);
    await sheet;
    try {
      return await exports;
    } catch (error) {
      throw new ModuleError(source.module, pkg, `failed to load: ${messageOf(error)}`, { cause: error });
    }
  }
  return {
    load,
    module(id2) {
      const source = own(modules(), id2);
      if (source === void 0) {
        return Promise.reject(new Error(`No package offers a module "${id2}".`));
      }
      return load(source, packageOf(id2));
    }
  };
}
function importModule(url) {
  return import(
    /* @vite-ignore */
    new URL(url, document.baseURI).href
  );
}
function linkStylesheet(url) {
  return new Promise((resolve, reject) => {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = url;
    link.addEventListener("load", () => {
      resolve();
    }, { once: true });
    link.addEventListener("error", () => {
      reject(new Error(`${url} could not be loaded.`));
    }, { once: true });
    document.head.append(link);
  });
}

// srcts/mount/measure.ts
var FALLBACK = { bg: "#ffffff", fg: "#000000", accent: "#155dfc" };
var ACCENT = "--bc-color-bg-accent";
function domMeasure() {
  return {
    info(el) {
      const rect = el.getBoundingClientRect();
      const width = Math.round(rect.width);
      const height = Math.round(rect.height);
      if (width <= 0 || height <= 0) return null;
      const style = getComputedStyle(el);
      return {
        width,
        height,
        pixel_ratio: window.devicePixelRatio > 0 ? window.devicePixelRatio : 1,
        bg: backgroundOf(el) ?? FALLBACK.bg,
        fg: hexOf(style.color) ?? FALLBACK.fg,
        accent: hexOf(style.getPropertyValue(ACCENT).trim()) ?? FALLBACK.accent,
        font: style.fontFamily
      };
    },
    watch(el, fn) {
      const stops = [];
      if (typeof ResizeObserver === "function") {
        const resized = new ResizeObserver(() => {
          fn();
        });
        resized.observe(el);
        stops.push(() => {
          resized.disconnect();
        });
      }
      const themed = new MutationObserver(() => {
        fn();
      });
      themed.observe(document.documentElement, {
        attributes: true,
        attributeFilter: ["data-bs-theme", "class", "style"]
      });
      stops.push(() => {
        themed.disconnect();
      });
      stops.push(watchRatio(fn));
      return () => {
        for (const stop of stops) stop();
      };
    }
  };
}
function watchRatio(fn) {
  if (typeof matchMedia !== "function") return () => void 0;
  let query;
  const changed = () => {
    listen();
    fn();
  };
  function listen() {
    query?.removeEventListener("change", changed);
    query = matchMedia(`(resolution: ${String(window.devicePixelRatio)}dppx)`);
    query.addEventListener("change", changed);
  }
  listen();
  return () => {
    query?.removeEventListener("change", changed);
  };
}
function backgroundOf(el) {
  for (let at = el; at !== null; at = at.parentElement) {
    const colour = getComputedStyle(at).backgroundColor;
    if (!transparent(colour)) return hexOf(colour);
  }
  return null;
}
function transparent(colour) {
  return colour === "" || colour === "transparent" || /^rgba\(.*,\s*0\)$/.test(colour);
}
function hexOf(colour) {
  const text = colour.trim();
  if (/^#[0-9a-f]{6}$/i.test(text)) return text.toLowerCase();
  if (/^#[0-9a-f]{3}$/i.test(text)) {
    const [r2, g2, b2] = [text.charAt(1), text.charAt(2), text.charAt(3)];
    return `#${r2}${r2}${g2}${g2}${b2}${b2}`.toLowerCase();
  }
  const rgb = /^rgba?\(\s*(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)/.exec(text);
  if (rgb !== null) return hex(rgb.slice(1, 4).map(Number));
  return text === "" ? null : drawn(text);
}
function hex(channels) {
  return `#${channels.map((c2) => Math.round(Math.min(255, Math.max(0, c2))).toString(16).padStart(2, "0")).join("")}`;
}
function drawn(colour) {
  try {
    const context = document.createElement("canvas").getContext("2d", { willReadFrequently: true });
    if (context === null) return null;
    context.fillStyle = "#000000";
    context.fillStyle = colour;
    context.fillRect(0, 0, 1, 1);
    const [r2 = 0, g2 = 0, b2 = 0] = context.getImageData(0, 0, 1, 1).data;
    return hex([r2, g2, b2]);
  } catch {
    return null;
  }
}

// srcts/mount/requests.ts
var ATTEMPTS = 3;
var RequestError = class extends Error {
  status;
  conditions;
  inputs;
  blocks;
  constructor(message, fields = {}) {
    const conditions = fields.conditions ?? [];
    super([message, ...conditions.map((condition) => `- ${condition.message}`)].join("\n"));
    this.name = "RequestError";
    this.status = fields.status ?? null;
    this.conditions = conditions;
    this.inputs = fields.inputs ?? [];
    this.blocks = fields.blocks ?? [];
  }
};
async function blockRequest(deps, block, entry, name3, given, opts) {
  const spec = own(entry.requests, name3);
  if (spec === void 0) {
    throw new Error(`The block class "${entry.id}" has no request "${name3}".`);
  }
  if (!isObject(given)) {
    throw new TypeError(`The args of the request "${name3}" are an object of state args and parameters.`);
  }
  const share = spec.action === true || opts.file !== void 0 ? "once" : spec.volatile === true ? "merge" : "keep";
  const what = `The request "${name3}" of block "${block}"`;
  for (let attempt3 = 1; ; attempt3++) {
    const reads = await readsOf2(deps, block, entry, name3, spec.inputs, opts.signal);
    if (!deps.bound()) throw deps.unboundError();
    const board = deps.store.board();
    const state2 = own(board.blocks, block)?.state;
    if (state2 === void 0) throw new Error(`Block "${block}" left the board, so its request "${name3}" did not go to R.`);
    const args = {};
    for (const arg of spec.state_args) {
      const value2 = own(state2, arg);
      if (value2 !== void 0 && !Object.hasOwn(given, arg)) args[arg] = value2;
    }
    for (const [arg, value2] of Object.entries(given)) {
      if (value2 !== void 0) args[arg] = value2;
    }
    const options = {};
    for (const id2 of spec.options) {
      const value2 = own(board.options, id2);
      if (value2 !== void 0) options[id2] = value2;
    }
    const request = {
      kind: "request",
      type: "block_class",
      registry_id: entry.id,
      block,
      name: name3,
      args,
      reads,
      ...spec.options.length === 0 ? {} : { options }
    };
    const { reply, body } = await deps.answers.ask(
      { request, share, ...opts.file === void 0 ? {} : { body: opts.file } },
      {
        ...opts.signal === void 0 ? {} : { signal: opts.signal },
        ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
      }
    );
    if (!deps.bound()) throw deps.unboundError();
    switch (reply.status) {
      case "ok":
        if (body !== null) return body;
        return "value" in reply && reply.value !== void 0 ? reply.value : null;
      case "missing":
        if (attempt3 < ATTEMPTS) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, {
          status: "missing"
        });
      case "cancelled":
        throw new RequestError(`R cancelled the request "${name3}" of block "${block}".`, { status: "cancelled" });
      default:
        throw new RequestError(`R replied ${reply.status} to the request "${name3}" of block "${block}".`, {
          status: reply.status,
          conditions: "conditions" in reply ? reply.conditions : []
        });
    }
  }
}
async function readsOf2(deps, block, entry, name3, names, signal) {
  const what = `The request "${name3}" of block "${block}"`;
  if (names.length === 0) return [];
  const inputs = deps.scheduler.inputs(block);
  if (inputs === void 0) throw new Error(`Block "${block}" left the board, so its request "${name3}" did not go to R.`);
  const unlinked = inputs.unlinked.filter((input2) => names.includes(input2));
  if (unlinked.length > 0) {
    throw new RequestError(`${what} did not go to R, since no link feeds its ${unlinked.length === 1 ? "input" : "inputs"} ${quoted(unlinked)}.`);
  }
  const read = inputs.recipe.filter(
    (input2) => names.includes(input2.input) || names.includes("...") && !entry.inputs.includes(input2.input)
  );
  const upstream = [...new Set(read.flatMap((input2) => input2.block ?? []))];
  if (upstream.length === 0) return read.map((input2) => ({ input: input2.input }));
  const statuses = await deps.scheduler.evaluate(upstream, void 0, signal === void 0 ? {} : { signal });
  const unready = read.flatMap((input2) => {
    if (input2.block === void 0) return [];
    const status = own(statuses, input2.block);
    return status === void 0 || status.status === "ready" ? [] : [{ input: input2.input, status }];
  });
  if (unready.length > 0) {
    const listed = unready.map(({ input: input2, status }) => `"${input2}" reads ${status.status}`);
    throw new RequestError(`${what} did not go to R, since an input it reads is not ready: ${listed.join(", ")}.`, {
      inputs: unready
    });
  }
  return read.map(
    (input2) => input2.block === void 0 ? { input: input2.input } : { input: input2.input, block: input2.block, revision: own(statuses, input2.block)?.revision ?? 0 }
  );
}

// srcts/mount/shared.ts
function moduleOf(exports, module, pkg) {
  if (!isObject(exports) || typeof exports.api !== "number") {
    throw new ModuleError(module, pkg, "declares no major of core's JS API, as `export const api = 1`.");
  }
  if (exports.api !== API) {
    throw new ModuleError(
      module,
      pkg,
      `was built against major ${String(exports.api)} of core's JS API, where core's is ${String(API)}.`
    );
  }
  const mod = exports.default;
  if (!isObject(mod) || typeof mod.render !== "function") {
    throw new ModuleError(module, pkg, "has no default export with a render() function.");
  }
  const named2 = mod.options;
  if (named2 !== void 0 && !(Array.isArray(named2) && named2.every((id2) => typeof id2 === "string"))) {
    throw new ModuleError(module, pkg, "names its board options in something other than an array of their ids.");
  }
  return mod;
}
function showError(el, message) {
  const box = document.createElement("div");
  box.setAttribute("role", "alert");
  box.className = "bc:rounded bc:border bc:border-border-danger bc:p-2 bc:font-sans bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap";
  box.textContent = message;
  el.replaceChildren(box);
}
function attempt(fn) {
  try {
    fn();
  } catch (error) {
    report(error);
  }
}
function onAbort(signal, fn) {
  signal.addEventListener("abort", fn, { once: true });
  return () => {
    signal.removeEventListener("abort", fn);
  };
}
var REPLACED = "Another board opened, so core mounts nothing more of the board before.";
function announceTo(listeners) {
  for (const fn of [...listeners]) attempt(fn);
}

// srcts/mount/option.ts
function mountOption(deps, el, option) {
  const { store, loader } = deps;
  if (deps.replaced.aborted) throw new Error(REPLACED);
  const def = own(store.options.registered(), option);
  if (def === void 0) {
    throw new Error(`No package registers the board option "${option}" whose UI to mount.`);
  }
  const chosen = Object.entries(deps.renderers()).find(([, renderer]) => renderer.option === option);
  const controller = new AbortController();
  const ended = () => controller.signal.aborted;
  const listeners = /* @__PURE__ */ new Set();
  let value2;
  let fallback;
  let seen = store.board().options;
  let cleanup;
  function bound() {
    return !ended() && !deps.replaced.aborted;
  }
  const unboundError = () => new Error(`Another board opened, so the host of the board option "${option}" no longer changes it.`);
  function current2() {
    const held = own(store.board().options, option);
    return held === void 0 ? fallback : held;
  }
  const host = {
    option,
    value: () => value2,
    on(event, fn) {
      const heard = event;
      if (heard !== "value") {
        throw new Error(`The host of a board option announces "value", not ${JSON.stringify(event)}.`);
      }
      const wrapped = () => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    submit(next) {
      if (!bound()) return Promise.reject(unboundError());
      return deps.commands.run("blockr.core-set_option", { id: option, value: next });
    },
    request(name3, args = {}, opts = {}) {
      if (typeof name3 !== "string" || name3 === "") {
        return Promise.reject(new TypeError("A request of a board option is named by a non-empty string."));
      }
      if (!isObject(args)) return Promise.reject(new TypeError(`The args of the request "${name3}" are an object.`));
      if (!bound()) return Promise.reject(unboundError());
      return optionRequest(deps, option, name3, args, opts);
    }
  };
  const offStore = store.subscribe((_change, board) => {
    if (board.options === seen) return;
    seen = board.options;
    const next = current2();
    if (next === value2) return;
    value2 = next;
    announceTo(listeners);
  });
  const offEnd = onAbort(deps.replaced, end);
  function end() {
    if (ended()) return;
    controller.abort();
    offEnd();
    offStore();
    listeners.clear();
    if (cleanup !== void 0) attempt(cleanup);
    el.replaceChildren();
  }
  void (async () => {
    let mod;
    const [id2, renderer] = chosen ?? [];
    try {
      if (renderer === void 0 || id2 === void 0) {
        mod = def.ui;
      } else {
        const pkg = packageOf(id2);
        const source = renderer.css === void 0 ? { module: renderer.module } : { module: renderer.module, css: renderer.css };
        mod = moduleOf(await loader.load(source, pkg), renderer.module, pkg);
      }
      if (ended()) return;
      await deps.addOptions(mod.options ?? [], `the UI of the board option "${option}"`);
      if (ended()) return;
      fallback = Object.hasOwn(store.board().options, option) ? void 0 : await deps.defaultOf(option, def);
    } catch (error) {
      fail(error);
      return;
    }
    if (ended()) return;
    value2 = current2();
    try {
      const rendered = await mod.render({ el, host, signal: controller.signal });
      if (typeof rendered !== "function") return;
      if (ended()) attempt(rendered);
      else cleanup = rendered;
    } catch (error) {
      const module = renderer?.module ?? `the UI of the board option "${option}"`;
      fail(new ModuleError(module, id2 === void 0 ? null : packageOf(id2), `failed in render(): ${messageOf(error)}`, { cause: error }));
    }
  })();
  function fail(error) {
    if (ended()) return;
    showError(el, messageOf(error));
    report(error);
  }
  return end;
}
async function optionRequest(deps, option, name3, args, opts) {
  const { reply, body } = await deps.scheduler.request(
    { kind: "request", type: "option", option, name: name3, args: { ...args } },
    {
      ...opts.file === void 0 ? {} : { body: opts.file },
      ...opts.signal === void 0 ? {} : { signal: opts.signal },
      ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
    }
  );
  switch (reply.status) {
    case "ok":
      if (body !== null) return body;
      return "value" in reply && reply.value !== void 0 ? reply.value : null;
    case "cancelled":
      throw new RequestError(`R cancelled the request "${name3}" of the board option "${option}".`, { status: "cancelled" });
    default:
      throw new RequestError(`R replied ${reply.status} to the request "${name3}" of the board option "${option}".`, {
        status: reply.status,
        conditions: "conditions" in reply ? reply.conditions : []
      });
  }
}

// srcts/mount/output.ts
var SETTLE_MS = 150;
var ATTEMPTS2 = 3;
var EMPTY = Object.freeze({});
function createShelves() {
  const shelves = /* @__PURE__ */ new Map();
  const giveUp = (shelf) => {
    for (const flight of shelf.flights.values()) flight.controller.abort();
    shelf.flights.clear();
  };
  return {
    of(block) {
      let shelf = shelves.get(block);
      if (shelf === void 0) {
        shelf = { params: {}, last: void 0, mounts: /* @__PURE__ */ new Set(), flights: /* @__PURE__ */ new Map() };
        shelves.set(block, shelf);
      }
      return shelf;
    },
    drop(block) {
      const shelf = shelves.get(block);
      if (shelf === void 0) return;
      giveUp(shelf);
      shelves.delete(block);
    }
  };
}
function mountOutput(deps, el, block) {
  const { store, scheduler, loader, measure } = deps;
  if (deps.replaced.aborted) throw new Error(REPLACED);
  const found = own(store.board().blocks, block);
  if (found === void 0) {
    throw new Error(`The board holds no block "${block}" whose output to mount.`);
  }
  const entry = own(store.blockRegistry(), found.registry_id);
  if (entry === void 0) {
    throw new Error(`The block registry has no class "${found.registry_id}" for block "${block}".`);
  }
  const shelf = deps.shelves.of(block);
  const controller = new AbortController();
  const ended = () => controller.signal.aborted;
  let renderer = entry.renderer;
  let ready;
  const preparing = /* @__PURE__ */ new Map();
  let info = measure.info(el);
  let held;
  let asking;
  let error = null;
  let drawing;
  let options = store.board().options;
  const waiting2 = /* @__PURE__ */ new Set();
  const broken = /* @__PURE__ */ new WeakSet();
  function bound() {
    return !ended() && !deps.replaced.aborted && Object.hasOwn(store.board().blocks, block);
  }
  const unboundError = () => new Error(
    deps.replaced.aborted ? `Another board opened, so the host of the output of block "${block}" no longer asks R.` : `Block "${block}" left the board, so the host of its output no longer asks R.`
  );
  function prepare(given) {
    let loading = preparing.get(given.id);
    if (loading === void 0) {
      loading = (async () => {
        const pkg = packageOf(given.id);
        const source = given.css === void 0 ? { module: given.module } : { module: given.module, css: given.css };
        const mod = moduleOf(await loader.load(source, pkg), given.module, pkg);
        const named3 = mod.options ?? [];
        await deps.addOptions(named3, `the renderer "${given.id}"`);
        return { renderer: given, mod, named: named3 };
      })();
      preparing.set(given.id, loading);
    }
    return loading;
  }
  function expect(given) {
    renderer = given;
    if (ready?.renderer.id === given.id) return;
    prepare(given).then(
      (prepared) => {
        if (ended() || renderer?.id !== prepared.renderer.id) return;
        ready = prepared;
        sync();
      },
      (failure) => {
        fail(failure);
      }
    );
  }
  function named2() {
    if (renderer === void 0) return null;
    return ready?.renderer.id === renderer.id ? ready.named : void 0;
  }
  function values2(ids2) {
    const options2 = store.board().options;
    if (ids2 === null) return options2;
    return Object.fromEntries(
      ids2.flatMap((id2) => {
        const value2 = own(options2, id2);
        return value2 === void 0 ? [] : [[id2, value2]];
      })
    );
  }
  function wanted() {
    if (info === null) return void 0;
    const ids2 = named2();
    if (ids2 === void 0) return void 0;
    const status = scheduler.status(block);
    if (status?.status !== "ready") return void 0;
    return { asked: { revision: status.revision, options: values2(ids2), info, params: shelf.params }, ids: ids2 };
  }
  function fits2(asked, want, ids2) {
    if (asked.revision !== want.revision || !jsonEqual(asked.info, want.info)) return false;
    if (!jsonEqual(asked.params, want.params)) return false;
    if (ids2 === null) return jsonEqual(asked.options, want.options);
    return ids2.every((id2) => jsonEqual(own(asked.options, id2), own(want.options, id2)));
  }
  function sync() {
    if (ended()) return;
    const want = wanted();
    if (want !== void 0) {
      if (held !== void 0 && fits2(held.asked, want.asked, want.ids)) {
        stopAsking();
      } else if (shelf.last !== void 0 && shelf.last !== held && fits2(shelf.last.asked, want.asked, want.ids)) {
        stopAsking();
        take(shelf.last);
      } else {
        const key = keyOf2(want.asked);
        if (asking?.key !== key) {
          stopAsking();
          asking = join(key, want.asked);
          announce2();
        }
      }
    }
    wake();
  }
  function stopAsking() {
    if (asking === void 0) return;
    asking.leave();
    asking = void 0;
    announce2();
  }
  function join(key, asked) {
    let flight = shelf.flights.get(key);
    if (flight === void 0) {
      const aborted = new AbortController();
      const done = scheduler.request(
        { kind: "output", block, revision: asked.revision, options: asked.options, info: asked.info, params: asked.params },
        { signal: aborted.signal }
      ).then((answer) => {
        if (answer.reply.status === "ok" && "output" in answer.reply && !deps.replaced.aborted) {
          shelf.last = { asked, output: answer.reply.output, body: answer.body };
        }
        return answer;
      });
      const started = { done, controller: aborted, callers: 0 };
      void done.catch(() => void 0).finally(() => {
        if (shelf.flights.get(key) === started) shelf.flights.delete(key);
      });
      shelf.flights.set(key, started);
      flight = started;
    }
    const mine = flight;
    mine.callers += 1;
    let left2 = false;
    const ask = {
      key,
      leave() {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (mine.callers === 0) {
          if (shelf.flights.get(key) === mine) shelf.flights.delete(key);
          mine.controller.abort();
        }
      }
    };
    mine.done.then(
      (answer) => {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (asking === ask) asking = void 0;
        received2(asked, answer);
      },
      (failure) => {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (asking === ask) asking = void 0;
        fail(failure);
      }
    );
    return ask;
  }
  function received2(asked, { reply, body }) {
    if (ended()) return;
    switch (reply.status) {
      case "ok":
        take({ asked, output: reply.output, body });
        return;
      case "error": {
        const conditions = "conditions" in reply ? reply.conditions : [];
        failed2(
          conditions[0] ?? {
            severity: "error",
            class: ["blockr_error", "error", "condition"],
            phase: "output",
            message: `R failed to make the output of block "${block}".`
          },
          conditions
        );
        return;
      }
      default:
        announce2();
    }
  }
  function failed2(condition, conditions) {
    error = condition;
    if (held === void 0) {
      showError(el, conditions.length > 0 ? conditions.map((c2) => c2.message).join("\n") : condition.message);
    }
    announce2();
  }
  function fail(failure) {
    if (ended()) return;
    if (failure instanceof ModuleError) {
      showError(el, failure.message);
      if (!broken.has(failure)) report(failure);
      broken.add(failure);
      return;
    }
    failed2(
      { severity: "error", class: ["blockr_error", "error", "condition"], phase: "output", message: messageOf(failure) },
      []
    );
  }
  function take(next) {
    held = next;
    error = null;
    const named3 = next.output.renderer;
    if (renderer?.id !== named3.id) expect(named3);
    if (drawing !== void 0 && drawing.prepared.renderer.id !== named3.id) unmountRenderer();
    if (drawing === void 0) {
      prepare(named3).then(
        (prepared) => {
          if (ended() || drawing !== void 0 || held?.output.renderer.id !== prepared.renderer.id) return;
          mountRenderer(prepared);
        },
        (failure) => {
          fail(failure);
        }
      );
    }
    announce2();
    wake();
  }
  function mountRenderer(prepared) {
    const signal = new AbortController();
    const current3 = { prepared, host: outputHost(prepared), controller: signal, cleanup: void 0 };
    drawing = current3;
    el.replaceChildren();
    void (async () => {
      try {
        const rendered = await prepared.mod.render({ el, host: current3.host, signal: signal.signal });
        if (typeof rendered !== "function") return;
        if (signal.signal.aborted) attempt(rendered);
        else current3.cleanup = rendered;
      } catch (failure) {
        if (signal.signal.aborted) return;
        const pkg = packageOf(prepared.renderer.id);
        const why = new ModuleError(prepared.renderer.module, pkg, `failed in render(): ${messageOf(failure)}`, {
          cause: failure
        });
        showError(el, why.message);
        report(why);
      }
    })();
  }
  function unmountRenderer() {
    const current3 = drawing;
    if (current3 === void 0) return;
    drawing = void 0;
    current3.host.drop();
    current3.controller.abort();
    if (current3.cleanup !== void 0) attempt(current3.cleanup);
    el.replaceChildren();
  }
  function announce2() {
    drawing?.host.announce();
  }
  function wake() {
    announceTo(waiting2);
  }
  function optionsFor(prepared, asked) {
    const now = store.board().options;
    return Object.fromEntries(
      prepared.named.flatMap((id2) => {
        const value2 = asked !== void 0 && Object.hasOwn(asked.options, id2) ? asked.options[id2] : own(now, id2);
        return value2 === void 0 ? [] : [[id2, value2]];
      })
    );
  }
  function replaceParams(value2) {
    if (!bound()) return;
    const next = frozenCopy(value2);
    if (jsonEqual(next, shelf.params)) return;
    shelf.params = next;
    announceTo(shelf.mounts);
  }
  function current2(name3, signal) {
    return new Promise((resolve, reject) => {
      const settle2 = () => {
        if (!bound()) {
          reject(unboundError());
          return true;
        }
        if (signal?.aborted === true) {
          reject(asError(signal.reason));
          return true;
        }
        const status = scheduler.status(block);
        if (status === void 0) {
          reject(unboundError());
          return true;
        }
        if (status.status === "failed" || status.status === "invalid" || status.status === "waiting") {
          reject(
            new RequestError(
              `The request "${name3}" of the output of block "${block}" did not go to R, since the block reads ${status.status}.`,
              { blocks: [{ block, status }] }
            )
          );
          return true;
        }
        if (status.status === "ready" && held?.asked.revision === status.revision) {
          resolve(held);
          return true;
        }
        return false;
      };
      if (settle2()) return;
      const check2 = () => {
        if (settle2()) stop();
      };
      const stop = () => {
        waiting2.delete(check2);
        signal?.removeEventListener("abort", check2);
      };
      waiting2.add(check2);
      signal?.addEventListener("abort", check2, { once: true });
    });
  }
  async function request(prepared, name3, args, opts) {
    const what = `The request "${name3}" of the output of block "${block}"`;
    for (let attempt3 = 1; ; attempt3++) {
      const target2 = await current2(name3, opts.signal);
      const output = { block, revision: target2.asked.revision };
      const fields = { output, name: name3, args: { ...args }, options: target2.asked.options, params: target2.asked.params };
      const asked = opts.scope === "renderer" ? { kind: "request", type: "renderer", renderer: prepared.renderer.id, ...fields } : { kind: "request", type: "output_class", ...fields };
      const { reply, body } = await deps.answers.ask(
        { request: asked, share: opts.file === void 0 ? "keep" : "once", ...opts.file === void 0 ? {} : { body: opts.file } },
        {
          ...opts.signal === void 0 ? {} : { signal: opts.signal },
          ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
        }
      );
      if (!bound()) throw unboundError();
      switch (reply.status) {
        case "ok":
          if (body !== null) return body;
          return "value" in reply && reply.value !== void 0 ? reply.value : null;
        case "missing":
          if (attempt3 < ATTEMPTS2) continue;
          throw new RequestError(`${what} gave up, since R kept lacking the result it reads.`, { status: "missing" });
        case "cancelled":
          throw new RequestError(`R cancelled ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, { status: "cancelled" });
        default:
          throw new RequestError(`R replied ${reply.status} to ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, {
            status: reply.status,
            conditions: "conditions" in reply ? reply.conditions : []
          });
      }
    }
  }
  function checkRequest(name3, args, opts) {
    if (typeof name3 !== "string" || name3 === "") return new TypeError("A request of an output is named by a non-empty string.");
    if (!isObject(args)) return new TypeError(`The args of the request "${name3}" are an object.`);
    if (!isObject(opts)) return new TypeError(`The options of the request "${name3}" are an object.`);
    const scope = opts.scope;
    if (scope !== void 0 && scope !== "class" && scope !== "renderer") {
      return new TypeError(`A request of an output has the scope "class" or "renderer", not ${JSON.stringify(scope)}.`);
    }
    return void 0;
  }
  function outputHost(prepared) {
    const listeners = /* @__PURE__ */ new Set();
    let named3;
    let following = true;
    return {
      block,
      output: () => held?.output ?? null,
      body: () => held?.body ?? null,
      params: () => held?.asked.params ?? EMPTY,
      options() {
        let memo = named3;
        if (memo === void 0 || memo.for !== held) {
          memo = { for: held, values: optionsFor(prepared, held?.asked) };
          named3 = memo;
        }
        return memo.values;
      },
      loading: () => asking !== void 0,
      error: () => error,
      on(event, fn) {
        const heard = event;
        if (heard !== "output") {
          throw new Error(`The host of an output announces "output", not ${JSON.stringify(event)}.`);
        }
        const wrapped = () => {
          fn();
        };
        listeners.add(wrapped);
        return () => {
          listeners.delete(wrapped);
        };
      },
      setParams(params) {
        if (!isObject(params)) throw new TypeError("An output's parameters are an object.");
        if (following) replaceParams(params);
      },
      request(name3, args = {}, opts = {}) {
        const invalid = checkRequest(name3, args, opts);
        if (invalid !== void 0) return Promise.reject(invalid);
        if (!following || !bound()) return Promise.reject(unboundError());
        return request(prepared, name3, args, opts);
      },
      emit(name3, payload) {
        if (following && bound()) deps.emit({ block, name: name3, payload });
      },
      announce() {
        if (following) announceTo(listeners);
      },
      drop() {
        following = false;
        listeners.clear();
      }
    };
  }
  const offStore = store.subscribe((_change, next) => {
    if (!Object.hasOwn(next.blocks, block)) {
      end();
      return;
    }
    if (next.options === options) return;
    options = next.options;
    sync();
  });
  const offScheduler = scheduler.subscribe((changed) => {
    if (changed.includes(block)) sync();
  });
  shelf.mounts.add(sync);
  let settling;
  const stopWatching = measure.watch(el, () => {
    if (settling !== void 0) clearTimeout(settling);
    settling = setTimeout(
      () => {
        settling = void 0;
        const next = measure.info(el);
        if (jsonEqual(next, info)) return;
        info = next;
        sync();
      },
      info === null ? 0 : SETTLE_MS
    );
  });
  const offEnd = onAbort(deps.replaced, end);
  function end() {
    if (ended()) return;
    controller.abort();
    offEnd();
    offStore();
    offScheduler();
    shelf.mounts.delete(sync);
    stopWatching();
    if (settling !== void 0) clearTimeout(settling);
    asking?.leave();
    asking = void 0;
    unmountRenderer();
    el.replaceChildren();
    wake();
  }
  if (renderer !== void 0) expect(renderer);
  if (shelf.last !== void 0) take(shelf.last);
  sync();
  return end;
}
function keyOf2(asked) {
  return JSON.stringify([asked.revision, sorted2(asked.options), sorted2(asked.info), sorted2(asked.params)]);
}
function sorted2(x2) {
  if (Array.isArray(x2)) return x2.map(sorted2);
  if (isObject(x2)) {
    const object2 = x2;
    return Object.fromEntries(
      Object.keys(object2).sort().map((name3) => [name3, sorted2(object2[name3] ?? null)])
    );
  }
  return x2;
}

// srcts/mount/block.ts
var CORE = "blockr.core";
var NO_STATUS = Object.freeze(unevaluated(0, false));
function createMounts(options) {
  const { store, commands, scheduler, loader } = options;
  const replaced = options.signal ?? new AbortController().signal;
  const answers = createAnswers(scheduler, replaced);
  const shelves = createShelves();
  const measure = options.measure ?? domMeasure();
  const emit = options.emit ?? (() => void 0);
  const renderers = options.renderers ?? (() => ({}));
  const defaults = /* @__PURE__ */ new Map();
  store.subscribe((change) => {
    if ("opened" in change) return;
    for (const id2 of change.delta.blocks?.rm ?? []) shelves.drop(id2);
  });
  function defaultOf(id2, def) {
    let value2 = defaults.get(id2);
    if (value2 === void 0) {
      const given = def.default;
      value2 = typeof given === "function" ? Promise.resolve().then(given) : Promise.resolve(given);
      defaults.set(id2, value2);
    }
    return value2;
  }
  async function addOptions(ids2, by) {
    await Promise.all(
      ids2.map(async (id2) => {
        if (Object.hasOwn(store.board().options, id2)) return;
        const def = own(store.options.registered(), id2);
        if (def === void 0) {
          report(new Error(`The board option "${id2}", which ${by} requires, is defined by no package loaded.`));
          return;
        }
        let value2;
        try {
          value2 = await defaultOf(id2, def);
        } catch (error) {
          report(new Error(`The default of the board option "${id2}" failed: ${messageOf(error)}`, { cause: error }));
          return;
        }
        if (replaced.aborted || Object.hasOwn(store.board().options, id2)) return;
        try {
          await commands.run("blockr.core-set_option", { id: id2, value: value2 }, CORE);
        } catch (error) {
          report(error);
        }
      })
    );
  }
  function mountBlock(el, block) {
    if (replaced.aborted) throw new Error(REPLACED);
    const found = own(store.board().blocks, block);
    if (found === void 0) {
      throw new Error(`The board holds no block "${block}" to mount.`);
    }
    const entry = own(store.blockRegistry(), found.registry_id);
    if (entry === void 0) {
      throw new Error(`The block registry has no class "${found.registry_id}" for block "${block}".`);
    }
    const pkg = packageOf(entry.id);
    const controller = new AbortController();
    const { signal } = controller;
    const ended = () => signal.aborted;
    let cleanup;
    const end = () => {
      if (ended()) return;
      offEnd();
      host.drop();
      controller.abort();
      if (cleanup !== void 0) attempt(cleanup);
      el.replaceChildren();
    };
    const host = blockHost({
      store,
      commands,
      scheduler,
      loader,
      answers,
      block,
      entry,
      replaced,
      unbound: end
    });
    const offEnd = onAbort(replaced, end);
    void (async () => {
      let exports;
      try {
        exports = await loader.load({ module: entry.module, ...entry.css === void 0 ? {} : { css: entry.css } }, pkg);
      } catch (error) {
        fail(error);
        return;
      }
      if (ended()) return;
      let mod;
      try {
        mod = moduleOf(exports, entry.module, pkg);
        checkOptions(mod, entry, pkg);
      } catch (error) {
        fail(error);
        return;
      }
      const named2 = mod.options ?? [];
      await addOptions(named2, `the UI module of block class "${entry.id}"`);
      if (ended()) return;
      host.name(named2);
      try {
        const rendered = await mod.render({ el, host, signal });
        if (typeof rendered !== "function") return;
        if (ended()) attempt(rendered);
        else cleanup = rendered;
      } catch (error) {
        fail(new ModuleError(entry.module, pkg, `failed in render(): ${messageOf(error)}`, { cause: error }));
      }
    })();
    function fail(error) {
      if (ended()) return;
      showError(el, messageOf(error));
      report(error);
    }
    return end;
  }
  return {
    mountBlock,
    mountOutput: (el, block) => mountOutput(
      { store, scheduler, loader, answers, shelves, measure, replaced, addOptions, emit },
      el,
      block
    ),
    mountOption: (el, option) => mountOption(
      { store, commands, scheduler, loader, renderers, replaced, addOptions, defaultOf },
      el,
      option
    ),
    addOptions
  };
}
function checkOptions(mod, entry, pkg) {
  const named2 = new Set(mod.options ?? []);
  const gaps = Object.entries(entry.requests).flatMap(([name3, request]) => {
    const left2 = [...new Set(request.options)].filter((id2) => !named2.has(id2));
    if (left2.length === 0) return [];
    return [`- The request "${name3}" reads the board option${left2.length === 1 ? "" : "s"} ${quoted(left2)}.`];
  });
  if (gaps.length === 0) return;
  const what = `leaves out of its \`options\` board options that requests of block class "${entry.id}" read:`;
  throw new ModuleError(entry.module, pkg, [what, ...gaps].join("\n"));
}
function blockHost(deps) {
  const { store, commands, scheduler, block, entry } = deps;
  let seen = store.board();
  let named2 = [];
  let state2 = blockOf(seen)?.state ?? {};
  let context = contextOf(blockOf(seen));
  let following = true;
  let gone = false;
  const listeners = { state: /* @__PURE__ */ new Set(), context: /* @__PURE__ */ new Set() };
  function blockOf(board) {
    return own(board.blocks, block);
  }
  function contextOf(now) {
    const recipe = scheduler.inputs(block)?.recipe ?? [];
    return Object.freeze({
      name: now?.name ?? "",
      status: scheduler.status(block) ?? NO_STATUS,
      inputs: Object.freeze(
        recipe.flatMap(
          (input2) => input2.block === void 0 ? [] : [Object.freeze({ input: input2.input, revision: scheduler.status(input2.block)?.revision ?? 0 })]
        )
      ),
      options: Object.freeze(
        Object.fromEntries(
          named2.flatMap((id2) => {
            const value2 = own(seen.options, id2);
            return value2 === void 0 ? [] : [[id2, value2]];
          })
        )
      )
    });
  }
  function bound() {
    return !gone && !deps.replaced.aborted && Object.hasOwn(store.board().blocks, block);
  }
  function announce2(event) {
    for (const fn of [...listeners[event]]) {
      try {
        fn();
      } catch (error) {
        report(error);
      }
    }
  }
  function refresh() {
    const now = blockOf(seen);
    if (now === void 0) {
      unbind();
      return;
    }
    const stateChanged = now.state !== state2;
    if (stateChanged) state2 = now.state;
    const next = contextOf(now);
    const contextChanged = !sameContext(context, next);
    if (contextChanged) context = next;
    if (stateChanged) announce2("state");
    if (contextChanged) announce2("context");
  }
  function unbind() {
    if (gone) return;
    gone = true;
    stop();
    deps.unbound();
  }
  const offStore = store.subscribe((_change, board) => {
    seen = board;
    refresh();
  });
  const offScheduler = scheduler.subscribe(() => {
    if (following && store.board() === seen) refresh();
  });
  function stop() {
    following = false;
    offStore();
    offScheduler();
  }
  const unboundError = () => new Error(
    deps.replaced.aborted ? `Another board opened, so the host of block "${block}" no longer changes it.` : `Block "${block}" left the board, so its host no longer changes it.`
  );
  return {
    block,
    class: entry.id,
    state: () => state2,
    context: () => context,
    on(event, fn) {
      const heard = own(listeners, event);
      if (heard === void 0) {
        throw new Error(`A block's host announces "state" and "context", not ${JSON.stringify(event)}.`);
      }
      const wrapped = () => {
        fn();
      };
      heard.add(wrapped);
      return () => {
        heard.delete(wrapped);
      };
    },
    submit(change) {
      if (!bound()) return Promise.reject(unboundError());
      if (!isObject(change)) {
        return Promise.reject(new TypeError("A submit takes the state args it changes, as an object."));
      }
      const set = Object.fromEntries(Object.entries(change).filter(([, value2]) => value2 !== void 0));
      return commands.run("blockr.core-set_block", { id: block, state: set });
    },
    request(name3, args = {}, opts = {}) {
      if (!bound()) return Promise.reject(unboundError());
      return blockRequest({ ...deps, bound, unboundError }, block, entry, name3, args, opts);
    },
    async evaluate(params) {
      if (!bound()) throw unboundError();
      if (!isObject(params)) {
        throw new TypeError("A host's evaluate() takes the parameters that the block's block_eval() method reads, as an object.");
      }
      const statuses = await scheduler.evaluate([block], params);
      const status = own(statuses, block);
      if (status === void 0) throw unboundError();
      return status;
    },
    module(id2) {
      return deps.loader.module(id2);
    },
    name(options) {
      named2 = options;
      context = contextOf(blockOf(seen));
    },
    drop: stop
  };
}
function sameContext(a2, b2) {
  return a2.name === b2.name && a2.status === b2.status && a2.inputs.length === b2.inputs.length && a2.inputs.every((input2, i2) => input2.input === b2.inputs[i2]?.input && input2.revision === b2.inputs[i2].revision) && jsonEqual(a2.options, b2.options);
}

// srcts/options/schemas.ts
var optionSchemas = {
  n_rows: {
    type: "integer",
    // A page of a table holds a row at least, as the coercion of table
    // outputs takes it.
    minimum: 1,
    description: "How many rows of a table a block's output shows"
  },
  show_conditions: {
    type: "array",
    items: { enum: ["message", "warning", "error"] },
    uniqueItems: true,
    description: "The severities of a block's conditions that the board shows"
  },
  dark_mode: {
    enum: ["light", "dark"],
    description: "Whether the board shows in its light or its dark theme"
  }
};

// srcts/options/ui.tsx
import { Checklist, NumberInput, Select } from "@blockr/core/react";
import { reactModule, useValue } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
function numberUi(label, min) {
  function NumberUi({ host }) {
    return /* @__PURE__ */ jsx(NumberInput, { bound: useValue(host), label, min });
  }
  return reactModule(NumberUi);
}
function checklistUi(legend, choices) {
  function ChecklistUi({ host }) {
    return /* @__PURE__ */ jsx(Checklist, { bound: useValue(host), legend, choices, ordered: true });
  }
  return reactModule(ChecklistUi);
}
function selectUi(label, placeholder, choices) {
  function SelectUi({ host }) {
    return /* @__PURE__ */ jsx(Select, { bound: useValue(host), label, placeholder, choices });
  }
  return reactModule(SelectUi);
}

// srcts/options/options.ts
function coreOptions(core) {
  return [nRows(core), showConditions(core), darkMode(core)];
}
function nRows(core) {
  const schema = optionSchemas.n_rows;
  const fallback = 10;
  return {
    id: "n_rows",
    label: "Preview rows",
    category: "Table options",
    schema,
    // A string reads as R's as.numeric() reads it.
    default: async () => {
      const value2 = await core.blockrOption("n_rows");
      if (value2 === void 0) return fallback;
      return checked2("n_rows", schema, value2, typeof value2 === "string" ? numberOf(value2) : value2);
    },
    ui: numberUi("Preview rows", schema.minimum)
  };
}
var SEVERITIES = ["message", "warning", "error"];
function showConditions(core) {
  const schema = optionSchemas.show_conditions;
  const fallback = ["warning", "error"];
  return {
    id: "show_conditions",
    label: "Message types to display",
    category: "Board options",
    schema,
    // R writes a blockr option of one severity as a string, as an
    // environment variable gives it.
    default: async () => {
      const value2 = await core.blockrOption("show_conditions");
      if (value2 === void 0) return fallback;
      return checked2("show_conditions", schema, value2, typeof value2 === "string" ? [value2] : value2);
    },
    ui: checklistUi("Message types to display", SEVERITIES)
  };
}
function darkMode(core) {
  const schema = optionSchemas.dark_mode;
  return {
    id: "dark_mode",
    label: "Light/dark mode",
    category: "Theme options",
    schema,
    default: async () => {
      const value2 = await core.blockrOption("dark_mode");
      if (value2 === void 0) return preferredTheme();
      const logical = typeof value2 === "string" ? logicalOf(value2) : value2;
      if (typeof logical === "boolean") return logical ? "dark" : "light";
      return checked2("dark_mode", schema, value2);
    },
    ui: selectUi("Light/dark mode", "Choose a mode", [
      { value: "light", label: "Light" },
      { value: "dark", label: "Dark" }
    ]),
    onChange(value2) {
      setTheme(value2);
    }
  };
}
function setTheme(theme) {
  document.documentElement.setAttribute("data-bs-theme", theme === "dark" ? "dark" : "light");
}
function preferredTheme() {
  return matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light";
}
function checked2(id2, schema, sent2, value2 = sent2) {
  const json = typeof value2 !== "number" || Number.isFinite(value2);
  if (json && compile(`the board option "${id2}"`, schema)(value2).length === 0) return value2;
  throw new TypeError(`The blockr option ${id2} is ${JSON.stringify(sent2)}, which the board option ${id2} does not take.`);
}

// srcts/transport/shiny.ts
var name = "blockr";
function shinyTransport(options = {}) {
  const page = window;
  const shiny = options.shiny ?? page.Shiny;
  if (shiny === void 0) {
    throw new Error("The Shiny transport needs Shiny's JS API, window.Shiny.");
  }
  const fetcher = options.fetch ?? window.fetch.bind(window);
  const onDisconnected = options.onDisconnected ?? disconnected(page.jQuery);
  let start2 = null;
  let session = 0;
  return {
    async send(message, bodies) {
      const at = session;
      for (const [id2, body] of bodies) {
        await post(fetcher, start2, id2, body);
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
          start2 = message;
          listener.started();
        }
      });
      onDisconnected(() => {
        session += 1;
        start2 = null;
        listener.ended();
      });
    }
  };
}
function disconnected(jquery) {
  if (jquery === void 0) {
    throw new Error("The Shiny transport needs the jQuery that Shiny puts on the page.");
  }
  return (fn) => {
    jquery(document).on("shiny:disconnected", fn);
  };
}
function isSessionStart(message) {
  return typeof message === "object" && message !== null && typeof message.body_url === "string";
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
  const bytes = await response.blob();
  fetcher(url, { method: "DELETE" }).then((response2) => response2.text()).catch(() => void 0);
  return blob([bytes], body);
}
async function post(fetcher, start2, id2, body) {
  if (start2 === null) {
    throw new Error("No session runs that a body could be posted to.");
  }
  const max = start2.max_request_size;
  if (max !== void 0 && body.size > max) {
    throw new Error(
      `The body of request ${id2}, of ${String(body.size)} bytes, is larger than the ${String(max)} bytes that R takes.`
    );
  }
  const url = `${start2.body_url}${start2.body_url.includes("?") ? "&" : "?"}id=${encodeURIComponent(id2)}`;
  const response = await fetcher(url, {
    method: "POST",
    body,
    headers: {
      "Content-Type": body.type === "" ? "application/octet-stream" : body.type
    }
  });
  const text = await response.text();
  if (!response.ok) {
    throw new Error(`R refused the body of request ${id2}: ${text}`);
  }
}
function blob(parts, body) {
  return body.name === void 0 ? new Blob(parts, { type: body.type }) : new File(parts, body.name, { type: body.type });
}
function fromBase64(data) {
  const text = atob(data);
  const bytes = new Uint8Array(text.length);
  for (let i2 = 0; i2 < text.length; i2 += 1) {
    bytes[i2] = text.charCodeAt(i2);
  }
  return bytes;
}

// srcts/core/blockr-options.ts
function createBlockrOptions(client) {
  const found = /* @__PURE__ */ new Map();
  let batch;
  function ask(name3) {
    let asked = found.get(name3);
    if (asked === void 0) {
      asked = new Promise((resolve, reject) => {
        if (batch === void 0) {
          const sending = /* @__PURE__ */ new Map();
          batch = sending;
          void Promise.resolve().then(() => {
            if (batch === sending) batch = void 0;
            void send(sending);
          });
        }
        batch.set(name3, { resolve, reject });
      });
      found.set(name3, asked);
    }
    return asked;
  }
  async function send(names) {
    const asked = [...names.keys()];
    let values2;
    try {
      values2 = await valuesOf(client, asked);
    } catch (error) {
      const failure = new Error(`R sent no values of the blockr options ${quoted(asked)}: ${messageOf(error)}`, { cause: error });
      for (const [name3, settle2] of names) {
        found.delete(name3);
        settle2.reject(failure);
      }
      return;
    }
    for (const [name3, settle2] of names) {
      settle2.resolve(Object.hasOwn(values2, name3) ? values2[name3] ?? null : void 0);
    }
  }
  return {
    async read(name3) {
      if (typeof name3 !== "string" || name3 === "") {
        throw new TypeError("A blockr option is read by its name, a non-empty string.");
      }
      return ask(name3);
    }
  };
}
async function valuesOf(client, names) {
  for (; ; ) {
    let reply;
    try {
      ({ reply } = await client.request({ kind: "blockr_options", names }));
    } catch (error) {
      if (error instanceof SessionEndedError) continue;
      throw error;
    }
    if (reply.status !== "ok" || !("values" in reply)) {
      const said = "conditions" in reply ? reply.conditions.map((c2) => c2.message).join(" ") : "";
      throw new Error(`R replied ${reply.status}. ${said}`.trim());
    }
    return reply.values;
  }
}

// srcts/core/requests.ts
var ATTEMPTS3 = 3;
async function pluginRequest(deps, plugin, name3, args, opts) {
  if (typeof plugin !== "string" || plugin === "") {
    throw new TypeError("A plugin's request names the plugin by its id, a non-empty string.");
  }
  const held = own(deps.store.board().plugins, plugin);
  if (held === void 0) {
    throw new Error(`The board holds no plugin "${plugin}", so its request did not go to R.`);
  }
  if (typeof name3 !== "string" || name3 === "") {
    throw new TypeError(`A request of the plugin "${plugin}" is named by a non-empty string.`);
  }
  if (!isObject(args)) {
    throw new TypeError(`The args of the request "${name3}" of the plugin "${plugin}" are an object.`);
  }
  const reads = opts.reads ?? [];
  if (!Array.isArray(reads) || !reads.every((block) => typeof block === "string")) {
    throw new TypeError(`The blocks that the request "${name3}" of the plugin "${plugin}" reads are an array of their ids.`);
  }
  const what = `The request "${name3}" of the plugin "${plugin}"`;
  for (let attempt3 = 1; ; attempt3++) {
    const revisions = await readsOf3(deps, what, reads, opts.signal);
    const { reply, body } = await deps.scheduler.request(
      { kind: "request", type: "plugin", plugin: held.class, name: name3, args: { ...args }, reads: revisions },
      {
        ...opts.file === void 0 ? {} : { body: opts.file },
        ...opts.signal === void 0 ? {} : { signal: opts.signal },
        ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
      }
    );
    switch (reply.status) {
      case "ok":
        if (body !== null) return body;
        return "value" in reply && reply.value !== void 0 ? reply.value : null;
      case "missing":
        if (attempt3 < ATTEMPTS3) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, { status: "missing" });
      case "cancelled":
        throw new RequestError(`R cancelled the request "${name3}" of the plugin "${plugin}".`, { status: "cancelled" });
      default:
        throw new RequestError(`R replied ${reply.status} to the request "${name3}" of the plugin "${plugin}".`, {
          status: reply.status,
          conditions: "conditions" in reply ? reply.conditions : []
        });
    }
  }
}
async function readsOf3(deps, what, blocks, signal) {
  if (blocks.length === 0) return [];
  const absent = blocks.filter((block) => deps.scheduler.status(block) === void 0);
  if (absent.length > 0) {
    throw new Error(`${what} reads ${absent.length === 1 ? "the block" : "the blocks"} ${quoted(absent)}, which the board lacks.`);
  }
  const unique = [...new Set(blocks)];
  const statuses = await deps.scheduler.evaluate(unique, void 0, signal === void 0 ? {} : { signal });
  const unready = unique.flatMap((block) => {
    const status = own(statuses, block);
    return status === void 0 || status.status === "ready" ? [] : [{ block, status }];
  });
  if (unready.length > 0) {
    const listed = unready.map(({ block, status }) => `"${block}" reads ${status.status}`);
    throw new RequestError(`${what} did not go to R, since a block it reads is not ready: ${listed.join(", ")}.`, {
      blocks: unready
    });
  }
  const left2 = unique.filter((block) => own(statuses, block) === void 0);
  if (left2.length > 0) {
    throw new Error(`${what} did not go to R, since ${left2.length === 1 ? "the block" : "the blocks"} ${quoted(left2)} left the board.`);
  }
  return unique.map((block) => ({ block, revision: own(statuses, block)?.revision ?? 0 }));
}

// srcts/core/core.ts
function createCore(parts) {
  const { store, commands, scheduler, loader, blockrOptions } = parts;
  const listeners = /* @__PURE__ */ new Set();
  parts.signal?.addEventListener(
    "abort",
    () => {
      listeners.clear();
    },
    { once: true }
  );
  const outputEvents = {
    subscribe(fn) {
      const wrapped = (event) => {
        fn(event);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    }
  };
  const mounts = createMounts({
    store,
    commands,
    scheduler,
    loader,
    renderers: parts.renderers,
    ...parts.measure === void 0 ? {} : { measure: parts.measure },
    ...parts.signal === void 0 ? {} : { signal: parts.signal },
    emit(event) {
      for (const fn of [...listeners]) {
        try {
          fn(event);
        } catch (error) {
          report(error);
        }
      }
    }
  });
  const api = {
    store,
    commands,
    scheduler: {
      demand: (owner, blocks, options) => scheduler.demand(owner, blocks, options),
      evaluate: (blocks, params, options) => scheduler.evaluate(blocks, params, options),
      invalidate: (id2) => {
        scheduler.invalidate(id2);
      },
      status: (id2) => scheduler.status(id2),
      subscribe: (fn) => scheduler.subscribe(fn)
    },
    blockRegistry: () => store.blockRegistry(),
    options: store.options,
    plugins: {
      register: (def) => store.plugins.register(def)
    },
    lock: parts.lock,
    outputEvents,
    request: (plugin, name3, args = {}, opts = {}) => pluginRequest({ store, scheduler }, plugin, name3, args, opts),
    module: (id2) => loader.module(id2),
    blockrOption: (name3) => blockrOptions.read(name3),
    mountBlock: (el, block) => mounts.mountBlock(el, block),
    mountOutput: (el, block) => mounts.mountOutput(el, block),
    mountOption: (el, option) => mounts.mountOption(el, option)
  };
  let heard = {};
  store.subscribe((change, board) => {
    const before = "opened" in change ? {} : heard;
    if (board.options === heard && !("opened" in change)) return;
    heard = board.options;
    for (const [id2, value2] of Object.entries(board.options)) {
      if (!jsonEqual(own(before, id2), value2)) changed(id2, value2);
    }
  });
  function changed(id2, value2) {
    const def = own(store.options.registered(), id2);
    if (def?.onChange === void 0) return;
    try {
      def.onChange(value2, api);
    } catch (error) {
      report(error);
    }
  }
  return api;
}

// srcts/core/frontends.ts
function loadingNote() {
  const box = document.createElement("div");
  box.setAttribute("role", "status");
  box.className = "bc:p-4 bc:font-sans bc:text-sm bc:text-text-muted";
  box.textContent = "Loading the board\u2026";
  return box;
}
function pageFrontEnds(el) {
  let drawn2;
  let next;
  function end() {
    const current2 = drawn2;
    if (current2 === void 0) return;
    drawn2 = void 0;
    current2.off();
    current2.controller.abort();
    if (current2.cleanup !== void 0) attempt(current2.cleanup);
    el.replaceChildren(loadingNote());
  }
  return {
    async prepare(front, loader) {
      let prepared;
      const { id: id2, source } = front;
      const pkg = packageOf(id2);
      if (source === void 0) {
        prepared = { id: id2, module: id2, error: new Error(`R serves no module ${id2}, the front-end that draws the board.`) };
      } else {
        try {
          const mod = moduleOf(await loader.load(source, pkg), source.module, pkg);
          prepared = { id: id2, module: source.module, render: mod.render.bind(mod) };
        } catch (error) {
          prepared = { id: id2, module: source.module, error };
        }
      }
      end();
      next = prepared;
    },
    async render(core, signal) {
      const prepared = next;
      next = void 0;
      if (prepared === void 0 || signal.aborted) return;
      el.replaceChildren();
      if (prepared.render === void 0) {
        showError(el, `The front-end ${prepared.id} cannot draw the board: ${messageOf(prepared.error)}`);
        report(prepared.error);
        return;
      }
      const controller = new AbortController();
      const ending = () => {
        if (drawn2 === current2) end();
      };
      signal.addEventListener("abort", ending, { once: true });
      const current2 = {
        controller,
        off: () => {
          signal.removeEventListener("abort", ending);
        },
        cleanup: void 0
      };
      drawn2 = current2;
      try {
        const rendered = await prepared.render({ el, core, signal: controller.signal });
        if (typeof rendered !== "function") return;
        if (controller.signal.aborted) attempt(rendered);
        else current2.cleanup = rendered;
      } catch (error) {
        if (controller.signal.aborted) return;
        const why = new ModuleError(prepared.module, packageOf(prepared.id), `failed in render(): ${messageOf(error)}`, {
          cause: error
        });
        showError(el, why.message);
        report(why);
      }
    }
  };
}

// srcts/store/words.ts
var adjectives = [
  "ablaze",
  "able",
  "acid",
  "acidic",
  "active",
  "actual",
  "added",
  "adept",
  "adored",
  "adroit",
  "aerial",
  "agile",
  "agreed",
  "airy",
  "alert",
  "alive",
  "allied",
  "almond",
  "amber",
  "ample",
  "amused",
  "apt",
  "arcane",
  "arctic",
  "ardent",
  "arid",
  "artful",
  "arty",
  "ash",
  "astral",
  "astute",
  "atomic",
  "auburn",
  "avian",
  "awake",
  "aware",
  "azure",
  "balmy",
  "basic",
  "beachy",
  "beige",
  "benign",
  "beryl",
  "best",
  "big",
  "binary",
  "bionic",
  "black",
  "blithe",
  "blocky",
  "blond",
  "blowy",
  "blue",
  "bold",
  "bonny",
  "boreal",
  "bouncy",
  "brainy",
  "brass",
  "brave",
  "breezy",
  "brick",
  "brief",
  "bright",
  "briny",
  "brisk",
  "broad",
  "bronze",
  "brown",
  "bubbly",
  "bumpy",
  "bushy",
  "busy",
  "calm",
  "candid",
  "carbon",
  "caring",
  "catchy",
  "cerise",
  "chalky",
  "chatty",
  "cheeky",
  "cheery",
  "chewy",
  "chic",
  "chief",
  "chilly",
  "choral",
  "chosen",
  "chummy",
  "chunky",
  "citric",
  "civic",
  "civil",
  "classy",
  "clay",
  "clean",
  "clever",
  "cloudy",
  "cocoa",
  "cold",
  "comfy",
  "comic",
  "common",
  "cool",
  "copper",
  "coral",
  "cosmic",
  "cotton",
  "covert",
  "coy",
  "cozy",
  "crafty",
  "craggy",
  "cream",
  "creamy",
  "crispy",
  "cubic",
  "cuddly",
  "curly",
  "curved",
  "cushy",
  "cute",
  "cyan",
  "dainty",
  "dapper",
  "daring",
  "dark",
  "dear",
  "decent",
  "deep",
  "deft",
  "demure",
  "denim",
  "dense",
  "dewy",
  "divine",
  "diving",
  "docile",
  "dogged",
  "dotted",
  "dovish",
  "downy",
  "dreamy",
  "dressy",
  "droll",
  "dry",
  "dusky",
  "dusty",
  "eager",
  "earthy",
  "easy",
  "ebony",
  "edgy",
  "edible",
  "elated",
  "elder",
  "elfin",
  "elite",
  "elvish",
  "equal",
  "even",
  "exact",
  "exotic",
  "expert",
  "extra",
  "fab",
  "fabled",
  "fair",
  "famed",
  "famous",
  "fancy",
  "far",
  "fast",
  "feisty",
  "ferny",
  "fierce",
  "fiery",
  "final",
  "fine",
  "firm",
  "first",
  "fit",
  "fixed",
  "fizzy",
  "flashy",
  "flat",
  "flax",
  "fleecy",
  "flinty",
  "floaty",
  "floppy",
  "floral",
  "fluent",
  "fluffy",
  "fluid",
  "flying",
  "foamy",
  "foggy",
  "fond",
  "forest",
  "formal",
  "frank",
  "free",
  "fresh",
  "frilly",
  "frisky",
  "frizzy",
  "frosty",
  "frothy",
  "frozen",
  "frugal",
  "fruity",
  "full",
  "fun",
  "funny",
  "furry",
  "future",
  "fuzzy",
  "garnet",
  "gauzy",
  "gem",
  "genial",
  "genius",
  "gentle",
  "giant",
  "giddy",
  "gifted",
  "giggly",
  "glad",
  "glass",
  "glassy",
  "glazed",
  "global",
  "glossy",
  "gnarly",
  "gold",
  "golden",
  "good",
  "goodly",
  "goofy",
  "gothic",
  "grainy",
  "grand",
  "grassy",
  "great",
  "green",
  "grey",
  "groovy",
  "grown",
  "gutsy",
  "handy",
  "happy",
  "hasty",
  "hazy",
  "heavy",
  "herbal",
  "herby",
  "heroic",
  "hidden",
  "high",
  "hilly",
  "hip",
  "hollow",
  "homey",
  "honest",
  "huge",
  "humane",
  "humble",
  "humid",
  "hungry",
  "iced",
  "iconic",
  "icy",
  "ideal",
  "idle",
  "immune",
  "impish",
  "indigo",
  "indoor",
  "inky",
  "inland",
  "inner",
  "intact",
  "ionic",
  "ironic",
  "ivory",
  "jade",
  "jagged",
  "jaunty",
  "jazzy",
  "jet",
  "jewel",
  "jolly",
  "jovial",
  "joyful",
  "joyous",
  "juicy",
  "jumpy",
  "junior",
  "just",
  "keen",
  "key",
  "khaki",
  "kind",
  "kindly",
  "kingly",
  "knotty",
  "known",
  "lace",
  "lacy",
  "lapis",
  "large",
  "lavish",
  "lawful",
  "leafy",
  "lean",
  "legal",
  "lemon",
  "lemony",
  "level",
  "light",
  "liked",
  "lilac",
  "lime",
  "limpid",
  "linear",
  "linen",
  "little",
  "live",
  "lively",
  "living",
  "local",
  "long",
  "loved",
  "lovely",
  "loving",
  "low",
  "lowkey",
  "loyal",
  "lucid",
  "lucky",
  "lunar",
  "lupine",
  "lush",
  "magic",
  "main",
  "maize",
  "major",
  "marble",
  "marine",
  "maroon",
  "marshy",
  "mauve",
  "medium",
  "meek",
  "mega",
  "mellow",
  "merry",
  "metal",
  "mica",
  "mid",
  "middle",
  "mighty",
  "mild",
  "milky",
  "mini",
  "minor",
  "minute",
  "misty",
  "mobile",
  "modern",
  "modest",
  "modish",
  "mossy",
  "moving",
  "mutual",
  "myrtle",
  "narrow",
  "naval",
  "navy",
  "near",
  "nearby",
  "neat",
  "neon",
  "new",
  "newsy",
  "next",
  "nice",
  "nickel",
  "nifty",
  "nimble",
  "noble",
  "noir",
  "normal",
  "novel",
  "nylon",
  "oblong",
  "ochre",
  "olive",
  "onyx",
  "opal",
  "open",
  "optic",
  "orange",
  "ornate",
  "outer",
  "oval",
  "paper",
  "papery",
  "peach",
  "peachy",
  "pearl",
  "pearly",
  "pebbly",
  "peppy",
  "perky",
  "pine",
  "pink",
  "placid",
  "plaid",
  "plain",
  "plucky",
  "plush",
  "poetic",
  "polar",
  "polite",
  "postal",
  "potent",
  "pretty",
  "prim",
  "prime",
  "proper",
  "proud",
  "public",
  "pure",
  "purple",
  "quaint",
  "quartz",
  "quick",
  "rainy",
  "random",
  "rapid",
  "rapt",
  "rare",
  "ready",
  "real",
  "recent",
  "red",
  "reedy",
  "regal",
  "rich",
  "right",
  "ripe",
  "ritzy",
  "robust",
  "rocky",
  "roomy",
  "rosy",
  "round",
  "royal",
  "rubber",
  "ruby",
  "runic",
  "rural",
  "russet",
  "rust",
  "rustic",
  "safe",
  "saline",
  "salmon",
  "salted",
  "salty",
  "sand",
  "sandy",
  "sane",
  "sassy",
  "satin",
  "satiny",
  "savvy",
  "secret",
  "secure",
  "sedate",
  "select",
  "sepia",
  "serene",
  "shaded",
  "shaggy",
  "shared",
  "sharp",
  "sheer",
  "shiny",
  "short",
  "shrewd",
  "shy",
  "sienna",
  "silk",
  "silken",
  "silky",
  "silly",
  "silver",
  "simple",
  "single",
  "slate",
  "sleepy",
  "slick",
  "slight",
  "slow",
  "sly",
  "small",
  "smart",
  "smoky",
  "smooth",
  "snappy",
  "snazzy",
  "snowy",
  "snug",
  "soapy",
  "social",
  "soft",
  "solar",
  "sole",
  "solemn",
  "solid",
  "sonic",
  "sound",
  "spare",
  "sparse",
  "speedy",
  "spicy",
  "spiky",
  "spiny",
  "sporty",
  "square",
  "stable",
  "starry",
  "static",
  "steamy",
  "steel",
  "steep",
  "still",
  "stoic",
  "stone",
  "stony",
  "stormy",
  "stout",
  "strict",
  "stripy",
  "strong",
  "suave",
  "subtle",
  "sudden",
  "sudsy",
  "suede",
  "sugar",
  "sugary",
  "sunlit",
  "sunny",
  "super",
  "superb",
  "sure",
  "sweet",
  "swift",
  "swirly",
  "tall",
  "tame",
  "tamed",
  "tan",
  "tangy",
  "tasty",
  "taupe",
  "taut",
  "teal",
  "techy",
  "tender",
  "thick",
  "thin",
  "thorny",
  "tidal",
  "tidy",
  "timely",
  "timid",
  "tin",
  "tinted",
  "tiny",
  "topaz",
  "tough",
  "toy",
  "trendy",
  "tricky",
  "tweed",
  "tweedy",
  "twill",
  "undyed",
  "unique",
  "united",
  "unlit",
  "upbeat",
  "upper",
  "urban",
  "urbane",
  "usable",
  "useful",
  "usual",
  "valid",
  "valued",
  "varied",
  "vast",
  "vellum",
  "velour",
  "velvet",
  "vernal",
  "vinyl",
  "violet",
  "vital",
  "vivid",
  "wacky",
  "warm",
  "wavy",
  "wax",
  "waxy",
  "wee",
  "well",
  "wheat",
  "white",
  "wide",
  "wild",
  "windy",
  "winged",
  "wintry",
  "wise",
  "wispy",
  "witty",
  "wood",
  "wooded",
  "wooden",
  "woodsy",
  "woody",
  "wool",
  "woolly",
  "worthy",
  "yellow",
  "young",
  "yummy",
  "zany",
  "zippy",
  "zircon"
];
var animals = [
  "addax",
  "adder",
  "agouti",
  "alpaca",
  "anole",
  "ant",
  "ape",
  "auk",
  "avocet",
  "baboon",
  "badger",
  "bass",
  "bat",
  "beagle",
  "bear",
  "beaver",
  "bee",
  "beetle",
  "bilby",
  "bird",
  "bison",
  "boa",
  "boar",
  "bobcat",
  "bongo",
  "bonobo",
  "borzoi",
  "bream",
  "bronco",
  "buck",
  "budgie",
  "bug",
  "bull",
  "bunny",
  "burro",
  "caiman",
  "calf",
  "camel",
  "canary",
  "carp",
  "cat",
  "cicada",
  "civet",
  "clam",
  "coati",
  "cobra",
  "cod",
  "collie",
  "colt",
  "conch",
  "condor",
  "coot",
  "cow",
  "coyote",
  "coypu",
  "crab",
  "crane",
  "crow",
  "cub",
  "cuckoo",
  "curlew",
  "cygnet",
  "deer",
  "degu",
  "dikdik",
  "dingo",
  "dipper",
  "dodo",
  "doe",
  "dog",
  "donkey",
  "dove",
  "drake",
  "drongo",
  "duck",
  "dugong",
  "dunlin",
  "eagle",
  "eel",
  "egret",
  "eider",
  "eland",
  "elk",
  "emu",
  "ermine",
  "ewe",
  "falcon",
  "fawn",
  "ferret",
  "finch",
  "fish",
  "fly",
  "foal",
  "fossa",
  "fowl",
  "fox",
  "frog",
  "fulmar",
  "galago",
  "galah",
  "gander",
  "gannet",
  "gaur",
  "gecko",
  "gelada",
  "genet",
  "gerbil",
  "gibbon",
  "gnu",
  "goat",
  "godwit",
  "goose",
  "gopher",
  "grebe",
  "grouse",
  "gull",
  "guppy",
  "hake",
  "hare",
  "hawk",
  "hen",
  "heron",
  "hog",
  "hoopoe",
  "hornet",
  "horse",
  "hound",
  "husky",
  "hyena",
  "ibex",
  "ibis",
  "iguana",
  "impala",
  "jackal",
  "jaguar",
  "jay",
  "jerboa",
  "joey",
  "junco",
  "kakapo",
  "kite",
  "kitten",
  "kiwi",
  "koala",
  "koi",
  "krill",
  "kudu",
  "lamb",
  "langur",
  "lark",
  "lemur",
  "liger",
  "limpet",
  "lion",
  "lizard",
  "llama",
  "loon",
  "loris",
  "lynx",
  "macaw",
  "magpie",
  "mamba",
  "manta",
  "mantis",
  "mare",
  "marlin",
  "marmot",
  "marten",
  "martin",
  "mayfly",
  "merlin",
  "mink",
  "minnow",
  "mole",
  "monkey",
  "moose",
  "moray",
  "morpho",
  "moth",
  "mouse",
  "mule",
  "mullet",
  "muskox",
  "mussel",
  "myna",
  "nene",
  "newt",
  "numbat",
  "nutria",
  "nyala",
  "ocelot",
  "okapi",
  "orca",
  "oriole",
  "oryx",
  "osprey",
  "otter",
  "owl",
  "ox",
  "oyster",
  "paca",
  "panda",
  "parrot",
  "perch",
  "phoebe",
  "pig",
  "pigeon",
  "piglet",
  "pika",
  "pike",
  "pipit",
  "plover",
  "pony",
  "poodle",
  "possum",
  "prawn",
  "puffer",
  "puffin",
  "pug",
  "puma",
  "puppy",
  "python",
  "quagga",
  "quail",
  "quokka",
  "quoll",
  "rabbit",
  "ram",
  "raven",
  "ray",
  "rhea",
  "rhino",
  "robin",
  "rook",
  "sable",
  "saiga",
  "salmon",
  "scarab",
  "seal",
  "serval",
  "shark",
  "sheep",
  "shrew",
  "shrike",
  "shrimp",
  "siskin",
  "skink",
  "skua",
  "skunk",
  "sloth",
  "snail",
  "snake",
  "snipe",
  "spider",
  "squid",
  "stag",
  "steed",
  "steer",
  "stilt",
  "stoat",
  "stork",
  "swan",
  "swift",
  "takin",
  "tapir",
  "teal",
  "tenrec",
  "tern",
  "thrush",
  "tiger",
  "toad",
  "topi",
  "toucan",
  "towhee",
  "trout",
  "tuna",
  "turaco",
  "turkey",
  "turtle",
  "urchin",
  "vervet",
  "vicuna",
  "viper",
  "vole",
  "walrus",
  "wasp",
  "weasel",
  "whale",
  "wisent",
  "wolf",
  "wombat",
  "wrasse",
  "wren",
  "yak",
  "zebra",
  "zebu"
];

// srcts/store/ids.ts
var RAND_IDS = [
  "adjective_animal",
  "adjective_adjective_animal",
  "letters_6",
  "letters_8",
  "letters_12",
  "bytes_4",
  "bytes_8",
  "uuid"
];
function isRandId(value2) {
  return typeof value2 === "string" && RAND_IDS.includes(value2);
}
function isId(id2) {
  return typeof id2 === "string" && id2 !== "" && !/^\d+$/.test(id2);
}
function mintId(taken, randId = "adjective_animal") {
  const draw = draws[randId];
  for (let i2 = 0; i2 < 100; i2++) {
    const id3 = draw();
    if (isId(id3) && !taken(id3)) return id3;
  }
  const id2 = draw();
  let n2 = 2;
  while (taken(`${id2}_${String(n2)}`)) n2++;
  return `${id2}_${String(n2)}`;
}
function sentenceCase(id2) {
  const words = id2.replace(/([A-Z])/g, " $1").replace(/[_\-.]/g, " ").trim().split(" ");
  const [first = "", ...rest] = words;
  return [
    first.charAt(0).toUpperCase() + first.slice(1).toLowerCase(),
    ...rest.map((word) => word.toLowerCase())
  ].join(" ");
}
var draws = {
  adjective_animal: () => `${pick(adjectives)}_${pick(animals)}`,
  adjective_adjective_animal: () => {
    const [first, second] = pickTwo(adjectives);
    return `${first}_${second}_${pick(animals)}`;
  },
  letters_6: () => randomLetters(6),
  letters_8: () => randomLetters(8),
  letters_12: () => randomLetters(12),
  bytes_4: () => hex2(randomBytes(4)),
  bytes_8: () => hex2(randomBytes(8)),
  uuid: randomUuid
};
function randomInt(n2) {
  return Math.floor(Math.random() * n2);
}
function pick(words) {
  return words[randomInt(words.length)] ?? "";
}
function pickTwo(words) {
  const i2 = randomInt(words.length);
  let j2 = randomInt(words.length - 1);
  if (j2 >= i2) j2++;
  return [words[i2] ?? "", words[j2] ?? ""];
}
function randomLetters(n2) {
  return Array.from({ length: n2 }, () => String.fromCharCode(97 + randomInt(26))).join("");
}
function randomBytes(n2) {
  return Array.from({ length: n2 }, () => randomInt(256));
}
function hex2(bytes) {
  return bytes.map((byte) => byte.toString(16).padStart(2, "0")).join("");
}
function randomUuid() {
  const bytes = randomBytes(16);
  bytes[6] = (bytes[6] ?? 0) & 15 | 64;
  bytes[8] = (bytes[8] ?? 0) & 63 | 128;
  const text = hex2(bytes);
  return [text.slice(0, 8), text.slice(8, 12), text.slice(12, 16), text.slice(16, 20), text.slice(20)].join("-");
}

// srcts/store/plugins.ts
var ROLES = ["frontend", "storage"];
function inRole(plugins, classOf2, role) {
  return Object.entries(plugins).filter(([, plugin]) => classOf2(plugin.class)?.[role] !== void 0).map(([id2]) => id2);
}
function defaultState(def) {
  return frozenCopy(def.state === void 0 ? {} : def.state.default);
}
function keptPlugins(board, opening2, classOf2) {
  return Object.fromEntries(
    Object.entries(board).filter(
      (entry) => isObject(entry[1]) && typeof entry[1].class === "string" && own(opening2, entry[0])?.class === entry[1].class && classOf2(entry[1].class) !== void 0
    )
  );
}
function appPlugins(board, opening2, classOf2) {
  const kept = keptPlugins(board, opening2, classOf2);
  const plugins = {};
  for (const [id2, plugin] of Object.entries(opening2)) {
    const def = classOf2(plugin.class);
    if (def === void 0) continue;
    plugins[id2] = own(kept, id2) ?? { class: plugin.class, name: plugin.name, state: defaultState(def) };
  }
  return plugins;
}

// srcts/store/check.ts
var HEX_COLOR = "^#[0-9a-fA-F]{6}$";
var hexColor = new RegExp(HEX_COLOR);
function check(board, registry, registrations, changed) {
  const failures = [];
  if (!isId(board.id)) {
    failures.push(`The board's id "${board.id}" is empty or made of digits alone.`);
  }
  if (typeof board.name !== "string") {
    failures.push("The board's name is no string.");
  }
  checkBlocks(board, registry, registrations, changed, failures);
  checkLinks(board, registry, changed, failures);
  checkStacks(board, changed, failures);
  checkValues(board, registrations, changed, failures);
  checkPlugins(board, registrations, changed, failures);
  return failures;
}
function ids(what, record, failures) {
  for (const id2 of Object.keys(record)) {
    if (!isId(id2)) failures.push(`The ${what} id "${id2}" is empty or made of digits alone.`);
  }
}
function checkBlocks(board, registry, registrations, changed, failures) {
  ids("block", board.blocks, failures);
  for (const [id2, block] of Object.entries(board.blocks)) {
    if (changed !== void 0 && !changed.blocks.has(id2)) continue;
    const fields = shape(block, { registry_id: "string", state: "object", name: "string" });
    if (fields.length > 0) {
      failures.push(`The block "${id2}" ${fields.join(", ")}.`);
      continue;
    }
    if (own(registry, block.registry_id) === void 0) {
      failures.push(
        `The block "${id2}" is of the class "${block.registry_id}", which the block registry lacks.`
      );
    }
  }
}
function checkLinks(board, registry, changed, failures) {
  ids("link", board.links, failures);
  const fed = /* @__PURE__ */ new Map();
  for (const [id2, link] of Object.entries(board.links)) {
    if (changed === void 0 || changed.links.has(id2)) {
      const fields = shape(link, { from: "string", to: "string", input: "string" });
      if (fields.length > 0) {
        failures.push(`The link "${id2}" ${fields.join(", ")}.`);
        continue;
      }
    }
    const from = own(board.blocks, link.from);
    const to = own(board.blocks, link.to);
    if (from === void 0) {
      failures.push(`The link "${id2}" comes from block "${link.from}", which the board lacks.`);
    }
    if (to === void 0) {
      failures.push(`The link "${id2}" goes to block "${link.to}", which the board lacks.`);
      continue;
    }
    const inputs = own(registry, to.registry_id)?.inputs;
    if (inputs === void 0 || inputs.includes("...")) continue;
    if (!inputs.includes(link.input)) {
      failures.push(
        `The link "${id2}" feeds the input "${link.input}" of block "${link.to}", whose class has no such input.`
      );
      continue;
    }
    const at = JSON.stringify([link.to, link.input]);
    fed.set(at, [...fed.get(at) ?? [], id2]);
  }
  for (const [at, links] of fed) {
    if (links.length < 2) continue;
    const [to, input2] = JSON.parse(at);
    failures.push(
      `The input "${input2}" of block "${to}" takes the links ${quoted(links)}, where it takes one.`
    );
  }
  const cycle = graphOf(board).cycle();
  if (cycle !== void 0) {
    failures.push(`The links form a cycle through the blocks ${quoted(cycle)}.`);
  }
}
function checkStacks(board, changed, failures) {
  ids("stack", board.stacks, failures);
  const holder = /* @__PURE__ */ new Map();
  for (const [id2, stack] of Object.entries(board.stacks)) {
    if (changed === void 0 || changed.stacks.has(id2)) {
      const fields = shape(stack, { name: "string", blocks: "array", color: "string or null" });
      if (fields.length > 0) {
        failures.push(`The stack "${id2}" ${fields.join(", ")}.`);
        continue;
      }
      if (stack.color !== null && !hexColor.test(stack.color)) {
        failures.push(
          `The stack "${id2}" has the colour "${stack.color}", which is no hex colour of six digits, such as "#66c2a5".`
        );
      }
    }
    for (const block of stack.blocks) {
      if (own(board.blocks, block) === void 0) {
        failures.push(`The stack "${id2}" holds block "${block}", which the board lacks.`);
        continue;
      }
      const other = holder.get(block);
      if (other === id2) {
        failures.push(`The stack "${id2}" holds block "${block}" twice.`);
      } else if (other !== void 0) {
        failures.push(`The block "${block}" sits in the stacks "${other}" and "${id2}", where it sits in one.`);
      }
      holder.set(block, id2);
    }
  }
}
function checkValues(board, registrations, changed, failures) {
  for (const [id2, value2] of Object.entries(board.options)) {
    if (changed !== void 0 && !changed.options.has(id2)) continue;
    const option = registrations.option(id2);
    if (option === void 0) {
      failures.push(`The board holds a value of the board option "${id2}", which no package registered.`);
      continue;
    }
    for (const failure of option.validate(value2)) {
      failures.push(`The value of the board option "${id2}" ${failure}`);
    }
  }
}
function checkPlugins(board, registrations, changed, failures) {
  ids("plugin", board.plugins, failures);
  for (const [id2, plugin] of Object.entries(board.plugins)) {
    if (changed !== void 0 && !changed.plugins.has(id2)) continue;
    const fields = shape(plugin, { class: "string", name: "string", state: "value" });
    if (fields.length > 0) {
      failures.push(`The plugin "${id2}" ${fields.join(", ")}.`);
      continue;
    }
    const cls = registrations.plugin(plugin.class);
    if (cls === void 0) {
      failures.push(`The plugin "${id2}" is of the class "${plugin.class}", which no package registered.`);
      continue;
    }
    for (const failure of cls.validate(plugin.state)) {
      failures.push(`The state of the plugin "${id2}" ${failure}`);
    }
  }
  const classOf2 = (id2) => registrations.plugin(id2)?.def;
  for (const role of ROLES) {
    const held = inRole(board.plugins, classOf2, role);
    if (held.length > 1) {
      failures.push(`The board holds the ${ROLE_NAMES[role]} ${quoted(held)}, where it holds one at most.`);
    }
  }
}
var ROLE_NAMES = {
  frontend: "frontends",
  storage: "storage plugins"
};
function shape(value2, fields) {
  if (!isObject(value2)) return ["is no object"];
  const problems = [];
  for (const [field, kind] of Object.entries(fields)) {
    if (!fits(value2[field], kind)) problems.push(`has no ${kind} as its ${field}`);
  }
  for (const field of Object.keys(value2)) {
    if (!Object.hasOwn(fields, field)) problems.push(`holds "${field}", which core does not keep`);
  }
  return problems;
}
function fits(x2, kind) {
  switch (kind) {
    case "value":
      return x2 !== void 0;
    case "array":
      return Array.isArray(x2);
    case "object":
      return isObject(x2);
    case "string or null":
      return x2 === null || typeof x2 === "string";
    case "string":
      return typeof x2 === "string";
  }
}

// srcts/store/patch.ts
function pointer(path) {
  if (!path.startsWith("/")) return void 0;
  const parts = path.slice(1).split("/");
  if (parts.some((part) => /~(?![01])/.test(part))) return void 0;
  return parts.map((part) => part.replaceAll("~1", "/").replaceAll("~0", "~"));
}
function pluginOf(path) {
  const [top, plugin] = pointer(path) ?? [];
  return top === "plugins" ? plugin : void 0;
}
function pathOf(parts) {
  return parts.map((part) => `/${part.replaceAll("~", "~0").replaceAll("/", "~1")}`).join("");
}
function overlap(a2, b2) {
  const n2 = Math.min(a2.length, b2.length);
  return a2.slice(0, n2).every((part, i2) => part === b2[i2]);
}
function malformed(patch2) {
  if (!isObject(patch2)) return ["is no object"];
  const problems = [];
  const { op, path, edit } = patch2;
  if (op !== "add" && op !== "remove" && op !== "replace") {
    problems.push(`has the op ${JSON.stringify(op)}, where it takes "add", "remove" or "replace"`);
  }
  if (typeof path !== "string" || pointer(path) === void 0) {
    problems.push(`has the path ${JSON.stringify(path)}, which is no JSON Pointer below the board's top`);
  }
  if ((op === "add" || op === "replace") && patch2.value === void 0) {
    problems.push("has no value");
  }
  if (edit !== void 0 && (typeof edit !== "number" || !Number.isInteger(edit))) {
    problems.push(`has the edit id ${JSON.stringify(edit)}, which is no whole number`);
  }
  for (const field of Object.keys(patch2)) {
    if (!["op", "path", "value", "edit"].includes(field)) {
      problems.push(`holds "${field}", which a patch does not keep`);
    }
  }
  return problems;
}
function applyPatch(board, patch2, parts, place) {
  const result = step(board, parts, patch2, [], place);
  if (typeof result === "string") return { failure: result };
  return { ...result, next: result.next };
}
function step(at, parts, patch2, done, place) {
  const [part = "", ...rest] = parts;
  if (Array.isArray(at)) {
    const items = at;
    const last = rest.length === 0;
    const index = last && part === "-" && patch2.op === "add" ? items.length : arrayIndex(part);
    const bound = last && patch2.op === "add" ? items.length : items.length - 1;
    if (index === void 0 || index > bound) {
      return `finds no element "${part}" in the array at "${pathOf(done)}"`;
    }
    const here2 = pathOf([...done, String(index)]);
    if (!last) {
      const inner = step(items[index], rest, patch2, [...done, String(index)], place);
      if (typeof inner === "string") return inner;
      if (inner.next === items[index]) return { ...inner, next: at };
      return { ...inner, next: spliced(items, index, 1, inner.next) };
    }
    const old2 = items[index];
    switch (patch2.op) {
      case "add":
        return {
          next: spliced(items, index, 0, patch2.value),
          inverse: { op: "remove", path: here2 }
        };
      case "remove":
        return {
          next: spliced(items, index, 1),
          inverse: { op: "add", path: here2, value: old2 }
        };
      case "replace":
        if (jsonEqual(old2, patch2.value)) return { next: at, inverse: void 0 };
        return {
          next: spliced(items, index, 1, patch2.value),
          inverse: { op: "replace", path: here2, value: old2 }
        };
    }
  }
  if (!isObject(at)) {
    return `finds neither an object nor an array at "${pathOf(done)}"`;
  }
  const has = Object.hasOwn(at, part);
  const old = has ? at[part] : void 0;
  const here = pathOf([...done, part]);
  if (rest.length > 0) {
    if (!has) return `finds no "${part}" in "${pathOf(done)}"`;
    const inner = step(old, rest, patch2, [...done, part], place);
    if (typeof inner === "string") return inner;
    if (inner.next === old) return { ...inner, next: at };
    return { ...inner, next: { ...at, [part]: inner.next } };
  }
  switch (patch2.op) {
    case "add":
    case "replace":
      if (!has && patch2.op === "replace") return `finds no "${part}" in "${pathOf(done)}" to replace`;
      if (!has) {
        return { next: inserted(at, part, patch2.value, place), inverse: { op: "remove", path: here } };
      }
      if (jsonEqual(old, patch2.value)) return { next: at, inverse: void 0 };
      return {
        next: { ...at, [part]: patch2.value },
        inverse: { op: "replace", path: here, value: old }
      };
    case "remove": {
      if (!has) return `finds no "${part}" in "${pathOf(done)}" to remove`;
      const members = Object.keys(at);
      return {
        next: without(at, [part]),
        inverse: { op: "add", path: here, value: old },
        place: members[members.indexOf(part) + 1] ?? null
      };
    }
  }
}
function inserted(record, key, value2, before) {
  if (typeof before !== "string" || !Object.hasOwn(record, before)) {
    return { ...record, [key]: value2 };
  }
  const members = Object.entries(record);
  members.splice(members.findIndex(([name3]) => name3 === before), 0, [key, value2]);
  return Object.fromEntries(members);
}
function spliced(items, index, count, ...added) {
  const copy = [...items];
  copy.splice(index, count, ...added);
  return copy;
}
function arrayIndex(part) {
  return /^(0|[1-9]\d*)$/.test(part) ? Number(part) : void 0;
}

// srcts/store/commit.ts
function target(of) {
  return "block" in of ? JSON.stringify(["block", of.block]) : JSON.stringify(["plugin", of.plugin]);
}
function commit(board, delta, places = {}) {
  const failures = [];
  const targets = /* @__PURE__ */ new Set();
  const blocks = collection(
    (id2) => `block "${id2}"`,
    board.blocks,
    delta.blocks,
    (old, mod, id2) => modifyBlock(old, mod, id2, failures),
    places.blocks,
    void 0,
    failures
  );
  for (const id2 of blocks.touched) targets.add(target({ block: id2 }));
  const links = collection(
    (id2) => `link "${id2}"`,
    board.links,
    delta.links,
    modifyFields,
    places.links,
    delta.links?.before,
    failures
  );
  const stacks = collection(
    (id2) => `stack "${id2}"`,
    board.stacks,
    delta.stacks,
    modifyFields,
    places.stacks,
    void 0,
    failures
  );
  const options = values(
    board.options,
    delta.options,
    (id2) => `the board option "${id2}"`,
    failures
  );
  let next = {
    ...board,
    name: delta.name ?? board.name,
    blocks: blocks.next,
    links: links.next,
    stacks: stacks.next,
    options: options.next
  };
  const patched = patch(next, delta.patches ?? [], places.patches ?? {}, failures);
  for (const plugin of patched.plugins) targets.add(target({ plugin }));
  next = patched.board;
  if (failures.length > 0) return { failures };
  const inverse = {
    ...empty(blocks.inverse) ? {} : { blocks: blocks.inverse },
    ...empty(links.inverse) ? {} : { links: links.inverse },
    ...empty(stacks.inverse) ? {} : { stacks: stacks.inverse },
    ...delta.name === void 0 || delta.name === board.name ? {} : { name: board.name },
    ...patched.inverse.length === 0 ? {} : { patches: patched.inverse }
  };
  return {
    board: next,
    inverse: {
      delta: inverse,
      places: {
        blocks: blocks.places,
        links: links.places,
        stacks: stacks.places,
        patches: patched.places
      }
    },
    changed: {
      blocks: new Set(blocks.changed),
      links: new Set(links.changed),
      stacks: new Set(stacks.changed),
      options: options.changed,
      plugins: patched.plugins,
      targets
    }
  };
}
function patch(board, patches2, placed, failures) {
  let next = board;
  const inverse = [];
  const places = {};
  const plugins = /* @__PURE__ */ new Set();
  for (const [i2, given] of patches2.entries()) {
    const name3 = `The patch ${String(i2 + 1)}`;
    const problems = malformed(given);
    if (problems.length > 0) {
      failures.push(`${name3} ${problems.join(", ")}.`);
      continue;
    }
    const parts = pointer(given.path) ?? [];
    const misplaced2 = outside(parts, given.op);
    if (misplaced2 !== void 0) {
      failures.push(`${name3}, ${given.op} at "${given.path}", ${misplaced2}.`);
      continue;
    }
    const place = given.op === "add" ? own(placed, given.path) : void 0;
    const applied = applyPatch(next, given, parts, place);
    if ("failure" in applied) {
      failures.push(`${name3}, ${given.op} at "${given.path}", ${applied.failure}.`);
      continue;
    }
    next = applied.next;
    if (applied.inverse === void 0) continue;
    inverse.unshift(applied.inverse);
    if (applied.place !== void 0) places[applied.inverse.path] = applied.place;
    plugins.add(parts[1] ?? "");
  }
  return { board: next, inverse, places, plugins };
}
function outside(parts, op) {
  const [top, plugin, part] = parts;
  if (top !== "plugins") return `changes "${top ?? ""}", where a patch changes the board's plugins alone`;
  if (plugin === void 0) return "changes the board's plugins as a whole, where a patch changes one plugin";
  if (part === void 0) {
    return `changes the plugin "${plugin}" as a whole, where a patch changes a plugin's name or its state, since the app's opening board sets the plugins that a board holds`;
  }
  if (part !== "name" && part !== "state") {
    return `changes "${part}" of the plugin "${plugin}", where a patch below a plugin changes its name or its state`;
  }
  if (parts.length === 3 && op === "remove") {
    return `removes the ${part} of the plugin "${plugin}", which a plugin always holds`;
  }
  return void 0;
}
function collection(name3, current2, part, modify, places, before, failures) {
  if (part === void 0) {
    return { next: current2, inverse: {}, places: {}, changed: [], touched: [] };
  }
  const order = Object.keys(current2);
  const removed = /* @__PURE__ */ new Set();
  for (const id2 of part.rm ?? []) {
    if (Object.hasOwn(current2, id2)) removed.add(id2);
    else failures.push(`The board holds no ${name3(id2)} to remove.`);
  }
  const readd = {};
  const readdPlaces = {};
  for (const id2 of removed) {
    readd[id2] = current2[id2];
    const after = order.slice(order.indexOf(id2) + 1);
    readdPlaces[id2] = after.find((other) => !removed.has(other)) ?? null;
  }
  const kept = order.filter((id2) => !removed.has(id2)).map((id2) => [id2, current2[id2]]);
  const added = [];
  for (const [id2, value2] of Object.entries(part.add ?? {})) {
    if (kept.some(([other]) => other === id2)) {
      failures.push(`The board holds ${name3(id2)} already.`);
      continue;
    }
    const named2 = own(before ?? {}, id2);
    const successor = named2 === void 0 ? own(places ?? {}, id2) ?? null : removed.has(named2) ? own(readdPlaces, named2) ?? null : named2;
    const at = successor === null ? -1 : kept.findIndex(([other]) => other === successor);
    if (at < 0 && successor !== null && named2 !== void 0) {
      failures.push(
        `The board holds no ${name3(successor)} for ${name3(id2)} to go before.`
      );
      continue;
    }
    if (at < 0) kept.push([id2, value2]);
    else kept.splice(at, 0, [id2, value2]);
    added.push(id2);
  }
  const remod = {};
  const modded = [];
  for (const [id2, mod] of Object.entries(part.mod ?? {})) {
    const at = kept.findIndex(([other]) => other === id2);
    const entry = kept[at];
    if (entry === void 0) {
      failures.push(`The board holds no ${name3(id2)} to change.`);
      continue;
    }
    const { value: value2, inverse } = modify(entry[1], mod, id2);
    if (Object.keys(inverse).length === 0) continue;
    kept[at] = [id2, value2];
    if (!added.includes(id2)) remod[id2] = inverse;
    modded.push(id2);
  }
  return {
    next: Object.fromEntries(kept),
    inverse: {
      ...added.length === 0 ? {} : { rm: added },
      ...removed.size === 0 ? {} : { add: readd },
      ...Object.keys(remod).length === 0 ? {} : { mod: remod }
    },
    places: readdPlaces,
    changed: [.../* @__PURE__ */ new Set([...added, ...modded])],
    touched: [.../* @__PURE__ */ new Set([...removed, ...added, ...modded])]
  };
}
function empty(part) {
  return Object.keys(part).length === 0;
}
function changes(current2, set, unset, both, failures) {
  const setting2 = Object.entries(set).filter(
    ([name3, value2]) => !jsonEqual(own(current2, name3), value2)
  );
  const restore = {};
  const remove2 = [];
  const removing = [];
  for (const name3 of unset) {
    if (Object.hasOwn(set, name3)) failures.push(both(name3));
    const was = own(current2, name3);
    if (was === void 0) continue;
    removing.push(name3);
    restore[name3] = was;
  }
  for (const [name3] of setting2) {
    const was = own(current2, name3);
    if (was === void 0) remove2.push(name3);
    else restore[name3] = was;
  }
  return {
    next: setting2.length === 0 && removing.length === 0 ? current2 : { ...without(current2, removing), ...Object.fromEntries(setting2) },
    set: setting2.map(([name3]) => name3),
    unset: removing,
    restore,
    remove: remove2
  };
}
function values(current2, change, what, failures) {
  if (change === void 0) {
    return { next: current2, changed: /* @__PURE__ */ new Set() };
  }
  const result = changes(
    current2,
    change.set ?? {},
    change.unset ?? [],
    (name3) => `The change both sets and unsets ${what(name3)}.`,
    failures
  );
  return {
    next: result.next,
    changed: /* @__PURE__ */ new Set([...result.set, ...result.unset])
  };
}
function modifyBlock(old, mod, id2, failures) {
  const state2 = changes(
    old.state,
    mod.state ?? {},
    mod.unset ?? [],
    (arg) => `The change to block "${id2}" both sets and unsets "${arg}".`,
    failures
  );
  const name3 = mod.name === old.name ? void 0 : mod.name;
  return {
    value: { ...old, state: state2.next, name: name3 ?? old.name },
    inverse: {
      ...Object.keys(state2.restore).length === 0 ? {} : { state: state2.restore },
      ...state2.remove.length === 0 ? {} : { unset: state2.remove },
      ...name3 === void 0 ? {} : { name: old.name }
    }
  };
}
function modifyFields(old, mod) {
  const inverse = {};
  const changed = {};
  for (const field of Object.keys(mod)) {
    if (jsonEqual(old[field], mod[field])) continue;
    inverse[field] = old[field];
    changed[field] = mod[field];
  }
  return { value: { ...old, ...changed }, inverse };
}

// srcts/store/delta.ts
function isDeltas(delta) {
  return Array.isArray(delta);
}

// srcts/store/error.ts
var StoreError = class extends Error {
  failures;
  constructor(message, failures) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"));
    this.name = "StoreError";
    this.failures = failures;
  }
};

// srcts/store/expand.ts
var ROUNDS = 10;
function expand(delta, board, registrations) {
  let expanded = delta;
  for (let round = 0; round < ROUNDS; round++) {
    const adding = [];
    const failures = [];
    const core = expandCore(expanded, board);
    const grew = core !== expanded;
    expanded = core;
    for (const [id2, plugin] of Object.entries(board.plugins)) {
      const def = registrations.plugin(plugin.class)?.def;
      if (def?.update === void 0) continue;
      let patches2;
      try {
        patches2 = def.update(expanded, board, id2);
      } catch (error) {
        failures.push(`The plugin class "${def.id}" failed to update the delta for the plugin "${id2}": ${messageOf(error)}`);
        continue;
      }
      const wrong = misplaced(def.id, id2, patches2);
      if (wrong.length > 0) {
        failures.push(...wrong);
        continue;
      }
      const merged = merge(expanded, delta, patches2);
      if (merged !== expanded) adding.push(id2);
      expanded = merged;
    }
    if (failures.length > 0) return { failures };
    if (!grew && adding.length === 0) return expanded;
    if (round === ROUNDS - 1) {
      return {
        failures: [
          `The delta kept expanding after ${String(ROUNDS)} rounds, through the update of the plugin${adding.length > 1 ? "s" : ""} ${quoted(adding)}.`
        ]
      };
    }
  }
  return expanded;
}
function expandCore(delta, board) {
  const gone = new Set(delta.blocks?.rm ?? []);
  if (gone.size === 0) return delta;
  const linksRm = delta.links?.rm ?? [];
  const incident = Object.entries(board.links).filter(([id2, link]) => (gone.has(link.from) || gone.has(link.to)) && !linksRm.includes(id2)).map(([id2]) => id2);
  const stacksRm = delta.stacks?.rm ?? [];
  const stacksMod = delta.stacks?.mod ?? {};
  const placed = {};
  for (const [id2, stack] of Object.entries(board.stacks)) {
    if (stacksRm.includes(id2)) continue;
    const mod = own(stacksMod, id2);
    const blocks = mod?.blocks ?? stack.blocks;
    if (blocks.some((block) => gone.has(block))) {
      placed[id2] = { ...mod, blocks: blocks.filter((block) => !gone.has(block)) };
    }
  }
  if (incident.length === 0 && Object.keys(placed).length === 0) return delta;
  return {
    ...delta,
    ...incident.length === 0 ? {} : { links: { ...delta.links, rm: [...linksRm, ...incident] } },
    ...Object.keys(placed).length === 0 ? {} : { stacks: { ...delta.stacks, mod: { ...stacksMod, ...placed } } }
  };
}
function misplaced(cls, id2, patches2) {
  const of = `The plugin class "${cls}" updated the delta for the plugin "${id2}"`;
  if (!Array.isArray(patches2)) return [`${of} with no array of patches.`];
  const failures = [];
  for (const patch2 of patches2) {
    const problems = malformed(patch2);
    if (problems.length > 0) {
      failures.push(`${of} with a patch that ${problems.join(", ")}.`);
      continue;
    }
    const { path } = patch2;
    const [top, plugin, part] = pointer(path) ?? [];
    if (top !== "plugins" || plugin !== id2 || part !== "state") {
      failures.push(`${of} with a patch at "${path}", outside that plugin's state.`);
    }
  }
  return failures;
}
function merge(delta, given, patches2) {
  const held = delta.patches ?? [];
  const fixed2 = (given.patches ?? []).map(({ path }) => pointer(path) ?? []);
  const added = [];
  for (const patch2 of patches2) {
    const parts = pointer(patch2.path) ?? [];
    if ([...held, ...added].some((other) => same(other, patch2))) continue;
    if (fixed2.some((path) => overlap(path, parts))) continue;
    added.push(patch2);
  }
  if (added.length === 0) return delta;
  return { ...delta, patches: [...held, ...added] };
}
function same(a2, b2) {
  return a2.op === b2.op && a2.path === b2.path && jsonEqual(a2.value, b2.value);
}

// srcts/store/undo.ts
function fit(delta, board) {
  const blocks = collection2(board.blocks, delta.blocks);
  const kept = /* @__PURE__ */ new Set([
    ...Object.keys(board.blocks).filter((id2) => !(blocks.rm ?? []).includes(id2)),
    ...Object.keys(blocks.add ?? {})
  ]);
  const holds = (id2) => kept.has(id2);
  const links = collection2(
    board.links,
    delta.links,
    (link) => (link.from === void 0 || holds(link.from)) && (link.to === void 0 || holds(link.to))
  );
  const before = Object.fromEntries(
    Object.entries(delta.links?.before ?? {}).filter(([id2]) => Object.hasOwn(links.add ?? {}, id2))
  );
  const stacks = collection2(board.stacks, delta.stacks);
  const placed = (stack) => stack.blocks === void 0 ? stack : { ...stack, blocks: stack.blocks.filter(holds) };
  return prune({
    blocks,
    links: { ...links, ...Object.keys(before).length === 0 ? {} : { before } },
    stacks: {
      ...stacks.rm === void 0 ? {} : { rm: stacks.rm },
      ...stacks.add === void 0 ? {} : { add: Object.fromEntries(Object.entries(stacks.add).map(([id2, s2]) => [id2, placed(s2)])) },
      ...stacks.mod === void 0 ? {} : { mod: Object.fromEntries(Object.entries(stacks.mod).map(([id2, s2]) => [id2, placed(s2)])) }
    },
    ...delta.options === void 0 ? {} : { options: delta.options },
    ...delta.name === void 0 ? {} : { name: delta.name },
    patches: patches(delta.patches ?? [], board)
  });
}
function patches(given, board) {
  let at = board;
  const fitted = [];
  for (const patch2 of given) {
    const parts = pointer(patch2.path) ?? [];
    if (parts[0] !== "plugins") continue;
    const applied = applyPatch(at, patch2, parts);
    if ("failure" in applied) continue;
    at = applied.next;
    fitted.push(patch2);
  }
  return fitted;
}
function collection2(current2, part, keep = () => true) {
  if (part === void 0) return {};
  const rm = (part.rm ?? []).filter((id2) => Object.hasOwn(current2, id2));
  const there = (id2) => Object.hasOwn(current2, id2) && !rm.includes(id2);
  const add = Object.entries(part.add ?? {}).filter(([id2, entry]) => !there(id2) && keep(entry));
  const mod = Object.entries(part.mod ?? {}).filter(([id2, change]) => there(id2) && keep(change));
  return {
    ...rm.length === 0 ? {} : { rm },
    ...add.length === 0 ? {} : { add: Object.fromEntries(add) },
    ...mod.length === 0 ? {} : { mod: Object.fromEntries(mod) }
  };
}
function prune(delta) {
  const parts = Object.entries(delta);
  return Object.fromEntries(
    parts.filter(
      ([, part]) => Array.isArray(part) ? part.length > 0 : !isObject(part) || Object.keys(part).length > 0
    )
  );
}
function isEmpty(delta) {
  return Object.keys(prune(delta)).length === 0;
}

// srcts/store/registrations.ts
var BASIC_OPTIONS = ["show_conditions", "dark_mode"];
function createRegistrations() {
  const classes = y(/* @__PURE__ */ new Map());
  const options = y([]);
  const migrations = y(/* @__PURE__ */ new Map());
  const optionIndex = O(
    () => new Map(options.value.map((option) => [option.def.id, option]))
  );
  const classDefs = O(
    () => Object.fromEntries([...classes.value].map(([id2, cls]) => [id2, cls.def]))
  );
  const optionDefs = O(
    () => Object.fromEntries(options.value.map((option) => [option.def.id, option.def]))
  );
  function checkInitial(what, validate2, initial, field) {
    if (initial === void 0) return;
    const failures = validate2(initial);
    if (failures.length > 0) {
      throw new Error(`The ${field} of ${what} ${failures.join(" ")}`);
    }
  }
  function registerPlugin(def) {
    const { id: id2 } = def;
    if (typeof id2 !== "string" || !isPluginClassId(id2)) {
      throw new Error(
        `The plugin class id ${JSON.stringify(id2)} is not named as R names a plugin class, "{pkg}-{name}_plugin".`
      );
    }
    if (classes.value.has(id2)) {
      throw new Error(`The plugin class "${id2}" is registered already.`);
    }
    const given = def;
    for (const [member, article] of [["update", "an"], ["migrate", "a"], ["start", "a"]]) {
      if (given[member] !== void 0 && typeof given[member] !== "function") {
        throw new Error(`The plugin class "${id2}" has ${article} ${member} that is no function.`);
      }
    }
    for (const member of ["ui", "frontend"]) {
      if (given[member] !== void 0 && (typeof given[member] !== "string" || given[member] === "")) {
        throw new Error(`The plugin class "${id2}" has a ${member} that is no module's id.`);
      }
    }
    const storage = given.storage;
    if (storage !== void 0 && (!isObject(storage) || typeof storage.save !== "function" || typeof storage.open !== "function")) {
      throw new Error(`The plugin class "${id2}" has a storage without save() and open() functions.`);
    }
    if (given.options !== void 0 && (!Array.isArray(given.options) || !given.options.every((option) => typeof option === "string"))) {
      throw new Error(`The plugin class "${id2}" requires board options that are no array of their ids.`);
    }
    const what = `the state of the plugin class "${id2}"`;
    let validate2 = () => [];
    const state2 = given.state;
    if (state2 !== void 0) {
      if (!isObject(state2) || !isObject(state2.schema) || !Object.hasOwn(state2, "default")) {
        throw new Error(`The plugin class "${id2}" gives a state without a schema and a default.`);
      }
      validate2 = compile(what, state2.schema);
      checkInitial(what, validate2, state2.default, "default");
    }
    const item = { def, validate: validate2 };
    classes.value = new Map([...classes.value, [id2, item]]);
    return () => {
      if (classes.value.get(id2) !== item) return;
      const rest = new Map(classes.value);
      rest.delete(id2);
      classes.value = rest;
    };
  }
  return {
    plugins: {
      register: registerPlugin,
      registered: () => classDefs.value
    },
    options: {
      register(def) {
        const what = `the board option "${def.id}"`;
        if (optionIndex.value.has(def.id)) {
          throw new Error(`The board option "${def.id}" is registered already.`);
        }
        const ui = def.ui;
        if (!isObject(ui) || typeof ui.render !== "function") {
          throw new Error(`The board option "${def.id}" has no UI module with a render() function.`);
        }
        const { onChange } = def;
        if (onChange !== void 0 && typeof onChange !== "function") {
          throw new Error(`The onChange of the board option "${def.id}" is no function.`);
        }
        if (def.migrate !== void 0 && typeof def.migrate !== "function") {
          throw new Error(`The board option "${def.id}" has a migrate that is no function.`);
        }
        const validate2 = compile(what, def.schema);
        if (typeof def.default !== "function") {
          checkInitial(what, validate2, def.default, "default");
        }
        const item = { def, validate: validate2 };
        options.value = [...options.value, item];
        return () => {
          options.value = options.value.filter((x2) => x2 !== item);
        };
      },
      registered: () => optionDefs.value
    },
    registerMigration(registryId2, fn) {
      if (migrations.value.has(registryId2)) {
        throw new Error(`The block class "${registryId2}" has a migration registered already.`);
      }
      migrations.value = new Map([...migrations.value, [registryId2, fn]]);
      return () => {
        if (migrations.value.get(registryId2) !== fn) return;
        const rest = new Map(migrations.value);
        rest.delete(registryId2);
        migrations.value = rest;
      };
    },
    plugin: (id2) => classes.value.get(id2),
    option: (id2) => optionIndex.value.get(id2),
    migration: (registryId2) => migrations.value.get(registryId2)
  };
}
function isPluginClassId(id2) {
  const pkg = packageOf(id2);
  return pkg !== null && /^.+_plugin$/.test(id2.slice(pkg.length + 1));
}

// srcts/store/open.ts
var FORMAT = 1;
var COLLECTIONS = ["blocks", "links", "stacks", "options", "plugins"];
var PROPERTIES = ["id", "name", ...COLLECTIONS];
var refused = "The store refused to open the board";
function parse(text) {
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new StoreError(refused, [`The text holds no JSON: ${messageOf(error)}`]);
  }
}
function checkJsonBoard(json) {
  if (!isObject(json)) throw new StoreError(refused, ["The JSON is no JSON board."]);
  if (!Object.hasOwn(json, "format")) {
    throw new StoreError(refused, [
      "The JSON board has no format, as a file of today's stack has none, which a conversion outside core turns into a JSON board first."
    ]);
  }
  if (typeof json.format !== "number" || !Number.isInteger(json.format)) {
    throw new StoreError(refused, [`The JSON board's format ${JSON.stringify(json.format)} is no whole number.`]);
  }
  if (json.format > FORMAT) {
    throw new StoreError(refused, [
      `The JSON board's format is ${String(json.format)}, while this client reads format ${String(FORMAT)} at most.`
    ]);
  }
  if (!isObject(json.packages) || !Object.values(json.packages).every((v2) => typeof v2 === "string")) {
    throw new StoreError(refused, ["The JSON board names no packages with their versions."]);
  }
  if (!isObject(json.board)) throw new StoreError(refused, ["The JSON board holds no board."]);
  const board = json.board;
  const lacking = COLLECTIONS.filter((name3) => !isObject(board[name3]));
  if (lacking.length > 0) {
    throw new StoreError(refused, [`The board holds its ${listing(lacking)} in no object by id.`]);
  }
  checkVersions(board);
  return json;
}
function isVersion(value2) {
  return typeof value2 === "number" && Number.isInteger(value2) && value2 >= 1;
}
function checkVersions(board) {
  const failures = [];
  if (!isObject(board.versions) || !Object.values(board.versions).every(isVersion)) {
    failures.push("The board records the versions of its plugin classes as no whole numbers from 1 by class.");
  }
  for (const [id2, block] of Object.entries(board.blocks)) {
    if (isObject(block) && !isVersion(block.version)) {
      failures.push(`The block "${id2}" records no version of its class's state.`);
    }
  }
  for (const [id2, option] of Object.entries(board.options)) {
    if (!isObject(option) || !Object.hasOwn(option, "value") || !isVersion(option.version)) {
      failures.push(`The board option "${id2}" is recorded as no value with the version of its shape.`);
    }
  }
  if (failures.length > 0) throw new StoreError(refused, failures);
}
function unversioned(board) {
  const record = board;
  return {
    ...Object.fromEntries(Object.entries(record).filter(([name3]) => PROPERTIES.includes(name3))),
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id2, block]) => [
        id2,
        without(block, ["version"])
      ])
    ),
    options: Object.fromEntries(Object.entries(board.options).map(([id2, option]) => [id2, option.value]))
  };
}
function migrate(board, json, reply, registrations) {
  const failures = /* @__PURE__ */ new Set();
  const changed = /* @__PURE__ */ new Set();
  const plugins = Object.fromEntries(
    Object.entries(board.plugins).map(([id2, plugin]) => {
      const cls = classOf(plugin);
      const def = cls === void 0 ? void 0 : registrations.plugin(cls)?.def;
      if (cls === void 0 || def === void 0) return [id2, plugin];
      const from = own(json.versions, cls);
      const to = own(reply.plugin_versions, cls) ?? 1;
      if (from === void 0 || from === to) return [id2, plugin];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the plugin class "${cls}", while R gives version ${String(to)}.`
        );
        return [id2, plugin];
      }
      if (def.migrate === void 0) return [id2, plugin];
      changed.add("plugins");
      const state2 = migratedValue(
        `The migration of the plugin class "${cls}"`,
        () => def.migrate?.(frozenCopy(plugin.state), from)
      );
      return [id2, { ...plugin, state: state2 }];
    })
  );
  const blocks = Object.fromEntries(
    Object.entries(board.blocks).map(([id2, block]) => {
      const from = own(json.blocks, id2)?.version;
      const to = own(reply.block_registry, block.registry_id)?.version;
      if (from === void 0 || to === void 0 || from === to) return [id2, block];
      if (from > to) {
        failures.add(
          `The block "${id2}" records version ${String(from)} of the state of "${block.registry_id}", while its class is at version ${String(to)}.`
        );
        return [id2, block];
      }
      const fn = registrations.migration(block.registry_id);
      if (fn === void 0) return [id2, block];
      changed.add("blocks");
      const state2 = migrated(
        `The migration of the block class "${block.registry_id}"`,
        () => fn(frozenCopy(block.state), from)
      );
      return [id2, { ...block, state: state2 }];
    })
  );
  const options = Object.fromEntries(
    Object.entries(board.options).map(([id2, value2]) => {
      const from = own(json.options, id2)?.version;
      const to = own(reply.option_versions, id2) ?? 1;
      if (from === void 0 || from === to) return [id2, value2];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the value of the board option "${id2}", while R gives version ${String(to)}.`
        );
        return [id2, value2];
      }
      const def = registrations.option(id2)?.def;
      if (def?.migrate === void 0) return [id2, value2];
      changed.add("options");
      return [
        id2,
        migratedValue(`The migration of the board option "${id2}"`, () => def.migrate?.(frozenCopy(value2), from))
      ];
    })
  );
  if (failures.size > 0) throw new StoreError(refused, [...failures]);
  if (changed.size === 0) return board;
  return { ...board, plugins, blocks, options };
}
function migrated(what, fn) {
  const out = migratedValue(what, fn);
  if (!isObject(out)) throw new StoreError(refused, [`${what} returned no object.`]);
  return out;
}
function migratedValue(what, fn) {
  let out;
  try {
    out = fn();
  } catch (error) {
    throw new StoreError(refused, [`${what} failed: ${messageOf(error)}`]);
  }
  if (out === void 0) throw new StoreError(refused, [`${what} returned nothing.`]);
  return frozenCopy(out);
}
function openJsonBoard(json, reply, registrations) {
  const opening2 = openingPlugins(reply.board);
  const pluginClass = (id2) => registrations.plugin(id2)?.def;
  const read = unversioned(json.board);
  const held = keptPlugins(read.plugins, opening2, pluginClass);
  const plugins = Object.entries(read.plugins).flatMap(([id2, plugin]) => {
    const cls = classOf(plugin);
    return cls === void 0 || Object.hasOwn(held, id2) ? [] : [{ plugin: id2, class: cls, package: packageOf(cls) }];
  });
  const migrated2 = migrate({ ...read, plugins: held }, json.board, reply, registrations);
  const gone = Object.entries(migrated2.blocks).filter(([, block]) => own(reply.block_registry, block.registry_id) === void 0).map(([id2]) => id2);
  const blocks = gone.map((id2) => {
    const registryId2 = migrated2.blocks[id2]?.registry_id ?? "";
    return { block: id2, registry_id: registryId2, package: packageOf(registryId2) };
  });
  const kept = remove(prune2(migrated2, registrations), gone, registrations);
  const board = { ...kept, plugins: appPlugins(kept.plugins, opening2, pluginClass) };
  const failures = check(board, reply.block_registry, registrations);
  if (failures.length > 0) throw new StoreError(refused, failures);
  const packages = Object.entries(json.packages).filter(([pkg]) => own(reply.packages, pkg) === void 0).map(([pkg, version]) => ({ package: pkg, version }));
  return { board, dropped: [...packages, ...blocks, ...plugins] };
}
function classOf(plugin) {
  return isObject(plugin) && typeof plugin.class === "string" ? plugin.class : void 0;
}
function openingPlugins(opening2) {
  const plugins = isObject(opening2) && isObject(opening2.board) ? opening2.board.plugins : void 0;
  if (!isObject(plugins)) return {};
  return Object.fromEntries(
    Object.entries(plugins).filter(
      (entry) => isObject(entry[1]) && typeof entry[1].class === "string"
    )
  );
}
function prune2(board, registrations) {
  const unfit = Object.entries(board.options).filter(([id2, value2]) => {
    const def = registrations.option(id2);
    return def === void 0 || def.validate(value2).length > 0;
  }).map(([id2]) => id2);
  return { ...board, options: without(board.options, unfit) };
}
function remove(board, gone, registrations) {
  if (gone.length === 0) return board;
  const delta = expand({ blocks: { rm: gone } }, board, registrations);
  if ("failures" in delta) throw new StoreError(refused, delta.failures);
  const committed = commit(board, delta);
  if ("failures" in committed) throw new StoreError(refused, committed.failures);
  return committed.board;
}
async function required(board, registrations) {
  const lacking = BASIC_OPTIONS.filter((id2) => !Object.hasOwn(board.options, id2)).map((id2) => ({
    id: id2,
    option: registrations.option(id2)
  }));
  const missing = lacking.filter(({ option }) => option === void 0);
  if (missing.length > 0) {
    throw new StoreError(
      refused,
      missing.map(
        ({ id: id2 }) => `Core's basic frontend requires the board option "${id2}", which no loaded package defines.`
      )
    );
  }
  const defined = lacking.flatMap(({ id: id2, option }) => option === void 0 ? [] : [{ id: id2, option }]);
  const values2 = await Promise.all(
    defined.map(async ({ id: id2, option }) => {
      const what = `The default of the board option "${id2}", which core's basic frontend requires,`;
      let value2;
      try {
        const given = option.def.default;
        value2 = typeof given === "function" ? await given() : given;
      } catch (error) {
        return `${what} failed: ${messageOf(error)}`;
      }
      const failures2 = option.validate(value2);
      return failures2.length > 0 ? `${what} ${failures2.join(" ")}` : [id2, value2];
    })
  );
  const failures = values2.filter((entry) => typeof entry === "string");
  if (failures.length > 0) throw new StoreError(refused, failures);
  return Object.fromEntries(values2.filter((entry) => typeof entry !== "string"));
}

// srcts/store/store.ts
var ENDED = "Another board opened in a new store, so this store changes its board no more.";
function createStore(options = {}) {
  const registrations = createRegistrations();
  const board = y(void 0);
  let hello = options.hello;
  const registry = y(options.hello?.block_registry ?? {});
  const packages = y(options.hello?.packages ?? {});
  const history = y(/* @__PURE__ */ new Map());
  const subscribers = /* @__PURE__ */ new Set();
  let opening2 = false;
  const ended = () => options.signal?.aborted === true;
  options.signal?.addEventListener(
    "abort",
    () => {
      subscribers.clear();
    },
    { once: true }
  );
  const edits = /* @__PURE__ */ new Map();
  let lastStep = 0;
  let busy = false;
  function guard2() {
    if (busy) {
      throw new Error(
        "The store is applying or announcing a change, which a subscriber, a plugin class's update or a migration may not change, since what follows from a delta is a plugin class's update."
      );
    }
  }
  function current2(refused2) {
    if (ended()) throw new StoreError(refused2, [ENDED]);
    if (board.value !== void 0) return board.value;
    throw new StoreError(refused2, ["The store holds no board until it opens one."]);
  }
  function announce2(change, next) {
    for (const fn of [...subscribers]) {
      try {
        fn(change, next);
      } catch (error) {
        report(error);
      }
    }
  }
  function steps(origin) {
    return history.value.get(origin) ?? { undo: [], redo: [] };
  }
  function setSteps(origin, steps2) {
    history.value = new Map([...history.value, [origin, steps2]]);
  }
  function run(start2, deltas, how, refused2) {
    const applied = [];
    const inverses = [];
    let current3 = start2;
    for (const [i2, given] of deltas.entries()) {
      const prefix = deltas.length > 1 ? `Delta ${String(i2 + 1)}: ` : "";
      const fail = (failures2) => new StoreError(refused2, failures2.map((failure) => prefix + failure));
      const delta = how.fit === true ? fit(given, current3) : given;
      if (how.fit === true && isEmpty(delta)) continue;
      const expanded = expand(delta, current3, registrations);
      if ("failures" in expanded) throw fail(expanded.failures);
      const committed = commit(current3, expanded, how.places?.[i2]);
      if ("failures" in committed) throw fail(committed.failures);
      const undoable = !isEmpty(committed.inverse.delta);
      if (!undoable && committed.changed.options.size === 0) continue;
      const failures = check(
        committed.board,
        registry.value,
        registrations,
        committed.changed
      );
      if (failures.length > 0) throw fail(failures);
      applied.push({ delta: expanded, board: committed.board, changed: committed.changed });
      if (undoable) inverses.unshift(committed.inverse);
      current3 = committed.board;
    }
    return { applied, inverses };
  }
  function exists(next, key) {
    const [kind, id2] = JSON.parse(key);
    return Object.hasOwn(kind === "block" ? next.blocks : next.plugins, id2);
  }
  function describe2(key) {
    const [kind, id2] = JSON.parse(key);
    return kind === "block" ? `block "${id2}"` : `the plugin "${id2}"`;
  }
  function named2(deltas) {
    const failures = [];
    const compare = (key, edit) => {
      if (edit === void 0) return;
      const id2 = edits.get(key);
      if (id2 !== edit) {
        failures.push(
          id2 === void 0 ? `The delta was built on edit ${String(edit)} of ${describe2(key)}, which the board lacks.` : `The delta was built on edit ${String(edit)} of ${describe2(key)}, which another edit has replaced with edit ${String(id2)}.`
        );
      }
    };
    for (const delta of deltas) {
      for (const [id2, mod] of Object.entries(delta.blocks?.mod ?? {})) {
        compare(target({ block: id2 }), mod.edit);
      }
      for (const patch2 of delta.patches ?? []) {
        const plugin = pluginOf(patch2.path);
        if (plugin !== void 0) compare(target({ plugin }), patch2.edit);
      }
    }
    return [...new Set(failures)];
  }
  function number(applied, last) {
    const step2 = ++lastStep;
    for (const key of new Set(applied.flatMap(({ changed }) => [...changed.targets]))) {
      if (exists(last, key)) edits.set(key, step2);
      else edits.delete(key);
    }
  }
  function apply(delta, origin = "user") {
    guard2();
    const deltas = isDeltas(delta) ? delta : [delta];
    if (deltas.length === 0) return;
    busy = true;
    try {
      const refused2 = "The store rejected the delta";
      const start2 = current2(refused2);
      const failures = named2(deltas);
      if (failures.length > 0) throw new StoreError(refused2, failures);
      const { applied, inverses } = run(start2, deltas, {}, refused2);
      if (applied.length === 0) return;
      number(applied, applied.at(-1)?.board ?? start2);
      if (inverses.length > 0) {
        setSteps(origin, { undo: [...steps(origin).undo, inverses], redo: [] });
      }
      for (const { delta: delta2, board: next } of applied) {
        board.value = next;
        announce2({ delta: delta2, origin }, next);
      }
    } finally {
      busy = false;
    }
  }
  function revert(origin, back) {
    guard2();
    const { undo, redo } = steps(origin);
    const step2 = back ? undo.at(-1) : redo.at(-1);
    if (step2 === void 0) return;
    busy = true;
    try {
      const refused2 = back ? "The store refused to undo the step" : "The store refused to redo the step";
      const start2 = current2(refused2);
      const { applied, inverses } = run(
        start2,
        step2.map(({ delta }) => delta),
        { fit: true, places: step2.map(({ places }) => places) },
        refused2
      );
      number(applied, applied.at(-1)?.board ?? start2);
      setSteps(
        origin,
        back ? { undo: undo.slice(0, -1), redo: [...redo, inverses] } : { undo: [...undo, inverses], redo: redo.slice(0, -1) }
      );
      for (const { delta, board: next } of applied) {
        board.value = next;
        announce2({ delta, origin }, next);
      }
    } finally {
      busy = false;
    }
  }
  const refusedOpen = "The store refused to open the board";
  function jsonOf(given) {
    if (typeof given !== "string") {
      hello = given;
      registry.value = given.block_registry;
      packages.value = given.packages;
      return { json: checkJsonBoard(given.board), reply: given };
    }
    if (hello === void 0) {
      throw new StoreError(refusedOpen, ["The store opens a JSON board only once R has answered `hello`."]);
    }
    if (hello.lock !== null) {
      throw new StoreError(refusedOpen, ["The board is locked, so it opens no other board."]);
    }
    return { json: checkJsonBoard(parse(given)), reply: hello };
  }
  async function build(json, reply) {
    guard2();
    let opened;
    busy = true;
    try {
      opened = openJsonBoard(json, reply, registrations);
    } finally {
      busy = false;
    }
    const values2 = await required(opened.board, registrations);
    const next = Object.keys(values2).length === 0 ? opened.board : { ...opened.board, options: { ...opened.board.options, ...values2 } };
    return { board: next, dropped: opened.dropped };
  }
  function stillOpen() {
    if (ended()) throw new StoreError(refusedOpen, [ENDED]);
  }
  async function open(given) {
    stillOpen();
    if (opening2) {
      throw new StoreError(refusedOpen, [
        "The store holds a board or opens one already: a store opens one board, and core opens the next in a new store as it starts again."
      ]);
    }
    opening2 = true;
    try {
      const { json, reply } = jsonOf(given);
      await options.loaded?.(reply);
      stillOpen();
      const opened = await build(json, reply);
      stillOpen();
      guard2();
      busy = true;
      try {
        board.value = opened.board;
        const step2 = ++lastStep;
        for (const key of everyTarget(opened.board)) edits.set(key, step2);
        announce2({ opened: opened.board, dropped: opened.dropped }, opened.board);
        return opened.dropped;
      } finally {
        busy = false;
      }
    } catch (error) {
      opening2 = board.value !== void 0;
      throw error;
    }
  }
  async function checkText(text) {
    stillOpen();
    const { json, reply } = jsonOf(text);
    const opened = await build(json, reply);
    stillOpen();
    return opened.dropped;
  }
  function jsonBoard() {
    const next = board.value;
    if (next === void 0) {
      throw new Error("The store holds no board until it opens one.");
    }
    const classes = [...new Set(Object.values(next.plugins).map((plugin) => plugin.class))];
    const owned = /* @__PURE__ */ new Set([
      ...classes.flatMap((cls) => packageOf(cls) ?? []),
      ...Object.values(next.blocks).flatMap(({ registry_id }) => packageOf(registry_id) ?? [])
    ]);
    const recorded = Object.fromEntries(
      [...owned].sort().flatMap((pkg) => {
        const version = own(packages.value, pkg);
        return version === void 0 ? [] : [[pkg, version]];
      })
    );
    const versions = Object.fromEntries(
      classes.map((cls) => [cls, own(hello?.plugin_versions ?? {}, cls) ?? 1])
    );
    const blocks = Object.fromEntries(
      Object.entries(next.blocks).map(([id3, block]) => [
        id3,
        {
          registry_id: block.registry_id,
          version: own(registry.value, block.registry_id)?.version ?? 1,
          state: block.state,
          name: block.name
        }
      ])
    );
    const options2 = Object.fromEntries(
      Object.entries(next.options).map(([id3, value2]) => [
        id3,
        { value: value2, version: own(hello?.option_versions ?? {}, id3) ?? 1 }
      ])
    );
    const { id: id2, name: name3, links, stacks, plugins } = next;
    return {
      format: FORMAT,
      packages: recorded,
      board: { id: id2, versions, name: name3, blocks, links, stacks, options: options2, plugins }
    };
  }
  function save() {
    return `${JSON.stringify(jsonBoard(), null, 2)}
`;
  }
  function everyTarget(next) {
    const targets = [
      ...Object.keys(next.blocks).map((id2) => ({ block: id2 })),
      ...Object.keys(next.plugins).map((id2) => ({ plugin: id2 }))
    ];
    return targets.map(target);
  }
  return {
    board() {
      if (board.value === void 0) {
        throw new Error("The store holds no board until it opens one.");
      }
      return board.value;
    },
    hasBoard: () => board.value !== void 0,
    subscribe(fn) {
      if (ended()) return () => void 0;
      const wrapped = (change, next) => {
        fn(change, next);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },
    apply,
    open,
    check: checkText,
    undo: (origin = "user") => {
      revert(origin, true);
    },
    redo: (origin = "user") => {
      revert(origin, false);
    },
    canUndo: (origin = "user") => steps(origin).undo.length > 0,
    canRedo: (origin = "user") => steps(origin).redo.length > 0,
    plugins: registrations.plugins,
    registerMigration: (registryId2, fn) => registrations.registerMigration(registryId2, fn),
    options: registrations.options,
    jsonBoard,
    save,
    blockRegistry: () => registry.value,
    packages: () => packages.value,
    editId: (of) => edits.get(target(of))
  };
}

// srcts/commands/schemas.ts
var id = { type: "string", pattern: "\\D" };
var blockId = {
  ...id,
  description: "The id of a block, unique among the board's blocks."
};
var linkId = {
  ...id,
  description: "The id of a link, unique among the board's links."
};
var stackId = {
  ...id,
  description: "The id of a stack, unique among the board's stacks."
};
var registryId = {
  type: "string",
  minLength: 1,
  description: `A block class's id in the block registry, such as "blockr.core-head_block".`
};
var name2 = {
  type: "string",
  description: "A name that the user sees and may change, which nothing refers to."
};
var input = {
  type: "string",
  description: 'An input of the block a link goes to, such as "data", or empty for a variadic block, which takes such links in their order.'
};
var state = {
  type: "object",
  description: "Values of a block's state args, which the schema of its class types."
};
var stateArgs = {
  type: "array",
  items: { type: "string" },
  uniqueItems: true,
  description: "Names of a block's state args."
};
var blockIds = {
  type: "array",
  items: blockId,
  uniqueItems: true,
  description: "Ids of blocks, in order."
};
var color = {
  type: ["string", "null"],
  pattern: HEX_COLOR,
  description: 'A hex colour, such as "#66c2a5", which front-ends draw a stack in, or null for none. A front-end draws a stack without a colour in a default of its own.'
};
var optionId = {
  type: "string",
  minLength: 1,
  description: 'The id of a board option, such as "n_rows".'
};
var value = {
  description: "A JSON value, which the schema of what it is set on types."
};
var pluginId = {
  ...id,
  description: "The id of a plugin, unique among the board's plugins."
};
var statePatches = {
  type: "array",
  items: {
    type: "object",
    properties: {
      op: { enum: ["add", "remove", "replace"] },
      path: {
        type: "string",
        pattern: "^(/.*)?$",
        description: `A JSON Pointer within the plugin's state, such as "/positions/a", or "" for the whole state.`
      },
      value: { description: "What an add or a replace puts at the path." }
    },
    required: ["op", "path"],
    additionalProperties: false
  },
  description: "Changes of a plugin's state, as JSON Patch operations, which apply in their order and which the schema of the plugin's class types."
};
var described = /* @__PURE__ */ new WeakMap();
function stateSchema(entry) {
  const known = described.get(entry);
  if (known !== void 0) return known;
  const { properties } = entry.schema;
  let schema = entry.schema;
  if (isRecord(properties)) {
    schema = {
      ...schema,
      properties: Object.fromEntries(
        Object.entries(properties).map(([arg, property]) => {
          const spec = own(entry.arguments, arg);
          if (spec === void 0 || !isRecord(property)) return [arg, property];
          return [
            arg,
            {
              ...spec.description === void 0 ? {} : { description: spec.description },
              ...spec.example === void 0 ? {} : { examples: [spec.example] },
              ...property
            }
          ];
        })
      )
    };
  }
  described.set(entry, schema);
  return schema;
}
function isRecord(x2) {
  return isObject(x2);
}

// srcts/commands/core.ts
var newBlock = {
  registry_id: { ...registryId, description: "The class of the new block." },
  id: {
    ...blockId,
    description: "The new block's id, which is minted where it is left out."
  },
  name: {
    ...name2,
    description: "The new block's name, which its class gives where it is left out."
  },
  state: {
    ...state,
    description: "Values of the new block's state args, which the schema of its class types, in place of those its class starts a block with."
  }
};
function coreCommands(store, options = {}) {
  const validators = /* @__PURE__ */ new WeakMap();
  function mint(taken) {
    return mintId(taken, options.randId);
  }
  function stateFailures(entry, state2) {
    let validate2 = validators.get(entry);
    if (validate2 === void 0) {
      validate2 = compile(`the state of the block class "${entry.id}"`, stateSchema(entry));
      validators.set(entry, validate2);
    }
    return validate2(state2);
  }
  function classOf2(command2, registryId2) {
    const entry = own(store.blockRegistry(), registryId2);
    if (entry === void 0) {
      throw refusal(command2, [`The block registry has no block class "${registryId2}".`]);
    }
    return entry;
  }
  function added(command2, board2, args) {
    const entry = classOf2(command2, args.registry_id);
    const state2 = { ...entry.state, ...args.state };
    const failures = stateFailures(entry, state2);
    if (failures.length > 0) {
      throw refusal(command2, failures.map((failure) => `The state of the new block ${failure}`));
    }
    const id2 = args.id ?? mint((id3) => Object.hasOwn(board2.blocks, id3));
    return [
      id2,
      { registry_id: entry.id, state: state2, name: args.name ?? blockName(entry.id) },
      entry
    ];
  }
  function linkable(board2, to, given) {
    if (!Object.hasOwn(board2.blocks, to)) return false;
    return named(given) || (freeInputs(board2, store.blockRegistry(), to)?.length ?? 0) > 0;
  }
  function linkInput(command2, board2, to, given, except) {
    if (named(given)) return given;
    const free = freeInputs(board2, store.blockRegistry(), to, except) ?? [];
    return one(
      command2,
      free,
      `The block "${to}" has no input that no link feeds.`,
      `The block "${to}" has the free inputs ${quoted(free)}, so the args have to name the one to use.`
    );
  }
  function newInput(command2, entry, given) {
    if (named(given)) return given;
    if (entry.inputs.includes("...")) return "";
    return one(
      command2,
      entry.inputs,
      `The block class "${entry.id}" has no input to take a link.`,
      `The block class "${entry.id}" has the inputs ${quoted(entry.inputs)}, so the args have to name the one to use.`
    );
  }
  const blocks = [
    command({
      id: "blockr.core-add_block",
      label: "Add block",
      description: "Adds a block of a class from the block registry, with the state its class starts a block with, changed by the state args given.",
      args: object(newBlock, ["registry_id"]),
      run(board2, args) {
        const [id2, block] = added(this.id, board2, args);
        return { blocks: { add: { [id2]: block } } };
      }
    }),
    command({
      id: "blockr.core-remove_block",
      label: "Remove block",
      description: "Removes a block, with its links and its place in its stack.",
      args: object({ id: blockId }, ["id"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run: (_board, { id: id2 }) => ({ blocks: { rm: [id2] } })
    }),
    command({
      id: "blockr.core-rename_block",
      label: "Rename block",
      description: "Gives a block a new name.",
      args: object({ id: blockId, name: name2 }, ["id", "name"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run: (_board, { id: id2, name: name3 }) => ({ blocks: { mod: { [id2]: { name: name3 } } } })
    }),
    command({
      id: "blockr.core-set_block",
      label: "Set block state",
      description: "Sets values of a block's state args, which the schema of its class types, and unsets the state args named, as a block's UI submits an edit.",
      args: object(
        {
          id: blockId,
          state: { ...state, description: "Values of the block's state args to set." },
          unset: { ...stateArgs, description: "Names of the block's state args to unset." }
        },
        ["id"]
      ),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run(board2, { id: id2, state: state2, unset }) {
        const block = own(board2.blocks, id2);
        if (block === void 0) return [];
        const entry = classOf2(this.id, block.registry_id);
        const before = new Set(stateFailures(entry, block.state));
        const failures = stateFailures(entry, {
          ...without(block.state, unset ?? []),
          ...state2
        }).filter((failure) => !before.has(failure));
        if (failures.length > 0) {
          throw refusal(this.id, failures.map((failure) => `The state of block "${id2}" ${failure}`));
        }
        return {
          blocks: {
            mod: {
              [id2]: { ...state2 === void 0 ? {} : { state: state2 }, ...unset === void 0 ? {} : { unset } }
            }
          }
        };
      }
    }),
    command({
      id: "blockr.core-append_block",
      label: "Append block",
      description: "Adds a block that takes the result of another, through a new link from that block into an input of the new block.",
      args: object(
        {
          from: { ...blockId, description: "The block whose result the new block takes." },
          ...newBlock,
          input: {
            ...input,
            description: "The input of the new block that takes the result, which may be left out where the new block has one input or is variadic."
          }
        },
        ["from", "registry_id"]
      ),
      enabled: (board2, { from }) => Object.hasOwn(board2.blocks, from),
      run(board2, args) {
        const [id2, block, entry] = added(this.id, board2, args);
        const input2 = newInput(this.id, entry, args.input);
        const link = mint((id3) => Object.hasOwn(board2.links, id3));
        return {
          blocks: { add: { [id2]: block } },
          links: { add: { [link]: { from: args.from, to: id2, input: input2 } } }
        };
      }
    }),
    command({
      id: "blockr.core-prepend_block",
      label: "Prepend block",
      description: "Adds a block whose result another takes, through a new link from the new block into an input of that block.",
      args: object(
        {
          to: { ...blockId, description: "The block that takes the new block's result." },
          ...newBlock,
          input: {
            ...input,
            description: "The input of that block that takes the result, which may be left out where only one of its inputs is free or it is variadic."
          }
        },
        ["to", "registry_id"]
      ),
      enabled: (board2, { to, input: input2 }) => linkable(board2, to, input2),
      run(board2, args) {
        const input2 = linkInput(this.id, board2, args.to, args.input);
        const [id2, block] = added(this.id, board2, args);
        const link = mint((id3) => Object.hasOwn(board2.links, id3));
        return {
          blocks: { add: { [id2]: block } },
          links: { add: { [link]: { from: id2, to: args.to, input: input2 } } }
        };
      }
    }),
    command({
      id: "blockr.core-insert_block",
      label: "Insert block",
      description: "Adds a block on a link, which it replaces with a link from the link's source into an input of the new block and one from the new block into the input that the link fed.",
      args: object(
        {
          link: { ...linkId, description: "The link that the new block goes on." },
          ...newBlock,
          input: {
            ...input,
            description: "The input of the new block that takes the result of the link's source, which may be left out where the new block has one input or is variadic."
          }
        },
        ["link", "registry_id"]
      ),
      enabled: (board2, { link }) => Object.hasOwn(board2.links, link),
      run(board2, args) {
        const split = own(board2.links, args.link);
        if (split === void 0) return [];
        const [id2, block, entry] = added(this.id, board2, args);
        const input2 = newInput(this.id, entry, args.input);
        const near = mint((id3) => Object.hasOwn(board2.links, id3));
        const far = mint((id3) => Object.hasOwn(board2.links, id3) || id3 === near);
        return {
          blocks: { add: { [id2]: block } },
          links: {
            rm: [args.link],
            add: {
              [near]: { from: split.from, to: id2, input: input2 },
              [far]: { from: id2, to: split.to, input: split.input }
            },
            before: { [far]: args.link }
          }
        };
      }
    })
  ];
  const links = [
    command({
      id: "blockr.core-add_link",
      label: "Add link",
      description: "Adds a link, through which a block takes the result of another as one of its inputs.",
      args: object(
        {
          from: { ...blockId, description: "The block whose result the link carries." },
          to: { ...blockId, description: "The block that takes the result." },
          input: {
            ...input,
            description: "The input of the block that takes the result, which may be left out where only one of its inputs is free or it is variadic."
          },
          id: {
            ...linkId,
            description: "The new link's id, which is minted where it is left out."
          }
        },
        ["from", "to"]
      ),
      enabled: (board2, { from, to, input: input2 }) => Object.hasOwn(board2.blocks, from) && linkable(board2, to, input2),
      run(board2, { from, to, input: input2, id: id2 }) {
        const link = id2 ?? mint((id3) => Object.hasOwn(board2.links, id3));
        const resolved = linkInput(this.id, board2, to, input2);
        return { links: { add: { [link]: { from, to, input: resolved } } } };
      }
    }),
    command({
      id: "blockr.core-change_link",
      label: "Change link",
      description: "Changes the block a link comes from, the block it goes to or the input it feeds, keeping its id.",
      args: object(
        {
          id: linkId,
          from: { ...blockId, description: "The block whose result the link carries." },
          to: { ...blockId, description: "The block that takes the result." },
          input: {
            ...input,
            description: "The input of the block that takes the result, which the link keeps where it is left out, unless the link goes to another block, where it may be left out only if one input of that block is free or the block is variadic."
          }
        },
        ["id"]
      ),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.links, id2),
      run(board2, args) {
        const link = own(board2.links, args.id);
        if (link === void 0) return [];
        const to = args.to ?? link.to;
        const input2 = args.input === void 0 && to === link.to ? link.input : linkInput(this.id, board2, to, args.input, args.id);
        return { links: { mod: { [args.id]: { from: args.from ?? link.from, to, input: input2 } } } };
      }
    }),
    command({
      id: "blockr.core-remove_link",
      label: "Remove link",
      description: "Removes a link.",
      args: object({ id: linkId }, ["id"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.links, id2),
      run: (_board, { id: id2 }) => ({ links: { rm: [id2] } })
    })
  ];
  const stacks = [
    command({
      id: "blockr.core-add_stack",
      label: "Add stack",
      description: "Adds a stack of blocks, taking each out of the stack it is in, since a block sits in one stack at most.",
      args: object({
        id: {
          ...stackId,
          description: "The new stack's id, which is minted where it is left out."
        },
        name: { ...name2, description: `The new stack's name, "Stack" where it is left out.` },
        blocks: { ...blockIds, description: "The blocks of the new stack, in order." },
        color: {
          ...color,
          description: "The new stack's colour, which front-ends draw it in, or null for none. The stack gets null where this is left out, and a front-end draws a stack without a colour in a default of its own."
        }
      }),
      run(board2, { id: id2, name: name3 = "Stack", blocks: blocks2 = [], color: color2 = null }) {
        const stack = id2 ?? mint((id3) => Object.hasOwn(board2.stacks, id3));
        const left2 = leave(board2, stack, blocks2);
        return {
          stacks: {
            add: { [stack]: { name: name3, blocks: blocks2, color: color2 } },
            ...Object.keys(left2).length === 0 ? {} : { mod: left2 }
          }
        };
      }
    }),
    command({
      id: "blockr.core-change_stack",
      label: "Change stack",
      description: "Changes the blocks of a stack or its colour, taking each block out of the stack it is in, since a block sits in one stack at most.",
      args: object(
        {
          id: stackId,
          blocks: {
            ...blockIds,
            description: "The blocks of the stack, in order, which it keeps where they are left out."
          },
          color: {
            ...color,
            description: "The stack's colour, which front-ends draw it in, or null to clear it. A front-end draws a stack without a colour in a default of its own. The stack keeps its colour where this is left out."
          }
        },
        ["id"]
      ),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (board2, { id: id2, blocks: blocks2, color: color2 }) => ({
        stacks: {
          mod: {
            ...blocks2 === void 0 ? {} : leave(board2, id2, blocks2),
            [id2]: {
              ...blocks2 === void 0 ? {} : { blocks: blocks2 },
              ...color2 === void 0 ? {} : { color: color2 }
            }
          }
        }
      })
    }),
    command({
      id: "blockr.core-rename_stack",
      label: "Rename stack",
      description: "Gives a stack a new name.",
      args: object({ id: stackId, name: name2 }, ["id", "name"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (_board, { id: id2, name: name3 }) => ({ stacks: { mod: { [id2]: { name: name3 } } } })
    }),
    command({
      id: "blockr.core-remove_stack",
      label: "Remove stack",
      description: "Removes a stack, leaving its blocks on the board.",
      args: object({ id: stackId }, ["id"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (_board, { id: id2 }) => ({ stacks: { rm: [id2] } })
    })
  ];
  const board = [
    command({
      id: "blockr.core-rename_board",
      label: "Rename board",
      description: "Gives the board a new name.",
      args: object({ name: name2 }, ["name"]),
      run: (_board, { name: name3 }) => ({ name: name3 })
    }),
    command({
      id: "blockr.core-set_option",
      label: "Set board option",
      description: "Sets the value of a board option, which the board option's schema types.",
      args: object({ id: optionId, value }, ["id", "value"]),
      enabled: (_board, { id: id2 }) => own(store.options.registered(), id2) !== void 0,
      run: (_board, { id: id2, value: value2 }) => ({ options: { set: { [id2]: value2 } } })
    })
  ];
  const plugins = [
    command({
      id: "blockr.core-set_plugin_state",
      label: "Set plugin state",
      description: "Changes a plugin's state by patches at paths within that state, as JSON Patch has them, which the schema of the plugin's class types. It is not enabled on a locked board, where only the commands of a plugin's class know what the plugin locks.",
      args: object(
        {
          plugin: { ...pluginId, description: "The plugin whose state the patches change." },
          patches: statePatches
        },
        ["plugin", "patches"]
      ),
      enabled: (board2, { plugin }) => options.locked !== true && Object.hasOwn(board2.plugins, plugin),
      run: (_board, { plugin, patches: patches2 }) => ({
        patches: patches2.map((patch2) => ({ ...patch2, path: `${pathOf(["plugins", plugin, "state"])}${patch2.path}` }))
      })
    })
  ];
  return [...blocks, ...links, ...stacks, ...board, ...plugins];
}
function freeInputs(board, registry, to, except) {
  const block = own(board.blocks, to);
  if (block === void 0) return void 0;
  const inputs = own(registry, block.registry_id)?.inputs;
  if (inputs === void 0) return void 0;
  if (inputs.includes("...")) return [""];
  const fed = new Set(
    Object.entries(board.links).filter(([id2, link]) => id2 !== except && link.to === to).map(([, link]) => link.input)
  );
  return inputs.filter((input2) => !fed.has(input2));
}
function blockName(registryId2) {
  const pkg = packageOf(registryId2);
  const name3 = pkg === null ? registryId2 : registryId2.slice(pkg.length + 1);
  return sentenceCase(name3.replace(/_block$/, ""));
}
function leave(board, id2, blocks) {
  const moving = new Set(blocks);
  return Object.fromEntries(
    Object.entries(board.stacks).filter(([other, stack]) => other !== id2 && stack.blocks.some((block) => moving.has(block))).map(([other, stack]) => [
      other,
      { blocks: stack.blocks.filter((block) => !moving.has(block)) }
    ])
  );
}
function object(properties, required2 = []) {
  return { type: "object", properties, required: required2, additionalProperties: false };
}
function named(input2) {
  return input2 !== void 0 && input2 !== "";
}
function one(command2, inputs, none, several) {
  const [input2, ...rest] = inputs;
  if (input2 === void 0) throw refusal(command2, [none]);
  if (rest.length > 0) throw refusal(command2, [several]);
  return input2;
}
function command(def) {
  return {
    ...def,
    run: def.run.bind(def),
    ...def.enabled === void 0 ? {} : { enabled: def.enabled.bind(def) }
  };
}

// srcts/commands/commands.ts
var CommandError = class extends Error {
  failures;
  constructor(message, failures, options) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"), options);
    this.name = "CommandError";
    this.failures = failures;
  }
};
function refusal(id2, failures) {
  return new CommandError(`The command "${id2}" refused to run`, failures);
}
function createCommands(store, options = {}) {
  const commands = y(/* @__PURE__ */ new Map());
  const registered = O(
    () => Object.fromEntries([...commands.value].map(([id2, { command: command2 }]) => [id2, command2]))
  );
  const ended = () => options.signal?.aborted === true;
  function register(command2) {
    const { id: id2 } = command2;
    if (typeof id2 !== "string" || !isCommandId(id2)) {
      throw new Error(
        `The command id ${JSON.stringify(id2)} is not named as a registry id is, "{pkg}-{name}".`
      );
    }
    if (typeof command2.label !== "string" || command2.label === "") {
      throw new Error(`The command "${id2}" has no label.`);
    }
    if (typeof command2.run !== "function") {
      throw new Error(`The command "${id2}" has no run().`);
    }
    if (commands.value.has(id2)) {
      throw new Error(`The command "${id2}" is registered already.`);
    }
    const validate2 = command2.args === void 0 ? void 0 : compile(`the args of the command "${id2}"`, command2.args, "fail their schema");
    const item = { command: command2, validate: validate2 };
    commands.value = new Map([...commands.value, [id2, item]]);
    return () => {
      if (commands.value.get(id2) !== item) return;
      const rest = new Map(commands.value);
      rest.delete(id2);
      commands.value = rest;
    };
  }
  function lookup(id2, given) {
    const item = commands.value.get(id2);
    if (item === void 0) {
      throw refusal(id2, [`No command "${id2}" is registered.`]);
    }
    const args = given ?? {};
    const failures = item.validate?.(args) ?? [];
    if (failures.length > 0) {
      throw refusal(id2, failures.map((failure) => `The args ${failure}`));
    }
    return { command: item.command, args };
  }
  function current2(id2) {
    if (ended()) {
      throw refusal(id2, ["Another board opened in a new store, so this command registry runs no command."]);
    }
    if (!store.hasBoard()) {
      throw refusal(id2, ["The store holds no board until it opens one."]);
    }
    return store.board();
  }
  function allow(command2, board, args) {
    const enabled = attempt2(command2, () => command2.enabled?.(board, args) ?? true);
    if (!enabled) {
      throw refusal(command2.id, [
        `It is not enabled on the board as it is, with the args ${JSON.stringify(args)}.`
      ]);
    }
  }
  function build(command2, board, args) {
    const returned = attempt2(command2, () => command2.run(board, args));
    const deltas2 = isDeltas(returned) ? returned : [returned];
    return deltas2.map(withEdits);
  }
  function deltas(id2, given) {
    const { command: command2, args } = lookup(id2, given);
    const board = current2(id2);
    allow(command2, board, args);
    if (command2.prepare === void 0) return build(command2, board, args);
    return prepared(command2, board, args);
  }
  async function prepared(command2, board, args) {
    let input2;
    try {
      input2 = await command2.prepare?.(board, args);
    } catch (error) {
      throw failed(command2, error);
    }
    if (ended()) {
      throw refusal(command2.id, ["A board opened while it prepared, which drops it."]);
    }
    const now = current2(command2.id);
    allow(command2, now, args);
    return build(command2, now, input2);
  }
  function withEdits(delta) {
    const blocks = delta.blocks?.mod;
    const patches2 = delta.patches;
    if (blocks === void 0 && patches2 === void 0) return delta;
    const edit = (change, id2) => change.edit !== void 0 || id2 === void 0 ? change : { ...change, edit: id2 };
    return {
      ...delta,
      ...blocks === void 0 ? {} : {
        blocks: {
          ...delta.blocks,
          mod: Object.fromEntries(
            Object.entries(blocks).map(([id2, mod]) => [
              id2,
              edit(mod, store.editId({ block: id2 }))
            ])
          )
        }
      },
      ...patches2 === void 0 ? {} : {
        patches: patches2.map((patch2) => {
          const plugin = pluginOf(patch2.path);
          return plugin === void 0 ? patch2 : edit(patch2, store.editId({ plugin }));
        })
      }
    };
  }
  const registry = {
    register,
    registered: () => registered.value,
    enabled(id2, given) {
      try {
        const { command: command2, args } = lookup(id2, given);
        allow(command2, current2(id2), args);
        return true;
      } catch {
        return false;
      }
    },
    async run(id2, given, origin = "user") {
      const step2 = deltas(id2, given);
      store.apply(Array.isArray(step2) ? step2 : await step2, origin);
    },
    async plan(id2, given) {
      return deltas(id2, given);
    }
  };
  for (const command2 of coreCommands(store, {
    locked: (options.lock ?? null) !== null,
    randId: shapeOf(options.randId)
  })) {
    registry.register(command2);
  }
  return registry;
}
function shapeOf(randId) {
  if (randId === void 0) return "adjective_animal";
  if (isRandId(randId)) return randId;
  throw new RangeError(`The blockr option rand_id is ${JSON.stringify(randId)}, where it takes one of ${quoted(RAND_IDS)}.`);
}
function isCommandId(id2) {
  const pkg = packageOf(id2);
  return pkg !== null && id2.length > pkg.length + 1;
}
function failed(command2, error) {
  if (error instanceof CommandError) return error;
  return new CommandError(`The command "${command2.id}" failed`, [messageOf(error)], {
    cause: error
  });
}
function attempt2(command2, fn) {
  try {
    return fn();
  } catch (error) {
    throw failed(command2, error);
  }
}

// srcts/core/start.ts
var CAPABILITIES = ["cancel", "interrupt", "progress"];
var BASIC = "blockr.core-basic";
function start(options) {
  const page = new AbortController();
  const fail = options.fail ?? ((error) => {
    report(error);
  });
  let first;
  let latest;
  let running2;
  const holdsBoard = () => running2?.store.hasBoard() === true;
  let restarting;
  const nextRevision = counter();
  let settle2;
  const ready = new Promise((resolve, reject) => {
    settle2 = { resolve, reject };
  });
  ready.catch(() => void 0);
  const client = createClient(options.transport, {
    hello: () => ({ protocol: PROTOCOL_VERSION, capabilities: CAPABILITIES }),
    session(reply) {
      heard(reply);
    }
  });
  const blockrOptions = createBlockrOptions(client);
  const loader = createLoader({ ...options.loader, modules: () => latest?.modules ?? {} });
  function heard(reply) {
    if (page.signal.aborted) return;
    const error = helloError(reply);
    if (error !== void 0) {
      if (first === void 0) {
        settle2?.reject(error);
        fail(error);
      } else if (restarting !== void 0) {
        const waiting2 = restarting;
        restarting = void 0;
        waiting2.reject(error);
      } else {
        report(error);
      }
      return;
    }
    const hello = reply;
    latest = hello;
    if (first === void 0) {
      first = hello;
      boot(hello, void 0).then(
        ({ core }) => settle2?.resolve(core),
        (failure) => {
          settle2?.reject(failure);
          fail(asError(failure));
        }
      );
      return;
    }
    const changed = changedPackages(first, hello);
    if (changed.length > 0) (options.reload ?? askReload)(changed);
    if (restarting !== void 0) {
      const waiting2 = restarting;
      restarting = void 0;
      waiting2.resolve(hello);
      return;
    }
    running2?.scheduler.session();
  }
  async function boot(reply, later) {
    const controller = new AbortController();
    const { signal } = controller;
    const ending = () => {
      controller.abort();
    };
    if (page.signal.aborted) ending();
    page.signal.addEventListener("abort", ending, { once: true });
    signal.addEventListener(
      "abort",
      () => {
        page.signal.removeEventListener("abort", ending);
      },
      { once: true }
    );
    const [evaluationQueue, backgroundEvaluationDelay, gateVisibility, randId] = await Promise.all([
      blockrOptions.read("evaluation_queue"),
      blockrOptions.read("background_evaluation_delay"),
      blockrOptions.read("gate_visibility"),
      blockrOptions.read("rand_id")
    ]);
    const store = createStore({
      hello: reply,
      loaded: () => prepare(),
      signal
    });
    const commands = createCommands(store, { signal, lock: reply.lock, randId });
    for (const option of options.options({ blockrOption: (name3) => blockrOptions.read(name3) })) {
      store.options.register(option);
    }
    const bound = later === void 0 ? reply.bound : void 0;
    const scheduler = createScheduler({
      store,
      client,
      evaluationQueue,
      backgroundEvaluationDelay,
      gateVisibility,
      nextRevision,
      signal,
      ...bound === void 0 ? {} : { bound }
    });
    const core = createCore({
      store: {
        ...store,
        open: (given) => reopen(given, controller)
      },
      commands,
      scheduler,
      loader,
      blockrOptions,
      lock: (first ?? reply).lock,
      renderers: () => latest?.renderers ?? {},
      signal,
      ...options.measure === void 0 ? {} : { measure: options.measure }
    });
    running2 = { store, scheduler, core, controller };
    const named2 = await initialize(reply, core, signal, later === void 0);
    let dropped = [];
    let failure;
    if (later === void 0) {
      const instead = chosen(reply, named2);
      if (instead === void 0) {
        dropped = await store.open(reply);
      } else {
        try {
          dropped = await store.open(instead);
        } catch (error) {
          report(error);
          dropped = await store.open(reply);
        }
      }
    } else {
      try {
        dropped = await store.open(later.text);
      } catch (error) {
        failure = asError(error);
        try {
          await store.open(later.before);
        } catch (again) {
          report(again);
          throw failure;
        }
      }
    }
    try {
      await options.frontEnds?.render(core, signal);
    } finally {
      scheduler.start();
    }
    if (failure !== void 0) throw failure;
    return { core, dropped };
  }
  async function initialize(reply, core, signal, naming) {
    const named2 = [];
    for (const entry of reply.startup_modules) {
      if (signal.aborted) break;
      try {
        const source = entry.css === void 0 ? { module: entry.module } : { module: entry.module, css: entry.css };
        const mod = startupOf(await loader.load(source, entry.package), entry.module, entry.package);
        let result;
        try {
          result = await mod.initialize({ core, signal });
        } catch (error) {
          throw new ModuleError(entry.module, entry.package, `failed in initialize(): ${messageOf(error)}`, { cause: error });
        }
        if (typeof result === "string") {
          if (naming) {
            named2.push({ package: entry.package, text: result });
          } else {
            report(
              new Error(
                `The startup module of ${entry.package} named a board to open as another board opened, which core ignores, since only a startup module that runs as the page starts names one.`
              )
            );
          }
        } else if (result !== void 0) {
          throw new ModuleError(entry.module, entry.package, "resolved initialize() with something other than the text of a JSON board.");
        }
      } catch (error) {
        report(error);
      }
    }
    return named2;
  }
  function chosen(reply, named2) {
    const [only, ...more] = named2;
    if (only === void 0) return void 0;
    const packages = quoted(named2.map((entry) => entry.package));
    if (more.length > 0) {
      report(new Error(`The startup modules of ${packages} each named a board to open, so core opens the board that R sent.`));
      return void 0;
    }
    if (reply.lock !== null) {
      report(new Error(`The board is locked, so core opens the board that R sent rather than the one that ${only.package} named.`));
      return void 0;
    }
    try {
      JSON.parse(only.text);
    } catch (error) {
      report(new Error(`The startup module of ${only.package} named a board that holds no JSON, so core opens the board that R sent: ${messageOf(error)}`));
      return void 0;
    }
    return only.text;
  }
  async function prepare() {
    await options.frontEnds?.prepare({ id: BASIC, source: own(latest?.modules ?? {}, BASIC) }, loader);
  }
  let queue = Promise.resolve();
  function reopen(given, of) {
    const run = queue.then(() => reopening(given, of));
    queue = run.catch(() => void 0);
    return run;
  }
  async function reopening(given, of) {
    const refused2 = "The store refused to open the board";
    const current2 = running2;
    if (current2?.controller !== of || of.signal.aborted) {
      throw new StoreError(refused2, ["Another board opened since, so this store opens no board."]);
    }
    if (!current2.store.hasBoard()) {
      throw new StoreError(refused2, ["The store holds no board yet, so it opens none in place of it."]);
    }
    const text = typeof given === "string" ? given : JSON.stringify(given.board);
    await current2.store.check(text);
    const before = current2.store.save();
    of.abort();
    running2 = void 0;
    try {
      const reply = await afresh();
      const { dropped } = await boot(reply, { text, before });
      return dropped;
    } catch (error) {
      if (!page.signal.aborted && !holdsBoard()) fail(asError(error));
      throw error;
    }
  }
  function afresh() {
    return new Promise((resolve, reject) => {
      if (page.signal.aborted) {
        reject(new Error("Core has stopped."));
        return;
      }
      restarting = { resolve, reject };
      client.hello();
    });
  }
  return {
    ready,
    core: () => running2?.core,
    stop() {
      page.abort();
      const waiting2 = restarting;
      restarting = void 0;
      waiting2?.reject(new Error("Core has stopped."));
    }
  };
}
function helloError(reply) {
  if (reply.status !== "ok" || !("startup_modules" in reply)) {
    const said = "conditions" in reply ? reply.conditions.map((c2) => c2.message).join(" ") : "";
    return new Error(`R replied ${reply.status} to the client's hello. ${said}`.trim());
  }
  const theirs = reply.protocol.split(".")[0];
  const ours = PROTOCOL_VERSION.split(".")[0];
  if (theirs !== ours) {
    return new Error(
      `R speaks version ${reply.protocol} of the wire protocol, where this page speaks ${PROTOCOL_VERSION}. Reload the page, which then loads the client that R serves.`
    );
  }
  return void 0;
}
function changedPackages(first, reply) {
  return Object.entries(reply.packages).flatMap(([pkg, now]) => {
    const loaded = own(first.packages, pkg);
    return loaded === void 0 || loaded === now ? [] : [{ package: pkg, loaded, now }];
  });
}
function askReload(changed) {
  report(
    new Error(
      `R now runs ${changed.map((c2) => `${c2.package} ${c2.now}`).join(", ")}, where the page loaded ${changed.map((c2) => `${c2.package} ${c2.loaded}`).join(", ")}. Reload the page.`
    )
  );
}
function startupOf(exports, module, pkg) {
  if (!isObject(exports) || typeof exports.api !== "number") {
    throw new ModuleError(module, pkg, "declares no major of core's JS API, as `export const api = 1`.");
  }
  if (exports.api !== API) {
    throw new ModuleError(
      module,
      pkg,
      `was built against major ${String(exports.api)} of core's JS API, where core's is ${String(API)}.`
    );
  }
  const mod = exports.default;
  if (!isObject(mod) || typeof mod.initialize !== "function") {
    throw new ModuleError(module, pkg, "has no default export with an initialize() function.");
  }
  return mod;
}

// srcts/page/page.ts
function startPage(options) {
  const { el } = options;
  el.replaceChildren(loadingNote());
  return start({
    transport: options.transport ?? shinyTransport(),
    ...options.loader === void 0 ? {} : { loader: options.loader },
    ...options.measure === void 0 ? {} : { measure: options.measure },
    options: coreOptions,
    frontEnds: pageFrontEnds(el),
    reload(changed) {
      reloadBanner(changed);
    },
    fail(error) {
      showError(el, error.message);
      report(error);
    }
  });
}
function reloadBanner(changed) {
  const id2 = "blockr-core-reload";
  document.getElementById(id2)?.remove();
  const banner = document.createElement("div");
  banner.id = id2;
  banner.setAttribute("role", "alert");
  banner.className = "bc:fixed bc:inset-x-0 bc:top-0 bc:z-50 bc:flex bc:items-center bc:gap-3 bc:border-b bc:border-border-default bc:bg-white bc:p-3 bc:font-sans bc:text-sm bc:text-black";
  const text = document.createElement("span");
  const versions = changed.map((c2) => `${c2.package} ${c2.loaded} to ${c2.now}`).join(", ");
  text.textContent = `R now runs other versions of packages than this page loaded (${versions}). Save the board, then reload the page.`;
  const button = document.createElement("button");
  button.type = "button";
  button.className = "bc:rounded-md bc:border bc:border-border-default bc:px-2 bc:py-1";
  button.textContent = "Reload";
  button.addEventListener("click", () => {
    location.reload();
  });
  banner.append(text, button);
  document.body.append(banner);
}
export {
  SessionEndedError,
  createClient,
  shinyTransport,
  startPage
};
