// The parts of core's React SDK that take no binding, React Aria's in core's
// theme: the tags, the list box in place, which an Autocomplete narrows with a
// search field, the form, the drop zone and React Aria's FileTrigger, which a
// caller composes with a button.
// The rest, such as the keys and the focus, is React Aria's.

import { useState } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { drop, find, picking, texts, type } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import { labelled } from "../testing/select.ts";
import { Autocomplete, Button, Checkbox, DropZone, FileTrigger, Form, ListBox, ListBoxItem, Radio, RadioGroup, SearchField, Select, SelectItem, Switch, Tag, TagGroup, Text, TextField, Token, TokenField, useFilter } from "./index.ts";

afterEach(() => {
  document.body.replaceChildren();
});

// Draws the component on the page, and unmounts it.
async function draw(node: ReactElement): Promise<{ el: HTMLElement; unmount: () => Promise<void> }> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  await step(() => {
    root.render(node);
  });
  return {
    el,
    unmount: () =>
      step(() => {
        root.unmount();
      }),
  };
}

const fruits = [
  { id: "apple", name: "Apple" },
  { id: "banana", name: "Banana" },
  { id: "cherry", name: "Cherry" },
];

// The labels of the rows of a list box, by the role that React Aria gives a
// row of a list box, and of those it shows as picked.
function rows(list: ParentNode): { readonly all: string[]; readonly picked: string[] } {
  const options = [...list.querySelectorAll('[role="option"]')];
  return {
    all: options.map((option) => option.textContent.trim()),
    picked: options.filter((option) => option.getAttribute("aria-selected") === "true").map((option) => option.textContent.trim()),
  };
}

describe("the tags", () => {
  test("show their texts under the group's label, and each a button that removes it where the group takes onRemove", async () => {
    function Tags(): ReactElement {
      const [left, setLeft] = useState(fruits);
      return (
        <TagGroup
          label="Fruits"
          items={left}
          onRemove={(keys) => {
            setLeft((now) => now.filter((fruit) => !keys.has(fruit.id)));
          }}
        >
          {(fruit) => <Tag id={fruit.id}>{fruit.name}</Tag>}
        </TagGroup>
      );
    }
    const { el } = await draw(<Tags />);
    const list = find(el, '[role="grid"]');
    expect(el.ownerDocument.getElementById(list.getAttribute("aria-labelledby") ?? "")?.textContent).toBe("Fruits");
    expect(texts(list, '[role="row"]')).toEqual(["Apple", "Banana", "Cherry"]);

    await step(() => {
      find(list, '[role="row"]:nth-child(2) button').click();
    });

    expect(texts(list, '[role="row"]')).toEqual(["Apple", "Cherry"]);
  });
});

describe("the list box", () => {
  test("shows its rows in place, and picks one, or several, where its selectionMode does", async () => {
    const { el } = await draw(
      <>
        <ListBox aria-label="One" items={fruits} selectionMode="single" defaultSelectedKeys={["banana"]}>
          {(fruit) => <ListBoxItem id={fruit.id}>{fruit.name}</ListBoxItem>}
        </ListBox>
        <ListBox aria-label="Several" items={fruits} selectionMode="multiple">
          {(fruit) => <ListBoxItem id={fruit.id}>{fruit.name}</ListBoxItem>}
        </ListBox>
      </>,
    );
    const [one, several] = [find(el, '[aria-label="One"]'), find(el, '[aria-label="Several"]')];
    expect(rows(one)).toEqual({ all: ["Apple", "Banana", "Cherry"], picked: ["Banana"] });

    await step(() => {
      find(one, '[role="option"]:nth-child(1)').click();
    });
    await step(() => {
      find(several, '[role="option"]:nth-child(1)').click();
    });
    await step(() => {
      find(several, '[role="option"]:nth-child(3)').click();
    });

    expect([rows(one).picked, rows(several).picked]).toEqual([["Apple"], ["Apple", "Cherry"]]);
  });

  test("narrows its rows to those that hold what the user types into a search field, in an Autocomplete", async () => {
    function Narrowed(): ReactElement {
      const { contains } = useFilter({ sensitivity: "base" });
      return (
        <Autocomplete filter={contains}>
          <SearchField label="Find a fruit" />
          <ListBox aria-label="Fruits" items={fruits} renderEmptyState={() => "No fruit"}>
            {(fruit) => <ListBoxItem id={fruit.id}>{fruit.name}</ListBoxItem>}
          </ListBox>
        </Autocomplete>
      );
    }
    const { el } = await draw(<Narrowed />);
    const list = find(el, '[role="listbox"]');

    await step(() => {
      type(labelled(el, "Find a fruit"), "an");
    });
    expect(rows(list).all).toEqual(["Banana"]);

    await step(() => {
      type(labelled(el, "Find a fruit"), "kiwi");
    });
    expect([list.hasAttribute("data-empty"), list.textContent]).toEqual([true, "No fruit"]);
  });
});

describe("the form", () => {
  test("shows the errors of its validationErrors below the fields they name, and leaves the checks to its caller where its validationBehavior is aria", async () => {
    const submitted: string[] = [];
    const { el } = await draw(
      <Form
        aria-label="Name"
        validationBehavior="aria"
        validationErrors={{ name: "The board holds a block of that name." }}
        onSubmit={(event) => {
          event.preventDefault();
          submitted.push("submitted");
        }}
      >
        <TextField label="Name" name="name" isRequired />
        <button type="submit">Save</button>
      </Form>,
    );
    const form = find(el, "form", HTMLFormElement);

    expect([form.noValidate, form.getAttribute("aria-label")]).toEqual([true, "Name"]);
    expect(texts(el, '[slot="errorMessage"]')).toEqual(["The board holds a block of that name."]);
    expect(labelled(el, "Name").getAttribute("aria-invalid")).toBe("true");

    await step(() => {
      find(el, "button").click();
    });
    expect(submitted).toEqual(["submitted"]);
  });
});

describe("the drop zone", () => {
  test("hears a file that the user drops on it, and names itself by its label", async () => {
    const dropped: string[] = [];
    const { el } = await draw(
      <DropZone
        onDrop={(event) => {
          for (const item of event.items) if (item.kind === "file") dropped.push(item.name);
        }}
      >
        <Text slot="label">Drop a file</Text>
      </DropZone>,
    );
    const zone = find(el, ".bc\\:border-dashed");
    const button = find(zone, "button");
    expect(el.ownerDocument.getElementById(button.getAttribute("aria-labelledby")?.split(" ").at(-1) ?? "")?.textContent).toBe("Drop a file");

    await step(() => {
      drop(zone, new File(["a,b\n"], "sales.csv", { type: "text/csv" }));
    });
    // React Aria reads the dropped file in a promise.
    await step(() => undefined);

    expect(dropped).toEqual(["sales.csv"]);
  });
});

describe("the file trigger", () => {
  test("opens the browser's file picker from the button it holds, and hands on the files the user picks", async () => {
    const picked: string[] = [];
    const { el } = await draw(
      <FileTrigger
        acceptedFileTypes={["text/csv"]}
        onSelect={(files) => {
          for (const file of files ?? []) picked.push(file.name);
        }}
      >
        <Button>Choose a file</Button>
      </FileTrigger>,
    );
    const clicked = picking(new File(["a,b\n"], "sales.csv", { type: "text/csv" }));

    await step(() => {
      find(el, "button").click();
    });

    expect([clicked, picked]).toEqual([[{ accept: "text/csv", connected: true }], ["sales.csv"]]);
  });
});

describe("the boxes of the fields and of the controls that their outline alone marks", () => {
  // The colors of the borders that an element and what it holds draw at rest,
  // those of their states aside.
  const borders = (el: Element): string[] =>
    [el, ...el.querySelectorAll("*")].flatMap((e) => e.getAttribute("class")?.split(" ") ?? []).filter((name) => name.startsWith("bc:border-border-"));

  test("draw their borders in border-field: a search field, a select and a field of tokens", async () => {
    const { el } = await draw(
      <>
        <div data-field="search">
          <SearchField label="Find a fruit" />
        </div>
        <div data-field="select">
          <Select label="Fruit" items={fruits}>
            {(fruit) => <SelectItem id={fruit.id}>{fruit.name}</SelectItem>}
          </Select>
        </div>
        <div data-field="tokens">
          <TokenField label="Formula">{() => <Token />}</TokenField>
        </div>
      </>,
    );

    for (const field of ["search", "select", "tokens"]) {
      expect(borders(find(el, `[data-field="${field}"]`))).toEqual(["bc:border-border-field"]);
    }
  });

  test("draw the outlines of a checkbox, a radio button, a switch and a drop zone in border-field, since the outline alone marks each while it is unchecked, off or empty", async () => {
    const { el } = await draw(
      <>
        <div data-field="checkbox">
          <Checkbox>Keep</Checkbox>
        </div>
        <div data-field="radio">
          <RadioGroup label="End">
            <Radio value="head">Head</Radio>
          </RadioGroup>
        </div>
        <div data-field="switch">
          <Switch>On</Switch>
        </div>
        <div data-field="zone">
          <DropZone>
            <Text slot="label">Drop a file</Text>
          </DropZone>
        </div>
      </>,
    );

    for (const field of ["checkbox", "radio", "switch", "zone"]) {
      expect(borders(find(el, `[data-field="${field}"]`))).toEqual(["bc:border-border-field"]);
    }
  });
});
