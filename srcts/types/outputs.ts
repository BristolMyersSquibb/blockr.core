// The contracts of core's output classes, which every renderer of a class is
// written against. An output is what an output class's coercion makes in R
// from a block's result. R writes it as JSON by the JSON Schema that the build
// generates from these types into inst/outputs, and names in it the renderer
// that draws it. A subclass's type extends its class's, as its class vector
// does in R.

import type { Json } from "./json.ts";

// What every output holds, whatever its class.
export interface BlockOutput {
  // The renderer that R picked for the output's class, which R adds to every
  // output it sends.
  renderer: string;
}

// A table output: a table's columns, the rows of one page and the total row
// count. Its board option is n_rows, the rows of a page, which every renderer
// of table outputs brings.
export interface TableOutput extends BlockOutput {
  // In R's order, so that names such as "2019" and duplicates keep their
  // places.
  columns: TableColumn[];
  // One page of rows, each in the order of columns. A cell holds the value as
  // JSON has it, a factor's label, the text R formats another class of value
  // with, and for a list-column the summary of the cell, such as
  // "<lm>" or "<data.frame> [3 × 2]".
  rows: Json[][];
  // The total row count.
  total: number;
}

export interface TableColumn {
  name: string;
  // The column's first class in R, such as "numeric", "factor" or "list".
  class: string;
  // The column's label attribute, where it has one, as ADaM data carry.
  label?: string;
}

// The parameters that a renderer of table outputs keeps and the coercion reads.
export interface TableParams {
  // The page to show, 1 unless the renderer pages.
  page?: number;
  // The rows of a page, n_rows unless the renderer sets one.
  page_size?: number;
  sort?: {
    column: string;
    desc?: boolean;
  };
}

// A text output: a result as print() shows it, at R's own width, which the
// renderer scrolls.
export interface TextOutput extends BlockOutput {
  text: string;
}

// A plot output, whose PNG travels as the output's binary body.
export interface PlotOutput extends BlockOutput {
  // The size it was drawn at, from the output's info, in CSS pixels.
  width: number;
  height: number;
}

// The sample outputs of an output class, which every renderer of the class is
// tested on. R writes them, as core's are in inst/outputs/fixtures.
export interface OutputFixtures<T extends BlockOutput = BlockOutput> {
  description: string;
  outputs: OutputFixture<T>[];
}

export interface OutputFixture<T extends BlockOutput = BlockOutput> {
  description: string;
  // The board option values and the parameters the output was asked with.
  options: Record<string, Json>;
  params: Record<string, Json>;
  output: T;
  // The output's binary body, if it has one: its type, as a reply announces
  // it, and the file beside the fixture that holds its bytes.
  body?: { type: string; name?: string };
  body_file?: string;
}
