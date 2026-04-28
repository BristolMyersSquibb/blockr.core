# Include a pre-rendered mermaid diagram

Renders mermaid `.mmd` source to `.svg` when the source is newer than
the existing SVG and `mmdc` is available, then includes the SVG via
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html).

## Usage

``` r
include_mermaid(name, chromium_args = c("--no-sandbox"))
```

## Arguments

- name:

  Diagram name (without extension), resolved relative to a `mermaid/`
  directory alongside the calling document.

- chromium_args:

  Character vector of extra Chromium flags passed to `mmdc` via a
  temporary puppeteer config file.
