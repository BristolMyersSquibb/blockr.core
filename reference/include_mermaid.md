# Include a pre-rendered mermaid diagram

Renders mermaid `.mmd` source to `.svg` when the source is newer than
the existing SVG and `mmdc` is available, then includes the SVG via
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html).
The `MMDC_PUPPETEER_CONFIG` environment variable can point to a
puppeteer config file passed to `mmdc`.

## Usage

``` r
include_mermaid(name)
```

## Arguments

- name:

  Diagram name (without extension), resolved relative to a `mermaid/`
  directory alongside the calling document.
