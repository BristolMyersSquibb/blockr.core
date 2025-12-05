# 3. Browse for blocks

``` r
library(blockr.core)
```

## Introduction

The **registry** is a **environment** which provides access to multiple
**blocks** as well as some metadata:

- The block **description**.
- The block **category**.
- The block **package**.
- …

In other words, the **registry** is a **“supermarket”** for data
analysis. As shown below, if you develop your own blocks package and
registers blocks on load, these blocks become available to the end user.
Therefore this makes it powerful for **collaboration** between data
science teams.

``` mermaid
flowchart LR
  subgraph blockr_ggplot2[blockr.ggplot2]
    new_block1[New block]
    new_block2[New block]
  end
  subgraph blockr_echarts4r[blockr.echarts4r]
    new_block3[New block]
    new_block4[New block]
  end
  blockr_ggplot2 --> |register| registry
  blockr_echarts4r --> |register| registry
  subgraph registry[Registry]
    subgraph select_reg[Select block]
      reg_name[Name: select block]
      reg_descr[Description: select columns in a table]
      reg_classes[Classes: select_block, tranform_block]
      reg_category[Category: transform]
      reg_ctor[Construcor: new_select_block]
      reg_package[Package: blockr.dplyr]
    end
    subgraph filter_reg[Filter block]
    end
    filter_reg --x |unregister| trash['fa:fa-trash']
  end
```

## Previewing available blocks

Upon loading, `{blockr}` **registers** its internal **blocks** with
`register_blockr_blocks()`. You won’t have to call this function as it
is not exported anyway. This makes the **registry** environment ready to
be queried by
[`available_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md).
A truncated output example below:

``` r
available_blocks()[["dataset_block"]]
```

``` r
function(dataset = character(), package = "datasets", ...) {
  ...
}
<environment: namespace:blockr.core>
attr(,"name")
[1] "dataset block"
attr(,"description")
[1] "Choose a dataset from a package"
attr(,"classes")
[1] "dataset_block" "data_block"    "block"         "vctrs_vctr"    "list"         
attr(,"category")
[1] "data"
attr(,"ctor_name")
[1] "new_dataset_block"
attr(,"package")
[1] "blockr.core"
attr(,"class")
[1] "block_registry_entry"
```

``` r
names(available_blocks())
#>  [1] "csv_block"         "dataset_block"     "filebrowser_block"
#>  [4] "glue_block"        "head_block"        "merge_block"      
#>  [7] "rbind_block"       "scatter_block"     "static_block"     
#> [10] "subset_block"      "upload_block"
```

## Register a block

To register your own blocks, user facing functions are:

- [`register_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
  to register a block in the **registry**. If the **block** is already
  registered, it **overwrites** the existing one.
- [`register_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
  to register multiple blocks.

Note that, if you develop your block outside a package context, you must
call `register_block` (or `register_blocks`) passing the constructor as
a function and not as a string (see below).

Let’s say you want to create a new `new_dummy_block` which does nothing
specific in the `./R/dummy-block.R` script:

``` r
# ./R/dummy-block.R
new_dummy_block <- function(text = "Hello World", ...) {
  new_data_block(
    function(id) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive(quote(text)),
          state = list(text = text)
        )
      })
    },
    function(id) {
      shiny::tagList()
    },
    class = "dummy_block",
    ...
  )
}

register_dummy_blocks <- function() {
  register_blocks(
    c(new_dummy_block),
    name = c("dummy block"),
    description = c("A block that does nothing"),
    overwrite = TRUE
  )
}

register_dummy_blocks()
```

Finally, we create a `.R/zzz.R` script where you run this code to
register the block(s) whenever the package loads:

``` r
# ./R/zzz.R
.onLoad <- function(libname, pkgname) {
  register_dummy_blocks()
  invisible(NULL)
}
```

If we now query the registry, the new block is available:

``` r
available_blocks()[["dummy_block"]]
#> function (text = "Hello World", ...) 
#> {
#>     new_data_block(function(id) {
#>         moduleServer(id, function(input, output, session) {
#>             list(expr = reactive(quote(text)), state = list(text = text))
#>         })
#>     }, function(id) {
#>         shiny::tagList()
#>     }, class = "dummy_block", ...)
#> }
#> attr(,"name")
#> [1] "dummy block"
#> attr(,"description")
#> [1] "A block that does nothing"
#> attr(,"classes")
#> [1] "dummy_block" "data_block"  "block"       "vctrs_vctr"  "list"       
#> attr(,"category")
#> [1] "uncategorized"
#> attr(,"icon")
#> [1] "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16 16\" class=\"bi bi-question-square \" style=\"height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;\" aria-hidden=\"true\" role=\"img\" ><path d=\"M14 1a1 1 0 0 1 1 1v12a1 1 0 0 1-1 1H2a1 1 0 0 1-1-1V2a1 1 0 0 1 1-1h12zM2 0a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V2a2 2 0 0 0-2-2H2z\"></path>\n<path d=\"M5.255 5.786a.237.237 0 0 0 .241.247h.825c.138 0 .248-.113.266-.25.09-.656.54-1.134 1.342-1.134.686 0 1.314.343 1.314 1.168 0 .635-.374.927-.965 1.371-.673.489-1.206 1.06-1.168 1.987l.003.217a.25.25 0 0 0 .25.246h.811a.25.25 0 0 0 .25-.25v-.105c0-.718.273-.927 1.01-1.486.609-.463 1.244-.977 1.244-2.056 0-1.511-1.276-2.241-2.673-2.241-1.267 0-2.655.59-2.75 2.286zm1.557 5.763c0 .533.425.927 1.01.927.609 0 1.028-.394 1.028-.927 0-.552-.42-.94-1.029-.94-.584 0-1.009.388-1.009.94z\"></path></svg>"
#> attr(,"class")
#> [1] "block_registry_entry"
```

## Unregister a block

The counterpart of
[`register_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
is
[`unregister_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md).
We can remove our new `dummy_block` from the registry:

``` r
unregister_blocks(uid = "dummy_block")

# Check it out
names(available_blocks())
#>  [1] "csv_block"         "dataset_block"     "filebrowser_block"
#>  [4] "glue_block"        "head_block"        "merge_block"      
#>  [7] "rbind_block"       "scatter_block"     "static_block"     
#> [10] "subset_block"      "upload_block"
```

where **uid** is/are the block(s) class(s) passed in the constructor.
