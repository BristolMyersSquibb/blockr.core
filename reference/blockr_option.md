# Blockr Options

Retrieves options via
[`base::getOption()`](https://rdrr.io/r/base/options.html) or
[`base::Sys.getenv()`](https://rdrr.io/r/base/Sys.getenv.html), in that
order, and prefixes the option name passed as `name` with `blockr.` or
`blockr_` respectively. Additionally, the name is converted to lower
case for [`getOption()`](https://rdrr.io/r/base/options.html) and upper
case for environment variables. In case no value is available for a
given `name`, `default` is returned.

## Usage

``` r
blockr_option(name, default)

set_blockr_options(...)
```

## Arguments

- name:

  Option name

- default:

  Default value

- ...:

  Option key value pairs as named arguments

## Value

The value set as option `name` or `default` if not set. In case of the
option being available only as environment variable, the value will be a
string and if available as
[`base::options()`](https://rdrr.io/r/base/options.html) entry it may be
of any R type.

## Examples

``` r
blockr_option("test-example", "default")
#> [1] "default"

options(`blockr.test-example` = "non-default")
blockr_option("test-example", "default")
#> [1] "non-default"

Sys.setenv(`BLOCKR_TEST-EXAMPLE` = "another value")
tryCatch(
  blockr_option("test-example", "default"),
  error = function(e) conditionMessage(e)
)
#> [1] "Conflicting options set for test-example: check environment variable BLOCKR_TEST-EXAMPLE and option blockr.test-example."
options(`blockr.test-example` = NULL)
blockr_option("test-example", "default")
#> [1] "another value"

Sys.unsetenv("BLOCKR_TEST-EXAMPLE")
blockr_option("test-example", "default")
#> [1] "default"
```
