# GlobalOptions

[![R-CMD-check](https://github.com/jokergoo/GlobalOptions/workflows/R-CMD-check/badge.svg)](https://github.com/jokergoo/GlobalOptions/actions)
[![CRAN](http://www.r-pkg.org/badges/version/GlobalOptions)](https://cran.r-project.org/web/packages/GlobalOptions/index.html) 
[![codecov](https://img.shields.io/codecov/c/github/jokergoo/GlobalOptions.svg)](https://codecov.io/github/jokergoo/GlobalOptions) 


This package aims to provide a simple way to handle global configurations. It can:

1. validate the values (e.g. class, length and self-defined validations);
2. set read-only options;
3. set invisible options;
4. set private options which are only accessable in a certain namespace.
5. support local options and global options

There is a [vignette](https://cran.r-project.org/web/packages/GlobalOptions/vignettes/GlobalOptions.html) in 
the package which explains with more detail.


## Usage

The most simple way is to construct an option function (e.g. `opt()`) as:

```r
library(GlobalOptions)
opt = set_opt(
    "a" = 1,
    "b" = "text"
)
```

Then users can get or set the options by:

```r
opt()
opt("a")
opt$a
opt[["a"]]
opt(c("a", "b"))
opt("a", "b")
opt("a" = 2)
opt$a = 2
opt("a" = 2, "b" = "new_text")
```

Users can reset their default values by:

```r
opt(RESET = TRUE)
```

## Advanced control on options

The value for each option can be set as a list which may contain more configurations on the option:

```r
opt = set_opt(
    "a" = list(.value = 1,
               .length = 1,
               .class = "numeric",
               .validate = function(x) x > 0,
               .failed_msg = "'a' should be a positive number.",
               .filter = function(x) ifelse(x > 10, 10, x),
               .read.only = FALSE,
               .visible = TRUE,
               .private = FALSE),
    "b" = "text"
)
```

In above example, option value for `a` should pass following conditions:

1. length should be 1;
2. should have `numeric` class;
3. should be positive;
4. if the value is larger than 10, it will be enforced to 10.

Other fields mean:

1. default value of `a` is 1;
2. it is not read-only;
3. it is visible;
4. it is public.

## Dynamic option values

If the value of the option is set as a function and the class of the option is non-function.
The function will be executed everytime when querying the option. In following example, the
`prefix` option controls the prefix of the log message.

```r
opt = set_opt(
    prefix = list(.value = function() paste("[", Sys.time(), "] ", sep = " "),
                  .class = "character")
)
```

Then `opt("prefix")` will return the current time:

```r
opt("prefix")

## [1] "[ 2015-08-18 17:49:06 ] "

Sys.sleep(2)
opt("prefix")

## [1] "[ 2015-08-18 17:49:08 ] "
```

## Interact between options

One option value can depend on other option values and the value of the option changes
when the value of the dependent option changes.

```r
opt = set_opt(
    a = 1,
    b = function() .v$a * 2
)
opt()

## $a
## [1] 1
## 
## $b
## [1] 2

opt(a = 2); opt()

## $a
## [1] 2
## 
## $b
## [1] 4

```

## Local options

Local options can be created by specifying `LOCAL` to `TRUE`. The local mode will end when
`LOCAL` is set to `FALSE` explicitely or the environment changes.

```r
opt = set_opt(
    a = 1
)

f1 = function() {
    opt(LOCAL = TRUE)
    opt(a = 2)
    return(f2())
}
f1()

## [1] 2

opt("a")

## [1] 1

```

## License

MIT @ Zuguang Gu
