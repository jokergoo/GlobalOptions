[![ Status](https://travis-ci.org/jokergoo/GlobalOptions.svg)](https://travis-ci.org/jokergoo/GlobalOptions) [![CRAN](http://www.r-pkg.org/badges/version/GlobalOptions)](https://cran.r-project.org/web/packages/GlobalOptions/index.html) [![codecov](https://img.shields.io/codecov/c/github/jokergoo/GlobalOptions.svg)](https://codecov.io/github/jokergoo/GlobalOptions) 

## GlobalOptions

This package aims to provide a simple way to handle global configurations. It can:

1. validate the values (e.g. class, length and self-defined validations);
2. set read-only options;
3. set invisible options;
4. set private options which are only accessable in a certain namespace.
5. support local options and global options

### Usage

The most simple way is to construct an option function (e.g. `foo_opt()`) as:

```r
library(GlobalOptions)
foo_opt = setGlobalOptions(
    "a" = 1,
    "b" = "text"
)
```

Then users can get or set the options by:

```r
foo_opt()
foo_opt("a")
foo_opt$a
foo_opt(c("a", "b"))
foo_opt("a", "b")
foo_opt("a" = 2)
foo_opt$a = 2
foo_opt("a" = 2, "b" = "new_text")
```

Users can reset their default values by:

```r
foo_opt(RESET = TRUE)
```

### Advanced control on options

The value for each option can be set as a list which may contain more controls on the option:

```r
foo_opt = setGlobalOptions(
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

### Dynamic option values

If the value of the option is set as a function and the class of the option is non-function.
The function will be executed everytime when querying the option. In following example, the
`prefix` option controls the prefix of the log message.

```r
foo_opt = setGlobalOptions(
    prefix = list(.value = function() paste("[", Sys.time(), "] ", sep = " "),
                  .class = "character")
)
```

Then `foo_opt("prefix")` will return the current time:

```r
foo_opt("prefix")

## [1] "[ 2015-08-18 17:49:06 ] "

Sys.sleep(2)
foo_opt("prefix")

## [1] "[ 2015-08-18 17:49:08 ] "
```

### Local options

Local options can be created by specifying `LOCAL` to `TRUE`. The local mode will end when
`LOCAL` is set to `FALSE` explicitely or the environment changes.

```{r}
opt = setGlobalOptions(
    a = 1
)

f1 = function() {
    opt(LOCAL = TRUE)
    opt(a = 2)
    return(f2())
}
```

### License

GPL (>= 2)
