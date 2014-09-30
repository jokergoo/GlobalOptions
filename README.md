### Produce a function which can get or set global options

The most simple way is to construct an option function (e.g. `foo.options()`) as:

```r
library(GlobalOptions)
foo.options = setGlobalOptions(
    "a" = 1,
    "b" = "text"
)
```

Then users can get or set the options by 

```r
foo.options()
foo.options("a")
foo.options(c("a", "b"))
foo.options("a", "b")
foo.options("a" = 2)
foo.options("a" = 2, "b" = "new_text")
```

Users can reset their default values by:

```r
foo.options(RESET = TRUE)
```

The value for each option can be set as a list which may contain more controls on the option:

```r
foo.options = setGlobalOptions(
    "a" = list(.value = 1,
               .length = 1,
               .class = "numeric",
               .validate = function(x) x > 0,
               .filter = function(x) ifelse(x > 10, 10, x),
               .visible = TRUE,
               .private = FALSE),
    "b" = "text"
)
```

Please see the vignette to get more detailed explanation.
