### Produce a function which can get or set global options

The most simple way is to construct an option function (e.g. `foo.options()`) as:

```r    
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

And users can reset their default values by:

```r
foo.options(RESET = TRUE)
```

The value for each option can be set as a list which may contain more control on the options:

```r
foo.options = setGlobalOptions(
    "a" = list(.value = 1,
               .class = "numeric",
               .validate = function(x) x > 0),
    "b" = "text"
)
```

`.class` and `.validate` will be used to check users' input. Please note `.validate` function should only returns a logical value.
