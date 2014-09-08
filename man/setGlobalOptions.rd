\name{setGlobalOptions}
\alias{setGlobalOptions}
\title{
  Produce a function which can get or set global options  


}
\description{
  Produce a function which can get or set global options  


}
\usage{
setGlobalOptions(...)
}
\arguments{
  \item{...}{specification of options, see 'details' section}

}
\details{
  The most simple way is to construct an option function (e.g. \code{foo.options()}) as:  

  \preformatted{
  foo.options = setGlobalOptions(
      "a" = 1,
      "b" = "text"
  )
  }
  Then users can get or set the options by   

  \preformatted{
  foo.options()
  foo.options("a")
  foo.options(c("a", "b"))
  foo.options("a", "b")
  foo.options("a" = 2)
  foo.options("a" = 2, "b" = "new_text")
  }
  Options can be reset to their default values by:  

  \preformatted{
  foo.options(RESET = TRUE)
  }
  The value for each option can be set as a list which may contain more control of the option:  

  \preformatted{
  foo.options = setGlobalOptions(
      "a" = list(.value = 1,
                 .length = 1,
                 .class = "numeric",
                 .validate = function(x) x > 0),
  )
  }
  \code{.length}, \code{.class} and \code{.validate} will be used to check users' input. Please note \code{.validate} function should only returns a logical value.  

  For more detailed explanation, please go to the vignette.  


}
