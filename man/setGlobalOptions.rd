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
  \item{...}{see 'details' section}

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
  And users can reset their default values by:  

  \preformatted{
  foo.options(RESET = TRUE)
  }
  The value for each option can be set as a list which may contains more control of the options:  

  \preformatted{
  foo.options = setGlobalOptions(
      "a" = list(.value = 1,
                 .class = "numeric",
                 .validate = function(x) x > 0),
      "b" = "text"
  )
  }
  \code{.class} and \code{.validate} will be used to check users' input. Please note \code{.validate} function should only returns a logical value. 


}
