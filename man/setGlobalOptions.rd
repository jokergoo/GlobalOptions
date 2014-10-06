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
  The value for each option can be set as a list which contains more controls of the option:  

  \preformatted{
  foo.options = setGlobalOptions(
      "a" = list(.value = 1,
                 .length = 1,
                 .class = "numeric",
                 .validate = function(x) x > 0)
  )
  }
  The different fields in the list can be used to filter or validate the option values.  

\describe{
  \item{.value}{The default value.}
  \item{.length}{The valid length of the option value. It can be a vector, the check will be passed if one of the length fits.}
  \item{.class}{The valid class of the option value. It can be a vector, the check will be passed if one of the classes fits.}
  \item{.validate}{Validation function. The input parameter is the option value and should return a single logical value.}
  \item{.filter}{Filtering function. The input parameter is the option value and it should return a filtered option value.}
  \item{.read.only}{Logical. The option value can not be modified if it is set to \code{TRUE}.}
  \item{.visible}{Logical. Whether the option is visible to users.}
  \item{.private}{Logical. The option value can only be modified in the same namespace where the option function is created.}
}
  For more detailed explanation, please go to the vignette.  


}
