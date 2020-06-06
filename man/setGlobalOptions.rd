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
The function has a short name \code{\link{set_opt}}.

The most simple way is to construct an option function (e.g. \code{opt()}) as:

  \preformatted{
    opt = set_opt(
        "a" = 1,
        "b" = "text"
    )  }

Then users can get or set the options by

  \preformatted{
    opt()
    opt("a")
    opt$a
    opt[["a"]]
    opt(c("a", "b"))
    opt("a", "b")
    opt("a" = 2)
    opt$a = 2
    opt[["a"]] = 2
    opt("a" = 2, "b" = "new_text")  }

Options can be reset to their default values by:

  \preformatted{
    opt(RESET = TRUE)  }

The value for each option can be set as a list which contains more configurations of the option:

  \preformatted{
    opt = set_opt(
        "a" = list(.value = 1,
                   .length = 1,
                   .class = "numeric",
                   .validate = function(x) x > 0)
    )  }

The different fields in the list can be used to filter or validate the option values.

\describe{
  \item{\code{.value}}{The default value.}
  \item{\code{.length}}{The valid length of the option value. It can be a vector, the check will be passed if one of the length fits.}
  \item{\code{.class}}{The valid class of the option value. It can be a vector, the check will be passed if one of the classes fits.}
  \item{\code{.validate}}{Validation function. The input parameter is the option value and should return a single logical value.}
  \item{\code{.failed_msg}}{Once validation failed, the error message that is printed.}
  \item{\code{.filter}}{Filtering function. The input parameter is the option value and it should return a filtered option value.}
  \item{\code{.read.only}}{Logical. The option value can not be modified if it is set to \code{TRUE}.}
  \item{\code{.visible}}{Logical. Whether the option is visible to users.}
  \item{\code{.private}}{Logical. The option value can only be modified in the same namespace where the option function is created.}
  \item{\code{.synonymous}}{a single option name which should have been already defined ahead of current option. The option specified will be shared by current option.}
  \item{\code{.description}}{a short text for describing the option. The description is only used when printing the object.}
}

For more detailed explanation, please go to the vignette.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
opt = set_opt(
    a = 1,
    b = "text"
)
opt
# for more examples, please go to the vignette
}
