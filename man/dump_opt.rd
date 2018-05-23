\name{dump_opt}
\alias{dump_opt}
\title{
Print all fields of a single option
}
\description{
Print all fields of a single option
}
\usage{
dump_opt(opt, opt_name)
}
\arguments{

  \item{opt}{the option object returned by \code{\link{set_opt}} or \code{\link{setGlobalOptions}}.}
  \item{opt_name}{a single name of the option.}

}
\details{
Actually this function is identical to \code{opt[opt_name]}.
}
\author{
z.gu@dkfz.de
}
\examples{
opt = set_opt(a = 1, b = "b")
dump_opt(opt, "a")
dump_opt(opt, "b")
}
