\name{[.GlobalOptionsFun}
\alias{[.GlobalOptionsFun}
\alias{Extract.GlobalOptionsFun}
\title{
Get a single GlobalOption object
}
\description{
Get a single GlobalOption object
}
\usage{
\method{[}{GlobalOptionsFun}(x, nm)
}
\arguments{

  \item{x}{the option object returned by \code{\link{set_opt}} or \code{\link{setGlobalOptions}}.}
  \item{nm}{a single name of the option.}

}
\details{
This function is only used internally.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
opt = set_opt(a = 1, b = "b")
opt["a"]
opt["b"]
}
