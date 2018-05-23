\name{[[<-.GlobalOptionsFun}
\alias{[[<-.GlobalOptionsFun}
\title{
Set option value by subset operator
}
\description{
Set option value by subset operator
}
\usage{
\method{[[}{GlobalOptionsFun}(x, nm) <- value
}
\arguments{

  \item{x}{the option object returned by \code{\link{set_opt}} or \code{\link{setGlobalOptions}}.}
  \item{nm}{a single option name.}
  \item{value}{the value which is assigned to the option.}

}
\details{
\code{opt[["a"]] = 1} is same as \code{opt("a" = 1)} or \code{opt$a = 1}.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
opt = set_opt(a = 1)
opt[["a"]] = 2
opt$a
}
