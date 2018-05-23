\name{$.GlobalOptionsFun}
\alias{$.GlobalOptionsFun}
\title{
Get option value by dollar symbol
}
\description{
Get option value by dollar symbol
}
\usage{
\method{$}{GlobalOptionsFun}(x, nm)
}
\arguments{

  \item{x}{the object returned by \code{\link{set_opt}} or \code{\link{setGlobalOptions}}.}
  \item{nm}{a single option name.}

}
\details{
\code{opt$a} is same as \code{opt("a")}.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
opt = set_opt(a = 1)
opt$a
}
