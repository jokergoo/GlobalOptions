\name{[[.GlobalOptionsFun}
\alias{[[.GlobalOptionsFun}
\title{
Get option value by subset operator
}
\description{
Get option value by subset operator
}
\usage{
\method{[[}{GlobalOptionsFun}(x, nm)
}
\arguments{

  \item{x}{the option object returned by \code{\link{set_opt}} or \code{\link{setGlobalOptions}}.}
  \item{nm}{a single option name.}

}
\details{
\code{opt[["a"]]} is same as \code{opt("a")} or \code{opt$a}.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
opt = set_opt(a = 1)
opt[["a"]]
}
