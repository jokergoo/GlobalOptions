\name{$<-.GlobalOptionsFun}
\alias{$<-.GlobalOptionsFun}
\title{
Set option value by dollar symbol
}
\description{
Set option value by dollar symbol
}
\usage{
\method{$}{GlobalOptionsFun}(x, nm) <- value
}
\arguments{

  \item{x}{the function returned by \code{\link{setGlobalOptions}}}
  \item{nm}{a single option name}
  \item{value}{the value which is assigned to the option}

}
\details{
\code{opt$a = 1} is same as \code{opt("a" = 1)}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
opt = setGlobalOptions(a = 1)
opt$a = 2
opt$a
}
