\name{$.InternalOptionValue}
\alias{$.InternalOptionValue}
\alias{Subset.InternalOptionValue}
\title{
Get value of other options
}
\description{
Get value of other options
}
\usage{
\method{$}{InternalOptionValue}(x, nm)
}
\arguments{

  \item{x}{should always be \code{.v}}
  \item{nm}{name of the option}

}
\details{
\code{.v$nm} is basically a short version of \code{.v(nm)}.
}
\seealso{
\code{\link[=Dot.v]{.v}}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL
}
