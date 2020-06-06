\name{names.GlobalOptionsFun}
\alias{names.GlobalOptionsFun}
\title{
Option names
}
\description{
Option names
}
\usage{
\method{names}{GlobalOptionsFun}(x)
}
\arguments{

  \item{x}{the option object returned by \code{\link{set_opt}} or \code{\link{setGlobalOptions}}.}

}
\value{
A vector of option names
}
\examples{
opt = set_opt(
    a = 1,
    b = "text"
)
names(opt)
}
