\name{.v}
\alias{.v}
\title{
Get value of other options
}
\description{
Get value of other options
}
\usage{
.v(opt_name, name_is_character = NA)
}
\arguments{

  \item{opt_name}{name of the option, can be quoted or not quoted.}
  \item{name_is_character}{whether \code{opt_name} is a character, only used internally.}

}
\details{
When setting one option, the value can be dependent on other option names.
The current value of other option can be accessed by \code{v(nm)} or \code{v$nm}.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
opt = set_opt(a = 1, b = function() .v$a*2)
opt$b
opt(a = 2); opt$b
}
