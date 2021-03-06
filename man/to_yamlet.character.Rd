% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yamlet.R
\name{to_yamlet.character}
\alias{to_yamlet.character}
\title{Coerce Character to Yamlet Storage Format}
\usage{
\method{to_yamlet}{character}(x, ...)
}
\arguments{
\item{x}{character}

\item{...}{ignored}
}
\value{
length-one character
}
\description{
Coerces character to yamlet storage format.
Named character is processed as a named list.
NA_character is treated as the string 'NA'.
}
\examples{
to_yamlet('foo')
to_yamlet(c('a','b'))
to_yamlet(c(a = 'a',b = 'b'))
to_yamlet(c(no = 'n', yes = 'y'))
to_yamlet(NA)
}
\seealso{
Other to_yamlet: 
\code{\link{to_yamlet.NULL}()},
\code{\link{to_yamlet.default}()},
\code{\link{to_yamlet.list}()},
\code{\link{to_yamlet}()}
}
\concept{to_yamlet}
\keyword{internal}
