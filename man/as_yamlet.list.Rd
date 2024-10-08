% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yamlet.R
\name{as_yamlet.list}
\alias{as_yamlet.list}
\title{Coerce List to Yamlet}
\usage{
\method{as_yamlet}{list}(x, ...)
}
\arguments{
\item{x}{list}

\item{...}{ignored}
}
\value{
yamlet
}
\description{
Coerces list to yamlet. Assigns class 'yamlet'.
Checks that list has names.
}
\examples{
x <- list(a = 2, b = 3)
as_yamlet(x)
}
\seealso{
Other yamlet: 
\code{\link{[.yamlet}()},
\code{\link{as.character.yamlet}()},
\code{\link{as_yamlet}()},
\code{\link{as_yamlet.character}()},
\code{\link{as_yamlet.data.frame}()},
\code{\link{as_yamlet.yam}()},
\code{\link{as_yamlet.yamlet}()},
\code{\link{print.yamlet}()}
}
\concept{yamlet}
\keyword{internal}
