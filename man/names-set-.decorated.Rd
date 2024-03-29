% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decorated.R
\name{names<-.decorated}
\alias{names<-.decorated}
\title{Assign Names of Decorated}
\usage{
\method{names}{decorated}(x) <- value
}
\arguments{
\item{x}{decorated}

\item{value}{passed to next method}
}
\value{
decorated
}
\description{
Assigns names of decorated, retaining attributes.
}
\examples{
a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
a[[2]]
names(a[[2]]) <- 'c'
class(a)

}
\seealso{
Other decorated: 
\code{\link{[.decorated}()},
\code{\link{[<-.decorated}()},
\code{\link{[[.decorated}()},
\code{\link{[[<-.decorated}()},
\code{\link{merge.decorated}()}
}
\concept{decorated}
\keyword{internal}
