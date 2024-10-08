% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yamlet.R
\name{unnest.default}
\alias{unnest.default}
\title{Collapse Uninformative Levels by Default}
\usage{
\method{unnest}{default}(x, ...)
}
\arguments{
\item{x}{object}
}
\value{
list
}
\description{
The default unnest() method returns the unmodified object.
}
\examples{
unnest(yaml::yaml.load('ITEM:'))
}
\seealso{
Other unnest: 
\code{\link{unnest}()},
\code{\link{unnest.list}()}
}
\concept{unnest}
\keyword{internal}
