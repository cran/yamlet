% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/singularity.R
\name{promote.data.frame}
\alias{promote.data.frame}
\title{Promote Data Frame}
\usage{
\method{promote}{data.frame}(x, ...)
}
\arguments{
\item{x}{data.frame}

\item{...}{passed arguments}
}
\value{
same class as x
}
\description{
Promotes a data.frame by calling \code{\link{promote.list}}.
}
\examples{
example(promote.list)
}
\seealso{
Other promote: 
\code{\link{[.decorated}()},
\code{\link{filter.decorated}()},
\code{\link{promote}()},
\code{\link{promote.list}()},
\code{\link{singularity}()}
}
\concept{promote}
\keyword{internal}
