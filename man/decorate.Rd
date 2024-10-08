% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decorate.R
\name{decorate}
\alias{decorate}
\title{Decorate a List-like Object}
\usage{
decorate(x, ...)
}
\arguments{
\item{x}{object}

\item{...}{passed arguments}
}
\value{
a list-like object, typically data.frame
}
\description{
Decorates a list-like object. Generic.
See \code{\link{decorate.character}}.
}
\examples{
library(csv)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(as.csv(file))
identical(decorate(as.csv(file)), decorate(file))
decorations(x)


}
\seealso{
Other decorate: 
\code{\link{as_decorated}()},
\code{\link{as_decorated.default}()},
\code{\link{decorate.character}()},
\code{\link{decorate.data.frame}()},
\code{\link{decorate.list}()},
\code{\link{decorate_groups}()},
\code{\link{decorate_groups.data.frame}()},
\code{\link{decorations}()},
\code{\link{decorations.data.frame}()},
\code{\link{decorations_groups}()},
\code{\link{decorations_groups.data.frame}()},
\code{\link{group_by_decorations}()},
\code{\link{group_by_decorations.data.frame}()},
\code{\link{redecorate}()}
}
\concept{decorate}
\keyword{internal}
