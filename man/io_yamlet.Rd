% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_yamlet.R
\name{io_yamlet}
\alias{io_yamlet}
\title{Import and Export Yamlet}
\usage{
io_yamlet(x, ...)
}
\arguments{
\item{x}{object}

\item{...}{passed arguments}
}
\value{
see methods
}
\description{
Imports and exports yamlet.
Generic, with a read method \code{\link{io_yamlet.character}}
for character and a write method \code{\link{io_yamlet.data.frame}}
for data.frame. See also \code{\link{io_yamlet.yamlet}}.
}
\examples{
file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
x <- io_yamlet(file)
tmp <- tempdir()
out <- file.path(tmp, 'tmp.yaml')

# we can losslessly 'round-trip' x using to generic calls
identical(x, io_yamlet(io_yamlet(x, out)))
}
\seealso{
Other io: 
\code{\link{io_csv}()},
\code{\link{io_csv.character}()},
\code{\link{io_csv.data.frame}()},
\code{\link{io_res}()},
\code{\link{io_res.character}()},
\code{\link{io_res.decorated}()},
\code{\link{io_table}()},
\code{\link{io_table.character}()},
\code{\link{io_table.data.frame}()},
\code{\link{io_yamlet.character}()},
\code{\link{io_yamlet.data.frame}()},
\code{\link{io_yamlet.yamlet}()}
}
\concept{io}
\keyword{internal}
