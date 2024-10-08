% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_table.R
\name{io_table}
\alias{io_table}
\title{Import and Export Documented Tables}
\usage{
io_table(x, ...)
}
\arguments{
\item{x}{object}

\item{...}{passed arguments}
}
\value{
See methods.
}
\description{
Imports or exports documented tables.  Generic, with methods
that extend \code{\link{read.table}} and \code{\link{write.table}}.
}
\examples{
# generate some decorated data
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)

# get a temporary filepath
out <- file.path(tempdir(), 'out.tab')

# save file using io_table (returns filepath)
foo <- io_table(x, out)
stopifnot(identical(out, foo))

# read using this filepath
y <- io_table(foo, as.is = TRUE)

# lossless round-trip
attr(x, 'source') <- NULL
rownames(x) <- NULL
rownames(y) <- NULL
stopifnot(identical(x, y))
}
\seealso{
Other io: 
\code{\link{io_csv}()},
\code{\link{io_csv.character}()},
\code{\link{io_csv.data.frame}()},
\code{\link{io_res}()},
\code{\link{io_res.character}()},
\code{\link{io_res.decorated}()},
\code{\link{io_table.character}()},
\code{\link{io_table.data.frame}()},
\code{\link{io_yamlet}()},
\code{\link{io_yamlet.character}()},
\code{\link{io_yamlet.data.frame}()},
\code{\link{io_yamlet.yamlet}()}
}
\concept{io}
