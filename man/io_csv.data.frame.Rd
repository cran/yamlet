% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_csv.R
\name{io_csv.data.frame}
\alias{io_csv.data.frame}
\title{Export Documented Table as CSV}
\usage{
\method{io_csv}{data.frame}(
  x,
  file = "",
  ext = getOption("yamlet_extension", ".yaml"),
  meta = stdout(),
  gz = NULL,
  useBytes = FALSE,
  default_keys = getOption("yamlet_default_keys", list("label", "guide")),
  ...
)
}
\arguments{
\item{x}{data.frame}

\item{file}{passed to \code{\link[csv]{as.csv.data.frame}} (by method dispatch)}

\item{ext}{= extension for metadata equivalent of x}

\item{meta}{passed as \code{con} to \code{\link{io_yamlet}}}

\item{gz}{logical; guessed by default from \code{x}; if TRUE, '.gz' extension enforced present for (character) \code{file} and absent for default \code{meta}}

\item{useBytes}{passed to \code{\link{io_yamlet}}}

\item{default_keys}{passed to \code{\link{io_yamlet}}}

\item{...}{passed to \code{\link[csv]{as.csv}} and to \code{\link{io_yamlet}}}
}
\value{
invisible(file)
}
\description{
Exports a data.frame as comma-separated variable,
as well as a yamlet version of its decorations.
A wrapper for \code{\link[csv]{as.csv.data.frame}}.
}
\details{
You should be able to supply exactly the connections you want for 
\code{file} (the data file) and \code{meta} (the metadata file)
if \code{gz} is FALSE. If \code{gz} is NULL, it will be 
guessed from file (TRUE for character ending with '.gz' or '.GZ').
If TRUE, character \code{file} will have '.gz' extension enforced,
but any '.gz' ('.GZ') will be stripped when calculating \code{meta}.
}
\examples{
example(io_csv)
}
\seealso{
Other io: 
\code{\link{io_csv}()},
\code{\link{io_csv.character}()},
\code{\link{io_res}()},
\code{\link{io_res.character}()},
\code{\link{io_res.decorated}()},
\code{\link{io_table}()},
\code{\link{io_table.character}()},
\code{\link{io_table.data.frame}()},
\code{\link{io_yamlet}()},
\code{\link{io_yamlet.character}()},
\code{\link{io_yamlet.data.frame}()},
\code{\link{io_yamlet.yamlet}()}

Other interface: 
\code{\link{canonical.decorated}()},
\code{\link{classified.data.frame}()},
\code{\link{decorate.character}()},
\code{\link{decorate.data.frame}()},
\code{\link{desolve.decorated}()},
\code{\link{enscript.default}()},
\code{\link{ggplot.decorated}()},
\code{\link{io_csv.character}()},
\code{\link{io_res.character}()},
\code{\link{io_res.decorated}()},
\code{\link{io_table.character}()},
\code{\link{io_table.data.frame}()},
\code{\link{io_yamlet.character}()},
\code{\link{io_yamlet.data.frame}()},
\code{\link{is_parseable.default}()},
\code{\link{mimic.default}()},
\code{\link{modify.default}()},
\code{\link{promote.list}()},
\code{\link{read_yamlet}()},
\code{\link{resolve.decorated}()},
\code{\link{selected.default}()},
\code{\link{write_yamlet}()}
}
\concept{interface}
\concept{io}
\keyword{internal}
