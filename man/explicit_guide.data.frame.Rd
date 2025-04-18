% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explicit_guide.R
\name{explicit_guide.data.frame}
\alias{explicit_guide.data.frame}
\title{Coerce Data Frame Guide to Something More Explicit}
\usage{
\method{explicit_guide}{data.frame}(
  x,
  ...,
  overwrite = getOption("yamlet_explicit_guide_overwrite", TRUE),
  simplify = getOption("yamlet_explicit_guide_simplify", TRUE),
  expand = getOption("yamlet_expand_codelist", TRUE)
)
}
\arguments{
\item{x}{data.frame}

\item{...}{named arguments passed to \code{\link{as_yamlet}}, \code{\link{explicit_guide}}, and \code{\link{decorate}}; un-named arguments limit scope}

\item{overwrite}{passed as TRUE}

\item{simplify}{whether to remove guide attribute}

\item{expand}{whether to expand empty guide list using sorted unique values. NA likely excluded.}
}
\value{
data.frame
}
\description{
Coerces data.frame 'guide' attributes to something more explicit.
The attribute 'guide' generally suggests a guide
to interpretation of a data item, such as units, formats, codelists,
and encodings.  The idea here is to replace 'guide' with something
explicit in case required downstream.
}
\details{
This method pulls the 'decorations' off of the data.frame,
converts to yamlet, applies \code{\link{explicit_guide.yamlet}},
purges 'guide' attributes from the data.frame,
and then re-decorates using \code{overwrite = TRUE}.
}
\examples{
library(magrittr)
x <- data.frame(
 ID = 1,
 CONC = 1,
 RACE = 1,
 SEX = 1,
 DATE = 1
)
x \%<>\% modify(ID, label = 'subject identifier')
x \%<>\% modify(CONC, label = 'concentration', guide = 'ng/mL')
x \%<>\% modify(RACE, label = 'race', guide = list(white = 0, black = 1, asian = 2))
x \%<>\% modify(SEX, label = 'sex', guide = list(female = 0, male = 1))
x \%<>\% modify(DATE, label  = 'date', guide = '\%Y-\%m-\%d')
x \%>\% decorations
x \%>\% explicit_guide \%>\% decorations
x \%>\% explicit_guide(DATE) \%>\% decorations # limit scope
}
\seealso{
Other explicit_guide: 
\code{\link{explicit_guide}()},
\code{\link{explicit_guide.dvec}()},
\code{\link{explicit_guide.yamlet}()},
\code{\link{implicit_guide}()},
\code{\link{implicit_guide.data.frame}()},
\code{\link{implicit_guide.dvec}()},
\code{\link{infer_guide}()}

Other dvec: 
\code{\link{[.dvec}()},
\code{\link{[<-.dvec}()},
\code{\link{[[.dvec}()},
\code{\link{[[<-.dvec}()},
\code{\link{as.data.frame.dvec}()},
\code{\link{as_dvec}()},
\code{\link{as_dvec.character}()},
\code{\link{as_dvec.complex}()},
\code{\link{as_dvec.dvec}()},
\code{\link{as_dvec.integer}()},
\code{\link{as_dvec.logical}()},
\code{\link{as_dvec.numeric}()},
\code{\link{c.dvec}()},
\code{\link{classified.dvec}()},
\code{\link{desolve.dvec}()},
\code{\link{format.dvec}()},
\code{\link{implicit_guide.dvec}()},
\code{\link{length.dvec}()},
\code{\link{print.dvec}()},
\code{\link{resolve.classified}()},
\code{\link{resolve.dvec}()},
\code{\link{unclassified.dvec}()}
}
\concept{dvec}
\concept{explicit_guide}
\keyword{internal}
