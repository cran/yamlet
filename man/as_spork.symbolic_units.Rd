% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spork.R
\name{as_spork.symbolic_units}
\alias{as_spork.symbolic_units}
\title{Coerce Symbolic Units to Spork}
\usage{
\method{as_spork}{symbolic_units}(x, canonical = TRUE, ...)
}
\arguments{
\item{x}{symbolic_units; see \code{\link[units]{as_units}}}

\item{...}{ignored arguments}
}
\value{
spork
}
\description{
Coerces symbolic units to spork by coercing first
to unit_string.
}
\examples{
library(units)
library(spork)
x <- as_units('kg.m/s^2')
names(attributes(x))
y <- attr(x,'units')
class(y)
as.character(y)
as.character(attr(x, 'units'))
as_spork(y)
library(magrittr)
'kg.m^2/s^2' \%>\% as_units \%>\% attr('units') \%>\% as_spork
'kg.m2 s-2' \%>\% as_units \%>\% attr('units') \%>\% as_spork
'kg.m^2/s^2' \%>\% as_units \%>\% attr('units') \%>\% as_spork(FALSE)
'kg.m2 s-2' \%>\% as_units \%>\% attr('units') \%>\% as_spork(FALSE)
}
\seealso{
Other spork: 
\code{\link{as_spork.unit_string}()},
\code{\link{as_spork.units}()}
}
\concept{spork}
\keyword{internal}
