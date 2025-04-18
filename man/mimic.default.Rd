% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mimic.R
\name{mimic.default}
\alias{mimic.default}
\title{Try To Look Like Another Equal-length Variable}
\usage{
\method{mimic}{default}(x, y = x, ...)
}
\arguments{
\item{x}{vector-like}

\item{y}{vector-like, same length as x}

\item{...}{passed to \code{link{factor}}}
}
\value{
same class as x
}
\description{
Tries to mimic another vector or factor.
If meaningful and possible, x acquires
a guide attribute with labels from
corresponding values in y. Any codelist
attribute is removed. No guide is created
for zero-length x. If x is a factor,
unused levels are removed.
}
\examples{
library(magrittr)
library(dplyr)
let <- letters[1:5]
LET <- LETTERS[1:5]
int <- 0L:4L
num <- as.numeric(int)
fac <- factor(let)
css <- classified(let)

# any of these can mimic any other
str(mimic(LET, let))
str(mimic(num, let))
str(mimic(let, num))

# factors get a guide and classifieds get a named codelist
str(mimic(fac, int))
str(mimic(css, int))

# int can 'pick up' the factor levels as guide names
str(mimic(int, css))

# if two variables mean essentially the same thing,
# mimic lets you save space
x <- data.frame(id = 1:2, ID = c('A','B'))
x
x \%<>\% mutate(id = mimic(id, ID)) \%>\% select(-ID)
x
# ID still available, in principle:
x \%>\% as_decorated \%>\% resolve
}
\seealso{
Other mimic: 
\code{\link{mimic}()},
\code{\link{mimic.classified}()}

Other interface: 
\code{\link{canonical.decorated}()},
\code{\link{classified.data.frame}()},
\code{\link{decorate.character}()},
\code{\link{decorate.data.frame}()},
\code{\link{desolve.decorated}()},
\code{\link{enscript.default}()},
\code{\link{ggplot.decorated}()},
\code{\link{io_csv.character}()},
\code{\link{io_csv.data.frame}()},
\code{\link{io_res.character}()},
\code{\link{io_res.decorated}()},
\code{\link{io_table.character}()},
\code{\link{io_table.data.frame}()},
\code{\link{io_yamlet.character}()},
\code{\link{io_yamlet.data.frame}()},
\code{\link{is_parseable.default}()},
\code{\link{modify.default}()},
\code{\link{promote.list}()},
\code{\link{read_yamlet}()},
\code{\link{resolve.decorated}()},
\code{\link{selected.default}()},
\code{\link{write_yamlet}()}
}
\concept{interface}
\concept{mimic}
