#' Resolve Guide
#'
#' Resolves implicit usage of default key 'guide' to explicit usage.
#' Generic, with method \code{\link{resolve.decorated}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family resolve
#' @examples
#' example(resolve.decorated)
resolve <- function(x, ...)UseMethod('resolve')

#' Resolve Guide for Decorated
#'
#' Resolves implicit usage of default key 'guide' to
#' explicit usage for decorated class.
#' Simply calls \code{\link{explicit_guide}}
#' followed by \code{\link{classified}}.
#' @param x object
#' @param ... passed to \code{\link{explicit_guide}} and \code{\link{classified}}
#' @export
#' @return decorated
#' @family resolve
#' @family interface
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x %>% resolve %>% decorations(Age, glyco)
#' x %>% resolve(glyco) %>% decorations(Age, glyco)

resolve.decorated <- function(x, ...){
  x <- explicit_guide(x, ...)
  x <- classified(x, ...)
  # class(x) <- union('resolved', class(x))
  x
}
