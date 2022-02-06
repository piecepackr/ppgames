#' @importFrom rlang .data
#' @importFrom utils packageVersion
NULL

hasName <- function(x, name) match(name, names(x), nomatch = 0L) > 0L
