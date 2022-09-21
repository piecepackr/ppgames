#' Normalize game/book name(s)
#'
#' `normalize_name()` normalizes game/book name(s).
#'
#' `normalize_name(x, sep = "_") is used to help create R variable names
#' while `normalize_name(x, sep = "-")` is used to help create filenames and LaTeX labels.
#' @param x Game or book name to normalize.
#' @param sep Separator character.
#' @return Character vector of normalized game names.
#' @examples
#'   normalize_name("Fuji-san")
#'   normalize_name("Nine Men's Morris", sep = "-")
#' @export
normalize_name <- function(x, sep = "_") {
    x <- gsub('"|\'|-', "", x) # e.g. The "In" Crowd -> the_in_crowd
    snakecase::to_snake_case(x, sep_out = sep)
}
