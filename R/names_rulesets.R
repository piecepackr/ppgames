#' Get names of piecepack games we can generate rulesets for.
#'
#' `names_rulesets()` returns the names of piecepack games we can generate rulesets for.
#' @param book Book name or `NULL` (for all supported rules).
#'             Currently only supports "the historical piecepacker".
#' @seealso [save_ruleset()], [save_pamphlet()], and [save_pocketmod()].
#' @examples
#'   names_rulesets()
#' @export
names_rulesets <- function(book = NULL) {
    if (is.null(book)) {
        names <- list.files(system.file("rules", package = "ppgames"))
        names <- grep(".Rtex$", names, value=TRUE)
        names <- gsub(".Rtex$", "", names)
        names <- names[-grep("alice|seasons|ultima", names)]
        names <- gsub("mens-morris", "men's-morris", names)
        names <- gsub("-", " ", names)
    } else {
        book <- normalize_name(book, sep = "-")
        stopifnot(book == "the-historical-piecepacker")
        names <- c("alquerque",
                   "american checkers",
                   "awithlaknannai mosona",
                   "backgammon",
                   "chaturaji",
                   "cribbage",
                   "four field kono",
                   "international chess",
                   "ludo",
                   "nine men's morris",
                   "tablut",
                   "twelve men's morris",
                   "xiangqi")
    }
    names
}
