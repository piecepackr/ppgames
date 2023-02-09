#' @import R6
GameKit <- R6Class("game_kit",
    public = list(
        initialize = function(cfgs = list(),
                              piecepacks = names(cfgs)) {
            private$cfgs <- cfgs
            private$piecepacks <- piecepacks
        },
        get_piecepacks = function(n_piecepacks = 1) {
            ll <- list()
            for (ii in seq(n_piecepacks)) {
                ll[[paste0("cfg", ii)]] <- private$get_pp_helper(ii)
            }
            ll
        }
    ),
    private = list(cfgs = NULL,
                   piecepacks = NULL,
                   # copies = NULL,
                   # pyramids = NULL,
                   # saucers = NULL,
                   # matchsticks = NULL,
                   # subpacks = NULL,
                   # mirrored = NULL,
                   # dual = NULL,
                   # reversi = NULL,
                   get_pp_helper = function(n) {
                        private$cfgs[[private$piecepacks[n]]]
                   }
    )
)

#' Game Kit R6 object
#'
#' \code{game_kit} creates a game kit R6 object.
#'
#' @param cfgs A named list of [piecepackr::pp_cfg()] configuration list objects.
#'             If `NULL` (default) we will use the "piecepack" configuration from
#'             [piecepackr::game_systems()].
#' @examples
#'   cfg <- piecepackr::game_systems()$dual_piecepacks_expansion
#'   gk <- game_kit(list(cfg = cfg))
#'   if (Sys.which("xelatex") != "") {
#'     output <- tempfile(fileext = ".pdf")
#'     save_pamphlet("tablut", gk = gk, output = output)
#'     # xopen::xopen(output)
#'     # browseURL(output)
#'   }
#' @export
game_kit <- function(cfgs = NULL) {
    if (is.null(cfgs)) {
        if (piecepackr::has_font("Dejavu Sans"))
            cfg <- game_systems("dejavu")$piecepack
        else
            cfg <- game_systems("sans")$piecepack
        cfgs <- list(cfg = cfg)
    }
    GameKit$new(cfgs)
}
