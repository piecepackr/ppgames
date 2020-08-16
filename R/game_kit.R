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
#' @param cfgs A named list of \code{pp_cfg} configuration list R6 objects.
#' @export
game_kit <- function(cfgs = list(cfg = pp_cfg())) {
    GameKit$new(cfgs)
}
