#' Save promo image of game
#'
#' `save_promo_image()` saves a promo image of a game.
#' This is usually just a starting diagram for the game but
#' sometimes we may create a custom image.
#'
#' If we haven't created a custom image for this game then basically
#' we'll convert the game name to snake case, prepend a `"df_"`
#' in front and then `dynGet()` a function with that name and use
#' the resulting data frame with [piecepackr::render_piece()].
#' See [df_game] for list of games directly supported by this package.
#'
#' @param game Game name.  Will be normalized by [normalize_name()].
#' @inheritParams save_ruleset
#' @param file Filename for the image.  If `NULL` we'll generate
#'             a pdf image with an appropriate name given `game`.
#' @param ... Passed to [piecepackr::render_piece()].
#' @return A list with the width, height, and filename of the promo image.
#'         As a side effect we save an image to disk.
#' @examples
#' file <- tempfile(fileext = ".pdf")
#' whf <- save_promo_image("Fuji-san", gk = game_kit(), file = file)
#' print(whf)
#' unlink(whf$file)
#' @export
save_promo_image <- function(game, gk = game_kit(), file = NULL, ...) {
    game_name <- normalize_name(game)
    if (is.null(file)) file <- paste0(game_name, "_promo.pdf")
    cfg <- gk$get_piecepacks(1)[[1]]
    envir <- list(piecepack = cfg)
    switch(game_name,
           nine_mens_morris = {
               df <- promo_morris_df()
               wh <- render_piece(df, file = file, ...,
                                  envir = envir, op_scale = 0.5, trans = op_transform)
           },
           pass_the_food = {
               df <- promo_pass_the_food_df()
               wh <- render_piece(df, file = file, ...,
                                  envir = envir, op_scale = 0.5, trans = op_transform)
           },
           speedy_towers = {
               df <- promo_speedy_towers_df()
               wh <- render_piece(df, file = file, ...,
                                  envir = envir, op_scale = 0.5, trans = op_transform)
           },
           tablut = {
               df <- df_tablut(cfg$get_width("die_face"))
               wh <- render_piece(df, file = file, ...,
                                  envir = envir, op_scale = 0.5, trans = op_transform)
           },
           twelve_mens_morris = {
               df <- promo_morris_df()
               wh <- render_piece(df, file = file, ...,
                                  envir = envir, op_scale = 0.5, trans = op_transform)
           },
           {
               df <- get_starting_df_from_name(game_name)
               wh <- render_piece(df, file = file, ...,
                                  envir = envir, op_scale = 0.5, trans = op_transform)
           })
    invisible(c(wh, list(file = file)))
}

promo_morris_df <- function() {
    df_tiles <- df_nine_mens_morris()
    df_coins <- tibble(piece_side = "coin_back",
                       x = c(3, 3, 5, 7, 7, 9, 9, 9, 11, 11, 11),
                       y = c(3, 7, 7, 1, 3, 5, 7, 9, 3, 7, 11),
                       suit = c(3, 4, 4, 1, 2, 2, 1, 1, 4, 3, 3),
                       rank = c(1, 1, 2, 1, 1, 2, 2, 3, 3, 2, 3))
    bind_rows(df_tiles, df_coins)
}

promo_pass_the_food_df <- function() {
    df_tiles <- df_pass_the_food()
    withr::local_seed(36)
    df_coins <- tibble(piece_side = "coin_back",
                       rank = rep(1:6, 4), suit = rep(1:4, each=6),
                       x = stats::runif(24, -1.10, 8.70),
                       y = stats::runif(24, -1.20, 13.50),
                       angle = stats::runif(24, 0, 360))
    bind_rows(df_tiles, df_coins)
}

promo_speedy_towers_df <- function() {
    withr::local_seed(72)
    df_tiles <- tibble(piece_side = "tile_face",
                       suit = rep(1:4, 6),
                       rank = rep(1:6, each = 4))[sample.int(24L), ]
    df_tiles$x <- c(5, rep(1, 10), rep(5, 12), 3)
    df_tiles$y <- c(rep(1, 23), 7)
    df_tiles$angle <- c(c(rep_len(0, 12), rep_len(180, 11))[sample.int(23L)], 180)

    df_coins <- tibble(piece_side = "coin_back",
                       suit = rep(1:4, 6),
                       rank = rep(1:6, each = 4))[sample.int(24L), ]
    df_coins$x <- c(rep(1, 10), rep(5, 12), 5, 6)
    df_coins$y <- c(rep(1, 22), 7, 7)
    df_coins$angle <- c(c(rep_len(0, 12), rep_len(180, 10))[sample.int(22L)], 180, 180)
    # interleave coins
    df_inter <- bind_rows(df_tiles[3:23, ], df_coins[1:22, ])[sample.int(43L), ]
    # add pawns
    df_pawns <- tibble(piece_side = "pawn_face",
                       suit = c(1, 3), rank = NA_integer_,
                       x = c(1, 5), y = c(7, 1), angle = c(180, 0))
    df <- bind_rows(df_tiles[1:2, ], df_inter, df_pawns, df_tiles[24,, ], df_coins[23:24,, ])
    df
}
