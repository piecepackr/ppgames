plot_canonical_image <- function(game, gk = game_kit(), file = NULL) {
    game <- gsub("-", "_", game)
    if (is.null(file)) file <- paste0(game, "_canonical.pdf")
    cfg <- gk$get_piecepacks(1)[[1]]
    envir <- list(piecepack = cfg)
    switch(game,
           pass_the_food = {
               df_tiles <- get_starting_df_from_name(game)
               df_tiles$id <- NULL
               df_tiles$cfg <- NULL
               withr::local_seed(36)
               df_coins <- tibble(piece_side = "coin_back", rank = rep(1:6, 4), suit = rep(1:4, each=6),
                                  x = stats::runif(24, -1.10, 8.70),
                                  y = stats::runif(24, -1.20, 13.50),
                                  angle = stats::runif(24, 0, 360))
               df <- bind_rows(df_tiles, df_coins)
               wh <- render_piece(df, file = file, annotate = FALSE,
                                  envir = envir,
                                  op_scale = 0.5, trans = op_transform)
           },
           tablut = {
               df <- df_tablut(cfg$get_width("die_face"))
               wh <- render_piece(df, file = file, annotate = FALSE,
                                  envir = envir,
                                  op_scale = 0.5, trans = op_transform)

           }, {
               df <- get_starting_df_from_name(game)
               wh <- render_piece(df, file = file, annotate = FALSE,
                                  envir = envir,
                                  op_scale = 0.5, trans = op_transform)
           })
    invisible(c(wh, list(file = file)))
}
