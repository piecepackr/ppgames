# # invisible(try(dev.off()))
# # dev.new(width=25, height=5)

# inkscape -z -e images/backgammon.png -w 1200 -h 240 images/backgammon.svg 
# svg("images/backgammon.svg", width = 25, height=5)
# df <- df_backgammon()
# pmap_piece(df, cfg=cfg, default.units="in")
# invisible(dev.off())

# inkscape -z -e images/tablut.png -w 768 -h 768 images/tablut.svg
# svg("images/tablut.svg", width=10, height=10)
# df <- df_tablut()
# pmap_piece(df, cfg=cfg, default.units="in")
# invisible(dev.off())

# svg("shogi.svg", width=10, height=10)
# df <- df_shogi(cfg)
# ee <- list(cfg=cfg, cfg2=cfg)
# pmap_piece(df, default.units="in", envir=ee)
# invisible(dev.off())

# cfg <- pp_cfg(list(suit_color.unsuited="black", border_color="black", border_lex=4))
# svg("cribbage.svg", width=9, height=33)
# df <- df_cribbage_board()
# pmap_piece(df, cfg=cfg, default.units="in")
# dev.off()

#' Data frames of starting diagrams for various games
#'
#' \code{tibble} data frames of starting diagrams for various games.
#'   data frame output can usually be plotted with \code{pmap_piece(df, cfg=cfg, default.units="in")}.
#'
#' @param cfg A \code{pp_cfg} configuration list object
#' @param cfg2 A \code{pp_cfg} configuration list object
#' @rdname df_game
#' @name df_game
NULL

#' @rdname df_game
#' @importFrom dplyr bind_rows
#' @export
df_backgammon <- function() {
    y_top <- 4
    y_bot <- 1
    x_1 <- 25-2+1
    x_6 <- 25-12+1
    x_12 <- 1
    x_08 <- 5*2-1

    # tiles
    df_t <- tibble(piece_side = "tile_face", 
                 suit = c(3+1:6%%2, 3+(1:6+1)%%2, 1+(1:6+1)%%2, 1+(1:6+0)%%2),
                 rank = rep(1:6, 4),
                 x = c(26-2*1:6, 13-2*1:6, 26-2*1:6, 13-2*1:6),
                 y = rep(c(1, y_top, y_top, 1), each=6),
                 angle = rep(c(0, 0, 180, 180), each=6))

    # coins
    df_c1 <- tribble( ~piece_side, ~x, ~y, ~suit,
                          "coin_back", x_6 +0.5, y_bot+0.5, 4,
                          "coin_back", x_6 +0.5, y_bot-0.5, 4,
                          "coin_back", x_6 -0.5, y_bot+0.5, 4,
                          "coin_back", x_6 -0.5, y_bot-0.5, 4,
                          "coin_back", x_12+0.5, y_top+0.5, 3,
                          "coin_back", x_12+0.5, y_top-0.5, 3,
                          "coin_back", x_12-0.5, y_top+0.5, 3,
                          "coin_back", x_12-0.5, y_top-0.5, 3,
                          "coin_back", x_12-0.0, y_top-0.0, 3,
                          "coin_back", x_08+0.5, y_bot-0.5, 4,
                          "coin_back", x_08-0.5, y_bot+0.5, 4,
                          "coin_back", x_08+0.5, y_bot+0.5, 3)
    df_c2 <- tribble( ~piece_side, ~x, ~y, ~suit,
                          "coin_back", x_6 +0.5, y_top+0.5, 1,
                          "coin_back", x_6 +0.5, y_top-0.5, 1,
                          "coin_back", x_6 -0.5, y_top+0.5, 1,
                          "coin_back", x_6 -0.5, y_top-0.5, 1,
                          "coin_back", x_12+0.5, y_bot+0.5, 2,
                          "coin_back", x_12+0.5, y_bot-0.5, 2,
                          "coin_back", x_12-0.5, y_bot+0.5, 2,
                          "coin_back", x_12-0.5, y_bot-0.5, 2,
                          "coin_back", x_12-0.0, y_bot-0.0, 2,
                          "coin_back", x_08+0.5, y_top-0.5, 1,
                          "coin_back", x_08-0.5, y_top+0.5, 1,
                          "coin_back", x_08-0.5, y_top-0.5, 2)
    df_c2$angle <- 180

    # pawns
    df_p <- tibble(piece_side = "pawn_face", suit=4:1, 
                   x=x_1+c(-0.5,0.5,-0.5,0.5), 
                   y=c(y_top+0.5,y_top-0.5, y_bot+0.5, y_bot-0.5),
                   angle=c(0, 0, 180, 180))

    # dice
    df_d <- tibble(piece_side = "die_face", suit=c(4,1,3,2), rank=c(1,1,2,2),
                   x=c(x_6, x_6, 6.5-0.5, 6.5+0.5),
                   y=c(y_bot, y_top, y_bot+1.5, y_bot+1.5),
                   angle=c(0, 180, 0, 180))
    bind_rows(df_t, df_c1, df_c2, df_p, df_d)
}

df_shogi <- function(cfg=pp_cfg(), cfg2=cfg) {
    ee <- list(cfg=cfg, cfg2=cfg2)
    # board
    x_t <- seq(2, 8, by=2)
    y_tr <- rep(c(4, 6), each=4)
    y_tb <- rep(c(2, 8), each=4)
    df_t <- tibble(piece_side="tile_back", 
                   x=rep(x_t,4), y=c(y_tr, y_tb),
                   cfg=rep(c("cfg2", "cfg"), each=8))
    
    # pawns
    df_pb <- tibble(piece_side="coin_back",
                   suit=1:9 %% cfg$n_suits +1,
                   x=1:9, y=3,
                   cfg="cfg")
    df_pt <- df_pb
    df_pt$y <- 7
    df_pt$angle <- 180
    
    # bishops
    df_b <- tibble(piece_side="coin_face", rank=3, cfg="cfg2",
                   x=c(2, 10-2), y=c(2, 10-2), angle=c(0, 180))

    # rooks
    df_r <- tibble(piece_side="coin_face", rank=4, cfg="cfg2",
                   x=c(8, 10-8), y=c(2, 10-2), angle=c(0, 180))

    # lances
    df_l <- tibble(piece_side="coin_face", rank=5, cfg="cfg2",
                   x=c(1,9,10-1,10-9), y=c(1,1,10-1,10-1), angle=c(0,0,180,180))
    # knights
    df_n <- tibble(piece_side="coin_face", rank=2, cfg="cfg2",
                   x=c(2,8,10-2,10-8), y=c(1,1,10-1,10-1), angle=c(0,0,180,180))

    # silvers
    df_s <- tibble(piece_side="coin_face", rank=6, cfg="cfg2",
                   x=c(3,7,10-3,10-7), y=c(1,1,10-1,10-1), angle=c(0,0,180,180))
    # golds
    df_g <- tibble(piece_side="die_face", suit=1:4, rank=6, cfg="cfg",
                   x=c(4,6,10-4,10-6), y=c(1,1,10-1,10-1), angle=c(0,0,180,180))
    # kings
    df_k <- tibble(piece_side="pawn_face", suit=cfg$n_suits-0:1, cfg="cfg",
                   x=c(5,10-5), y=c(1,10-1), angle=c(0,180))
    bind_rows(df_t, df_pb, df_pt, df_b, df_r, df_l, df_n, df_s, df_g, df_k)
}

#' @rdname df_game
#' @export
df_tablut <- function(cfg=pp_cfg()) {
    df_t <- df_rect_board_tiles(9, 9)
    df_cf <- tibble(piece_side="coin_face",
                 rank=rep(3:6, 4),
                 x=c(5,4,5,6,5,6,5,4,2,1,1,1,8,9,9,9),
                 y=c(2,1,1,1,8,9,9,9,5,6,5,4,5,4,5,6),
                 angle=c(rep(0,4),rep(180,4), rep(-90, 4), rep(90, 4)))
    df_cb <- tibble(piece_side="coin_back",
                 suit=rep(1:4, each=2),
                 x=c(5,5,6,7,5,5,4,3),
                 y=c(6,7,5,5,4,3,5,5),
                 angle=rep(c(0, -90, 180,  90), each=2))
    if (cfg$get_width("die_face") > 0.25 * cfg$get_width("tile_back")) {
        df_d <- tibble(piece_side="die_face", 
                       suit=3, rank=1, x=5, y=5, angle=0)
    } else {
        df_d <- tibble(piece_side="die_face",
                     suit=1:4, rank=1,
                     x=c(4.75,5.25,5.25,4.75),
                     y=c(5.25,5.25,4.75,4.75),
                     angle=c(0,-90,180,90))
    }
    df_p <- tibble(piece_side="pawn_face", suit=3, x=5, y=5)
    bind_rows(df_t, df_cf, df_cb, df_d, df_p)
}

#' @rdname df_game
#' @export
df_cribbage_board <- function() {
    df_l <- df_rect_board_tiles(30, 3, x0=1, y0=3, max_tiles=12)
    df_r <- df_rect_board_tiles(30, 3, x0=6, y0=3, max_tiles=12)
    df_c <- tibble(piece_side="coin_face", x=rep(c(2, 7), each=12),
		   rank=rep(rep(1:6, each=2), 2),
		   y=rep(c(3,7,8,12,13,17,18,22,23,27,28,32), 2))
    df_p <- tibble(piece_side="pawn_face", x=c(1,3,6,8), y=1, suit=1:4)
    df_d <- tibble(piece_side="die_face", x=c(2,7), y=1, suit=c(1,3))
    bind_rows(df_l, df_r, df_c, df_p, df_d)
}
