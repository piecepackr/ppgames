#' @rdname df_game
#' @export
df_alice_chess <- function(has_subpack = FALSE, max_tiles = 24) {
    max_tiles_per_board <- floor(max_tiles / 2)
    df_t1 <- df_rect_board_tiles(8, 8, max_tiles = max_tiles_per_board)
    df_t2 <- df_rect_board_tiles(8, 8, max_tiles = max_tiles_per_board, x0 = 11)
    df_p <- df_chess_pieces(has_subpack)
    bind_rows(df_t1, df_t2, df_p)
}

#' @rdname df_game
#' @export
df_chaturaji <- function(has_subpack = FALSE) {
    df_t <- df_rect_board_tiles(8, 8)
    df_p <- tibble(piece_side = "coin_back",
                   x = c(rep(2,4), 5:8, rep(7,4), 1:4),
                   y = c(5:8, rep(7,4), 1:4, rep(2,4)),
                   suit = rep(1:4, each = 4),
                   angle = rep(c(-90,180,90,0), each = 4))
    df_b <- tibble(piece_side = "coin_face", rank = 3, suit = 1:4,
                   x = c(1,8,8,1), y = c(8,8,1,1), angle = c(-90,180,90,0))
    df_h <- tibble(piece_side = "coin_face", rank = 2, suit = 1:4,
                   x = c(1,7,8,2), y = c(7,8,2,1), angle = c(-90,180,90,0))
    df_r <- tibble(piece_side = "die_face", rank = 4, suit = 1:4,
                   x = c(1,6,8,3), y = c(6,8,3,1), angle = c(-90,180,90,0))
    df_k <- tibble(piece_side = "pawn_face", rank = 6, suit = 1:4,
                   x = c(1,5,8,4), y = c(5,8,4,1), angle = c(-90,180,90,0))
    df_sb <- bind_rows(df_b, df_h, df_r, df_k)
    if (has_subpack) {
        df_sb$piece_side <- "tile_face"
        df_sb$cfg <- "subpack"
    } else {
        df_sb$cfg <- "piecepack"
    }
    df <- bind_rows(df_t, df_p, df_sb)
    df$cfg <- ifelse(is.na(df$cfg), "piecepack", df$cfg)
    df
}

#' @rdname df_game
#' @export
df_four_seasons_chess <- function(has_subpack = FALSE) {
    df_t <- df_rect_board_tiles(8, 8)
    df_t$cfg <- "piecepack"
    angles <- c(180,90,0,-90)
    suits <- c(1,4,2,3)
    df_p <- tibble(piece_side = "coin_back", suit = rep(suits, each = 4),
                   x = c(1,2,3,3,6,6,7,8,8,7,6,6,3,3,2,1),
                   y = c(6,6,7,8,8,7,6,6,3,3,2,1,1,2,3,3),
                   angle = rep(angles, each = 4), cfg = "piecepack")
    df_k <- tibble(piece_side = "pawn_face",
                   suit = suits, rank = 6,
                   x = c(1,8,8,1), y = c(8,8,1,1), angle = angles)
    df_r <- tibble(piece_side = "die_face",
                   suit = suits, rank = 4,
                   x = c(2,7,7,2), y = c(8,8,1,1), angle = angles)
    df_b <- tibble(piece_side = "coin_face",
                   suit = suits, rank = ifelse(has_subpack, 3, 1),
                   x = c(2,7,7,2), y = c(7,7,2,2), angle = angles)
    df_n <- tibble(piece_side = "coin_face",
                   suit = suits, rank = 2,
                   x = c(1,8,8,1), y = c(7,7,2,2), angle = angles)
    df_sb <- bind_rows(df_k, df_r, df_b, df_n)
    if (has_subpack) {
        df_sb$piece_side <- "tile_face"
        df_sb$cfg <- "subpack"
    } else {
        df_sb$cfg <- "piecepack"
    }
    df <- bind_rows(df_t, df_p, df_sb)
    df$cfg <- ifelse(is.na(df$cfg), "piecepack", df$cfg)
    df
}

df_chess_pieces <- function(has_subpack = FALSE) {
    if (has_subpack) {
        df_pb <- tibble(piece_side = "coin_back", cfg = "piecepack",
                        suit = rep(1:2, each = 4), x = 1:8, y = 7, angle = 180)
        df_ob <- tibble(piece_side = "tile_face", cfg = "subpack",
                        suit = rep(1:2, each = 4), x = 1:8, y = 8, angle = 180,
                        rank = c(4,2,3,5,6,3,2,4))
        df_pw <- tibble(piece_side = "coin_back", cfg = "piecepack",
                        suit = rep(4:3, each = 4), x = 1:8, y = 2)
        df_ow <- tibble(piece_side = "tile_face", cfg = "subpack",
                        suit = rep(4:3, each = 4), x = 1:8, y = 1,
                        rank = c(4,2,3,5,6,3,2,4))
        bind_rows(df_pb, df_pw, df_ow, df_ob)
    } else {
        df_p1 <- tibble(piece_side = "coin_back",
                        suit = (1:8+1) %% 2 + 1, x = 1:8, y = 7, angle = 180)
        df_p2 <- tibble(piece_side = "coin_back",
                        suit = 1:8 %% 2 + 3, x = 1:8, y = 2)
        df_r <- tibble(piece_side = "die_face", suit = 1:4, rank = 4,
                       x = c(1,8,8,1), y = c(8,8,1,1), angle = c(180,180,0,0))
        df_n <- tibble(piece_side = "coin_face", rank = 2,
                       x = c(2,7,7,2), y = c(8,8,1,1), angle = c(180,180,0,0))
        df_b <- tibble(piece_side = "pawn_face", suit = 1:4,
                       x = c(3,6,6,3), y = c(8,8,1,1), angle = c(180,180,0,0))
        df_q <- tibble(piece_side = "coin_face", rank = 5,
                       x = 4, y = c(8,1), angle = c(180,0))
        df_k <- tibble(piece_side = "coin_face", rank = 6,
                       x = 5, y = c(8,1), angle = c(180,0))
        bind_rows(df_p1, df_p2, df_r, df_n, df_b, df_q, df_k)
    }
}

#' @rdname df_game
#' @export
df_international_chess <- function(has_subpack = FALSE) {
    df_t <- df_rect_board_tiles(8, 8)
    if (has_subpack) df_t$cfg <- "piecepack"
    df_p <- df_chess_pieces(has_subpack)
    bind_rows(df_t, df_p)
}

#' @rdname df_game
#' @export
df_chess <- df_international_chess


#' @rdname df_game
#' @export
df_shogi <- function(has_subpack = FALSE, cfg2 = "piecepack") {
    # board
    x_t <- seq(2, 8, by = 2)
    y_tr <- rep(c(4, 6), each = 4)
    y_tb <- rep(c(2, 8), each = 4)
    df_t <- tibble(piece_side = "tile_back",
                   x = rep(x_t,4), y = c(y_tr, y_tb),
                   cfg = rep(c(cfg2, "piecepack"), each = 8))

    # pawns
    df_pb <- tibble(piece_side = "coin_back",
                   suit = 1:9 %% 4 +1,
                   x = 1:9, y = 3,
                   cfg = "piecepack")
    df_pt <- df_pb
    df_pt$y <- 7
    df_pt$angle <- 180

    # bishops
    df_b <- tibble(piece_side = "coin_face", rank = 3, cfg = cfg2, suit = c(2,4),
                   x = c(2, 10-2), y = c(2, 10-2), angle = c(0, 180))

    # rooks
    df_r <- tibble(piece_side = "coin_face", rank = 4, cfg = cfg2, suit = c(3,1),
                   x = c(8, 10-8), y = c(2, 10-2), angle = c(0, 180))

    # lances
    df_l <- tibble(piece_side = "coin_face", rank = 5, cfg = cfg2, suit = 1:4,
                   x = c(1,9,10-1,10-9), y = c(1,1,10-1,10-1), angle = c(0,0,180,180))
    # knights
    df_n <- tibble(piece_side = "coin_face", rank = 2, cfg = cfg2, suit = c(4,1:3),
                   x = c(2,8,10-2,10-8), y = c(1,1,10-1,10-1), angle = c(0,0,180,180))

    # silvers
    df_s <- tibble(piece_side = "coin_face", rank = 6, cfg = cfg2, suit = c(3:4, 1:2),
                   x = c(3,7,10-3,10-7), y = c(1,1,10-1,10-1), angle = c(0,0,180,180))
    # golds
    df_g <- tibble(piece_side = "die_face", suit = 1:4, rank = 6, cfg = "piecepack",
                   x = c(4,6,10-4,10-6), y = c(1,1,10-1,10-1), angle = c(0,0,180,180))
    # kings
    df_k <- tibble(piece_side = "pawn_face", suit = 4-0:1, cfg = "piecepack",
                   x = c(5,10-5), y = c(1,10-1), angle = c(0,180))
    df <- bind_rows(df_t, df_pb, df_pt, df_b, df_r, df_l, df_n, df_s, df_g, df_k)
    if (has_subpack) {
        df$cfg <- ifelse(df$piece_side == "coin_face", "subpack", cfg2)
        df$piece_side <- ifelse(df$piece_side == "coin_face", "tile_face", df$piece_side)
    }
    df
}

#' @rdname df_game
#' @export
df_ultima <- function(has_subpack = FALSE) {
    df_t <- df_rect_board_tiles(8, 8)
    df_p1 <- tibble(piece_side = "coin_back",
                    suit = (1:8+1) %% 2 + 1, x = 1:8, y = 7, angle = 180)
    df_p2 <- tibble(piece_side = "coin_back",
                    suit = 1:8 %% 2 + 3, x = 1:8, y = 2)
    df_r <- tibble(piece_side = ifelse(has_subpack, "tile_face", "coin_face"),
                   suit = 1:4, x = c(1,8,8,1), y = c(8,8,1,1))
    if (has_subpack) {
        df_r$cfg <- rep(c("subpack", "piecepack"), 2)
        df_r$piece_side <- rep(c("tile_face", "die_face"), 2)
        df_r$rank <- 4
        df_r$angle <- c(180,0,0,180)
    } else {
        df_r$cfg <- "piecepack"
        df_r$piece_side <- "coin_face"
        df_r$rank <- c(4,1,4,1)
        df_r$angle <- c(180,180,0,0)
    }
    df_n <- tibble(piece_side = "die_face",
                   suit = 1:4, rank = 2,
                   x = c(2,7,7,2), y = c(8,8,1,1), angle = c(180,180,0,0))
    df_b <- tibble(piece_side = "pawn_face",
                   suit = 1:4, rank = 3,
                   x = c(3,6,6,3), y = c(8,8,1,1), angle = c(180,180,0,0))
    df_q <- tibble(piece_side = "coin_face",
                   suit = c(1,3), rank = 5,
                   x = c(4,5), y = c(8,1), angle = c(180,0))
    df_k <- tibble(piece_side = "coin_face",
                   suit = c(2,4), rank = 6,
                   x = c(5,4), y = c(8,1), angle = c(180,0))
    df_sb <- bind_rows(df_n, df_b, df_q, df_k)
    if (has_subpack) {
        df_sb$piece_side <- "tile_face"
        df_sb$cfg <- "subpack"
    } else {
        df_sb$cfg <- "piecepack"
    }
    df <- bind_rows(df_t, df_p1, df_p2, df_r, df_sb)
    df$cfg <- ifelse(is.na(df$cfg), "piecepack", df$cfg)
    df
}

#' @rdname df_game
#' @export
df_baroque_chess <- df_ultima

#' @rdname df_game
#' @export
df_xiangqi <- function(has_subpack = FALSE) {
    ang2 <- rep(c(180, 0), each = 2)
    suits <- c(1,2,4,3)
    x2 <- function(x) rep(c(x, 10-x), 2)
    y2 <- function(y) rep(c(11-y, y), each = 2)
    df_t1 <- df_rect_board_tiles(10, 9)
    df_t2 <- tibble(piece_side = "tile_face", suit = c(1,3), rank = 2,
                    x = 5, y = c(9, 2), angle = c(180, 0))
    df_zu1 <- tibble(piece_side = "coin_back",
                   suit = (1:5+1) %% 2 + 3, x = seq(1, 9, 2), y = 4)
    df_zu2 <- tibble(piece_side = "coin_back",
                   suit = (1:5+1) %% 2 + 1, x = seq(1, 9, 2), y = 7, angle = 180)
    df_pao <- tibble(piece_side = "pawn_face", suit = suits,
                     x = x2(2), y = y2(3), angle = ang2)
    df_che <- tibble(piece_side = "die_face", suit = suits, rank = 4,
                     x = x2(1), y = y2(1), angle = ang2)
    df_ma <- tibble(piece_side = "coin_face", suit = suits, rank = 2,
                    x = x2(2), y = y2(1), angle = ang2)
    df_xiang <- tibble(piece_side = "coin_face", suit = suits, rank = 3,
                    x = x2(3), y = y2(1), angle = ang2)
    df_shi <- tibble(piece_side = "coin_face", suit = suits, rank = 5,
                    x = x2(4), y = y2(1), angle = ang2)
    df_jiang <- tibble(piece_side = "coin_face", suit = c(3,1), rank = 6,
                       x = 5, y = c(1,10), angle = c(0, 180))
    df_sb <- bind_rows(df_ma, df_xiang, df_shi, df_jiang, df_che)
    if (has_subpack) {
        df_sb$piece_side <- "tile_face"
        df_sb$cfg <- "subpack"
    } else {
        df_sb$cfg <- "piecepack"
    }
    df <- bind_rows(df_t1, df_t2, df_zu1, df_zu2, df_pao, df_sb)
    df$cfg <- ifelse(is.na(df$cfg), "piecepack", df$cfg)
    df
}
