#' Data frames of starting diagrams for various games
#'
#' \code{tibble} data frames of starting diagrams for various games.
#'   data frame output can usually be plotted with \code{pmap_piece(df, cfg=cfg, default.units="in")}.
#'
#' @param seed Seed that determines setup, either an integer or \code{NULL}
#' @param cfg2 A string of a piecepack expansion (or perhaps \code{"piecepack"} for a second piecepack)
#' @param has_matchsticks Has matchsticks
#' @param has_subpack Has a piecepack subpack
#' @param coins String of coin layout
#' @param dice String of dice layout
#' @param tiles String of tile layout
#' @param die_width Width of dice
#' @param max_tiles Maximum number of (piecepack) tiles available to build boards
#' @param suit_colors Character vector of the suit colors
#' @rdname df_game
#' @importFrom dplyr bind_rows
#' @name df_game
NULL

#' @rdname df_game
#' @export
df_alien_city <- function(seed=NULL, tiles=NULL) {
    set.seed(seed)
    df_t1 <- tibble(piece_side="tile_face", 
                   x=0.5+rep(seq(1,7,2),5),
                   y=0.5+rep(seq(9,1,-2),each=4))
    if(is.null(tiles)) {
        df_t2 <- tibble(suit=rep(1:4, each=5),
                        rank=rep(1:5, 4)+1,
                        angle=90*(sample(4,20,replace=TRUE)-1))
        df_t2 <- df_t2[sample.int(20),]
    } else {
        df_t2 <- process_alien_city_tiles(tiles)
    }
    cbind(df_t1, df_t2)
}

process_alien_city_tiles <- function(tiles) {
    tiles <- gsub("[[:space:]]", "", tiles)
    tiles <- gsub("[/:;\\\\|]", "", tiles)
    tiles <- stringr::str_split(tiles, "")[[1]]
    if(length(tiles) == 40) {
        suits <- tiles[which(seq(40) %% 2 == 1)]
        angles <- tiles[which(seq(40) %% 2 + 1 == 1)]
        ranks <- integer(20)
    } else if (length(tiles) == 60) {
        suits <- tiles[which(seq(60) %% 3 == 1)]
        ranks <- tiles[which(seq(60) %% 3 + 1 == 1)]
        angles <- tiles[which(seq(60) %% 3 + 2 == 1)]
    } else {
        stop(paste("Don't know how to handle tiles string", tiles))
    }
    suits <- toupper(suits)
    suits <- gsub("S|R", "1", suits)
    suits <- gsub("M|K", "2", suits)
    suits <- gsub("C|G", "3", suits)
    suits <- gsub("A|B", "4", suits)
    suits <- as.numeric(suits)
    if(length(tiles) == 40) {
        ranks[which(suits==1)] <- sample.int(5)
        ranks[which(suits==2)] <- sample.int(5)
        ranks[which(suits==3)] <- sample.int(5)
        ranks[which(suits==4)] <- sample.int(5)
    } else { 
        ranks <- gsub("n", "0", ranks)
        ranks <- gsub("a", "1", ranks)
        ranks <- as.numeric(ranks) + 1
    }
    angles <- gsub("\\^", "0", angles)
    angles <- gsub("<", "90", angles)
    angles <- gsub("v", "180", angles)
    angles <- gsub(">", "270", angles)
    angles <- as.numeric(angles)
    tibble(suit=suits, rank=ranks, angle=angles)
}

#' @rdname df_game
#' @export
df_everest <- function() {
    df_t1 <- tibble(piece_side="tile_back",
                    x=0.5+c(seq(1,7,2),seq(2,6,2), seq(1,7,2)),
                    y=0.5+c(rep(1, 4), rep(3, 3), rep(5,4)))
    df_t2 <- df_rect_board_tiles(4,6, x0=2, y0=2)
    df_t3 <- df_rect_board_tiles(4,4, x0=3, y0=2)
    df_t4 <- df_rect_board_tiles(2,4, x0=3, y0=3)
    df_t5 <- tibble(piece_side="tile_back", x=4.5, y=3.5)
    df_p <- tibble(piece_side="pawn_face",
                   x=c(1,8,8,1), y=c(5,5,2,2), suit=1:4)
    bind_rows(df_t1, df_t2, df_t3, df_t4, df_t5, df_p)
}

#' @rdname df_game
#' @export
df_four_field_kono <- function() {
    df_t <- df_rect_board_tiles(4,4)
    df_c <- tibble(piece_side="coin_back",
                   suit=rep(1:4, each=4), 
                   x=c(1:2,1:2,3:4,3:4,3:4,3:4,1:2,1:2),
                   y=rep(c(4,3,4,3,2,1,2,1), each=2),
                   angle=rep(c(180,0), each=8))
    bind_rows(df_t, df_c)
}

#' @rdname df_game
#' @export
df_cell_management <- function(seed=NULL) {
    set.seed(seed)

    # hexagon distances
    #    between closest vertices: 2
    #    between vertex to opposite vertix: 4
    #    between vertex to vertex 2 away: 2*sqrt(3)
    x0 <- 5.5 
    y0 <- 4.5+sqrt(3) 
    # Sun and Crown tiles
    theta <- seq(30, 330, by=60)
    r <- sqrt(3) + 1
    xt <- x0 + to_x(theta, r)
    yt <- y0 + to_y(theta, r)
    df_t <- tibble(piece_side="tile_face", suit=1, rank=sample.int(6), 
                   x=xt, y=yt, angle=theta-90)
    df_t[sample.int(6,3), "suit"] <- 3

    # Moon tiles and Coins
    last_played <- 1
    moon_ranks <- c(sample.int(5)+1, 1)
    df_tm <- tibble(piece_side="tile_face", suit=2, rank=moon_ranks, x=NA, y=NA, angle=NA)
    df_c <- tibble(piece_side="coin_face", rank=rep(NA, 12), x=NA, y=NA, angle=NA)
    for (ii in seq(along=moon_ranks)) {
        angle <- as.numeric(df_t[which(df_t$rank == last_played), "angle"])
        theta <- angle+90
        xt <- x0 + to_x(theta, sqrt(3) + 3)
        yt <- y0 + to_y(theta, sqrt(3) + 3)
        last_played <- moon_ranks[ii]

        df_tm[ii, "angle"] <- angle
        df_tm[ii, "x"] <- xt
        df_tm[ii, "y"] <- yt

        is <- c(2*ii-1,2*ii)
        xc <- xt + to_x(theta+c(-135,135), 0.5*sqrt(2))
        yc <- yt + to_y(theta+c(-135,135), 0.5*sqrt(2))
        df_c[is, "angle"] <- angle
        df_c[is, "x"] <- xc
        df_c[is, "y"] <- yc
        df_c[is, "rank"] <- moon_ranks[ii]
    }
    # Guards
    df_p <- tibble(piece_side="pawn_face", suit=1:2, x=x0+c(4,5), y=y0)

    bind_rows(df_t, df_tm, df_c, df_p)
}

#' @rdname df_game
#' @export
df_fujisan <- function(seed=NULL, coins=NULL, dice=NULL) {
    set.seed(seed)
    if(is.null(coins)) {
        coins <- random_fujisan_coins()
    } else if (is.character(coins)) {
        coins <- process_fujisan_coins(coins)
    }
    if (is.vector(coins)) {
        coins <- matrix(coins, nrow=2, byrow=TRUE)   
    }
    df_t <- tibble(piece_side="tile_back", y=1.5,
                   x=1.5+c(seq(1,11,2),seq(2,10,2),seq(3,9,2),4,6,8,5,7,5,7,6,6))
    suit <- rev((0:23%%4)+1)
    df_c <- tibble(piece_side="coin_face", x=rep(2:13, 2), y=rep(1:2, each=12),
                   suit=suit, rank=c(coins[2,], coins[1,])+1)
    df_p <- tibble(piece_side="pawn_face", x=c(1,14,14,1), y=c(2,2,1,1), suit=1:4)
    if (first_move_needs_dice(coins)) {
        if(is.null(dice)) {
            dice <- random_dice()
        } else {
            dice <- process_fujisan_coins(dice)
        }
        df_d <- tibble(piece_side="die_face", x=c(16,17,16,17), y=c(2,2,1,1), suit=c(1,2,4,3), rank=dice+1)
        bind_rows(df_t, df_c, df_p, df_d)
    } else {
        bind_rows(df_t, df_c, df_p)
    }
}

#' @rdname df_game
#' @export
df_nine_mens_morris <- function(has_matchsticks=FALSE) {
    df <- tibble(piece_side="tile_face",
           suit=rep(1:4, each=6),
           rank=rep(1:6, 4),
           x=c(7,1,7,3,7,5,     13,13,11,11,9,9,
               7,13,7,11,7,9,   1,1,3,3,5,5),
           y=c(13,13,11,11,9,9,   7,13,7,11,7,9,
               1,1,3,3,5,5,       7,1,7,3,7,5))
    if (has_matchsticks) {
        df_m <- tibble(piece_side="matchstick_face",
                       suit=rep(1:4, each=6),
                       rank=4, 
                       x=c(1,1,3,3,5,5, 9,9,11,11,13,13,
                           9,9,11,11,13,13, 1,1,3,3,5,5),
                       y=c(9,11,9,13,13,11, 11,13,13,9,11,9,
                           3,1,1,5,5,3, 3,5,5,1,1,3),
                       angle=rep(rep(c(0,90,90,0,90,0,0,90),each=3)))
        df <- bind_rows(df, df_m)
    }
    df
}

#' @rdname df_game
#' @export
df_twelve_mens_morris <- df_nine_mens_morris

#' @rdname df_game
#' @export
df_american_checkers <- function() {
    df_t <- df_rect_board_tiles(8,8)
    df_c <- tibble(piece_side="coin_back", suit=rep(1:4, each=6),
                   x=c(1,3,2,4,1,3,  5,7,6,8,5,7,
                       6,8,5,7,6,8,  2,4,1,3,2,4),
                   y=rep(c(8,7,6,8,7,6,3,2,1,3,2,1), each=2),
                   angle=rep(c(180,0), each=12))
    bind_rows(df_t, df_c)
}

#' @rdname df_game
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

#' @rdname df_game
#' @export
df_chaturaji <- function() {
    df_t <- df_rect_board_tiles(8, 8)
    df_p <- tibble(piece_side="coin_back", 
                   x=c(rep(2,4), 5:8, rep(7,4), 1:4),
                   y=c(5:8, rep(7,4), 1:4, rep(2,4)),
                   suit=rep(1:4, each=4), 
                   angle=rep(c(-90,180,90,0), each=4))
    df_b <- tibble(piece_side="coin_face", rank=3,
                   x=c(1,8,8,1), y=c(8,8,1,1), angle=c(-90,180,90,0))
    df_h <- tibble(piece_side="coin_face", rank=2,
                   x=c(1,7,8,2), y=c(7,8,2,1), angle=c(-90,180,90,0))
    df_r <- tibble(piece_side="die_face", rank=4, suit=1:4,
                   x=c(1,6,8,3), y=c(6,8,3,1), angle=c(-90,180,90,0))
    df_k <- tibble(piece_side="pawn_face", suit=1:4,
                   x=c(1,5,8,4), y=c(5,8,4,1), angle=c(-90,180,90,0))
    bind_rows(df_t, df_p, df_b, df_h, df_r, df_k)
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

#' @rdname df_game
#' @export
textGrob_cribbage_board <- function(suit_colors=rep("black", 4)) {
    gp1 <- gpar(col=c(suit_colors[1], suit_colors[2]), fontsize=32)
    gp2 <- gpar(col=c(suit_colors[3], suit_colors[4]), fontsize=32)
    grobTree(textGrob(1:30, x=0.5, y=3:32, default.units="in", gp=gp1),
             textGrob(31:60, x=3.5, y=32:3, default.units="in", gp=gp1),
             textGrob(1:30, x=5.5, y=3:32, default.units="in", gp=gp2),
             textGrob(31:60, x=8.5, y=32:3, default.units="in", gp=gp2)
             )
}

df_fide_chess_pieces <- function(has_subpack=FALSE) {
    if (has_subpack) {
        df_pb <- tibble(piece_side="coin_back", cfg="piecepack",
                        suit=rep(1:2, each=4), x=1:8, y=7, angle=180)
        df_ob <- tibble(piece_side="tile_face", cfg="subpack",
                        suit=rep(1:2, each=4), x=1:8, y=8, angle=180,
                        rank=c(4,2,3,5,6,3,2,4))
        df_pw <- tibble(piece_side="coin_back", cfg="piecepack",
                        suit=rep(4:3, each=4), x=1:8, y=2)
        df_ow <- tibble(piece_side="tile_face", cfg="subpack",
                        suit=rep(4:3, each=4), x=1:8, y=1, 
                        rank=c(4,2,3,5,6,3,2,4))
        bind_rows(df_pb, df_pw, df_ow, df_ob)
    } else {
        df_p1 <- tibble(piece_side="coin_back",
                        suit=(1:8+1) %% 2 + 1, x=1:8, y=7, angle=180)
        df_p2 <- tibble(piece_side="coin_back",
                        suit=1:8 %% 2 + 3, x=1:8, y=2)
        df_r <- tibble(piece_side="die_face", suit=1:4, rank=4,
                       x=c(1,8,8,1), y=c(8,8,1,1), angle=c(180,180,0,0))
        df_n <- tibble(piece_side="coin_face", rank=2,
                       x=c(2,7,7,2), y=c(8,8,1,1), angle=c(180,180,0,0))
        df_b <- tibble(piece_side="pawn_face", suit=1:4, 
                       x=c(3,6,6,3), y=c(8,8,1,1), angle=c(180,180,0,0))
        df_q <- tibble(piece_side="coin_face", rank=5,
                       x=4, y=c(8,1), angle=c(180,0))
        df_k <- tibble(piece_side="coin_face", rank=6,
                       x=5, y=c(8,1), angle=c(180,0))
        bind_rows(df_p1, df_p2, df_r, df_n, df_b, df_q, df_k)
    }
}

#' @rdname df_game
#' @export
df_fide_chess <- function(has_subpack=FALSE) {
    df_t <- df_rect_board_tiles(8, 8)
    if(has_subpack) { df_t$cfg <- "piecepack" }
    df_p <- df_fide_chess_pieces(has_subpack)
    bind_rows(df_t, df_p)
}

#' @rdname df_game
#' @export
df_ultima <- function() {
    df_t <- df_rect_board_tiles(8, 8)
    df_p1 <- tibble(piece_side="coin_back",
                    suit=(1:8+1) %% 2 + 1, x=1:8, y=7, angle=180)
    df_p2 <- tibble(piece_side="coin_back",
                    suit=1:8 %% 2 + 3, x=1:8, y=2)
    df_r <- tibble(piece_side="coin_face", suit=1:4, rank=c(1,4,4,1),
                   x=c(1,8,8,1), y=c(8,8,1,1), angle=c(180,180,0,0))
    df_n <- tibble(piece_side="die_face", suit=1:4, rank=2,
                   x=c(2,7,7,2), y=c(8,8,1,1), angle=c(180,180,0,0))
    df_b <- tibble(piece_side="pawn_face", suit=1:4, 
                   x=c(3,6,6,3), y=c(8,8,1,1), angle=c(180,180,0,0))
    df_q <- tibble(piece_side="coin_face", rank=5,
                   x=4, y=c(8,1), angle=c(180,0))
    df_k <- tibble(piece_side="coin_face", rank=6,
                   x=5, y=c(8,1), angle=c(180,0))
    bind_rows(df_t, df_p1, df_p2, df_r, df_n, df_b, df_q, df_k)
}

#' @rdname df_game
#' @export
df_baroque_chess <- df_ultima

#' @rdname df_game
#' @export
df_alice_chess <- function(has_subpack=FALSE, max_tiles=24) {
    max_tiles_per_board <- floor(max_tiles / 2)
    df_t1 <- df_rect_board_tiles(8, 8, max_tiles=max_tiles_per_board)
    df_t2 <- df_rect_board_tiles(8, 8, max_tiles=max_tiles_per_board, x0=11)
    df_p <- df_fide_chess_pieces(has_subpack)
    bind_rows(df_t1, df_t2, df_p)
}

#' @rdname df_game
#' @export
df_four_seasons_chess <- function() {
    df_t <- df_rect_board_tiles(8, 8)
    angles <- c(180,90,0,-90)
    suits <- c(1,4,2,3)
    df_p <- tibble(piece_side="coin_back", suit=rep(suits, each=4),
                   x=c(1,2,3,3,6,6,7,8,8,7,6,6,3,3,2,1),
                   y=c(6,6,7,8,8,7,6,6,3,3,2,1,1,2,3,3),
                   angle=rep(angles, each=4))
    df_k <- tibble(piece_side="pawn_face", suit=suits,
                   x=c(1,8,8,1), y=c(8,8,1,1), angle=angles)
    df_r <- tibble(piece_side="die_face", suit=suits, rank=4,
                   x=c(2,7,7,2), y=c(8,8,1,1), angle=angles)
    df_b <- tibble(piece_side="coin_face", suit=suits, rank=1,
                   x=c(2,7,7,2), y=c(7,7,2,2), angle=angles)
    df_n <- tibble(piece_side="coin_face", suit=suits, rank=2,
                   x=c(1,8,8,1), y=c(7,7,2,2), angle=angles)
    bind_rows(df_t, df_p, df_k, df_r, df_b, df_n)
}

#' @rdname df_game
#' @export
df_shogi <- function(cfg2="piecepack") {
    # board
    x_t <- seq(2, 8, by=2)
    y_tr <- rep(c(4, 6), each=4)
    y_tb <- rep(c(2, 8), each=4)
    df_t <- tibble(piece_side="tile_back", 
                   x=rep(x_t,4), y=c(y_tr, y_tb),
                   cfg=rep(c(cfg2, "piecepack"), each=8))
    
    # pawns
    df_pb <- tibble(piece_side="coin_back",
                   suit=1:9 %% 4 +1,
                   x=1:9, y=3,
                   cfg="piecepack")
    df_pt <- df_pb
    df_pt$y <- 7
    df_pt$angle <- 180
    
    # bishops
    df_b <- tibble(piece_side="coin_face", rank=3, cfg=cfg2,
                   x=c(2, 10-2), y=c(2, 10-2), angle=c(0, 180))

    # rooks
    df_r <- tibble(piece_side="coin_face", rank=4, cfg=cfg2,
                   x=c(8, 10-8), y=c(2, 10-2), angle=c(0, 180))

    # lances
    df_l <- tibble(piece_side="coin_face", rank=5, cfg=cfg2,
                   x=c(1,9,10-1,10-9), y=c(1,1,10-1,10-1), angle=c(0,0,180,180))
    # knights
    df_n <- tibble(piece_side="coin_face", rank=2, cfg=cfg2,
                   x=c(2,8,10-2,10-8), y=c(1,1,10-1,10-1), angle=c(0,0,180,180))

    # silvers
    df_s <- tibble(piece_side="coin_face", rank=6, cfg=cfg2,
                   x=c(3,7,10-3,10-7), y=c(1,1,10-1,10-1), angle=c(0,0,180,180))
    # golds
    df_g <- tibble(piece_side="die_face", suit=1:4, rank=6, cfg="piecepack",
                   x=c(4,6,10-4,10-6), y=c(1,1,10-1,10-1), angle=c(0,0,180,180))
    # kings
    df_k <- tibble(piece_side="pawn_face", suit=4-0:1, cfg="piecepack",
                   x=c(5,10-5), y=c(1,10-1), angle=c(0,180))
    bind_rows(df_t, df_pb, df_pt, df_b, df_r, df_l, df_n, df_s, df_g, df_k)
}

#' @rdname df_game
#' @export
df_tablut <- function(die_width=0.5) {
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
    if (piecepackr:::less_than(0.5, die_width)) {
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
df_triactor <- function(seed=NULL, cfg2="playing_cards_expansion") {
    set.seed(NULL)
    df_tb <- tibble(piece_side="tile_back", cfg="piecepack",
                    x=0.5+rep(c(seq(5,15,2),1,2,18,19),2),
                    y=0.5+c(rep(1,6),5,3,3,5,rep(11,6),7,9,9,7),
                    angle=rep(c(rep(0,7),90,90,0),2))
    df_tf <- tibble(piece_side="tile_face", cfg="piecepack",
                    x=0.5+c(3,17,17,3), y=0.5+c(11,11,1,1),
                    suit=1:4, rank=1)
    df_c1 <- tibble(piece_side="coin_back", cfg="piecepack",
                    suit=1:4, rank=sample.int(1:6, 4, replace=TRUE),
                    x=0.5+c(5,15,15,5),y=0.5+c(11,11,1,1))
    df_c2 <- tibble(piece_side="coin_back", cfg=cfg2,
                    suit=1:4, rank=sample.int(1:6, 4, replace=TRUE),
                    x=0.5+c(2,18,18,2), y=0.5+c(9,9,3,3))
    df_p <- tibble(piece_side="pawn_face", cfg=rep(c("piecepack", cfg2), each=4),
                   x=10.5, y=0.5+0:7, angle=90, suit=rep(1:4, 2))
    bind_rows(df_tb, df_tf, df_c1, df_c2, df_p)
}

#' @rdname df_game
#' @export
df_xiangqi <- function() {
    ang2 <- rep(c(180, 0), each=2)
    suits <- c(1,2,4,3)
    x2 <- function(x) { rep(c(x, 10-x), 2) }
    y2 <- function(y) { rep(c(11-y, y), each=2) }
    df_t1 <- df_rect_board_tiles(10, 9)
    df_t2 <- tibble(piece_side="tile_face", suit=c(1,3), rank=2, 
                    x=5, y=c(9, 2), angle=c(180, 0))
    df_che <- tibble(piece_side="die_face", suit=suits, rank=4,
                     x=x2(1), y=y2(1), angle=ang2)
    df_ma <- tibble(piece_side="coin_face", rank=2,
                    x=x2(2), y=y2(1), angle=ang2)
    df_xiang <- tibble(piece_side="coin_face", rank=3,
                    x=x2(3), y=y2(1), angle=ang2)
    df_shi <- tibble(piece_side="coin_face", rank=5,
                    x=x2(4), y=y2(1), angle=ang2)
    df_jiang <- tibble(piece_side="coin_face", rank=6,
                       x=5, y=c(1,10), angle=c(0, 180))
    df_pao <- tibble(piece_side="pawn_face", suit=suits,
                     x=x2(2), y=y2(3), angle=ang2)
    df_zu1 <- tibble(piece_side="coin_back",
                   suit=(1:5+1) %% 2 + 3, x=seq(1, 9, 2), y=4)
    df_zu2 <- tibble(piece_side="coin_back",
                   suit=(1:5+1) %% 2 + 1, x=seq(1, 9, 2), y=7, angle=180)

    bind_rows(df_t1, df_t2, df_che, df_ma, df_xiang, df_shi, df_jiang,
              df_pao, df_zu1, df_zu2)
}
