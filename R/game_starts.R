#' Data frames of starting diagrams for various games
#'
#' \code{tibble} data frames of starting diagrams for various games.
#'   data frame output can usually be plotted with \code{pmap_piece(df, cfg = cfg, default.units = "in")}.
#'
#' Here is some more information about the various games:
#'   \describe{
#'   \item{Alice Chess}{Popular chess variant invented by V. R. Parton.
#'                      See \url{https://en.wikipedia.org/wiki/Alice_chess}.}
#'  \item{Alien City}{Awarding winning game by Michael Schoessow that is traditionally played with both a piecepack
#'                    and icehouse pieces.  See \url{http://www.ludism.org/ppwiki/AlienCity}.}
#'  \item{Backgammon}{One of the oldest board games.  See \url{http://www.ludism.org/ppwiki/Backgammon}.}
#'  \item{Cell Management}{Solitaire game for the piecepack by Mark Goadrich.
#'                         See \url{http://www.ludism.org/ppwiki/CellManagement}.}
#'  \item{Chaturaji}{An old 4-player chess variant.
#'                   See \url{http://www.ludism.org/ppwiki/Chaturaji}.}
#'  \item{(American) Checkers}{A traditional board game also known as \dQuote{(English) Draughts}.
#'                  First adapted to the piecepack by Mark A. Biggar.
#'                  See \url{http://www.ludism.org/ppwiki/Checkers}.}
#'  \item{(International) Chess}{Very popular board game first adapted to the piecepack by Ron Hale-Evans.
#'                               See \url{http://www.ludism.org/ppwiki/Chess}.}
#'  \item{Cribbage}{Traditional card game traditionally uses a special board to keep score
#'                  but one can use a piecepack as a cribbage board instead.
#'                  See \url{http://www.ludism.org/ppwiki/Cribbage}.}
#'  \item{Desfases}{Game by Antonio Recuenco Muñoz.
#'                  Runner-up for best game in the eighth community piecepack game design contest.
#'                  See \url{http://www.ludism.org/ppwiki/Desfases}.}
#'  \item{Everest}{Game by Mark A. Biggar that features a Roborally programmed movement system.
#'                 See \url{http://www.ludism.org/ppwiki/Everest}.}
#'  \item{Four Field Kono}{Traditional 2-player Korean abstract adapted to piecepack by Michael Schoessow.
#'                         See \url{http://www.ludism.org/ppwiki/FourFieldKono}.}
#'  \item{Four Seasons Chess}{A traditional four person chess variant recorded in \emph{Libro del Acedrex}.
#'                            See \url{https://www.chessvariants.com/historic.dir/4seiz.html}.}
#'  \item{Fujisan}{Popular solitaire game for the piecepack by James \dQuote{Kyle} Droscha.
#'                 See \url{http://www.ludism.org/ppwiki/Fuji-san}.}
#'  \item{Ice Floe}{Game by Tim Schutz requiring a piecepack and piecepack pyramids.
#'                  See \url{http://www.ludism.org/ppwiki/IceFloe}.}
#'  \item{Ley Lines}{Piecepack game by James \dQuote{Kyle} Droscha.
#'                   See \url{http://www.ludism.org/ppwiki/LeyLines}.}
#'  \item{Nine Men's Morris}{Traditional board game.
#'        See \url{https://en.wikipedia.org/wiki/Nine_men\%27s_morris}.}
#'  \item{Plans Of Action}{Solitaire piecepack game by L. Edward Pulley.
#'        See \url{http://www.ludism.org/ppwiki/PlansOfAction}.}
#'  \item{Relativity}{Piecepack game by Marty and Ron Hale-Evans.
#'                    See \url{http://www.ludism.org/ppwiki/Relativity}.}
#'  \item{Salta}{Two-player abstract invented in 1899 by Konrad Heinrich Büttgenbach.
#'               See \url{https://en.wikipedia.org/wiki/Salta_(game)}.}
#'  \item{San Andreas}{Piecepack game about the big earthquake that will hit California.
#'                    See \url{http://www.ludism.org/ppwiki/SanAndreas}.}
#'  \item{Shogi}{AKA \dQuote{Japanese chess} is a major chess variant.
#'               See \url{http://www.ludism.org/ppwiki/Shogi}.}
#'  \item{Tablut}{Traditional two-player abstract played by the Sámi people until at least the 1700s.
#'                See \url{http://www.ludism.org/ppwiki/Tablut}.}
#'  \item{The \dQuote{In} Crowd}{Piecepack game by Jeb Havens and Ian Schreiber.
#'        See \url{http://www.ludism.org/ppwiki/TheInCrowd}.}
#'  \item{Triactor}{Piecepack game by Jonathan Dietrich, Julie Taylor, and Ken MacKeigan.
#'                  See \url{http://www.ludism.org/ppwiki/Triactor}.}
#'  \item{Twelve Men's Morris}{Traditional two-player abstract.
#'                             See \url{https://en.wikipedia.org/wiki/Morabaraba}.}
#'  \item{Ultima}{AKA \dQuote{Baroque chess}, a chess variant by Robert Abbott.
#'                See \url{https://en.wikipedia.org/wiki/Baroque_chess}.}
#'  \item{Xiangqi}{AKA \dQuote{Chinese chess}, a major chess variant.
#'                 See \url{http://www.ludism.org/ppwiki/Xiangqi}.}
#'  \item{Wormholes}{Piecepack game by Marty and Ron Hale-Evans.
#'                   See \url{http://www.ludism.org/ppwiki/Wormholes}.}
#'}
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
#' @name df_game
NULL

#' @importFrom dplyr %>%
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate

#' @rdname df_game
#' @export
df_alien_city <- function(seed = NULL, tiles = NULL) {
    set.seed(seed)
    df_t1 <- tibble(piece_side = "tile_face",
                   x = 0.5+rep(seq(1,7,2),5),
                   y = 0.5+rep(seq(9,1,-2), each = 4))
    if (is.null(tiles)) {
        df_t2 <- tibble(suit = rep(1:4, each = 5),
                        rank = rep(1:5, 4)+1,
                        angle = 90 * (sample(4, 20, replace = TRUE)-1))
        df_t2 <- df_t2[sample.int(20), ]
    } else {
        df_t2 <- process_tiles(tiles, 20)
    }
    bind_cols(df_t1, df_t2)
}

#' @rdname df_game
#' @export
df_cell_management <- function(seed = NULL) {
    set.seed(seed)

    # hexagon distances
    #    between closest vertices: 2
    #    between vertex to opposite vertix: 4
    #    between vertex to vertex 2 away: 2*sqrt(3)
    x0 <- 5.5
    y0 <- 4.5+sqrt(3)
    # Sun and Crown tiles
    theta <- seq(30, 330, by = 60)
    r <- sqrt(3) + 1
    xt <- x0 + to_x(theta, r)
    yt <- y0 + to_y(theta, r)
    df_t <- tibble(piece_side = "tile_face", suit = 1, rank = sample.int(6),
                   x = xt, y = yt, angle = theta-90)
    df_t[sample.int(6,3), "suit"] <- 3

    # Moon tiles and Coins
    last_played <- 1
    moon_ranks <- c(sample.int(5)+1, 1)
    df_tm <- tibble(piece_side = "tile_face", suit = 2, rank = moon_ranks, x = NA, y = NA, angle = NA)
    df_c <- tibble(piece_side = "coin_face", rank = rep(NA, 12), x = NA, y = NA, angle = NA)
    for (ii in seq(along = moon_ranks)) {
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
    df_p <- tibble(piece_side = "pawn_face", suit = 1:2, x = x0+c(4,5), y = y0)

    bind_rows(df_t, df_tm, df_c, df_p)
}

#' @rdname df_game
#' @export
df_desfases <- function(seed = NULL, tiles = NULL, dice = NULL) {
    set.seed(seed)
    df_txy <- tibble(piece_side = "tile_face",
                     x = 2+rep(seq(1,9,2), 5), y = 2+rep(seq(1,9,2), each=5))
    df_txy <- df_txy[-13, ]
    if (is.null(tiles)) {
        df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24), ]
        df_tsr$angle <- ((df_tsr$suit + 1) * -90) %% 360
    } else {
        df_tsr <- process_tiles(tiles)
    }
    df_t <- bind_cols(df_txy, df_tsr)

    df_c <- tibble(piece_side = "coin_face",
                   x = c(11:6, rep(13, 6), 3:8, rep(1, 6)),
                   y = c(rep(13, 6), 3:8, rep(1, 6), 11:6),
                   suit = rep(1:4, each=6), rank = rep(1:6, 4),
                   angle = rep(c(180, 90, 0, -90), each=6))

    if (is.null(dice)) {
        dice <- random_dice()
    } else {
        dice <- process_ranks(dice)
    }
    df_d <- tibble(piece_side = "die_face",
                   x = c(3, 13, 11, 1), y = c(13, 11, 1, 3),
                   angle = c(180, 90, 0, -90), suit = 1:4,
                   rank = dice)

    df_p <- df_d %>% mutate(piece_side = "pawn_face", rank = NULL)

    for (i in seq(4)) {
        # Move relevant coins under their respective dice
        index <- which(df_c$rank == dice[i] & df_c$suit == i)
        df_c[index, "piece_side"] <- "coin_back"
        df_c[index, "x"] <- df_d$x[i]
        df_c[index, "y"] <- df_d$y[i]

        # Move pawns un top of their respective dice
        index <- which(df_t$rank == dice[i] & df_t$suit == i)
        df_p[i, "x"] <- df_t$x[index]
        df_p[i, "y"] <- df_t$y[index]
    }

    bind_rows(df_t, df_c, df_d, df_p)
}

#' @rdname df_game
#' @export
df_everest <- function() {
    df_t1 <- tibble(piece_side = "tile_back",
                    x = 0.5+c(seq(1,7,2),seq(2,6,2), seq(1,7,2)),
                    y = 0.5+c(rep(1, 4), rep(3, 3), rep(5,4)))
    df_t2 <- df_rect_board_tiles(4,6, x0 = 2, y0 = 2)
    df_t3 <- df_rect_board_tiles(4,4, x0 = 3, y0 = 2)
    df_t4 <- df_rect_board_tiles(2,4, x0 = 3, y0 = 3)
    df_t5 <- tibble(piece_side = "tile_back", x = 4.5, y = 3.5)
    df_p <- tibble(piece_side = "pawn_face",
                   x = c(1,8,8,1), y = c(5,5,2,2), suit = 1:4)
    bind_rows(df_t1, df_t2, df_t3, df_t4, df_t5, df_p)
}

#' @rdname df_game
#' @export
df_fujisan <- function(seed = NULL, coins = NULL, dice = NULL) {
    set.seed(seed)
    if (is.null(coins)) {
        coins <- random_fujisan_coins()
    } else if (is.character(coins)) {
        coins <- process_ranks(coins) - 1
    }
    if (is.vector(coins)) {
        coins <- matrix(coins, nrow = 2, byrow = TRUE)
    }
    df_t <- tibble(piece_side = "tile_back", y = 1.5,
                   x = 1.5+c(seq(1,11,2),seq(2,10,2),seq(3,9,2),4,6,8,5,7,5,7,6,6))
    suit <- rev((0:23%%4)+1)
    df_c <- tibble(piece_side = "coin_face", x = rep(2:13, 2), y = rep(1:2, each = 12),
                   suit = suit, rank = c(coins[2, ], coins[1, ]) + 1)
    df_p <- tibble(piece_side = "pawn_face", x = c(1,14,14,1), y = c(2,2,1,1), suit = 1:4)
    if (first_move_needs_dice(coins)) {
        if (is.null(dice)) {
            dice <- random_dice()
        } else {
            dice <- process_ranks(dice)
        }
        df_d <- tibble(piece_side = "die_face", x = c(16,17,16,17), y = c(2,2,1,1), suit = c(1,2,4,3), rank = dice)
        bind_rows(df_t, df_c, df_p, df_d)
    } else {
        bind_rows(df_t, df_c, df_p)
    }
}

#' @rdname df_game
#' @export
df_ice_floe <- function() {
    df <- tibble(piece_side = "tile_face",
           x = rep(seq(1,9,2), 5),
           y = rep(seq(9,1,-2), each=5),
           suit = c(1,1,3,2,2, 1,1,4,2,2, 2,3,NA,1,4, 4,4,2,3,3, 4,4,1,3,3),
           rank = c(2,3,1,2,3, 4,5,1,4,5, 0,0,NA,0,0, 2,3,1,2,3, 4,5,1,4,5) + 1)
    df[-13, ]
}

#' @rdname df_game
#' @export
df_ley_lines <- function() {
    df <- tibble(piece_side = "tile_back",
                 x = c(  6,8,   7,9,   7,9,   3,5, 8,10, 2,4, 9,11, 2,4,13, 7,9,11, 13, 9,11, 7) - 0.5,
                 y = c(15,15, 13,13, 11,11, 10,10,  9,9, 8,8, 7,7,  6,6,6,  5,5,5,  4,  3,3,  2) - 0.5,
                 suit = rep(1:4, each=6), rank = rep(1:6, 4))
}

#' @rdname df_game
#' @export
df_plans_of_action <- function(seed = NULL, coins = NULL) {
    if (is.null(coins)) {
        set.seed(seed)
        suits <- sample(rep(1:4, 6), 24)
    } else {
        suits <- process_suits(coins)
    }
    df_tiles <- df_rect_board_tiles(nrows=8, ncols=8)
    df_coins <- tibble(piece_side = "coin_back", suit = suits,
                       x = rep(2:7, 4), y = rep(6:3, each=6))
    bind_rows(df_tiles, df_coins)
}

#' @rdname df_game
#' @export
df_relativity <- function(seed = NULL, coins = NULL) {
    df_tiles <- df_rect_board_tiles(nrows=4, ncols=6)
    if (is.null(coins)) {
        set.seed(seed)
        ranks <- c(sample.int(6), sample.int(6), sample.int(6), sample.int(6))
        while (should_resample_relativity(ranks)) {
            ranks <- c(sample.int(6), sample.int(6), sample.int(6), sample.int(6))
        }
        ranks <- ranks[c(1:3, 7:9, 4:6, 10:12, 13:15, 19:21, 16:18, 22:24)]
    } else {
        ranks <- process_ranks(coins)
    }
    df_coins <- tibble(piece_side = "coin_face", rank = ranks,
                       x = rep(1:6, 4), y = rep(4:1, each=6),
                       suit = rep(c(1,2,1,2,4,3,4,3), each=3))
    bind_rows(df_tiles, df_coins)
}

should_resample_relativity <- function(coins) {
    stats::sd(c(coins[6], coins[4], coins[3], coins[1])) == 0
}

#' @rdname df_game
#' @export
df_the_in_crowd <- function() {
    df_t1 <- df_rect_board_tiles(6, 6)
    df_t2 <- df_rect_board_tiles(4, 4, 2, 2)
    df_t3 <- tibble(piece_side="tile_back", x=3.5, y=3.5)
    bind_rows(df_t1, df_t2, df_t3)
}

#' @rdname df_game
#' @export
df_san_andreas <- function() {
    x <- 0.5+c(rep(c(1,3,5), 3), 2,4,6, 3,5,7, 4,6,8, 5,7,9, 7,9)
    y <- 0.5+c(rep(c(15,13,11,9,7,5,3), each=3), 1, 1)
    tibble(piece_side="tile_back", x=x, y=y,
           suit = rep(1:4, each=6, length.out=23), rank = rep(1:6, 4, length.out=23))
}


#' @rdname df_game
#' @export
df_triactor <- function(seed = NULL, cfg2 = "playing_cards_expansion") {
    set.seed(seed)
    df_tb <- tibble(piece_side = "tile_back", cfg = "piecepack",
                    x = 0.5+rep(c(seq(5,15,2),1,2,18,19),2),
                    y = 0.5+c(rep(1,6),5,3,3,5,rep(11,6),7,9,9,7),
                    angle = rep(c(rep(0,7),90,90,0),2))
    df_tf <- tibble(piece_side = "tile_face", cfg = "piecepack",
                    x = 0.5+c(3,17,17,3), y = 0.5+c(11,11,1,1),
                    suit = 1:4, rank = 1)
    df_c1 <- tibble(piece_side = "coin_back", cfg = "piecepack",
                    suit = 1:4, rank = sample.int(1:6, 4, replace = TRUE),
                    x = 0.5+c(5,15,15,5),y = 0.5+c(11,11,1,1))
    df_c2 <- tibble(piece_side = "coin_back", cfg = cfg2,
                    suit = 1:4, rank = sample.int(1:6, 4, replace = TRUE),
                    x = 0.5+c(2,18,18,2), y = 0.5+c(9,9,3,3))
    df_p <- tibble(piece_side = "pawn_face", cfg = rep(c("piecepack", cfg2), each = 4),
                   x = 10.5, y = 0.5+0:7, angle = 90, suit = rep(1:4, 2))
    bind_rows(df_tb, df_tf, df_c1, df_c2, df_p)
}


#' @rdname df_game
#' @export
df_wormholes <- function() {
    df_tiles <- tibble(piece_side = "tile_back",
                       x = -0.5 + 2*c(1,2, 2,3, 2,3,4, 3,4,5, 2,3,4, 1,2,3, 2,3,4, 3,4, 4,5),
                       y = -0.5 + 2*c(9,9, 8,8, 7,7,7, 6,6,6, 5,5,5, 4,4,4, 3,3,3, 2,2, 1,1))
    df_coins <- tibble(piece_side = "pawn_face", x=c(10,10,1,1), y=c(1,2,17,18), suit=1:4)
    bind_rows(df_tiles, df_coins)
}
