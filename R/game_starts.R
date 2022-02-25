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
#'                    and icehouse pieces.  See \url{https://www.ludism.org/ppwiki/AlienCity}.}
#'  \item{Alquerque}{Traditional game also known as Qirkat.  See \url{https://en.wikipedia.org/wiki/Alquerque}.}
#'  \item{Awithlaknannai Mosona}{Traditional Zuni Alquerque variant.
#'                               See \url{https://en.wikipedia.org/wiki/Awithlaknannai_Mosona}.}
#'  \item{Backgammon}{One of the oldest board games.  See \url{https://www.ludism.org/ppwiki/Backgammon}.}
#'  \item{Brandubh}{AKA Brandub, a two-player abstract of the hnefatafl family played by the Irish.
#'                  See \url{http://www.cyningstan.com/game/125/brandub} for more information.}
#'  \item{Breakthrough}{Two-player abstract invented by Dan Troyka in 2000
#'                      that won the 2001 8x8 Game Design Competition.
#'                      See \url{https://en.wikipedia.org/wiki/Breakthrough_(board_game)}.}
#'  \item{Cell Management}{Solitaire game for the piecepack by Mark Goadrich.
#'                         See \url{https://www.ludism.org/ppwiki/CellManagement}.}
#'  \item{Chaturaji}{An old 4-player chess variant.
#'                   See \url{https://www.ludism.org/ppwiki/Chaturaji}.}
#'  \item{(American) Checkers}{A traditional board game also known as \dQuote{(English) Draughts}.
#'                  First adapted to the piecepack by Mark A. Biggar.
#'                  See \url{https://www.ludism.org/ppwiki/Checkers}.}
#'  \item{(International) Chess}{Very popular board game first adapted to the piecepack by Ron Hale-Evans.
#'                               See \url{https://www.ludism.org/ppwiki/Chess}.}
#'  \item{Chinese Checkers}{A port of Chinese Checkers by Mark A. Biggar.
#'                          See \url{https://www.ludism.org/ppwiki/ChineseCheckers}.}
#'  \item{Coin Collectors}{A solitaire by Don Kirkby.
#'                         See \url{https://www.ludism.org/ppwiki/CoinCollectors}.}
#'  \item{Cribbage}{Traditional card game traditionally uses a special board to keep score
#'                  but one can use a piecepack as a cribbage board instead.
#'                  See \url{https://www.ludism.org/ppwiki/Cribbage}.}
#'  \item{Crossings}{An abstract invented by Robert Abbot.
#'                   See \url{https://en.wikipedia.org/wiki/Crossings_(game)}}
#'  \item{Desfases}{Game by Antonio Recuenco Muñoz.
#'                  Runner-up for best game in the eighth community piecepack game design contest.
#'                  See \url{https://www.ludism.org/ppwiki/Desfases}.}
#'  \item{Easy Slider}{A solitaire by Ron Hale-Evans and Marty Hale-Evans.
#'                  See \url{https://www.ludism.org/ppwiki/EasySlider}.}
#'  \item{Evade}{Adaption by Mark A. Biggar of a game by Alex Randolph.
#'                 See \url{https://www.ludism.org/ppwiki/Evade}.}
#'  \item{Everest}{Game by Mark A. Biggar that features a Roborally programmed movement system.
#'                 See \url{https://www.ludism.org/ppwiki/Everest}.}
#'  \item{Four Blind Mice}{Multi-player puzzle by Tim Farley inspired by Ricochet Robot by Alex Randolph.
#'                         See \url{https://www.ludism.org/ppwiki/FourBlindMice}.}
#'  \item{Four Field Kono}{Traditional 2-player Korean abstract adapted to piecepack by Michael Schoessow.
#'                         See \url{https://www.ludism.org/ppwiki/FourFieldKono}.}
#'  \item{Four Seasons Chess}{A traditional four person chess variant recorded in \emph{Libro del Acedrex}.
#'                            See \url{https://www.chessvariants.com/historic.dir/4seiz.html}.}
#'  \item{Froggy Bottom}{Game by Clay Blankenship.
#'                       See \url{https://www.ludism.org/ppwiki/FroggyBottom}.}
#'  \item{Fujisan}{Popular solitaire game for the piecepack by James \dQuote{Kyle} Droscha.
#'                 See \url{https://www.ludism.org/ppwiki/Fuji-san}.}
#'  \item{Grasshopper}{Two-player Halma variant playable on a draughts board.
#'                     See \url{http://www.cyningstan.com/game/71/grasshopper}.}
#'  \item{Iceberg}{Game by Ken Leyhe.
#'                 See \url{https://www.ludism.org/ppwiki/Iceberg}.}
#'  \item{Ice Floe}{Game by Tim Schutz requiring a piecepack and piecepack pyramids.
#'                  See \url{https://www.ludism.org/ppwiki/IceFloe}.}
#'  \item{Japan}{Game by Daniel Ajoy and María Fernanda Ausay.
#'               See \url{https://www.ludism.org/ppwiki/Japan}.}
#'  \item{Jul-Gonu}{Traditional 2-player Korean abstract adapted to piecepack by Michael Schoessow.
#'                         See \url{https://www.ludism.org/ppwiki/JulGonu}.}
#'  \item{Landlocked}{Piecepack game by Eric Witt.
#'                    See \url{https://www.ludism.org/ppwiki/Landlocked}.}
#'  \item{Ley Lines}{Piecepack game by James \dQuote{Kyle} Droscha.
#'                   See \url{https://www.ludism.org/ppwiki/LeyLines}.}
#'  \item{Lines of Action}{An abstract designed by Claude Soucie.
#'                         See \url{https://en.wikipedia.org/wiki/Lines_of_Action}.}
#'  \item{Ludo}{Racing game based off Pachisi.
#'              See \url{https://en.wikipedia.org/wiki/Ludo_(board_game)}.}
#'  \item{Mathrix}{A solitaire by Clark Rodeffer.
#'                 See \url{https://www.ludism.org/ppwiki/Mathrix}}
#'  \item{Nine Men's Morris}{Traditional board game.
#'        See \url{https://en.wikipedia.org/wiki/Nine_men\%27s_morris}.}
#'  \item{Pass the Food}{A dexterity game by Trevor L Davis.
#'                       See \url{https://www.ludism.org/ppwiki/PassTheFood}}
#'  \item{Piece Gaps}{A solitaire by Chris Brooks inspired by the Gaps card game.
#'                    See \url{https://www.ludism.org/ppwiki/PieceGaps}}
#'  \item{Piece Packing Pirates}{A solitaire sea adventure by Clark Rodeffer.
#'                               See \url{https://www.ludism.org/ppwiki/PiecePackingPirates}.}
#'  \item{Piecepack Klondike}{A solitaire game by Eric Witt inspired by the Klondike card game.
#'        See \url{https://ludism.org/ppwiki/PiecepackKlondike}}
#'  \item{Piecepackman}{A cooperative maze game by Dan Burkey inspired by the video game
#'                      Pac-Man, designed for Namco by Toru Iwatani.
#'                      See \url{https://www.ludism.org/ppwiki/Piecepackman}}
#'  \item{Plans Of Action}{Solitaire piecepack game by L. Edward Pulley.
#'        See \url{https://www.ludism.org/ppwiki/PlansOfAction}.}
#'  \item{Quatri}{An abstract adapted to piecepack by Michael Schoessow.
#'                See \url{https://www.ludism.org/ppwiki/Quatri}.}
#'  \item{Relativity}{Piecepack game by Marty and Ron Hale-Evans.
#'                    See \url{https://www.ludism.org/ppwiki/Relativity}.}
#'  \item{Salta}{Two-player abstract invented in 1899 by Konrad Heinrich Büttgenbach.
#'               See \url{https://en.wikipedia.org/wiki/Salta_(game)}.}
#'  \item{San Andreas}{Piecepack game about the big earthquake that will hit California.
#'                    See \url{https://www.ludism.org/ppwiki/SanAndreas}.}
#'  \item{Shogi}{AKA \dQuote{Japanese chess} is a major chess variant.
#'               See \url{https://www.ludism.org/ppwiki/Shogi}.}
#'  \item{Skyscrapers}{A solitaire by Michael Schoessow.
#'                     See \url{https://www.ludism.org/ppwiki/Skyscrapers}.}
#'  \item{Slides of Action}{An abstract connection game by Clark Rodeffer.
#'                          See \url{https://www.ludism.org/ppwiki/SlidesOfAction}.}
#'  \item{Tablut}{Traditional two-player abstract played by the Sámi people until at least the 1700s.
#'                See \url{https://www.ludism.org/ppwiki/Tablut}.}
#'  \item{The \dQuote{In} Crowd}{Piecepack game by Jeb Havens and Ian Schreiber.
#'        See \url{https://www.ludism.org/ppwiki/TheInCrowd}.}
#'  \item{The Magic Bag}{Piecepack game by Rob LeGood.
#'                       See \url{https://www.ludism.org/ppwiki/The_Magic_Bag}.}
#'  \item{Tower of Babel}{Solitaire piecepack game by Mark A. Biggar.
#'        See \url{https://www.ludism.org/ppwiki/TowerOfBabel}.}
#'  \item{Triactor}{Piecepack game by Jonathan Dietrich, Julie Taylor, and Ken MacKeigan.
#'                  See \url{https://www.ludism.org/ppwiki/Triactor}.}
#'  \item{Tula}{Solitaire piecepack game by James \dQuote{Kyle} Droscha.
#'              See \url{https://www.ludism.org/ppwiki/Tula}.}
#'  \item{Turkish Draughts}{AKA \dQuote{Dama} is a traditional checkers variant played in the Mediterranean.
#'                          See \url{https://en.wikipedia.org/wiki/Turkish_draughts}.}
#'  \item{Twelve Men's Morris}{Traditional two-player abstract.
#'                             See \url{https://en.wikipedia.org/wiki/Morabaraba}.}
#'  \item{Ultima}{AKA \dQuote{Baroque chess}, a chess variant by Robert Abbott.
#'                See \url{https://en.wikipedia.org/wiki/Baroque_chess}.}
#'  \item{Wormholes}{Piecepack game by Marty and Ron Hale-Evans.
#'                   See \url{https://www.ludism.org/ppwiki/Wormholes}.}
#'  \item{Xiangqi}{AKA \dQuote{Chinese chess}, a major chess variant.
#'                 See \url{https://www.ludism.org/ppwiki/Xiangqi}.}
#'}
#'
#' @param seed Seed that determines setup, either an integer or \code{NULL}
#' @param cfg2 A string of a piecepack expansion (or perhaps \code{"piecepack"} for a second piecepack)
#' @param has_matchsticks Has matchsticks
#' @param has_subpack Has a piecepack subpack
#' @param tiles String of tile layout
#' @param coins String of coin layout
#' @param dice String of dice layout
#' @param pawns String of pawns layout
#' @param die_width Width of dice
#' @param max_tiles Maximum number of (piecepack) tiles available to build boards
#' @param suit_colors Character vector of the suit colors
#' @param variant Number of variant.
#' @rdname df_game
#' @name df_game
NULL

#' @importFrom dplyr %>%
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr setdiff

#' @rdname df_game
#' @export
df_alien_city <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_t1 <- tibble(piece_side = "tile_face",
                   x = 0.5+rep(seq(1,7,2),5),
                   y = 0.5+rep(seq(9,1,-2), each = 4))
    if (is.null(tiles)) {
        df_t2 <- tibble(suit = rep(1:4, each = 5),
                        rank = rep(1:5, 4)+1,
                        angle = 90 * (sample(4, 20, replace = TRUE)-1))
        df_t2 <- df_t2[sample.int(20L), ]
    } else {
        df_t2 <- process_tiles(tiles, 20)
    }
    bind_cols(df_t1, df_t2)
}

#' @rdname df_game
#' @export
df_cell_management <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)

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
    df_t <- tibble(piece_side = "tile_face", suit = 1, rank = sample.int(6L),
                   x = xt, y = yt, angle = theta-90)
    df_t[sample.int(6L, 3), "suit"] <- 3

    # Moon tiles and Coins
    last_played <- 1
    moon_ranks <- c(sample.int(5L)+1, 1)
    df_tm <- tibble(piece_side = "tile_face", suit = 2, rank = moon_ranks,
                    x = NA_real_, y = NA_real_, angle = NA_real_)
    df_c <- tibble(piece_side = "coin_face", rank = rep(NA_integer_, 12),
                   x = NA_real_, y = NA_real_, angle = NA_real_)
    for (ii in seq(along = moon_ranks)) {
        angle <- as.numeric(df_t[which(df_t$rank == last_played), "angle"])
        theta <- angle + 90
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
df_chinese_checkers <- function() {
    df_t <- df_rect_board_tiles(8, 8)
    df_c <- tibble(piece_side = "coin_back",
                   x = c(1:3, 1:2, 1, 6:8, 7:8, 8, 8, 7:8, 6:8, 1, 1:2, 1:3),
                   y = c(8,8,8, 7,7, 6, 8,8,8, 7,7, 6, 3, 2,2, 1,1,1, 3, 2,2, 1,1,1),
                   suit = rep(1:4, each=6), rank = rep(1:6, 4),
                   angle = 45 + rep(c(180, 90, 0, -90), each=6))
    bind_rows(df_t, df_c)
}

#' @rdname df_game
#' @export
df_mini_halma <- df_chinese_checkers

#' @rdname df_game
#' @export
df_coin_collectors <- function(seed = NULL, tiles = NULL, coins = NULL, dice = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- df_donut_tiles(seed = NULL, tiles = tiles, x0 = 1.5, y0 = 1.5)
    if (is.null(coins)) {
       ranks <- rep(1:6, 4)[sample.int(24L)]
    } else {
       ranks <- process_ranks(coins)
    }
    df_coins <- tibble(piece_side = "coin_face",
                       x = df_tiles$x + 0.5, y = df_tiles$y - 0.5,
                       rank = ranks, suit = 1L)
    ranks <- if (is.null(dice)) sample.int(6L, 4L, TRUE) else process_ranks(dice)
    df_dice <- tibble(piece_side = "die_face", x = 12, y = c(9, 7, 5, 3), rank = ranks, suit = 1:4)
    df_pawn <- tibble(piece_side = "pawn_face", x = c(5, 6, 6, 5), y = c(6, 6, 5, 5), rank = 1L, suit = 1:4)
    bind_rows(df_tiles, df_coins, df_dice, df_pawn)
}

#' @rdname df_game
#' @export
df_desfases <- function(seed = NULL, tiles = NULL, dice = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_txy <- tibble(piece_side = "tile_face",
                     x = 2+rep(seq(1, by=3, length.out=5), 5),
                     y = 2+rep(seq(1, by=3, length.out=5), each=5))
    df_txy <- df_txy[-13, ]
    if (is.null(tiles)) {
        df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
        df_tsr$angle <- ((df_tsr$suit + 1) * -90) %% 360
    } else {
        df_tsr <- process_tiles(tiles)
    }
    df_t <- bind_cols(df_txy, df_tsr)

    df_c <- tibble(piece_side = "coin_face",
                   x = c(14:9, rep(17, 6), 4:9, rep(1, 6)),
                   y = c(rep(17, 6), 4:9, rep(1, 6), 14:9),
                   suit = rep(1:4, each=6), rank = rep(1:6, 4),
                   angle = rep(c(180, 90, 0, -90), each=6))

    if (is.null(dice)) {
        dice <- random_dice()
    } else {
        dice <- process_ranks(dice)
    }
    df_d <- tibble(piece_side = "die_face",
                   x = c(7, 17, 11, 1), y = c(17, 11, 1, 7),
                   angle = c(180, 90, 0, -90), suit = 1:4,
                   rank = dice)

    df_p <- df_d %>% mutate(piece_side = "pawn_face", rank = NULL)

    for (i in seq(4)) {
        # Move relevant coins under their respective dice
        index <- which(df_c$rank == dice[i] & df_c$suit == i)
        df_c[index, "piece_side"] <- "coin_back"
        df_c[index, "x"] <- df_d$x[i]
        df_c[index, "y"] <- df_d$y[i]

        # Move pawns on top of their respective tiles
        index <- which(df_t$rank == dice[i] & df_t$suit == i)
        df_p[i, "x"] <- df_t$x[index]
        df_p[i, "y"] <- df_t$y[index]
    }

    df <- bind_rows(df_t, df_c, df_d, df_p)
    attr(df, "scale_factor") <- 3
    df
}

#' @rdname df_game
#' @export
df_easy_slider <- function(seed = NULL, tiles = NULL, coins = NULL, pawns = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- df_donut_tiles(seed = NULL, tiles = tiles, x0 = 2, y0 = 2)
    ranks <- if (is.null(coins)) sample.int(5L) + 1L else process_ranks(coins)
    df_coins <- tibble(piece_side = "coin_face", x = seq(2, 10, 2), y = 11.5, rank = ranks, suit = 1L)
    suits <- if (is.null(pawns)) sample.int(4L) else process_suits(pawns)
    df_pawns <- tibble(piece_side = "pawn_face", x = 0.5, y = seq(10, 4, -2), rank = 1L, suit = suits)
    df <- bind_rows(df_tiles, df_coins, df_pawns)
    attr(df, "scale_factor") <- 2
    df
}

#' @rdname df_game
#' @export
df_evade <- function() df_rect_board_tiles(ncols = 6, nrows = 6)

#' @rdname df_game
#' @export
df_everest <- function() {
    df_t1 <- tibble(piece_side = "tile_back",
                    x = 0.5 + c(seq(1, 7, 2),seq(2, 6, 2), seq(1, 7, 2)),
                    y = 0.5 + c(rep(1, 4), rep(3, 3), rep(5, 4)))
    df_t2 <- df_rect_board_tiles(4, 6, x0 = 2, y0 = 2)
    df_t3 <- df_rect_board_tiles(4, 4, x0 = 3, y0 = 2)
    df_t4 <- df_rect_board_tiles(2, 4, x0 = 3, y0 = 3)
    df_t5 <- tibble(piece_side = "tile_back", x = 4.5, y = 3.5)
    df_p <- tibble(piece_side = "pawn_face",
                   x = c(1,8,8,1), y = c(5,5,2,2), suit = 1:4)
    bind_rows(df_t1, df_t2, df_t3, df_t4, df_t5, df_p)
}

#' @rdname df_game
#' @export
df_four_blind_mice <- function() df_rect_board_tiles(ncols = 8, nrows = 8)

#' @rdname df_game
#' @export
df_froggy_bottom <- function() df_rect_board_tiles(ncols = 6, nrows = 8)

#' @rdname df_game
#' @export
df_fujisan <- function(seed = NULL, coins = NULL, dice = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
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
df_iceberg <- function(seed = NULL, tiles = NULL) {
    df_donut_tiles(seed = seed, tiles = tiles, x0 = 1, y0 = 1, face = FALSE)
}

#' @rdname df_game
#' @export
df_ice_floe <- function() {
    tiles <- "S2S3CaM2M3/S4S5AaM4M5/MnCnSnAn/A2A3MaC2C3/A4A5SaC4C5"
    df_donut_tiles(tiles = tiles, x0 = 1.5, y0 = 1.5)
}

#' @rdname df_game
#' @export
df_japan <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- tibble(piece_side = "tile_back",
                       x = 0.5 + c(rep(seq(1, by=4, length.out=4), each = 3),
                                   rep(seq(3, by=4, length.out=3), each = 4)),
                       y = 0.5 + c(rep(seq(2, by=2, length.out=3), 4),
                                   rep(seq(1, by=2, length.out=4), 3)),
                       suit = rep(1:4, each=6), rank = rep(1:6, 4))

    # data frame of possible coin coordinates
    xy <- xy_ll <- xy_ul <- xy_lr <- xy_ur <- df_tiles[, c("x", "y")]
    xy_ll$x <- xy$x - 0.5
    xy_ul$x <- xy$x - 0.5
    xy_lr$x <- xy$x + 0.5
    xy_ur$x <- xy$x + 0.5
    xy_ll$y <- xy$y - 0.5
    xy_ul$y <- xy$y + 0.5
    xy_lr$y <- xy$y - 0.5
    xy_ur$y <- xy$y + 0.5
    xy <- bind_rows(xy_ll, xy_ul, xy_lr, xy_ur)
    # find 24 random "non-orthogonal" coordinates
    xy_coins <- tibble()
    for (i in 1:24) {
        i_new <- sample.int(nrow(xy), 1L)
        xy_coins <- bind_rows(xy_coins, xy[i_new,])
        i_remove <- c(i_new,
                      which(xy$x == xy$x[i_new] & xy$y == xy$y[i_new] + 1),
                      which(xy$x == xy$x[i_new] & xy$y == xy$y[i_new] - 1),
                      which(xy$x == xy$x[i_new] + 1 & xy$y == xy$y[i_new]),
                      which(xy$x == xy$x[i_new] - 1 & xy$y == xy$y[i_new]))
        xy <- xy[-i_remove,]
    }
    df_coins <- tibble(piece_side = "coin_face",
                       x = xy_coins$x, y = xy_coins$y,
                       suit = rep(1:4, each=6), rank = rep(1:6, 4))

    bind_rows(df_tiles, df_coins)
}

#' @rdname df_game
#' @export
df_landlocked <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- tibble(piece_side = rep(c("tile_back", "tile_face"), each = 12),
                       suit = rep(1:4, each = 6), rank = rep(1:6, 4))[sample.int(24L), ]
    df_tiles$x <- 0.5 + c(seq(3, 9, 2), rep(seq(1, 9, 2), 4))
    df_tiles$y <- 0.5 + c(rep(9, 4), rep(seq(7, 1, -2), each = 5))
    df_pawn <- tibble(piece_side = "pawn_face", suit = 1, rank = 1, x = 1, y = 9)
    bind_rows(df_tiles, df_pawn)
}

#' @rdname df_game
#' @export
df_ley_lines <- function() {
    df <- tibble(piece_side = "tile_back",
                 x = c(6,8,     7,9,   7,9,   3,5, 8,10, 2,4, 9,11, 2,4,13, 7,9,11, 13, 9,11, 7) - 0.5,
                 y = c(15,15, 13,13, 11,11, 10,10,  9,9, 8,8, 7,7,  6,6,6,  5,5,5,  4,  3,3,  2) - 0.5,
                 suit = rep(1:4, each=6), rank = rep(1:6, 4))
}

#' @rdname df_game
#' @export
df_mathrix <- function(seed = NULL, coins = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- df_rect_board_tiles(nrows=4, ncols=6)
    if (is.null(coins)) {
        ranks <- rep(1:6, 4)[sample.int(24L)]
    } else {
        ranks <- process_ranks(coins)
    }
    suits <- integer(24)
    for (i in seq.int(6L)) suits[which(ranks == 1)] <- sample.int(4L)
    df_coins <- tibble(piece_side = "coin_face",
                       rank = ranks, suit = suits,
                       x = rep(1:6, 4), y = rep(4:1, each=6))
    bind_rows(df_tiles, df_coins)
}

#' @rdname df_game
#' @export
df_piecepackman <- function(seed = NULL, variant = 1) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- tibble(piece_side = "tile_back",
                       x = 0.5 + c(rep(seq(1, 9, 2), 4), seq(2, 8, 2)),
                       y = 0.5 + c(rep(c(1, 3, 7, 9), each = 5), rep(5, 4)),
                       suit = rep(1:4, 6), rank = rep(1:6, each = 4))
    df_p <- switch(variant,
                   df_roundabout(),
                   abort(paste("Can't handle Piecepackman variant ", variant), class = "board_setup"))
    p_xy <- filter(df_p, !grepl("matchstick", .data$piece_side)) %>%
            mutate(x_y = paste(.data$x, .data$y, sep = "_")) %>%
            select(.data$x_y)
    x_y_omit <- c(p_xy$x_y, "1_5", "1_6", "10_5", "10_6")
    df_x_y <- expand.grid(x = 1:10, y = 1:10, stringsAsFactors = FALSE) %>%
            mutate(x_y = paste(.data$x, .data$y, sep = "_")) %>%
            select(.data$x_y)
    x_y <- setdiff(df_x_y$x_y, x_y_omit)
    x_y_nulls <- str_split(sample(x_y, 24), "_", simplify = TRUE)
    df_n <- tibble(piece_side = "matchstick_face",
                   x = as.numeric(x_y_nulls[, 1]), y = as.numeric(x_y_nulls[, 2]),
                   rank = 1, suit = rep(1:4, 6))

    bind_rows(df_tiles, df_p, df_n)
}

df_roundabout <- function() {
    df_c <- tibble(piece_side = "coin_back",
                   x = c(1, 1, 10, 10),
                   y = c(1, 10, 10, 1),
                   suit = c(4, 3, 2, 1))
    df_p <- tibble(piece_side = "pawn_face",
                   x = c(5, 5, 6, 6),
                   y = c(5, 6, 6, 5),
                   suit = c(4, 2, 3, 1))
    df_n <- tibble(piece_side = "coin_face",
                   x = 6, y = 4, rank = 1)
    df_mav <- tibble(piece_side = "matchstick_face",
                     rank = 2, angle = 0,
                     x = 0.5 + c(4, 4, 6, 6, 5, 5),
                     y = c(6, 8, 8, 5, 2:3),
                     suit = c(2, 2, 2, 1, 4, 4))
    df_mah <- tibble(piece_side = "matchstick_face",
                     rank = 2, angle = 90,
                     x = c(3, 8, 5, 6, 3, 8),
                     y = 0.5 + c(9, 9, 4, 4, 2, 2),
                     suit = c(2, 2, 1, 1, 4, 4))
    df_m3v <- tibble(piece_side = "matchstick_face",
                     rank = 4, angle = 0,
                     x = 0.5 + c(1, 9, 5, 1, 9),
                     y = 0.5 + c(2, 2, 8, 8, 8),
                     suit = c(4, 4, 2, 3, 3))
    df_m3h <- tibble(piece_side = "matchstick_face",
                     rank = 4, angle = 90,
                     x = 0.5 + c(3, 4, 6, 7, 5, 2, 2, 8, 8, 2, 2, 8, 8, 3, 7, 4, 6, 3, 7),
                     y = 0.5 + c(8, 9, 9, 8, 6, 6, 7, 6, 7, 4, 5, 4, 5, 3, 3, 2, 2, 1, 1),
                     suit = c(rep(2, 5), rep(3, 4), rep(1, 6), rep(4, 4)))

    bind_rows(df_c, df_p, df_n, df_mav, df_mah, df_m3v, df_m3h)
}

#' @rdname df_game
#' @export
df_pass_the_food <- function() {
    tibble(piece_side = "tile_face", angle = 0,
           suit = rep(1:4, each = 6),
           rank = rep(c(1, 3:6, 2), 4),
           x = rep(2 * 1:4 - 0.5, each = 6),
           y = rep(2 * 1:6 - 0.5, 4))
}

#' @rdname df_game
#' @export
df_piecepack_klondike <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
    df_tiles <- tibble(piece_side = c(rep("tile_back", 15),
                                      rep("tile_face", 6),
                                      rep("tile_back", 3)),
                       x = c(seq(4, 12, 2), seq(6, 12, 2), seq(8, 12, 2),
                             10, 12, 12, seq(2, 12, 2), rep(2, 3)),
                       y = c(rep(2, 21), rep(6, 3)), angle = 0)
    bind_cols(df_tiles, df_tsr)
}

#' @rdname df_game
#' @export
df_piece_gaps <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
    df_tiles <- tibble(piece_side = "tile_face",
                       x = rep(seq(2, 12, 2), 4),
                       y = rep(seq(2, 8, 2), each=6), angle = 0)
    df <- bind_cols(df_tiles, df_tsr)
    df[-which(df$rank == 1), ]
}

#' @rdname df_game
#' @export
df_piece_packing_pirates <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    new <- "0_0"
    tiles_xy <- new
    impossible <- c()
    possible <- c()
    while (length(tiles_xy) < 24L) {
        impossible <- union(impossible, new_impossible(new))
        possible <- union(possible, new_possible(new))
        possible <- setdiff(possible, impossible)
        new <- sample(possible, 1L)
        tiles_xy <- c(tiles_xy, new)
    }
    xy <- str_split(tiles_xy, "_")
    x <- as.numeric(sapply(xy, function(x) x[1]))
    y <- as.numeric(sapply(xy, function(x) x[2]))
    df_tiles <- tibble(piece_side = "tile_back",
                       x = x - min(x) + 1.5,
                       y = y - min(y) + 1.5,
                       angle = sample(c(0, 90, 180, 270), 24, replace=TRUE))
    df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
    bind_cols(df_tiles, df_tsr)
}

p_ <- function(...) paste(..., sep = "_")
new_impossible <- function(x_y) {
    xy <- as.integer(str_split(x_y, "_")[[1]])
    c(p_(xy[1] - 1L, xy[2] + -1:1),
      p_(xy[1], xy[2] + -1:1),
      p_(xy[1] + 1L, xy[2] + -1:1))
}
new_possible <- function(x_y) {
    xy <- as.integer(str_split(x_y, "_")[[1]])
    c(p_(xy[1] - 2L, xy[2] + -1:1),
      p_(xy[1] - 1L, xy[2] + c(-2L, 2L)),
      p_(xy[1] + 0L, xy[2] + c(-2L, 2L)),
      p_(xy[1] + 1L, xy[2] + c(-2L, 2L)),
      p_(xy[1] + 2L, xy[2] + -1:1))
}

#' @rdname df_game
#' @export
df_plans_of_action <- function(seed = NULL, coins = NULL) {
    df_tiles <- df_rect_board_tiles(nrows=8, ncols=8)
    if (is.null(coins)) {
        if (!is.null(seed)) withr::local_seed(seed)
        suits <- sample(rep(1:4, 6), 24)
    } else {
        suits <- process_suits(coins)
    }
    df_coins <- tibble(piece_side = "coin_back", suit = suits,
                       x = rep(2:7, 4), y = rep(6:3, each=6), angle=0)
    df_coins <- arrange(df_coins, .data$suit)
    df_coins$rank <- rep(1:6, 4)
    bind_rows(df_tiles, df_coins)
}

#' @rdname df_game
#' @export
df_quatri <- function() {
    df_tiles <- df_rect_board_tiles(nrows=4, ncols=4)
    df_coins <- tibble(piece_side = "coin_back",
                       x = rep(1:4, 2), y = rep(c(4,1), each=4),
                       suit = c(1,2,1,2, 2,1,2,1),
                       rank = c(2,2,3,3, 5,5,4,4),
                       angle = c(180, 0, 180, 0, 0, 180, 0, 180))
    bind_rows(df_tiles, df_coins)
}

#' @rdname df_game
#' @export
df_relativity <- function(seed = NULL, coins = NULL) {
    df_tiles <- df_rect_board_tiles(nrows=4, ncols=6)
    if (is.null(coins)) {
        if (!is.null(seed)) withr::local_seed(seed)
        ranks <- c(sample.int(6L), sample.int(6L), sample.int(6L), sample.int(6L))
        while (should_resample_relativity(ranks)) {
            ranks <- c(sample.int(6L), sample.int(6L), sample.int(6L), sample.int(6L))
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
    all(diff(c(coins[6], coins[4], coins[3], coins[1])) == 0)
}

#' @rdname df_game
#' @export
df_skyscrapers <- function(seed = NULL, tiles = NULL) {
    df_tiles <- df_donut_tiles(seed = seed, tiles = tiles, x0 = 1.5, y0 = 1.5)
    df_pawn <- filter(df_tiles, .data$rank == 1)
    df_pawn <- mutate(df_pawn, piece_side = "pawn_face",
                      x = .data$x + 0.5, y = .data$y - 0.5)
    bind_rows(df_tiles, df_pawn)
}

#' @rdname df_game
#' @export
df_slides_of_action <- function() {
    df_tiles <- df_rect_board_tiles(4, 4)
    df_coins <- tibble(piece_side = "coin_back",
                       suit = rep(c(1,3,4), each = 5), rank = rep(1:5, 3),
                       x = c(3,1,4,2,3, 2,3,1,4,2, 1,4,2,3,1),
                       y = c(4,3,3,2,1, 4,3,2,2,1, 4,4,3,2,1))
    bind_rows(df_tiles, df_coins)
}

#' @rdname df_game
#' @export
df_the_in_crowd <- function() {
    df_t1 <- df_rect_board_tiles(6, 6)
    df_t2 <- df_rect_board_tiles(4, 4, 2, 2, rank = 4)
    df_t3 <- tibble(piece_side="tile_back", x=3.5, y=3.5,
                    angle = 0, suit = 2, rank = 3)
    bind_rows(df_t1, df_t2, df_t3)
}

#' @rdname df_game
#' @export
df_san_andreas <- function() {
    x <- 0.5+c(rep(c(1,3,5), 3), 2,4,6, 3,5,7, 4,6,8, 5,7,9, 7,9)
    y <- 0.5+c(rep(c(15,13,11,9,7,5,3), each=3), 1, 1)
    tibble(piece_side="tile_back", x=x, y=y,
           suit = rep(1:4, each=6, length.out=23),
           rank = rep(1:6, 4, length.out=23))
}

#' @rdname df_game
#' @export
df_the_magic_bag <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- tibble(piece_side = "tile_face", suit = rep(1:4, each = 6),
                       rank = rep(1:6, 4))[sample.int(24L), ]
    df_tiles$x <- 0.5 + c(rep(1, 9), rep(3, 7), rep(5, 5), rep(7, 3))
    df_tiles$y <- 0.5 + c(seq(17, 1, -2), seq(13, 1, -2), seq(9, 1, -2), seq(5, 1, -2))
    df_tiles
}

#' @rdname df_game
#' @export
df_tower_of_babel <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_txy <- tibble(piece_side = "tile_back", x = 2, y = 4)
    if (is.null(tiles)) {
        df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
    } else {
        df_tsr <- process_tiles(tiles)
    }
    df <- bind_cols(df_txy, df_tsr)
    df[24, "y"] <- 2
    df[24, "piece_side"] <- "tile_face"
    attr(df, "scale_factor") <- 2
    df
}

#' @rdname df_game
#' @export
df_triactor <- function(seed = NULL, cfg2 = "playing_cards_expansion") {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tb <- tibble(piece_side = "tile_back", cfg = "piecepack",
                    x = 0.5+rep(c(seq(5,15,2),1,2,18,19),2),
                    y = 0.5+c(rep(1,6),5,3,3,5,rep(11,6),7,9,9,7),
                    angle = rep(c(rep(0,7),90,90,0),2))
    df_tf <- tibble(piece_side = "tile_face", cfg = "piecepack",
                    x = 0.5+c(3,17,17,3), y = 0.5+c(11,11,1,1),
                    suit = 1:4, rank = 1)
    df_c1 <- tibble(piece_side = "coin_back", cfg = "piecepack",
                    suit = 1:4, rank = sample.int(6L, 4L, replace = TRUE),
                    x = 0.5+c(5,15,15,5),y = 0.5+c(11,11,1,1))
    df_c2 <- tibble(piece_side = "coin_back", cfg = cfg2,
                    suit = 1:4, rank = sample.int(6L, 4L, replace = TRUE),
                    x = 0.5+c(2,18,18,2), y = 0.5+c(9,9,3,3))
    df_p <- tibble(piece_side = "pawn_face", cfg = rep(c("piecepack", cfg2), each = 4),
                   x = 10.5, y = 0.5+0:7, angle = 90, suit = rep(1:4, 2))
    bind_rows(df_tb, df_tf, df_c1, df_c2, df_p)
}

#' @rdname df_game
#' @export
df_tula <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_txy <- tibble(piece_side = "tile_back",
                     x = c(rep(seq(1,7,2), 3), 4,
                           rep(seq(2,6,2), 2), 4,
                           3, 5, 4, 4),
                     y = c(rep(seq(7,3,-2), each=4), 1,
                           rep(c(6,4), each=3), 2,
                           5, 5, 3, 4))
    if (is.null(tiles)) {
        df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
    } else {
        df_tsr <- process_tiles(tiles)
    }
    bind_cols(df_txy, df_tsr)
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
