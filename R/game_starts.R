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
#'  \item{Black Pawn Trucking}{A solitaire by Philip Lerche.
#'        See \url{https://www.ludism.org/ppwiki/BlackPawnTrucking}.}
#'  \item{Brain Burn}{A solitaire by Mark A. Biggar.
#'                    See \url{https://www.ludism.org/ppwiki/BrainBurn}.}
#'  \item{Brandubh}{AKA Brandub, a two-player abstract of the hnefatafl family played by the Irish.
#'                  See \url{http://www.cyningstan.com/game/125/brandub} for more information.}
#'  \item{Breakthrough}{Two-player abstract invented by Dan Troyka in 2000
#'                      that won the 2001 8x8 Game Design Competition.
#'                      See \url{https://en.wikipedia.org/wiki/Breakthrough_(board_game)}.}
#'  \item{Burbuja}{Solitaire by Antonio Recuenco Muñoz.
#'                 See \url{https://www.ludism.org/ppwiki/Burbuja}.}
#'  \item{Cardinal's Guards}{A solitaire by Michael Schoessow and Stephen Schoessow.
#'                           See \url{https://ludism.org/ppwiki/CardinalsGuards}.}
#'  \item{Cell Management}{Solitaire game for the piecepack by Mark Goadrich.
#'                         See \url{https://www.ludism.org/ppwiki/CellManagement}.}
#'  \item{Change Change}{Solitaire by Sid Sackson adapted by Ron Hale-Evans.
#'                       See \url{https://www.ludism.org/ppwiki/ChangeChange}.}
#'  \item{Chariots}{Multiplayer racing game by Mark A. Biggar.
#'                  See \url{https://www.ludism.org/ppwiki/Chariots}.}
#'  \item{Chaturaji}{An old 4-player chess variant.
#'                   See \url{https://www.ludism.org/ppwiki/Chaturaji}.}
#'  \item{(American) Checkers}{A traditional board game also known as \dQuote{(English) Draughts}.
#'                  First adapted to the piecepack by Mark A. Biggar.
#'                  See \url{https://www.ludism.org/ppwiki/Checkers}.}
#'  \item{(International) Chess}{Very popular board game first adapted to the piecepack by Ron Hale-Evans.
#'                               See \url{https://www.ludism.org/ppwiki/Chess}.}
#'  \item{Piecepack Halma aka Chinese Checkers}{A port of Halma by Mark A. Biggar.
#'                          See \url{https://www.ludism.org/ppwiki/ChineseCheckers}.}
#'  \item{Coin Collectors}{A solitaire by Don Kirkby.
#'                         See \url{https://www.ludism.org/ppwiki/CoinCollectors}.}
#'  \item{Cribbage}{Traditional card game traditionally uses a special board to keep score
#'                  but one can use a piecepack as a cribbage board instead.
#'                  See \url{https://www.ludism.org/ppwiki/Cribbage}.}
#'  \item{Climbing Man}{A solitaire by Jorge Arroyo.  See \url{https://www.ludism.org/ppwiki/ClimbingMan}.}
#'  \item{Crocodile Hop}{A solitaire by Tim Schutz.  See \url{https://www.ludism.org/ppwiki/CrocodileHop}.}
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
#'  \item{Galaxy Express}{Solitaire by Matt Worden about delivering shipments between planets in the distant future.
#'                        See \url{https://www.ludism.org/ppwiki/GalaxyExpress}.}
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
#'  \item{Lab Rats}{Piecepack solitaire by Mark A. Biggar.
#'                  See \url{https://www.ludism.org/ppwiki/LabRats}.}
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
#'  \item{Minishogi}{A smaller version of Japanese chess invented (or perhaps rediscovered) by
#'                   by Shigeo Kusumoto in 1970.}
#'  \item{Nine Men's Morris}{Traditional board game.
#'        See \url{https://en.wikipedia.org/wiki/Nine_men\%27s_morris}.}
#'  \item{One Man Thrag}{A solitaire by Jim Doherty.
#'                       See \url{https://www.ludism.org/ppwiki/OneManThrag}}
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
#'  \item{Sarcophagus}{A multiplayer piecepack game by Phillip Lerche.
#'                     See \url{https://www.ludism.org/ppwiki/Sarcophagus}.}
#'  \item{Shogi}{AKA \dQuote{Japanese chess} is a major chess variant.
#'               See \url{https://www.ludism.org/ppwiki/Shogi}.}
#'  \item{Shopping Mall}{A multiplayer game by Michael Schoessow and Stephen Schoessow.
#'                       See \url{https://www.ludism.org/ppwiki/ShoppingMall}.}
#'  \item{Skyscrapers}{A solitaire by Michael Schoessow.
#'                     See \url{https://www.ludism.org/ppwiki/Skyscrapers}.}
#'  \item{Slides of Action}{An abstract connection game by Clark Rodeffer.
#'                          See \url{https://www.ludism.org/ppwiki/SlidesOfAction}.}
#'  \item{Speedy Towers}{A real-time tower building game by Jessica Eccles.
#'                       See \url{https://ludism.org/ppwiki/SpeedyTowers}.}
#'  \item{Steppin' Stones}{A solitaire by Ken Maher.
#'                        See \url{https://ludism.org/ppwiki/Steppin'_Stones}.}
#'  \item{Tablut}{Traditional two-player abstract played by the Sámi people until at least the 1700s.
#'                See \url{https://www.ludism.org/ppwiki/Tablut}.}
#'  \item{The \dQuote{In} Crowd}{Piecepack game by Jeb Havens and Ian Schreiber.
#'        See \url{https://www.ludism.org/ppwiki/TheInCrowd}.}
#'  \item{The Magic Bag}{Piecepack game by Rob LeGood.
#'                       See \url{https://www.ludism.org/ppwiki/The_Magic_Bag}.}
#'  \item{The Penguin Game}{A piecepack game by Jonathan C. Dietrich.
#'                          See \url{https://www.ludism.org/ppwiki/PenguinGame}}
#'  \item{Tower of Babel}{Solitaire piecepack game by Mark A. Biggar.
#'        See \url{https://www.ludism.org/ppwiki/TowerOfBabel}.}
#'  \item{Tracers}{A two-player abstract by Marty Hale-Evans.
#'                 See \url{https://www.ludism.org/ppwiki/Tracers}.}
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
#' @param n_players Number of players
#' @param suit_colors Character vector of the suit colors
#' @param variant Name of variant
#' @rdname df_game
#' @name df_game
NULL

#' @importFrom dplyr %>%
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr setdiff
#' @importFrom dplyr ungroup

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
df_black_pawn_trucking <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    i_sr <- sample.int(24)
    df_tiles <- tibble(piece_side = "tile_face",
                       x = c(7,9,     8,10,   8,10,   3,5, 9,11, 2,4, 10,12, 2,4,14, 8,10,12, 14, 10,12, 8) - 0.5,
                       y = c(15,15, 13,13, 11,11, 10,10,  9,9, 8,8, 7,7,  6,6,6,  5,5,5,  4,  3,3,  2) - 0.5,
                       suit = rep(1:4, each=6)[i_sr],
                       rank = rep(1:6, 4)[i_sr])
    df_dice <- tibble(piece_side = "die_face",
                      x = c(1, 2, 1, 2), y = c(2, 2, 1, 1),
                      suit = 1:4,
                      rank = sample.int(6, 4, replace = TRUE))
    i_sr_c <- sample.int(24, 4)
    df_coins <- filter(df_tiles,
                       paste(.data$rank, .data$suit) %in% paste(df_dice$rank, df_dice$suit))
    df_pawn <- filter(df_coins, .data$suit == 2)
    df_pawn$piece_side <- "pawn_face"
    df_pawn$x <- df_pawn$x + 0.5
    df_pawn$y <- df_pawn$y + 0.5
    df_pawn$rank <- NA_integer_
    df_coins$piece_side <- "coin_back"
    df_coins$x <- df_coins$x + 0.5
    df_coins$y <- df_coins$y - 0.5
    df_coins$rank <- rep(1:6, 4)[i_sr_c]
    df_coins$suit <- rep(1:4, each = 6)[i_sr_c]
    bind_rows(df_tiles, df_dice, df_coins, df_pawn)
}

#' @rdname df_game
#' @export
df_brain_burn <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- tibble(piece_side = "tile_face",
                       x = rep(seq(1.5, 10, 2), 5),
                       y = rep(seq(1.5, 10, 2), each = 5))[-sample.int(25, 1), ]
    i_sr <- sample.int(24)
    df_tiles$suit <- rep(1:4, each = 6)[i_sr]
    df_tiles$rank <- rep(1:6, 4)[i_sr]
    df_coins <- df_tiles
    df_coins$piece_side <- "coin_face"
    df_coins$x <- df_coins$x + 0.5
    df_coins$y <- df_coins$y - 0.5
    i_sr_c <- sample.int(24)
    df_coins$suit <- rep(1:4, each = 6)[i_sr_c]
    df_coins$rank <- rep(1:6, 4)[i_sr_c]
    df <- bind_rows(df_tiles, df_coins)
    # attr(df, "scale_factor") <- 2
    df
}

#' @rdname df_game
#' @export
df_burbuja <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- df_donut_tiles(seed = NULL, tiles = tiles, x0 = 6.0, y0 = 10.0)
    df_coins <- tibble(piece_side = rep(c(rep_len("coin_back", 5), "coin_face"), 4),
                       suit = rep(1:4, each = 6),
                       rank = c((1:6)[sample.int(6)], (1:6)[sample.int(6)],
                                (1:6)[sample.int(6)], (1:6)[sample.int(6)]),
                       x = rep(seq.int(from = 2, by = 2, length.out = 4), each = 6),
                       y = rep(c(rep_len(4, 5), 2), 4))
    df_pawns <- tibble(piece_side = "pawn_face", suit = 1:4, x = seq.int(3, length.out = 4, by = 2), y = 3)
    bind_rows(df_tiles, df_coins, df_pawns)
}

#' @rdname df_game
#' @export
df_cardinals_guards <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- df_donut_tiles(seed = NULL, tiles = tiles, x0 = 4.0, y0 = 4.0)
    df_coins <- tibble(piece_side = "coin_back",
                       suit = rep(1:4, each = 6),
                       rank = rep(1:6, 4))[sample.int(24), ]
    df_coins$x <- c(rep(c(2, 14), each = 5),
                    rep(seq.int(from = 4, by = 2, length.out = 5), 2),
                    rep_len(18, 4))
    df_coins$y <- c(rep(seq.int(from = 4, by = 2, length.out = 5), 2),
                    rep(c(2, 14), each = 5),
                    seq.int(from = 2, by = 2, length.out = 4))
    df_dice <- tibble(piece_side = "die_face",
                      suit = 1:4, rank = 1,
                      x = 16, y = seq.int(from = 2, by = 2, length.out = 4))
    df_pawns <- filter(df_tiles, .data$rank == 1)
    df_pawns$piece_side <- "pawn_face"
    df <- bind_rows(df_tiles, df_coins, df_dice, df_pawns)
    attr(df, "scale_factor") <- 2
    df
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
df_change_change <- function(seed = NULL, coins = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    if (is.null(coins)) {
       suits <- rep.int(1:4, c(1, 2, 4, 4))[sample.int(11L)]
    } else {
       suits <- process_suits(coins)
    }
    df <- tibble(piece_side = "coin_back",
                 x = c(1:4, 1:4, 1:3),
                 y = rep.int(3:1, c(4, 4, 3)),
                 suit = suits)
    # Plausible coin ranks
    df <- df %>%
        group_by(.data$suit) %>%
        mutate(rank = sample.int(n())) %>%
        ungroup()
    df
}

#' @rdname df_game
#' @export
df_chariots <- function() {
    tibble(piece_side = "tile_back", suit = rep(1:4, each = 6), rank = rep(1:6, 4),
           x = c(rep(seq(5.5, 9.5, 2), 4),
                 rep(c(3.5, 11.5), 4),
                 rep(c(1.5, 13.5), 2)),
           y = c(rep(c(9.5,7.5,3.5,1.5), each = 3),
                 rep(c(2.00, 4.00, 7.00, 9.00), each = 2),
                 rep(c(4.5, 6.5), each = 2)))
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
df_piecepack_halma <- df_chinese_checkers

#' @rdname df_game
#' @export
df_mini_halma <- function() {
    # .Deprecated("df_piecepack_halma")
    df_chinese_checkers()
}

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
                       rank = ranks)
    # Plausible coin suits
    df_coins <- df_coins %>%
        group_by(rank) %>%
        mutate(suit = sample.int(n())) %>%
        ungroup()
    ranks <- if (is.null(dice)) sample.int(6L, 4L, TRUE) else process_ranks(dice)
    df_dice <- tibble(piece_side = "die_face", x = 12, y = c(9, 7, 5, 3), rank = ranks, suit = 1:4)
    df_pawn <- tibble(piece_side = "pawn_face", x = c(5, 6, 6, 5), y = c(6, 6, 5, 5), rank = 1L, suit = 1:4)
    bind_rows(df_tiles, df_coins, df_dice, df_pawn)
}

#' @rdname df_game
#' @export
df_climbing_man <- function(seed = NULL, variant = c("Basic", "Free")) {
    if (!is.null(seed)) withr::local_seed(seed)
    variant <- match.arg(variant)
    if (variant == "Free") {
        df_none()
    } else {
        df_tiles <- df_rect_board_tiles(nrows = 12, ncols = 8, y0 = 2)
        repeat {
            df_coins <- df_tiles
            df_coins$x <- df_coins$x + sample(c(-0.5, 0.5), 24, replace = TRUE)
            df_coins$y <- df_coins$y + sample(c(-0.5, 0.5), 24, replace = TRUE)
            if (!any_adjacent_coins(df_coins)) {
                break
            }
        }
        i_sr <- sample.int(24)
        df_coins$piece_side <- "coin_back"
        df_coins$suit <- rep(1:4, each = 6)[i_sr]
        df_coins$rank <- rep(1:6, 4)[i_sr]
        bind_rows(df_tiles, df_coins)
    }
}

any_adjacent_coins <- function(df) {
    for(i in seq_len(nrow(df))) {
        if (any(df$x[-i] == df$x[i] & abs(df$y[-i] - df$y[i]) == 1))
            return(TRUE)
        if (any(abs(df$x[-i] - df$x[i]) == 1 & df$y[-i] == df$y[i]))
            return(TRUE)
    }
    FALSE
}

#' @rdname df_game
#' @export
df_crocodile_hop <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    i_t <- sample.int(24)
    df_tiles <- tibble(piece_side = rep.int(c("tile_back", "tile_face", "tile_back"), c(19, 2, 3)),
                       x = rep.int(rep(c(1, 3, 6.5, 8.5, 10.5), 2), c(8, 8, rep(1, 8))),
                       y = rep.int(c(3, 3.5, 1, 1.5), c(16, 3, 2, 3)),
                       suit = rep(1:4, each = 6)[i_t],
                       rank = rep(1:6, 4)[i_t])
    df_coins <- tibble(piece_side = "coin_back",
                       x = rep(seq(6, by = 1, length.out = 6), each = 4),
                       y = rep(4:1, 6),
                       suit = c(sample.int(4), sample.int(4), sample.int(4), sample.int(4), sample.int(4), sample.int(4)),
                       rank = rep(c(2:6, 1), each = 4))
    last_tile_sr <- paste(df_tiles$suit[24], df_tiles$rank[24])
    df_coins <- df_coins[which(paste(df_coins$suit, df_coins$rank) != last_tile_sr), ]
    repeat { # re-roll die if equals bottom-right tile (removed lily pond)
        df_dice <- tibble(piece_side = "die_face", x = seq(6, length.out = 4), y = 5,
                          suit = 1:4, rank = c(sample.int(6, 1), sample.int(6, 1), sample.int(6, 1), sample.int(6, 1)))
        if (!any(paste(df_dice$suit, df_dice$rank) %in% last_tile_sr))
            break
    }
    df_pawns <- filter(df_coins, paste(.data$suit, .data$rank) %in% paste(df_dice$suit, df_dice$rank))
    df_pawns$piece_side <- "pawn_face"
    df_pawns$rank <- NA_integer_
    bind_rows(df_tiles, df_coins, df_dice, df_pawns)
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
    df_txy <- tibble(piece_side = "tile_face",
                     x = 2 + c(rep(seq(0, 8, 2), 4), 0, 2, 4, 6),
                     y = 2 + c(rep(c(8, 6, 4, 2), each = 5), rep.int(0, 4)))
    if (is.null(tiles)) {
        df_tsr <- tibble(suit = rep(1:4, each = 6), rank = rep(1:6, 4))[sample.int(24L), ]
    } else {
        df_tsr <- process_tiles(tiles)
    }
    df_tiles <- bind_cols(df_txy, df_tsr)
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
df_galaxy_express <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_board <- df_rect_board_tiles(nrows = 6, ncols = 8, y0 = 2,
                                    rank = rep(4:6, each = 4), suit = rep(1:4, 3))
    df_arms <- df_board[sample.int(12, 6), ]
    df_arms$x <- df_arms$x + sample(c(-0.5, 0.5), 6, replace = TRUE)
    df_arms$y <- df_arms$y + sample(c(-0.5, 0.5), 6, replace = TRUE)
    df_arms$piece_side <- c(rep_len("coin_back", 5L), "coin_face")
    df_arms$suit <- 4
    df_arms$rank <- sample.int(6)
    df_pawn <- df_arms[6, ]
    df_pawn$piece_side <- "pawn_face"
    df_pawn$rank <- NA_integer_
    df_crowns <- tibble(piece_side = "coin_face", x = 1, y = 1, suit = 3, rank = sample.int(6))
    if (df_arms$rank[6] == df_crowns$rank[6])
        df_crowns$rank <- df_crowns$rank[c(6, 1:5)]
    df_dice <- tibble(piece_side = "die_face", x = c(3, 4, 11, 12),
                      y = c(1, 1, 4, 4), rank = 1, suit = c(3, 4, 1, 2))
    df_speed_tiles <- tibble(piece_side = "tile_face", x = 10.5, y = c(1.5, 5.5),
                             rank = 2, suit = c(2, 1))
    df_speed_coins <- tibble(piece_side = rep.int(rep(c("coin_back", "coin_face"), 2), c(4, 2, 4, 2)),
                             suit = rep.int(1:2, c(6, 6)), rank = c(sample.int(6), sample.int(6)),
                             x = 12, y = rep.int(c(7, 6, 5, 3, 2, 1), c(4, 1, 1, 4, 1, 1)))
    bind_rows(df_board, df_arms, df_crowns, df_dice,
              df_speed_tiles, df_speed_coins, df_pawn)
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
df_lab_rats <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- tibble(piece_side = "tile_back",
                       x = 1.5, y = 1.5,
                       suit = rep(1:4, each = 6), rank = rep(1:6, 4), angle = 0)
    repeat {
        x0 <- 1.5
        y0 <- 1.5
        a0 <- 0
        for (i in seq.int(2, 24)) {
            die <- sample.int(6, 1)
            xo <- switch(die, 0, -2, -1, 0, 1, 2)
            yo <- switch(die, 2, 1, 2, 2, 2, 1)
            ao <- switch(die, 0, 90, 0, 0, 0, -90)
            a1 <- (a0 + ao) %% 360
            if (a0 == 0) {
                x1 <- x0 + xo
                y1 <- y0 + yo
            } else if (a0 == 90) {
                x1 <- x0 - yo
                y1 <- y0 + xo
            } else if (a0 == 180) {
                x1 <- x0 - xo
                y1 <- y0 - yo
            } else { # a0 == 270
                x1 <- x0 + yo
                y1 <- y0 - xo
            }
            # cat(i, die, xo, yo, ao, x1, y0, a1, "\n")
            df_tiles$x[i] <- x0 <- x1
            df_tiles$y[i] <- y0 <- y1
            df_tiles$angle[i] <- a0 <- a1
        }
        if (is_legal_labrats(df_tiles))
            break
    }
    df_pawns <- tibble(piece_side = "pawn_face", suit = 1:2, angle = a1)
    if (a0 == 0) {
        df_pawns$x <- df_tiles$x[24] + 0.5 * c(-1, 1)
        df_pawns$y <- df_tiles$y[24] + 1.5
    } else if (a0 == 90) {
        df_pawns$x <- df_tiles$x[24] - 1.5
        df_pawns$y <- df_tiles$y[24] + 0.5 * c(-1, 1)
    } else if (a0 == 180) {
        df_pawns$x <- df_tiles$x[24] - 0.5 * c(-1, 1)
        df_pawns$y <- df_tiles$y[24] - 1.5
    } else { # a0 == 270
        df_pawns$x <- df_tiles$x[24] + 1.5
        df_pawns$y <- df_tiles$y[24] - 0.5 * c(-1, 1)
    }
    # "optimal" rat placement
    x <- df_tiles$x[which(df_tiles$x != 1.5)[1]]
    df_rat <- tibble(piece_side = "pawn_face", suit = 3, rank = NA_integer_, angle = 0, y = 1)
    if (x < 1.5)
        df_rat$x <- 1
    else
        df_rat$x <- 2
    i_c <- sample.int(24)
    df_coins <- tibble(piece_side = "coin_face",
                       suit = rep(1:4, each = 6)[i_c],
                       rank = rep(1:6, 4)[i_c])
    df_coins$x <- df_tiles$x[1] + rep(c(-0.5, 0.5, 1.5), each = 8)
    df_coins$y <- df_tiles$y[1] + rep(c(-1.5, -1.5, -0.5), each = 8)
    bind_rows(df_tiles, df_pawns, df_rat, df_coins)
}

is_legal_labrats <- function(df) {
    for (i in seq.int(2, 23)) {
        if (sum(sqrt((df$x[i] - df$x[-i])^2 + (df$y[i] - df$y[-i])^2) <= sqrt(5)) != 2)
            return(FALSE)
    }
    if (sum(sqrt((df$x[1] - df$x[-1])^2 + (df$y[1] - df$y[-1])^2) <= sqrt(5)) != 1)
        return(FALSE)
    if (sum(sqrt((df$x[24] - df$x[-24])^2 + (df$y[24] - df$y[-24])^2) <= sqrt(5)) != 1)
        return(FALSE)
    TRUE
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
    tibble(piece_side = "tile_back",
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
df_piecepackman <- function(seed = NULL, variant = "Roundabout") {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- tibble(piece_side = "tile_back",
                       x = 0.5 + c(rep(seq(1, 9, 2), 4), seq(2, 8, 2)),
                       y = 0.5 + c(rep(c(1, 3, 7, 9), each = 5), rep(5, 4)),
                       suit = rep(1:4, 6), rank = rep(1:6, each = 4))
    df_p <- switch(variant,
                   Roundabout = df_roundabout(),
                   abort(paste("Can't handle Piecepackman variant ", variant), class = "board_setup"))
    p_xy <- filter(df_p, !grepl("matchstick", .data$piece_side)) %>%
            mutate(x_y = paste(.data$x, .data$y, sep = "_")) %>%
            select("x_y")
    x_y_omit <- c(p_xy$x_y, "1_5", "1_6", "10_5", "10_6")
    df_x_y <- expand.grid(x = 1:10, y = 1:10, stringsAsFactors = FALSE) %>%
            mutate(x_y = paste(.data$x, .data$y, sep = "_")) %>%
            select("x_y")
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
df_one_man_thrag <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_board <- df_rect_board_tiles(4, 4, x0 = 1, y0 = 1, suit = 1:4, rank = 1)
    df_tiles <- tibble(piece_side = "tile_back",
                       x = rep(6 + 2 * c(0, 1, 2, 3.5), each = 5),
                       y = rep(3, each = 20),
                       suit = rep(c(1, 3, 4, 2), each = 5),
                       rank = 1 + c(sample.int(5), sample.int(5), sample.int(5), sample.int(5)))
    df_dice <- tibble(piece_side = "die_face",
                      x = 1:4, y = 6, suit = c(1, 3, 4, 2),
                      rank = c(sample.int(6, 1), sample.int(6, 1), sample.int(6, 1), sample.int(6, 1)))
    df_coins <- tibble(piece_side = "coin_back",
                       x = rep.int(6 + 2 * c(0, 1, 2, 3.5), c(6, 6, 6, 3)),
                       y = 5, suit = rep.int(c(1, 3, 4, 2), c(6, 6, 6, 3)),
                       rank = c(sample.int(6), sample.int(6), sample.int(6), c(2, 4, 6)[sample.int(3)]))
    df_pawns <- tibble(piece_side = "pawn_face",
                       x = c(1, 6, 8, 10), y = c(4, 6, 6, 6), suit = c(2, 1, 3, 4))
    df_health <- tibble(piece_side = "coin_face",
                        x = 13, y = 6:8, suit = 2, rank = c(1, 3, 5))
    bind_rows(df_board, df_tiles, df_dice, df_coins, df_pawns, df_health)

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
df_san_andreas <- function() {
    x <- 0.5+c(rep(c(1,3,5), 3), 2,4,6, 3,5,7, 4,6,8, 5,7,9, 7,9)
    y <- 0.5+c(rep(c(15,13,11,9,7,5,3), each=3), 1, 1)
    tibble(piece_side="tile_back", x=x, y=y,
           suit = rep(1:4, each=6, length.out=23),
           rank = rep(1:6, 4, length.out=23))
}

#' @rdname df_game
#' @export
df_sarcophagus <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    i_s <- sample.int(6)
    df_sarcophagus <- tibble(piece_side = "tile_back",
                             suit = c(4, 3, 4:1)[i_s],
                             rank = c(1, 1, rep(6, 4))[i_s],
                             x = rep(c(6.5, 5.5, 7.5), 2),
                             y = rep(c(3.5, 1.5, 1.5), 2))
    i_o <- sample.int(18)
    df_other <- tibble(piece_side = "tile_back",
                       suit = c(2:1, rep(4:1, each = 4))[i_o],
                       rank = c(1, 1, rep(2:5, 4))[i_o],
                       x = c(6.5,  5.5,7.5,  4.5,6.5,8.5,  seq(3.5,9.5,2),
                             2.5,4.5,8.5,10.5,  1.5,3.5,9.5,11.5),
                       y = rep.int(seq(11.5,1.5,-2), c(1, 2, 3, 4, 4, 4)))
    bind_rows(df_sarcophagus, df_other)
}

#' @rdname df_game
#' @export
df_shopping_mall <- function(seed = NULL, cfg2 = "go") {
    if (!is.null(seed)) withr::local_seed(seed)
    i_tf <- sample.int(20)
    df_tf <- tibble(piece_side = "tile_face",
                    suit = rep(1:4, 5)[i_tf],
                    rank = rep(2:6, 4)[i_tf],
                    x = 3 + c(1, 5, 9,11,13,
                              5,
                              1, 5, 9, 13,
                              1,3, 7,9, 13,
                              1,3, 7, 11,13),
                    y = 3 + rep.int(c(13, 11, 9, 5, 1), c(5, 1, 4, 5, 5)),
                    angle = sample(c(0, 90, 180, 270), 20, replace = TRUE),
                    cfg = "piecepack")
    df_pennies <- tibble(piece_side = "bit_face",
                         suit = 1, rank = 1,
                         x = 3 + c(7, 3, 9, 3, 11, 1, 7, 1, 7, 11, 5, 15, 1),
                         y = 1 + c(17, 15, 13, 11, 11, 9, 9, 5, 5, 5, 3, 3, 1),
                         cfg = cfg2)
    df_nickels <- tibble(piece_side = "bit_face",
                         suit = 2, rank = 1,
                         x = 1 + c(1, 17, 11),
                         y = 1 + c(13, 13, 1),
                         cfg = cfg2)
    bind_rows(df_tf, df_pennies, df_nickels)
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

# 2 players = 24 tiles, 24 coins => 2 towers, 11 tiles each, 12 coins each
# 3 players = 48 tiles, 48 coins => 2 towers, 15 tiles each + 1 extra tile, 16 coins each
# 4 players = 48 tiles, 48 coins => 3 towers, 11 tiles each + 1 extra tile, 12 coins each
# 5 players = 72 tiles, 72 coins => 4 towers, 13 tiles each + 3 extra tiles, 16 coins each + 2 coins
# 6 players = 72 tiles, 72 coins => 5 towers, 11 tiles each + 1 extra tile, 12 coins each

#' @rdname df_game
#' @export
df_speedy_towers <- function(n_players = 2, seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    stopifnot(n_players >= 2, n_players <= 6)
    if (n_players == 2) {
        df_tiles <- tibble(piece_side = "tile_back",
                           suit = rep(1:4, 6),
                           rank = rep(1:6, each = 4))[sample.int(24L), ]
        df_tiles$x <- c(seq.int(2, by = 2, length.out = 11),
                        16, 10,
                        seq.int(24, by = -2, length.out = 11))
        df_tiles$y <- c(rep_len(4, 11), 8, 8, rep_len(12, 11))
        df_tiles$angle <- c(rep_len(0, 12), rep_len(180, 12))
        df_coins <- tibble(piece_side = "coin_back",
                           suit = rep(1:4, 6),
                           rank = rep(1:6, each = 4))[sample.int(24L), ]
        df_coins <- df_coins[c(order(df_coins$suit[1:12]),
                               12 + order(df_coins$suit[13:24])), ]
        df_coins$x <- c(seq(2, by = 2, length.out = 12),
                        seq(24, by = -2, length.out = 12))
        df_coins$y <- c(rep(2, 12), rep(14, 12))
        df_coins$angle <- c(rep_len(0, 12), rep_len(180, 12))
        df_pawns <- tibble(piece_side = "pawn_face",
                           suit = c(3, 1), rank = NA_integer_,
                           x = c(24, 2), y = c(4, 12),
                           angle = c(0, 180))
        bind_rows(df_tiles, df_coins, df_pawns)
    } else {
        stop("We haven't been programmed for this case yet")
    }
}

#' @rdname df_game
#' @export
df_steppin_stones <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles_faces <- tibble(piece_side = "tile_face", suit = 1:4,
                             rank = 2, x = c(1.5, 7.5, 7.5, 1.5),
                             y = c(7.5, 7.5, 1.5, 1.5), angle = c(180, 90, 0, -90))
    df_tiles_backs <- tibble(piece_side = "tile_back", suit = rep(1:4, each = 3),
                             rank = rep(3:5, 4),
                             x = c(3.5, 1.5, 3.5,  5.5, 5.5, 7.5,
                                   5.5, 5.5, 7.5, 3.5, 3.5, 1.5),
                             y = c(7.5, 5.5, 5.5,  7.5, 5.5, 5.5,
                                   3.5, 1.5, 3.5, 3.5, 1.5, 3.5))
    df_coins <- tibble(piece_side = "coin_face", suit = rep(1:4, each = 5),
                       rank = rep(c(1, 3:6), 4),
                       x = c(3, 2, 1, 3, 3,  6, 7, 8, 6, 6,
                             6, 7, 8, 6, 6,  3, 2, 1, 3, 3),
                       y = c(6, 6, 6, 7, 8,  6, 6, 6, 7, 8,
                             3, 3, 3, 2, 1,  3, 3, 3, 2, 1),
                       angle = rep(c(180, 90, 0, -90), each = 5))
    df_pawns <- tibble(piece_side = "pawn_face", suit = 1:4,
                       x = c(5, 4, 4, 5), y = c(4, 4, 5, 5),
                       angle = c(180, 90, 0, -90))
    bind_rows(df_tiles_faces, df_tiles_backs, df_coins, df_pawns)
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
df_the_penguin_game <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- df_rect_board_tiles(4, 4)
    i_coins <- sample.int(24)
    df_coins <- tibble(piece_side = "coin_back",
                       x = rep(1:4, 6),
                       y = c(rep(4:1, each = 4), 4:1, 1:4),
                       suit = rep(1:4, each = 6)[i_coins],
                       rank = rep(1:6, 4)[i_coins])
    bind_rows(df_tiles, df_coins)
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
df_tracers <- function() {
    df_tiles <- df_rect_board_tiles(8, 8)
    df_pawns <- tibble(piece_side = "pawn_face",
                       suit = 1:2, x = c(1, 8), y = c(1, 8),
                       angle = c(0, 180))
    bind_rows(df_tiles, df_pawns)
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
df_tula <- function(seed = NULL, tiles = NULL,
                    variant = c("Original", "Variant 1", "Variant 2", "Variant 3", "Variant 4")) {
    variant <- match.arg(variant)
    if (!is.null(seed)) withr::local_seed(seed)
    if (is.null(tiles)) {
        if (variant %in% c("Original", "Variant 1"))
            df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
        else
            df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[c(sample.int(24L), sample.int(24L)), ]
    } else {
        df_tsr <- process_tiles(tiles)
    }
    df_txy <- switch(variant,
                     Original = tibble(piece_side = c(rep("tile_back", 23), "tile_face"),
                                       x = c(rep(seq(1,7,2), 3), 4,
                                             rep(seq(2,6,2), 2), 4,
                                             3, 5, 4, 4),
                                       y = c(rep(seq(7,3,-2), each=4), 1,
                                             rep(c(6,4), each=3), 2,
                                             5, 5, 3, 4)),
                     `Variant 1` = tibble(piece_side = c(rep("tile_back", 21),
                                                         "tile_face", "tile_back", "tile_face"),
                                          x = c(seq(2,8,2), seq(1,9,2), seq(2,8,2),
                                                rep(seq(2,8,2), 2),
                                                seq(3,7,2)),
                                          y = c(rep.int(seq(5,1,-2), c(4, 5, 4)),
                                                rep(c(4,2), each=4),
                                                rep(3,3))),
                     `Variant 2` = tibble(piece_side = c(rep("tile_back", 28),
                                                         "tile_face", rep("tile_back",4), "tile_face", rep("tile_back",4), "tile_face", rep("tile_back",4), "tile_face",
                                                         rep("tile_face", 4)),
                                          x = c(6,8, seq(3,11,2), rep(seq(1,13,2),2), seq(3,11,2), 6,8,
                                                7, seq(4,10,2), seq(2,12,2), seq(4,10,2), 7,
                                                5,9,5,9),
                                          y = c(rep.int(seq(11,1,-2), c(2,5,7,7,5,2)),
                                                rep.int(seq(10,2,-2), c(1,4,6,4,1)),
                                                7,7,5,5)),
                     `Variant 3` = tibble(piece_side = c(rep("tile_back", 20),
                                                         rep("tile_back", 15),
                                                         "tile_face", rep("tile_back", 8), "tile_face",
                                                         "tile_face", "tile_back", "tile_face"),
                                          x = c(5,7, seq(2,10,2), seq(1,11,2), seq(2,10,2), 5,7,
                                                6, seq(3,9,2), seq(2,10,2), seq(3,9,2), 6,
                                                6, seq(3,9,2), seq(3,9,2), 6,
                                                4,6,8),
                                          y = c(rep.int(seq(9,1,-2), c(2,5,6,5,2)),
                                                rep.int(seq(9,1,-2), c(1,4,5,4,1)),
                                                rep.int(seq(8,2,-2), c(1,4,4,1)),
                                                rep(5,3))),
                     `Variant 4` = tibble(piece_side = c(rep("tile_back", 24),
                                                         rep("tile_back",4), "tile_face", rep("tile_back",5),"tile_face",rep("tile_back",4),
                                                         rep(c("tile_face","tile_back","tile_back","tile_face"),2),
                                                         "tile_face"),
                                          x = c(seq(4,12,2), seq(3,13,2), 1, 15, seq(3,13,2), seq(4,12,2),
                                                seq(5,11,2), seq(2,14,2), seq(5,11,2),
                                                seq(5,11,2), seq(5,11,2),
                                                8),
                                          y = c(rep.int(c(7,5,4,3,1), c(5, 6, 2, 6, 5)),
                                                rep.int(c(6,4,2), c(4, 7, 4)),
                                                rep.int(c(5,3), c(4, 4)),
                                                4)),
                     abort(paste("Can't handle Tula variant ", variant), class = "board_setup"))
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
