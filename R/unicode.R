#' Generate plaintext piecepack diagrams
#'
#' \code{cat_piece()} generates plaintext piecepack diagrams and
#'  outputs them using \code{base::cat()}.  \code{cat_move()} generates
#' a plaintext diagram for a move within a game.  \code{cat_game()}
#' renders an animation of a game in the terminal.
#'
#' @param df Data frame containing piece info.
#' @param color How should the text be colorized.  If \code{TRUE} or \code{"crayon"}
#'       will colorize output for the terminal.  If \code{FALSE} won't colorize output.
#' @param reorient Determines whether and how we should reorient (the angle) of pieces or symbols:\enumerate{
#'        \item{The default "none" (or \code{FALSE}) means don't reorient any pieces/symbols.}
#'        \item{"all" (or \code{TRUE}) means setting the angle to zero for all pieces
#'              (reorienting them all \dQuote{up}).}
#'        \item{"symbols" means just re-orient suit/rank symbols but not the orientation of the piece itself.
#'              In particular, in contrast with "all" this preserves the location
#'              of the upper-left "corner" of piecepack tile faces.}}
#' @param annotate If \code{TRUE} annotate the plot with \dQuote{algrebraic} coordinates,
#'                 if \code{FALSE} don't annotate,
#'                 if \code{"cartesian"} annotate the plot with \dQuote{cartesian} coordinates.
#' @param ... Passed to \code{cat}
#' @param file \code{file} argument of \code{cat}.
#'             Default (\code{""}) is to print to standard output.
#'             \code{NULL} means we don't \code{cat()}
#' @return Character vector of text diagram (returned invisibly).
#' @export
cat_piece <- function(df, color = NULL, reorient = "none", annotate = FALSE, ..., file = "") {
    if (is.null(color)) color <- TRUE
    if (!is.null(file) && file != "") color <- FALSE
    if (nrow(df) == 0) {
        cat(..., file = file)
        return(invisible(NULL))
    }
    nn <- names(df)
    if (!("cfg" %in% nn)) df$cfg <- "piecepack"
    df$cfg <- ifelse(is.na(df$cfg), "piecepack", df$cfg)
    if (!("rank" %in% nn)) df$rank <- NA_integer_
    df$rank <- ifelse(is.na(df$rank), 1L, df$rank)
    # checkers/chess boards rank is number of cells
    df$rank <- ifelse(df$rank == 1L & str_detect(df$piece_side, "^board") & str_detect(df$cfg, "[12]$"), 8L, df$rank)
    # go board rank is number of lines
    df$rank <- ifelse(str_detect(df$piece_side, "^board") & df$cfg == "go",
                      ifelse(df$rank == 1L, 18, df$rank - 1),
                      df$rank)
    if (!("suit" %in% nn)) df$suit <- NA_integer_
    df$suit <- ifelse(is.na(df$suit), 1L, df$suit)
    if (!("angle" %in% nn)) df$angle <- NA_real_
    df$angle <- ifelse(is.na(df$angle), 0, df$angle %% 360)
    if (isTRUE(reorient) || reorient == "all") df$angle <- 0

    lr <- range_heuristic(df)
    nc <- 2*lr$xmax+1
    nr <- 2*lr$ymax+1
    cm <- list(char = matrix(" ", nrow = nr, ncol = nc),
               bg = matrix("white", nrow = nr, ncol = nc),
               fg = matrix("black", nrow = nr, ncol = nc))

    for (rr in seq(nrow(df))) {
        ps <- as.character(df[rr, "piece_side"])
        suit <- as.numeric(df[rr, "suit"])
        rank <- as.numeric(df[rr, "rank"])
        x <- 2*as.numeric(df[rr, "x"])+1
        y <- 2*as.numeric(df[rr, "y"])+1
        angle <- as.numeric(df[rr, "angle"])
        cfg <- as.character(df[rr, "cfg"])
        cm <- add_piece(cm, ps, suit, rank, x, y, angle, cfg, reorient)
    }

    if (!isFALSE(annotate)) {
       x <- seq(3, nc, by=2)
       if (annotate == "cartesian") {
           x <- utils::head(x, 9)
           xt <- as.character(seq_along(x))
           cm$char[1, x] <- xt
       } else {
           if (length(x) > 26) x <- x[1:26]
           cm$char[1, x] <- letters[seq_along(x)]
       }
       y <- seq(3, nr, by=2)
       yt <- as.character(seq_along(y))
       if (length(yt) > 9) {
           yt <- stringr::str_pad(yt, 2, "right")
           cm$char[y, 2] <- substr(yt, 2, 2)
       }
       cm$char[y, 1] <- substr(yt, 1, 1)
    }
    if (isTRUE(color) || color == "crayon") {
        for (rr in seq(nrow(cm$char))) {
            for (cc in seq(ncol(cm$char))) {
                fg <- crayon::make_style(cm$fg[rr, cc])
                bg <- crayon::make_style(cm$bg[rr, cc], bg = TRUE)
                colorize <- crayon::combine_styles(fg, bg)
                cm$char[rr, cc] <- colorize(cm$char[rr, cc])
            }
        }
    }

    text <- rev(apply(cm$char, 1, function(x) paste(x, collapse = "")))
    if (!is.null(file)) cat(text, ..., file = file, sep = "\n")
    invisible(text)
}

#' @rdname cat_piece
#' @param game A list containing a parsed ppn game (as parsed by \code{read_ppn})
#' @param move Which move to cat game state (after the move, will use \code{game$dfs[[move]]})
#'             unless \code{NULL} in which case will cat the game state after the last move.
#' @export
cat_move <- function(game, move = NULL, ...) {
    df <- get_df_from_move(game, move)
    cat_piece(df, ...)
}

#' @rdname cat_piece
#' @param fps Frames per second.
#' @export
cat_game <- function(game, ..., fps = 1) {
    for (ii in seq_along(game$dfs)) {
        prev <- system.time(out <- cat_piece(game$dfs[[ii]], ..., file=NULL))[["elapsed"]]
        dur <- ifelse(1/fps - prev > 0, 1/fps - prev, 0)
        Sys.sleep(dur)
        clear_screen()
        cat(out, sep="\n")
    }
}

clear_screen <- function() {
    switch(.Platform$OS.type,
           unix = system("clear"),
           windows = system("cls"))
}

# nolint start
# Use Half-circle for Moons? \u25d0
# Use Arrows for Arms?
# nolint end
dominoes_ranks <- c(" ", "\u00b7", "\u280c", "\u22f0", "\u2237", "\u2059", "\u283f")
piecepack_ranks <- c("n", "a", "2", "3", "4", "5")
piecepack_suits <- c("\u2600", "\u263e", "\u265b", "\u2e38")
#### darkgreen sometimes shows up as black?
#### black instead of grey sometimes?
checkers_colors <- c("darkred", "grey", "green", "darkblue", "darkorange3", "black")
piecepack_colors <- dice_colors <- chess_colors <- go_colors <- checkers_colors
dice_colors[2] <- "grey"
ss_list <- list(piecepack = piecepack_suits,
                playing_cards_expansion = c("\u2665", "\u2660", "\u2663", "\u2666"),
                dual_piecepacks_expansion = c("\u2661", "\u2664", "\u2667", "\u2662"),
                subpack = piecepack_suits,
                checkers1 = c(rep("\u26c2", 5), "\u26c0"),
                checkers2 = c(rep("\u26c2", 5), "\u26c0"),
                chess1 = "",
                chess2 = "",
                dice = rep(" ", 6),
                dominoes = dominoes_ranks,
                dominoes_black = dominoes_ranks,
                dominoes_blue = dominoes_ranks,
                dominoes_green = dominoes_ranks,
                dominoes_red = dominoes_ranks,
                dominoes_white = dominoes_ranks,
                dominoes_yellow = dominoes_ranks,
                go = c(rep("\u25cf", 5), "\u25cb"),
                icehouse_pieces = c(rep("\u25b2", 5), "\u25b3"))
rs_list <- list(piecepack = piecepack_ranks,
                playing_cards_expansion = piecepack_ranks,
                dual_piecepacks_expansion = piecepack_ranks,
                subpack = piecepack_ranks,
                checkers1 = rep("\u26c2", 6),
                checkers2 = rep("\u26c2", 6),
                chess1 = c("\u265f", "\u265e", "\u265d", "\u265c", "\u265b", "\u265a"),
                chess2 = c("\u265f", "\u265e", "\u265d", "\u265c", "\u265b", "\u265a"),
                dice = dominoes_ranks[-1],
                dominoes = dominoes_ranks,
                dominoes_black = dominoes_ranks,
                dominoes_blue = dominoes_ranks,
                dominoes_green = dominoes_ranks,
                dominoes_red = dominoes_ranks,
                dominoes_white = dominoes_ranks,
                dominoes_yellow = dominoes_ranks,
                icehouse_pieces = rep(" ", 6),
                go = rep("\u25cf", 6))
fg_list <- list(piecepack = piecepack_colors,
                dual_piecepacks_expansion = piecepack_colors,
                playing_cards_expansion = piecepack_colors[c(1, 2, 2, 1)],
                subpack = piecepack_colors,
                chess1 = chess_colors,
                chess2 = chess_colors,
                checkers1 = checkers_colors,
                checkers2 = checkers_colors,
                dice = dice_colors,
                dominoes = rep("black", 7),
                dominoes_black = rep(dice_colors[2], 7),
                dominoes_blue = rep(dice_colors[4], 7),
                dominoes_green = rep(dice_colors[3], 7),
                dominoes_red = rep(dice_colors[1], 7),
                dominoes_white = rep(dice_colors[6], 7),
                dominoes_yellow = rep(dice_colors[5], 7),
                icehouse_pieces = dice_colors,
                go = go_colors)

add_piece <- function(cm, piece_side, suit, rank, x, y, angle, cfg, reorient = "none") {
    if (piecepackr:::has_suit(piece_side)) {
        ss <- ss_list[[cfg]][suit]
        if (piece_side == "pyramid_top") ss <- top_subs[[ss]]
        if (!grepl("matchstick", piece_side)) ss <- rotate(ss, angle, reorient)
        fg <- fg_list[[cfg]][suit]
    } else {
        fg <- "black"
    }
    if (piecepackr:::has_rank(piece_side)) {
        rs <- rs_list[[cfg]][rank]
        if (grepl("chess", cfg) && suit == 6L) rs <- unicode_chess_white[rank]
        if (!grepl("matchstick", piece_side)) rs <- rotate(rs, angle, reorient)
    }
    if (grepl("2", cfg)) {
        cell <- 2
    } else {
        cell <- 1
    }
    switch(piece_side,
           coin_back = add_coin_back(cm, ss, x, y, fg),
           coin_face = add_coin_face(cm, rs, x, y, fg),
           die_face = add_die_face(cm, rs, x, y, angle, fg),
           pawn_face = add_pawn_face(cm, ss, x, y, angle, fg),
           pawn_back = add_pawn_back(cm, ss, x, y, angle, fg),
           tile_face = add_tile_face(cm, ss, rs, x, y, angle, fg, cfg),
           tile_back = add_tile_back(cm, x, y, angle, cfg),
           bit_back = add_bit_back(cm, ss, x, y, fg),
           bit_face = add_bit_face(cm, rs, x, y, fg),
           board_back = add_board(cm, x, y, cell * rank, cell * rank, cell),
           board_face = add_board(cm, x, y, cell * rank, cell * rank, cell),
           matchstick_back = add_matchstick_face(cm, x, y, angle, fg, rank),
           matchstick_face = add_matchstick_face(cm, x, y, angle, fg, rank),
           pyramid_top = add_pyramid_top(cm, ss, x, y, angle, fg, rank),
           pyramid_face = add_pyramid_face(cm, ss, x, y, angle, fg, rank),
           pyramid_left = add_pyramid_face(cm, ss, x, y, angle, fg, rank),
           pyramid_right = add_pyramid_face(cm, ss, x, y, angle, fg, rank),
           pyramid_back = add_pyramid_face(cm, ss, x, y, angle, fg, rank),
           { # nolint
               warning("Don't know how to draw ", piece_side)
               cm
           })
}
add_matchstick_face <- function(cm, x, y, angle, fg, rank) {
    switch(rank,
           add_matchstick_face1(cm, x, y, angle, fg),
           add_matchstick_face2(cm, x, y, angle, fg),
           add_matchstick_face3(cm, x, y, angle, fg),
           add_matchstick_face4(cm, x, y, angle, fg),
           add_matchstick_face5(cm, x, y, angle, fg),
           add_matchstick_face6(cm, x, y, angle, fg),
           stop("Don't know how to draw matchstick of rank", rank))
}
add_matchstick_face1 <- function(cm, x, y, angle, fg) {
    if (angle %in% c(0, 90, 180, 270)) {
        cm$char[y, x] <- "\u25a0"
    } else if (angle %in% c(45, 135, 225, 315)) {
        cm$char[y, x] <- "\u25c6"
    } else {
        stop("Can't handle angle ", angle)
    }
    cm$fg[y, x] <- fg
    cm
}
add_matchstick_face2 <- function(cm, x, y, angle, fg) {
    if (angle %in% c(0, 180)) {
        cm$char[y, x] <- "\u2503"
    } else if (angle %in% c(90, 270)) {
        cm$char[y, x] <- "\u2501"
    } else if (angle %in% c(45, 225)) {
        cm$char[y, x] <- "\u2572"
    } else if (angle %in% c(135, 315)) {
        cm$char[y, x] <- "\u2571"
    } else {
        stop("Can't handle angle ", angle)
    }
    cm$fg[y, x] <- fg
    cm
}
add_matchstick_face3 <- add_matchstick_face2

add_matchstick_face4 <- function(cm, x, y, angle, fg) {
    if (angle %in% c(0, 180)) {
        cm$char[y+-1:1, x] <- "\u2503"
        cm$fg[y+-1:1, x] <- fg
    } else if (angle %in% c(90, 270)) {
        cm$char[y, x+-1:1] <- "\u2501"
        cm$fg[y, x+-1:1] <- fg
    } else if (angle %in% c(45, 225)) {
        cm$char[y+1, x+-1] <- "\u2572"
        cm$fg[y+1, x+-1] <- fg
        cm$char[y, x] <- "\u2572"
        cm$fg[y, x] <- fg
        cm$char[y-1, x+1] <- "\u2572"
        cm$fg[y-1, x+1] <- fg
    } else if (angle %in% c(135, 315)) {
        cm$char[y+-1, x+-1] <- "\u2571"
        cm$fg[y+-1, x+-1] <- fg
        cm$char[y, x] <- "\u2571"
        cm$fg[y, x] <- fg
        cm$char[y+1, x+1] <- "\u2571"
        cm$fg[y+1, x+1] <- fg
    } else if (angle %in% c(60, 240)) {
        cm$char[y, x] <- "\u2572"
        cm$fg[y, x] <- fg
    } else if (angle %in% c(120, 300)) {
        cm$char[y, x] <- "\u2571"
        cm$fg[y, x] <- fg
    } else {
        stop("Can't handle angle ", angle)
    }
    cm
}
add_matchstick_face5 <- add_matchstick_face4
add_matchstick_face6 <- function(cm, x, y, angle, fg) {
    if (angle %in% c(0, 180)) {
        cm$char[y+-2:2, x] <- "\u2503"
        cm$fg[y+-2:2, x] <- fg
    } else if (angle %in% c(90, 270)) {
        cm$char[y, x+-2:2] <- "\u2501"
        cm$fg[y, x+-2:2] <- fg
    } else if (angle %in% c(45, 225)) {
        cm$char[y+1, x+-1] <- "\u2572"
        cm$fg[y+1, x+-1] <- fg
        cm$char[y, x] <- "\u2572"
        cm$fg[y, x] <- fg
        cm$char[y-1, x+1] <- "\u2572"
        cm$fg[y-1, x+1] <- fg
    } else if (angle %in% c(135, 315)) {
        cm$char[y+-1, x+-1] <- "\u2571"
        cm$fg[y+-1, x+-1] <- fg
        cm$char[y, x] <- "\u2571"
        cm$fg[y, x] <- fg
        cm$char[y+1, x+1] <- "\u2571"
        cm$fg[y+1, x+1] <- fg
    } else {
        stop("Can't handle angle ", angle)
    }
    cm
}
add_bit_face <- function(cm, rs, x, y, fg) {
    cm$char[y, x] <- rs
    cm$fg[y, x] <- fg
    cm
}
add_bit_back <- function(cm, ss, x, y, fg) {
    cm$char[y, x] <- ss
    cm$fg[y, x] <- fg
    cm
}
add_coin_back <- function(cm, ss, x, y, fg) {
    cm$char[y, x] <- paste0(ss, "\u20dd")
    cm$fg[y, x] <- fg
    cm
}
add_coin_face <- function(cm, rs, x, y, fg) {
    cm$char[y, x] <- paste0(rs, "\u20dd")
    cm$fg[y, x] <- fg
    cm
}
add_die_face <- function(cm, rs, x, y, angle, fg) {
    if (angle %% 90 == 0) {
        char <- paste0(rs, "\u20de")
    } else {
        char <- paste0(rs, "\u20df")
    }
    # nolint start
    # ds <- die_subs[[char]]
    # if (!is.null(ds)) char <- ds
    # nolint end
    cm$char[y, x] <- char
    cm$fg[y, x] <- fg
    cm
}
add_pawn_face <- function(cm, ss, x, y, angle, fg) {
    if (angle %% 90 == 0) {
        cm$char[y, x] <- paste0(ss, "\u20df")
    } else {
        cm$char[y, x] <- paste0(ss, "\u20de")
    }
    cm$fg[y, x] <- fg
    cm
}
add_pawn_back <- function(cm, ss, x, y, angle, fg) {
    if (angle %% 90 == 0) {
        cm$char[y, x] <- paste0(ss, "\u20df")
    } else {
        cm$char[y, x] <- paste0(ss, "\u20de")
    }
    cm$fg[y, x] <- fg
    cm
}
add_pyramid_face <- function(cm, ss, x, y, angle, fg, rank = 1) {
    # nolint start
    # if (angle %% 90 == 0) {
    #     cm$char[y, x] <- paste0(ss, "\u20de")
    # } else {
    #     cm$char[y, x] <- paste0(ss, "\u20df")
    # }
    # nolint end
    cm$char[y, x] <- paste0(ss, get_dots(rank))
    cm$fg[y, x] <- fg
    cm
}
# top dots U+0307 U+0308 U+20db U+20dc
# bottom dots U+0323 U+0324 U+20ef
get_dots <- function(rank) {
    switch(rank,
           "\u0323",
           "\u0324",
           "\u20e8",
           stop("Doesn't support ", rank, " dots"))
}
add_pyramid_top <- function(cm, ss, x, y, angle, fg, rank = 1) {
    # nolint start
    # if (angle %% 90 == 0) {
    #     cm$char[y, x] <- paste0(ss, "\u20de")
    # } else {
    #     cm$char[y, x] <- paste0(ss, "\u20df")
    # }
    # nolint end
    cm$char[y, x] <- paste0(ss, get_dots(rank))
    cm$fg[y, x] <- fg
    cm
}
add_tile_back <- function(cm, x, y, angle, cfg) {
    if (angle %% 90 != 0)
        stop("Don't know how to handle angle ", angle)

    if (cfg == "subpack") {
        add_tile_back_subpack(cm, x, y)
    } else if (grepl("dominoes", cfg)) {
        add_tile_back_dominoes(cm, x, y, angle)
    } else {
        add_tile_back_piecepack(cm, x, y)
    }
}
add_tile_back_dominoes <- function(cm, x, y, angle) {
    if (angle %% 180 == 0) { # vertical
        cm$fg[y+-2:2, x+-1:1] <- "black"
        cm$char[y+-1:1, x] <- " "
        cm <- add_border(cm, x, y, width = 1, height = 2)
        cm
    } else if (angle %% 90 == 0) { # horizontal
        cm$fg[y+-1:1, x+-2:2] <- "black"
        cm$char[y, x+-1:1] <- " "
        cm <- add_border(cm, x, y, width = 2, height = 1)
        cm
    }
}
add_tile_back_piecepack <- function(cm, x, y) {
    cm$fg[y+-2:2, x+-2:2] <- "black"
    cm$char[y+-1:1, x+-1:1] <- " "
    cm <- add_border(cm, x, y)
    cm <- add_gridlines(cm, x, y)
    cm
}
add_tile_back_subpack <- function(cm, x, y) {
    cm$fg[y+-1:1, x+-1:1] <- "black"
    cm <- add_border(cm, x, y, 1, 1)
    cm <- add_gridlines(cm, x, y, 1, 1, 0.5)
    cm
}
add_tile_face <- function(cm, ss, rs, x, y, angle, fg, cfg) {
    if (angle %% 90 != 0)
        stop("Don't know how to handle angle ", angle)

    if (cfg == "subpack") {
        add_tile_face_subpack(cm, rs, x, y, fg)
    } else if (grepl("dominoes", cfg)) {
        add_tile_face_dominoes(cm, ss, rs, x, y, angle, fg)
    } else {
        add_tile_face_piecepack(cm, ss, rs, x, y, angle, fg)
    }
}
add_tile_face_subpack <- function(cm, rs, x, y, fg) {
    cm$fg[y+-1:1, x+-1:1] <- "black"
    cm$char[y, x] <- rs
    cm$fg[y, x] <- fg
    cm <- add_border(cm, x, y, 1, 1)
    cm
}
add_tile_face_dominoes <- function(cm, ss, rs, x, y, angle, fg) {
    if (angle == 0) {
        cm$fg[y+-2:2, x+-1:1] <- "black"
        cm <- add_border(cm, x, y, width = 1, height = 2)
        cm$char[y+-1:1, x] <-  c(ss, "\u2501", rs)
        cm$fg[y+-1:1, x] <- fg
    } else if (angle == 90) {
        cm$fg[y+-1:1, x+-2:2] <- "black"
        cm$char[y, x+-1:1] <- " "
        cm <- add_border(cm, x, y, width = 2, height = 1)
        cm$char[y, x+-1:1] <-  c(rs, "\u2503", ss)
        cm$fg[y, x+-1:1] <- fg
    }
    if (angle == 180) {
        cm$fg[y+-2:2, x+-1:1] <- "black"
        cm <- add_border(cm, x, y, width = 1, height = 2)
        cm$char[y+-1:1, x] <-  c(rs, "\u2501", ss)
        cm$fg[y+-1:1, x] <- fg
    } else if (angle == 270) {
        cm$fg[y+-1:1, x+-2:2] <- "black"
        cm <- add_border(cm, x, y, width = 2, height = 1)
        cm$char[y, x+-1:1] <-  c(ss, "\u2503", rs)
        cm$fg[y, x+-1:1] <- fg
    }
    cm
}
add_tile_face_piecepack <- function(cm, ss, rs, x, y, angle, fg) {
    cm$fg[y+-2:2, x+-2:2] <- "black"
    cm$char[y+-1:1, x+-1:1] <- " "
    cm <- add_border(cm, x, y)
    # rank symbol
    cm$char[y, x] <- rs
    cm$fg[y, x] <- fg
    # suit symbol
    if (angle == 0) {
        cm$char[y+1, x-1] <- ss
        cm$fg[y+1, x-1] <- fg
    } else if (angle == 90) {
        cm$char[y-1, x-1] <- ss
        cm$fg[y-1, x-1] <- fg
    } else if (angle == 180) {
        cm$char[y-1, x+1] <- ss
        cm$fg[y-1, x+1] <- fg
    } else if (angle == 270) {
        cm$char[y+1, x+1] <- ss
        cm$fg[y+1, x+1] <- fg
    }
    cm
}
add_board <- function(cm, x, y, width = 8, height = 8, cell = 1) {
    cm$fg[y+-height:height, x+-width:width] <- "black"
    cm <- add_border(cm, x, y, width, height)
    cm <- add_gridlines(cm, x, y, width, height, cell)
    cm
}

add_gridlines <- function(cm, x, y, width = 2, height = 2, cell = 1) {
    # gridlines
    xgs <- x + seq(2 * cell - width, width - 2 * cell, 2 * cell)
    ygs <- y + seq(2 * cell - height, height - 2 * cell, 2 * cell)
    xo <- x + seq(1 - width, width - 1)
    yo <- y + seq(1 - height, height - 1)

    cm$char[ygs, xo] <- "\u2501" # horizontal lines
    cm$char[yo, xgs] <- "\u2503" # vertical lines
    cm$char[ygs, xgs] <- "\u254b" # crosses

    # intersection gridlines and border line
    for (xg in xgs) {
        cm <- add_box_edge(cm, xg, y+height, c(NA, 1, 2, 1)) # top
        cm <- add_box_edge(cm, xg, y-height, c(2, 1, NA, 1)) # bottom
    }
    for (yg in ygs) {
        cm <- add_box_edge(cm, x+width, yg, c(1, NA, 1, 2)) # right
        cm <- add_box_edge(cm, x-width, yg, c(1, 2, 1, NA)) # left
    }
    cm
}

add_border <- function(cm, x, y, width = 2, height = 2) {
    for (i in seq(1 - width, width - 1)) {
        cm <- add_box_edge(cm, x+i, y+height, c(NA, 1, 0, 1)) # top side
        cm <- add_box_edge(cm, x+i, y-height, c(0, 1, NA, 1)) # bottom side
    }
    for (j in seq(1 - height, height - 1)) {
        cm <- add_box_edge(cm, x+width, y+j, c(1, NA, 1, 0)) # right side
        cm <- add_box_edge(cm, x-width, y+j, c(1, 0, 1, NA)) # left side
    }
    cm <- add_box_edge(cm, x-width, y+height, c(NA, 1, 1, NA)) # ul corner
    cm <- add_box_edge(cm, x+width, y+height, c(NA, NA, 1, 1)) # ur corner
    cm <- add_box_edge(cm, x-width, y-height, c(1, 1, NA, NA)) # ll corner
    cm <- add_box_edge(cm, x+width, y-height, c(1, NA, NA, 1)) # lr corner
    cm
}

add_box_edge <- function(cm, x, y, box_info) {
    # [top, right, bottom, left] 0-none 1-light 2-dark
    bi <- char2bi[[cm$char[y, x]]]
    if (is.null(bi)) bi <- c(0, 0, 0, 0)
    ind <- which(!is.na(box_info))
    for (ii in ind) {
        bi[ii] <- box_info[ii]
    }
    cm$char[y, x] <- box2char[[paste(bi, collapse = "")]]
    cm
}

rotate <- function(char, angle, reorient = "none") {
    if (angle == 0 || reorient == "symbols") {
        rchar <- char
    } else if (angle == 45) {
        rchar <- r45[[char]]
    } else if (angle == 90) {
        rchar <- r90[[char]]
    } else if (angle == 135) {
        rchar <- r135[[char]]
    } else if (angle == 180) {
        rchar <- r180[[char]]
    } else if (angle == 225) {
        rchar <- r225[[char]]
    } else if (angle == 270) {
        rchar <- r270[[char]]
    } else if (angle == 315) {
        rchar <- r315[[char]]
    } else {
        rchar <- NULL
    }
    if (is.null(rchar)) {
        warning(paste("Can't rotate", char, angle, "degrees"))
        char
    } else {
        rchar
    }
}
