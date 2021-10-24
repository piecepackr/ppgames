#' Generate plaintext piecepack diagrams
#'
#' `cat_piece()` generates plaintext piecepack diagrams and
#'  outputs them using `base::cat()`.  `cat_move()` generates
#' a plaintext diagram for a move within a game.  `cat_game()`
#' renders an animation of a game in the terminal.
#'
#' @param df Data frame containing piece info.
#' @param color How should the text be colorized.
#'              If `FALSE` won't colorize output at all.
#'              If `"html"` will colorize output for html.
#'              Otherwise will colorize output for the terminal using ANSI CSI SGR control sequences.
#' @param reorient Determines whether and how we should reorient (the angle) of pieces or symbols:\enumerate{
#'        \item{The default "none" (or `FALSE`) means don't reorient any pieces/symbols.}
#'        \item{"all" (or `TRUE`) means setting the angle to zero for all pieces
#'              (reorienting them all \dQuote{up}).}
#'        \item{"symbols" means just re-orient suit/rank symbols but not the orientation of the piece itself.
#'              In particular, in contrast with "all" this preserves the location
#'              of the upper-left "corner" of piecepack tile faces.}}
#' @param annotate If `TRUE` or `"algebraic"` annotate the plot
#'                  with \dQuote{algrebraic} coordinates,
#'                 if `FALSE` or `"none"` don't annotate,
#'                 if `"cartesian"` annotate the plot with \dQuote{cartesian} coordinates.
#' @param ... Passed to `cat()`
#' @param file `file` argument of `cat()`.
#'             Default (`""`) is to print to standard output.
#'             `NULL` means we don't `cat()`
#' @param annotation_scale Multiplicative factor that scales (stretches) any annotation coordinates.
#'                         By default uses `attr(df, "scale_factor") %||% 1`.
#' @param style If "Unicode" (default) only use glyphs in Unicode proper.
#'              If "Game Bit Duo" use glyphs in Private Use Area of "Game Bit Duo" font.
#'              If "Game Bit Mono" use glyphs in Private Use Area of "Game Bit Mono" font.
#' @return String of text diagram (returned invisibly).
#' @importFrom rlang %||% abort
#' @seealso See <https://trevorld/game-bit-font> for more information about the \dQuote{Game Bit} family of fonts.
#' @export
cat_piece <- function(df, color = NULL, reorient = "none", annotate = FALSE, ...,
                      file = "", annotation_scale = NULL, style = c("Unicode", "Game Bit Mono", "Game Bit Duo")) {
    cat_piece_helper(df, ..., color = color, reorient = reorient, annotate = annotate, ...,
                     file = file, annotation_scale = annotation_scale, style = style)
}
cat_piece_helper <- function(df, color = NULL, reorient = "none", annotate = FALSE, ...,
                             xoffset = NULL, yoffset = NULL,
                             file = "", annotation_scale = NULL, style = "Unicode") {
    color <- color %||% (is.null(file) || file == "")
    annotation_scale <- annotation_scale %||% attr(df, "scale_factor") %||% 1
    if (nrow(df) == 0) {
        if (!is.null(file)) cat("", file = file)
        return(invisible(""))
    }
    style <- get_style(style = style[1])
    df <- clean_df(df)
    if (isTRUE(reorient) || reorient == "all") df$angle <- 0

    lr <- range_heuristic(df)
    offset <- get_df_offsets(df, lr, xoffset, yoffset, annotate)
    df$x <- df$x + offset$x
    df$y <- df$y + offset$y
    nc <- 2 * (lr$xmax + offset$x) + 1
    nr <- 2 * (lr$ymax + offset$y) + 1
    cm <- list(char = matrix(style$space, nrow = nr, ncol = nc),
               bg = matrix("#FFFFFF", nrow = nr, ncol = nc),
               fg = matrix("black", nrow = nr, ncol = nc))

    for (rr in seq(nrow(df))) {
        ps <- as.character(df[rr, "piece_side"])
        suit <- as.numeric(df[rr, "suit"])
        rank <- as.numeric(df[rr, "rank"])
        x <- 2*as.numeric(df[rr, "x"])+1
        y <- 2*as.numeric(df[rr, "y"])+1
        angle <- as.numeric(df[rr, "angle"])
        cfg <- as.character(df[rr, "cfg"])
        cm <- add_piece(cm, ps, suit, rank, x, y, angle, cfg, reorient, style)
    }
    cm <- annotate_text(cm, nc, nr, offset$x, offset$y, annotate, annotation_scale)
    cm <- color_text(cm, color)

    text <- rev(apply(cm$char, 1, function(x) paste(x, collapse = "")))
    text <- paste(text, collapse = "\n")
    if (color == "html") {
        piecepackr:::assert_suggested("fansi")
        text <- fansi::sgr_to_html(text)
    }
    text <- paste0(text, "\n")
    if (!is.null(file)) cat(text, ..., file = file)
    invisible(text)
}

get_style <- function(style = "Unicode") {
    style <- tolower(style)
    style <- gsub("-", "", style)
    style <- gsub(" ", "", style)
    style <- match.arg(style, c("unicode", "gamebitduo", "gamebitmono"))

    if (style == "gamebitduo") {
        space <- "  " ####
    } else {
        space <- " "
    }

    list(rotate = get_style_rotate(style),
         rs = get_style_rs(style),
         rs_big = get_style_rs(style, big = TRUE),
         ss = get_style_ss(style),
         ss_big = get_style_ss(style, big = TRUE),
         fg = get_style_fg(style),
         combining = get_style_combining(style),
         space = space,
         has_pua_box_drawing = style != "unicode"
         )
}

get_style_combining <- function(style) {
    if (style == "unicode")
        coin <- "\u20dd"
    else
        coin <- "\U000FCE50"

    if (style == "unicode")
        pawn <- "\u20df"
    else
        pawn <- "\U000FCDE0"

    die_suits <- rep("\u20de", 6)
    if (style == "unicode") {
        piecepack_suits <- die_suits
        french_suits_black <- die_suits
        french_suits_white <- die_suits
    } else {
        piecepack_suits    <- intToUtf8(utf8ToInt("\U000FCE00") + 0:3, multiple = TRUE)
        french_suits_black <- intToUtf8(utf8ToInt("\U000FCE20") + 0:3, multiple = TRUE)
        french_suits_white <- intToUtf8(utf8ToInt("\U000FCE30") + 0:3, multiple = TRUE)
    }
    die <- list(piecepack = piecepack_suits,
                playing_cards_expansion = french_suits_black,
                dual_piecepacks_expansion = french_suits_white,
                subpack = piecepack_suits,
                dice = die_suits)

    list(coin = coin, die = die, pawn = pawn)
}

get_style_rs <- function(style, big = FALSE) {

    if (style == "unicode") {
        dominoes_ranks <- c(" ", "\u00b7", "\u280c", "\u22f0", "\u2237", "\u2059", "\u283f")
    } else {
        dominoes_ranks <- c("\U000FCA00", "\U000FCA01", "\U000FCA02", "\U000FCA03", "\U000FCA04",
                            "\U000FCA05", "\U000FCA06", "\U000FCA07", "\U000FCA08", "\U000FCA09",
                            "\U000FCA0A", "\U000FCA0B", "\U000FCA0C", "\U000FCA0D", "\U000FCA0E",
                            "\U000FCA0F", "\U000FCA10", "\U000FCA11", "\U000FCA12")
    }

    if (style == "unicode") {
        piecepack_ranks <- c("n", "a", "2", "3", "4", "5")
    } else {
        if (big)
            piecepack_ranks <- intToUtf8(utf8ToInt("\U000FCB50") + 0:11, multiple = TRUE)
        else
            piecepack_ranks <- intToUtf8(utf8ToInt("\U000FCC50") + 0:11, multiple = TRUE)
    }

    rs <- list(piecepack = piecepack_ranks,
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
    rs
}

get_style_ss <- function(style, big = FALSE) {
    # nolint start
    # Use Half-circle for Moons? \u25d0
    # Use Arrows for Arms?
    # nolint end
    if (style == "unicode") {
        dominoes_ranks <- c(" ", "\u00b7", "\u280c", "\u22f0", "\u2237", "\u2059", "\u283f")
        piecepack_suits <- c("\u2600", "\u263e", "\u265b", "\u2e38")
        french_suits_black <- c("\u2665", "\u2660", "\u2663", "\u2666")
        french_suits_white <- c("\u2661", "\u2664", "\u2667", "\u2662")
    } else {
        dominoes_ranks <- c("\U000FCA00", "\U000FCA01", "\U000FCA02", "\U000FCA03", "\U000FCA04",
                            "\U000FCA05", "\U000FCA06", "\U000FCA07", "\U000FCA08", "\U000FCA09")
        if (big) {
            piecepack_suits    <- intToUtf8(utf8ToInt("\U000FCB00") + 0:3, multiple = TRUE)
            french_suits_black <- intToUtf8(utf8ToInt("\U000FCB20") + 0:3, multiple = TRUE)
            french_suits_white <- intToUtf8(utf8ToInt("\U000FCB30") + 0:3, multiple = TRUE)
        } else {
            piecepack_suits    <- intToUtf8(utf8ToInt("\U000FCC00") + 0:3, multiple = TRUE)
            french_suits_black <- intToUtf8(utf8ToInt("\U000FCC20") + 0:3, multiple = TRUE)
            french_suits_white <- intToUtf8(utf8ToInt("\U000FCC30") + 0:3, multiple = TRUE)
        }
    }

    ss <- list(piecepack = piecepack_suits,
                    playing_cards_expansion = french_suits_black,
                    dual_piecepacks_expansion = french_suits_white,
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
    ss
}

get_style_fg <- function(style) {
    fg <- list(piecepack = piecepack_colors,
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

    fg
}

color_text <- function(cm, color) {
    if (!isFALSE(color)) {
        for (rr in seq(nrow(cm$char))) {
            for (cc in seq(ncol(cm$char))) {
                fg <- crayon::make_style(cm$fg[rr, cc])
                bg <- crayon::make_style(cm$bg[rr, cc], bg = TRUE)
                colorize <- crayon::combine_styles(fg, bg)
                cm$char[rr, cc] <- colorize(cm$char[rr, cc])
            }
        }
    }
    cm
}

annotate_text <- function(cm, nc, nr, xoffset, yoffset, annotate, annotation_scale) {
    if (isFALSE(annotate) || annotate == "none") return(cm)
    step <- 2 * annotation_scale
    x <- seq(1 + step + 2 * xoffset, nc, by = step)
    if (annotate == "cartesian") {
        x <- utils::head(x, 9)
        xt <- as.character(seq_along(x))
        cm$char[1, x] <- xt
    } else {
        if (length(x) > 26) x <- x[1:26]
        cm$char[1, x] <- letters[seq_along(x)]
    }
    y <- seq(1 + step + 2 * yoffset, nr, by= step)
    yt <- as.character(seq_along(y))
    if (length(yt) > 9) {
        yt <- stringr::str_pad(yt, 2, "right")
        cm$char[y[-seq(9)], 2L] <- substr(yt[-seq(9)], 2, 2)
    }
    cm$char[y, 1L] <- substr(yt, 1L, 1L)
    cm
}

clean_df <- function(df) {
    if (!hasName(df, "cfg")) df$cfg <- "piecepack"
    df$cfg <- ifelse(is.na(df$cfg), "piecepack", df$cfg)
    if (!hasName(df, "rank")) df$rank <- NA_integer_
    df$rank <- ifelse(is.na(df$rank), 1L, df$rank)
    # checkers/chess boards rank is number of cells
    df$rank <- ifelse(df$rank == 1L & str_detect(df$piece_side, "^board") & str_detect(df$cfg, "[12]$"), 8L, df$rank)
    # go board rank is number of lines
    df$rank <- ifelse(str_detect(df$piece_side, "^board") & df$cfg == "go",
                      ifelse(df$rank == 1L, 18, df$rank - 1),
                      df$rank)
    if (!hasName(df, "suit")) df$suit <- NA_integer_
    df$suit <- ifelse(is.na(df$suit), 1L, df$suit)
    if (!hasName(df, "angle")) df$angle <- NA_real_
    df$angle <- ifelse(is.na(df$angle), 0, df$angle %% 360)
    attr(df, "was_cleaned") <- TRUE
    df
}

#' @rdname cat_piece
#' @param game A list containing a parsed ppn game (as parsed by \code{read_ppn})
#' @param move Which move to cat game state (after the move, will use \code{game$dfs[[move]]})
#'             unless \code{NULL} in which case will cat the game state after the last move.
#' @export
cat_move <- function(game, move = NULL, ...) {
    df <- get_df_from_move(game, move)
    cat_piece_helper(df, ...)
}

get_df_offsets <- function(df, lr, xoffset, yoffset, annotate = FALSE) {
    if (!(isFALSE(annotate) || annotate == "none")) {
        xlbound <- ifelse(lr$ymax >= 10, 1.0, 0.5)
        ylbound <- 0.5
    } else {
        xlbound <- 0
        ylbound <- 0
    }
    if (is.null(xoffset)) xoffset <- min2offset(lr$xmin, xlbound)
    if (is.null(yoffset)) yoffset <- min2offset(lr$ymin, ylbound)
    list(x = xoffset, y = yoffset)
}

get_game_offsets <- function(game, annotate = FALSE, ...) {
    ranges <- lapply(game$dfs, range_heuristic)
    ymax <- max(sapply(ranges, function(x) x$ymax), na.rm = TRUE)
    ymin <- min(sapply(ranges, function(x) x$ymin), na.rm = TRUE)
    xmin <- min(sapply(ranges, function(x) x$xmin), na.rm = TRUE)
    if (!(isFALSE(annotate) || annotate == "none")) {
        xoffset <- min2offset(xmin, ifelse(ymax >= 10, 1.0, 0.5))
        yoffset <- min2offset(ymin, 0.5)
    } else {
        xoffset <- min2offset(xmin, 0)
        yoffset <- min2offset(ymin, 0)
    }
    list(x = xoffset, y = yoffset)
}

#' @rdname cat_piece
#' @param fps Frames per second.
#' @export
cat_game <- function(game, ..., fps = 1) {
    offset <- get_game_offsets(game, ...)
    for (ii in seq_along(game$dfs)) {
        prev <- system.time({
            out <- cat_piece_helper(game$dfs[[ii]], ..., xoffset=offset$x, yoffset=offset$y, file=NULL)
        })[["elapsed"]]
        dur <- ifelse(1/fps - prev > 0, 1/fps - prev, 0)
        Sys.sleep(dur)
        clear_screen()
        cat(out)
    }
}

clear_screen <- function() {
    switch(.Platform$OS.type,
           unix = system("clear"),
           windows = system("cls"))
}

#### darkgreen sometimes shows up as black?
#### black instead of grey sometimes?
checkers_colors <- c("darkred", "black", "green", "darkblue", "darkorange3", "black")
piecepack_colors <- dice_colors <- chess_colors <- go_colors <- checkers_colors
dice_colors[2] <- "grey"

add_piece <- function(cm, piece_side, suit, rank, x, y, angle, cfg, reorient = "none", style = get_style()) {
    if (piece_side %in% c("tile_back", "coin_face", "card_back")) {
        fg <- "black"
    } else {
        if (grepl("pyramid", piece_side)) cfg <- "icehouse_pieces"
        if (piece_side == "tile_face")
            ss <- style$ss_big[[cfg]][suit]
        else
            ss <- style$ss[[cfg]][suit]
        if (piece_side == "pyramid_top") ss <- top_subs[[ss]]
        if (!grepl("matchstick", piece_side)) ss <- style$rotate(ss, angle, reorient)
        fg <- style$fg[[cfg]][suit]
    }
    if (!(piece_side %in% c("tile_back", "coin_back", "card_back", "pawn_face", "pawn_back"))) {
        if (piece_side == "tile_face")
            rs <- style$rs_big[[cfg]][rank]
        else
            rs <- style$rs[[cfg]][rank]
        if (grepl("chess", cfg) && suit == 6L) rs <- unicode_chess_white[rank]
        if (!grepl("matchstick", piece_side)) rs <- style$rotate(rs, angle, reorient)
    }
    if (grepl("2", cfg)) {
        cell <- 2
    } else {
        cell <- 1
    }
    switch(piece_side,
           coin_back = add_coin_back(cm, ss, x, y, angle, fg, style),
           coin_face = add_coin_face(cm, rs, x, y, angle, fg, style),
           die_face = add_die_face(cm, rs, x, y, angle, fg, cfg, style, suit),
           pawn_face = add_pawn_face(cm, ss, x, y, angle, fg, style),
           pawn_back = add_pawn_back(cm, ss, x, y, angle, fg, style),
           tile_face = add_tile_face(cm, ss, rs, x, y, angle, fg, cfg, style),
           tile_back = add_tile_back(cm, x, y, angle, cfg, style),
           bit_back = add_bit_back(cm, ss, x, y, fg),
           bit_face = add_bit_face(cm, rs, x, y, fg),
           board_back = add_board(cm, x, y, cell * rank, cell * rank, cell, style),
           board_face = add_board(cm, x, y, cell * rank, cell * rank, cell, style),
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
           abort(paste("Don't know how to draw matchstick of rank", rank),
                 class = "unicode_diagram"))
}
abort_angle <- function(angle) {
    abort(paste("Can't handle angle", angle),
          class = "unicode_diagram",
          angle = angle)

}
add_matchstick_face1 <- function(cm, x, y, angle, fg) {
    if (angle %in% c(0, 90, 180, 270)) {
        cm$char[y, x] <- "\u25a0"
    } else if (angle %in% c(45, 135, 225, 315)) {
        cm$char[y, x] <- "\u25c6"
    } else {
        abort_angle(angle)
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
        abort_angle(angle)
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
        abort_angle(angle)
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
        abort_angle(angle)
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
add_coin_back <- function(cm, ss, x, y, angle, fg, style) {
    enclosing_coin <- style$rotate(style$combining$coin, angle)
    cm$char[y, x] <- paste0(ss, enclosing_coin)
    cm$fg[y, x] <- fg
    cm
}
add_coin_face <- function(cm, rs, x, y, angle, fg, style) {
    enclosing_coin <- style$rotate(style$combining$coin, angle)
    cm$char[y, x] <- paste0(rs, enclosing_coin)
    cm$fg[y, x] <- fg
    cm
}
add_die_face <- function(cm, rs, x, y, angle, fg, cfg, style, suit) {
    enclosing_die <- style$rotate(style$combining$die[[cfg]][suit], angle)
    # nolint start
    # ds <- die_subs[[char]]
    # if (!is.null(ds)) char <- ds
    # nolint end
    char <- paste0(rs, enclosing_die)
    cm$char[y, x] <- char
    cm$fg[y, x] <- fg
    cm
}
add_pawn_face <- function(cm, ss, x, y, angle, fg, style) {
    enclosing_pawn <- style$rotate(style$combining$pawn, angle)
    cm$char[y, x] <- paste0(ss, enclosing_pawn)
    cm$fg[y, x] <- fg
    cm
}
add_pawn_back <- function(cm, ss, x, y, angle, fg, style) {
    enclosing_pawn <- style$rotate(style$combining$pawn, angle)
    cm$char[y, x] <- paste0(ss, enclosing_pawn)
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
    switch(rank, "\u0323", "\u0324", "\u20e8", "\u0324\u0308", "\u20e8\u0308", "\u20e8\u20db",
           abort(paste("Doesn't support", rank, "dots")), class = "unicode_diagram")
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
add_tile_back <- function(cm, x, y, angle, cfg, style) {
    if (angle %% 90 != 0) abort_angle(angle)

    if (cfg == "subpack") {
        add_tile_back_subpack(cm, x, y, style)
    } else if (grepl("dominoes", cfg)) {
        add_tile_back_dominoes(cm, x, y, angle, style)
    } else {
        add_tile_back_piecepack(cm, x, y, style)
    }
}
add_tile_back_dominoes <- function(cm, x, y, angle, style) {
    if (angle %% 180 == 0) { # vertical
        cm$fg[y+-2:2, x+-1:1] <- "black"
        cm <- add_border(cm, x, y, width = 1, height = 2, space = style$space)
        cm
    } else if (angle %% 90 == 0) { # horizontal
        cm$fg[y+-1:1, x+-2:2] <- "black"
        cm <- add_border(cm, x, y, width = 2, height = 1, space = style$space)
        cm
    }
}
add_tile_back_piecepack <- function(cm, x, y, style) {
    cm$fg[y+-2:2, x+-2:2] <- "black"
    cm <- add_border(cm, x, y, space = style$space)
    cm <- add_gridlines(cm, x, y, has_pua_box_drawing = style$has_pua_box_drawing)
    cm
}
add_tile_back_subpack <- function(cm, x, y, style) {
    cm$fg[y+-1:1, x+-1:1] <- "black"
    cm <- add_border(cm, x, y, 1, 1, space = style$space)
    cm <- add_gridlines(cm, x, y, 1, 1, 0.5,
                        has_pua_box_drawing = style$has_pua_box_drawing)
    cm
}
add_tile_face <- function(cm, ss, rs, x, y, angle, fg, cfg, style) {
    if (angle %% 90 != 0) abort_angle(angle)

    if (cfg == "subpack") {
        add_tile_face_subpack(cm, rs, x, y, fg, style)
    } else if (grepl("dominoes", cfg)) {
        add_tile_face_dominoes(cm, ss, rs, x, y, angle, fg, style)
    } else {
        add_tile_face_piecepack(cm, ss, rs, x, y, angle, fg, style)
    }
}
add_tile_face_subpack <- function(cm, rs, x, y, fg, style) {
    cm$fg[y+-1:1, x+-1:1] <- "black"
    cm <- add_border(cm, x, y, 1, 1, space = style$space)
    cm$char[y, x] <- rs
    cm$fg[y, x] <- fg
    cm
}
add_tile_face_dominoes <- function(cm, ss, rs, x, y, angle, fg, style) {
    if (angle == 0) {
        cm$fg[y+-2:2, x+-1:1] <- "black"
        cm <- add_border(cm, x, y, width = 1, height = 2, space = style$space)
        cm$char[y+-1:1, x] <-  c(ss, "\u2501", rs)
        cm$fg[y+-1:1, x] <- fg
    } else if (angle == 90) {
        cm$fg[y+-1:1, x+-2:2] <- "black"
        cm <- add_border(cm, x, y, width = 2, height = 1, space = style$space)
        cm$char[y, x+-1:1] <-  c(rs, "\u2503", ss)
        cm$fg[y, x+-1:1] <- fg
    }
    if (angle == 180) {
        cm$fg[y+-2:2, x+-1:1] <- "black"
        cm <- add_border(cm, x, y, width = 1, height = 2, space = style$space)
        cm$char[y+-1:1, x] <-  c(rs, "\u2501", ss)
        cm$fg[y+-1:1, x] <- fg
    } else if (angle == 270) {
        cm$fg[y+-1:1, x+-2:2] <- "black"
        cm <- add_border(cm, x, y, width = 2, height = 1, space = style$space)
        cm$char[y, x+-1:1] <-  c(ss, "\u2503", rs)
        cm$fg[y, x+-1:1] <- fg
    }
    cm
}
add_tile_face_piecepack <- function(cm, ss, rs, x, y, angle, fg, style) {
    cm$fg[y+-2:2, x+-2:2] <- "black"
    cm <- add_border(cm, x, y, space = style$space)
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
add_board <- function(cm, x, y, width = 8, height = 8, cell = 1, style = get_style("Unicode")) {
    cm$fg[y+-height:height, x+-width:width] <- "black"
    cm <- add_border(cm, x, y, width, height, space = style$space)
    cm <- add_gridlines(cm, x, y, width, height, cell)
    cm
}

add_gridlines <- function(cm, x, y, width = 2, height = 2, cell = 1,
                          has_pua_box_drawing = FALSE) {
    # gridlines
    xgs <- x + seq(2 * cell - width, width - 2 * cell, 2 * cell)
    ygs <- y + seq(2 * cell - height, height - 2 * cell, 2 * cell)
    xo <- x + seq(1 - width, width - 1)
    yo <- y + seq(1 - height, height - 1)

    cm$char[ygs, xo] <- "\u2501" # horizontal lines
    cm$char[yo, xgs] <- "\u2503" # vertical lines
    cm$char[ygs, xgs] <- "\u254b" # crosses

    # intersection gridlines and border line
    hv <- ifelse(has_pua_box_drawing, 3, 2)
    for (xg in xgs) {
        cm <- add_box_edge(cm, xg, y+height, c(NA, 1, hv, 1)) # top
        cm <- add_box_edge(cm, xg, y-height, c(hv, 1, NA, 1)) # bottom
    }
    for (yg in ygs) {
        cm <- add_box_edge(cm, x+width, yg, c(1, NA, 1, hv)) # right
        cm <- add_box_edge(cm, x-width, yg, c(1, hv, 1, NA)) # left
    }
    cm
}

add_border <- function(cm, x, y, width = 2, height = 2, space = " ") {
    for (i in seq(1 - height, height - 1)) {
        for (j in seq(1 - width, width - 1)) {
            cm$char[y + i, x + j] <- space
        }
    }
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
    # [top, right, bottom, left] 0-none 1-light 2-heavy 3-matted-heavy
    bi <- char2bi[[cm$char[y, x]]]
    if (is.null(bi)) bi <- c(0, 0, 0, 0)
    ind <- which(!is.na(box_info))
    for (ii in ind) {
        bi[ii] <- box_info[ii]
    }
    cm$char[y, x] <- box2char[[paste(bi, collapse = "")]]
    cm
}

get_style_rotate <- function(style = "unicode") {
    rl <- list(r45 = r45, r90 = r90, r135 = r135,
               r180 = r180, r225 = r225, r270 = r270, r315 = r315)

    if (style == "gamebitmono") {
        r90[["\u283f"]] <- "\u3000\u20db\u20e8"
        r270[["\u283f"]] <- "\u3000\u20db\u20e8"
    }

    function(char, angle, reorient = "none") {
        if (angle == 0 || reorient == "symbols") {
            rchar <- char
        } else if (angle == 45) {
            rchar <- rl$r45[[char]]
        } else if (angle == 90) {
            rchar <- rl$r90[[char]]
        } else if (angle == 135) {
            rchar <- rl$r135[[char]]
        } else if (angle == 180) {
            rchar <- rl$r180[[char]]
        } else if (angle == 225) {
            rchar <- rl$r225[[char]]
        } else if (angle == 270) {
            rchar <- rl$r270[[char]]
        } else if (angle == 315) {
            rchar <- rl$r315[[char]]
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
}
