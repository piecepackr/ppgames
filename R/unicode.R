#' Generate plaintext piecepack diagrams
#'
#' \code{format_piece} generates plaintext piecepack diagrams and
#'  outputs them using \code{cat}.
#'
#' @param df Data frame containing piece info.
#' @param color How should the text be colorized.  If \code{TRUE} or \code{"crayon"}
#'       will colorize output for the terminal.  If \code{FALSE} won't colorize output.
#' @param ... Passed to \code{cat}
#' @return None (invisible \code{NULL})
#' @export
cat_piece <- function(df, color = NULL, ...) {
    if (nrow(df) == 0) {
        cat(...)
        return(invisible(NULL))
    }
    if (is.null(color)) color <- TRUE
    file <- list(...)$file
    if (!is.null(file)) {
        if (file != "") color <- FALSE
    }
    nn <- names(df)
    if (!("rank" %in% nn)) df$rank <- NA
    if (!("suit" %in% nn)) df$suit <- NA
    if (!("angle" %in% nn)) df$angle <- NA
    df$angle <- ifelse(is.na(df$angle), 0, df$angle %% 360)
    if (!("cfg" %in% nn)) df$cfg <- "piecepack"
    df$cfg <- ifelse(is.na(df$cfg), "piecepack", df$cfg)

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
        cm <- add_piece(cm, ps, suit, rank, x, y, angle, cfg)
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
    cat(text, ..., sep = "\n")
    invisible(NULL)
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
# nolint start
# Use Half-circle for Moons? \u25d0
# Use Arrows for Arms?
# nolint end
piecepack_suits <- c("\u2600", "\u263e", "\u265b", "\u2e38")
ss_list <- list(piecepack = piecepack_suits,
                playing_cards_expansion = c("\u2665", "\u2660", "\u2663", "\u2666"),
                dual_piecepacks_expansion = c("\u2661", "\u2664", "\u2667", "\u2662"),
                subpack = piecepack_suits,
                checkers1 = c(rep("\u26c2", 5), "\u26c0"),
                checkers2 = c(rep("\u26c2", 5), "\u26c0"),
                dice = rep(" ", 6),
                icehouse_pieces = rep(" ", 6))
piecepack_ranks <- c("n", "a", "2", "3", "4", "5")
rs_list <- list(piecepack = piecepack_ranks,
                playing_cards_expansion = piecepack_ranks,
                dual_piecepacks_expansion = piecepack_ranks,
                subpack = piecepack_ranks,
                checkers1 = rep("\u26c2", 6),
                checkers2 = rep("\u26c2", 6),
                dice = c("\u00b7", "\u280c", "\u22f0", "\u2237", "\u2059", "\u283f"),
                icehouse_pieces = c("\u2191", "\u21d1", "\u2b06")) # "\u21e7"))
# darkgreen sometimes shows up as black?
piecepack_colors <- c("darkred", "black", "green", "darkblue")
checkers_colors <- c("darkred", "black", "green", "darkblue", "darkorange3", "black")
dice_colors <- c("darkred", "grey40", "green", "darkblue", "darkorange3", "black")

fg_list <- list(piecepack = piecepack_colors,
                dual_piecepacks_expansion = piecepack_colors,
                playing_cards_expansion = c("darkred", "black", "black", "red"),
                subpack = piecepack_colors,
                checkers1 = checkers_colors, checkers2 = checkers_colors,
                dice = dice_colors, icehouse_pieces = dice_colors)
add_piece <- function(cm, piece_side, suit, rank, x, y, angle, cfg) {
    if (piecepackr:::has_suit(piece_side)) {
        if (is.na(suit)) suit <- 1
        ss <- ss_list[[cfg]][suit]
        ss <- rotate(ss, angle)
        fg <- fg_list[[cfg]][suit]
    } else {
        fg <- "black"
    }
    if (piecepackr:::has_rank(piece_side)) {
        if (is.na(rank)) rank <- 1
        rs <- rs_list[[cfg]][rank]
        if (piece_side == "pyramid_top") rs <- top_subs[[rs]]
        rs <- rotate(rs, angle)
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
           tile_back = add_tile_back(cm, x, y, cfg),
           bit_back = add_bit_back(cm, ss, x, y, fg),
           bit_face = add_bit_face(cm, rs, x, y, fg),
           board_back = add_board(cm, x, y, cell * rank, cell * rank, cell),
           board_face = add_board(cm, x, y, cell * rank, cell * rank, cell),
           pyramid_top = add_pyramid_top(cm, rs, x, y, angle, fg),
           pyramid_face = add_pyramid_face(cm, rs, x, y, angle, fg),
           pyramid_left = add_pyramid_face(cm, rs, x, y, angle, fg),
           pyramid_right = add_pyramid_face(cm, rs, x, y, angle, fg),
           pyramid_back = add_pyramid_face(cm, rs, x, y, angle, fg),
           cm)
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
add_pyramid_face <- function(cm, ss, x, y, angle, fg) {
    if (angle %% 90 == 0) {
        cm$char[y, x] <- paste0(ss, "\u20de")
    } else {
        cm$char[y, x] <- paste0(ss, "\u20df")
    }
    cm$fg[y, x] <- fg
    cm
}
add_pyramid_top <- function(cm, ss, x, y, angle, fg) {
    if (angle %% 90 == 0) {
        cm$char[y, x] <- paste0(ss, "\u20de")
    } else {
        cm$char[y, x] <- paste0(ss, "\u20df")
    }
    cm$fg[y, x] <- fg
    cm
}
add_tile_back <- function(cm, x, y, cfg) {
    if (cfg == "subpack") {
        add_tile_back_subpack(cm, x, y)
    } else {
        add_tile_back_piecepack(cm, x, y)
    }
}
add_tile_back_piecepack <- function(cm, x, y) {
    cm$fg[y+-2:2, x+-2:2] <- "black"
    cm$char[y+-1:1, x+-1:1] <- " "
    # border line
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
    if (cfg == "subpack") {
        add_tile_face_subpack(cm, rs, x, y, angle, fg)
    } else {
        add_tile_face_piecepack(cm, ss, rs, x, y, angle, fg)
    }
}
add_tile_face_subpack <- function(cm, rs, x, y, angle, fg) {
    cm$fg[y+-1:1, x+-1:1] <- "black"
    cm$char[y, x] <- rs
    cm$fg[y, x] <- fg
    cm <- add_border(cm, x, y, 1, 1)
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
    } else {
        stop(paste("Don't know how to handle angle", angle))
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

rotate <- function(char, angle) {
    if (angle == 0) {
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
