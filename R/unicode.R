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
ss_list <- list(piecepack = c("\u2600", "\u263e", "\u265b", "\u2e38"),
                playing_cards_expansion = c("\u2665", "\u2660", "\u2663", "\u2666"),
                dual_piecepacks_expansion = c("\u2661", "\u2664", "\u2667", "\u2662"))
piecepack_ranks <- c("n", "a", "2", "3", "4", "5")
rs_list <- list(piecepack = piecepack_ranks,
                playing_cards_expansion = piecepack_ranks,
                dual_piecepacks_expansion = piecepack_ranks)
piecepack_colors <- c("darkred", "black", "darkgreen", "darkblue")

fg_list <- list(piecepack = piecepack_colors,
                dual_piecepacks_expansion = piecepack_colors,
                playing_cards_expansion = c("darkred", "black", "black", "red"))
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
        rs <- rotate(rs, angle)
    }
    switch(piece_side,
           coin_back = add_coin_back(cm, ss, x, y, angle, fg),
           coin_face = add_coin_face(cm, rs, x, y, angle, fg),
           die_face = add_die_face(cm, rs, x, y, angle, fg),
           pawn_face = add_pawn_face(cm, ss, x, y, angle, fg),
           pawn_back = add_pawn_back(cm, ss, x, y, angle, fg),
           tile_face = add_tile_face(cm, ss, rs, x, y, angle, fg),
           tile_back = add_tile_back(cm, x, y),
           cm)
}

add_coin_back <- function(cm, ss, x, y, angle, fg) {
    cm$char[y, x] <- paste0(ss, "\u20dd")
    cm$fg[y, x] <- fg
    cm
}
add_coin_face <- function(cm, rs, x, y, angle, fg) {
    cm$char[y, x] <- paste0(rs, "\u20dd")
    cm$fg[y, x] <- fg
    cm
}
add_die_face <- function(cm, rs, x, y, angle, fg) {
    cm$char[y, x] <- paste0(rs, "\u20de")
    cm$fg[y, x] <- fg
    cm
}
add_pawn_face <- function(cm, ss, x, y, angle, fg) {
    cm$char[y, x] <- paste0(ss, "\u20df")
    cm$fg[y, x] <- fg
    cm
}
add_pawn_back <- function(cm, ss, x, y, angle, fg) {
    cm$char[y, x] <- paste0(ss, "\u20df")
    cm$fg[y, x] <- fg
    cm
}
add_tile_back <- function(cm, x, y) {
    cm$fg[y+-2:2, x+-2:2] <- "black"
    # gridlines
    cm$char[y, x] <- "\u254b" # "\u256c"
    cm$char[y, x+c(-1, 1)] <- "\u2501" # "\u2550"
    cm$char[y+c(-1, 1), x] <- "\u2503" # "\u2551"
    # intersection gridlines and border line
    cm <- add_box_edge(cm, x, y+2, c(NA,1,2,1)) # t
    cm <- add_box_edge(cm, x, y-2, c(2,1,NA,1)) # b
    cm <- add_box_edge(cm, x+2, y, c(1,NA,1,2)) # r
    cm <- add_box_edge(cm, x-2, y, c(1,2,1,NA)) # l
    # border line
    cm <- add_tile_border(cm, x, y)
    cm
}
add_tile_face <- function(cm, ss, rs, x, y, angle, fg) {
    cm$fg[y+-2:2, x+-2:2] <- "black"
    # rank symbol
    cm$char[y, x] <- rs
    cm$fg[y, x] <- fg
    # interior blanks
    cm$char[y+1, x-1] <- " "
    cm$char[y-1, x-1] <- " "
    cm$char[y-1, x+1] <- " "
    cm$char[y+1, x+1] <- " "
    cm$char[y, x+c(-1,1)] <- " "
    cm$char[y+c(-1,1), x] <- " "
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
    # border line
    cm <- add_box_edge(cm, x, y+2, c(NA,1,0,1)) # t
    cm <- add_box_edge(cm, x, y-2, c(0,1,NA,1)) # b
    cm <- add_box_edge(cm, x+2, y, c(1,NA,1,0)) # r
    cm <- add_box_edge(cm, x-2, y, c(1,0,1,NA)) # l
    cm <- add_tile_border(cm, x, y)
    cm
}

add_tile_border <- function(cm, x, y) {
    # border line
    cm <- add_box_edge(cm, x-1, y+2, c(NA,1,0,1)) # t
    cm <- add_box_edge(cm, x+1, y+2, c(NA,1,0,1)) # t
    cm <- add_box_edge(cm, x-1, y-2, c(0,1,NA,1)) # b
    cm <- add_box_edge(cm, x+1, y-2, c(0,1,NA,1)) # b
    cm <- add_box_edge(cm, x+2, y-1, c(1,NA,1,0)) # r
    cm <- add_box_edge(cm, x+2, y+1, c(1,NA,1,0)) # r
    cm <- add_box_edge(cm, x-2, y-1, c(1,0,1,NA)) # l
    cm <- add_box_edge(cm, x-2, y+1, c(1,0,1,NA)) # l
    cm <- add_box_edge(cm, x-2, y+2, c(NA,1,1,NA)) # ul
    cm <- add_box_edge(cm, x+2, y+2, c(NA,NA,1,1)) # ur
    cm <- add_box_edge(cm, x-2, y-2, c(1,1,NA,NA)) # ll
    cm <- add_box_edge(cm, x+2, y-2, c(1,NA,NA,1)) # lr
    cm
}

add_box_edge <- function(cm, x, y, box_info) {
    # [top, right, bottom, left] 0-none 1-light 2-dark
    bi <- char2bi[[cm$char[y, x]]]
    if (is.null(bi)) bi <- c(0,0,0,0)
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
    } else if (angle == 90) {
        rchar <- r90[[char]]
    } else if (angle == 180) {
        rchar <- r180[[char]]
    } else if (angle == 270) {
        rchar <- r270[[char]]
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
