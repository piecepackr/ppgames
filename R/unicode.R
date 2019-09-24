#' Generate plaintext piecepack diagrams
#' 
#' \code{format_piece} generates plaintext piecepack diagrams and 
#'  outputs them using \code{cat}.
#' 
#' @param df Data frame containing piece info.
#' @param ... Passed to \code{cat}
#' @return None (invisible \code{NULL})
#' @export
cat_piece <- function(df, ...) {
    nn <- names(df)
    if (!("rank" %in% nn)) df$rank <- NA
    if (!("suit" %in% nn)) df$suit <- NA
    if (!("angle" %in% nn)) df$angle <- NA
    df$angle <- ifelse(is.na(df$angle), 0, df$angle %% 360)
    nc <- 2*xrange(df)[2]+1
    nr <- 2*yrange(df)[2]+1
    cm <- matrix(" ", nrow=nr, ncol=nc)

    for(rr in seq(nrow(df))) {
        ps <- as.character(df[rr, "piece_side"])
        suit <- as.numeric(df[rr, "suit"])
        rank <- as.numeric(df[rr, "rank"])
        x <- 2*as.numeric(df[rr, "x"])+1
        y <- 2*as.numeric(df[rr, "y"])+1
        angle <- as.numeric(df[rr, "angle"])
        cm <- add_piece(cm, ps, suit, rank, x, y, angle)
    }

    text <- rev(apply(cm, 1, function(x) paste(x, collapse="")))
    cat(text, ..., sep="\n")
    invisible(NULL)
}

#' @rdname cat_piece
#' @param game A list containing a parsed ppn game (as parsed by \code{read_ppn})
#' @param move Which move to cat game state (after the move, will use \code{game$dfs[[move]]})  
#'             unless \code{NULL} in which case will cat the game state after the last move.
#' @export
cat_move <- function(game, move=NULL, ...) {
    df <- get_df_from_move(game, move)
    cat_piece(df, ...)
}

add_piece <- function(cm, piece_side, suit, rank, x, y, angle) {
    switch(piece_side, 
           coin_back = add_coin_back(cm, suit, x, y, angle),
           coin_face = add_coin_face(cm, rank, x, y, angle),
           die_face = add_die_face(cm, suit, rank, x, y, angle),
           pawn_face = add_pawn_face(cm, suit, x, y, angle),
           pawn_back = add_pawn_back(cm, suit, x, y, angle),
           tile_face = add_tile_face(cm, suit, rank, x, y, angle),
           tile_back = add_tile_back(cm, x, y),
           cm)
}

# sss <- c("\u2665","\u2660","\u2663","\u2666","\u263c")
# sss <- c("\u2600","\u263e","\u2641","\u2e38"," ")
sss <- c("\u2600","\u263e","\u265b","\u2e38"," ")
rss <- c("n", "a", "2", "3", "4", "5")


add_coin_back <- function(cm, suit, x, y, angle) {
    ss <- switch(suit, sss[1], sss[2], sss[3], sss[4], sss[5])
    ss <- rotate(ss, angle)
    cm[y,x] <- paste0(ss, "\u20dd")
    cm
}
add_coin_face <- function(cm, rank, x, y, angle) {
    rs <- switch(rank, rss[1], rss[2], rss[3], rss[4], rss[5], rss[6])
    rs <- rotate(rs, angle)
    cm[y,x] <- paste0(rs, "\u20dd")
    cm
}
add_die_face <- function(cm, suit, rank, x, y, angle) {
    rs <- switch(rank, rss[1], rss[2], rss[3], rss[4], rss[5], rss[6])
    rs <- rotate(rs, angle)
    cm[y,x] <- paste0(rs, "\u20de")
    cm
}
add_pawn_face <- function(cm, suit, x, y, angle) {
    ss <- switch(suit, sss[1], sss[2], sss[3], sss[4], sss[5])
    ss <- rotate(ss, angle)
    cm[y,x] <- paste0(ss, "\u20df")
    cm
}
add_pawn_back <- function(cm, suit, x, y, angle) {
    ss <- switch(suit, sss[1], sss[2], sss[3], sss[4], sss[5])
    ss <- rotate(ss, angle)
    cm[y,x] <- paste0(ss, "\u20df")
    cm
}
add_tile_back <- function(cm, x, y) {
    # gridlines
    cm[y,x] <- "\u254b" # "\u256c"
    cm[y,x+c(-1,1)] <- "\u2501" # "\u2550"
    cm[y+c(-1,1),x] <- "\u2503" # "\u2551"
    # intersection gridlines and border line
    cm <- add_box_edge(cm, x, y+2, c(NA,1,2,1)) # t
    cm <- add_box_edge(cm, x, y-2, c(2,1,NA,1)) # b
    cm <- add_box_edge(cm, x+2, y, c(1,NA,1,2)) # r
    cm <- add_box_edge(cm, x-2, y, c(1,2,1,NA)) # l
    # cm[y,x-2] <- "\u251d" # "\u255e" # l
    # cm[y,x+2] <- "\u2525" # "\u2561" # r
    # cm[y+2,x] <- "\u2530" # "\u2565" # t
    # cm[y-2,x] <- "\u2538" # "\u2568" # b
    # border line
    cm <- add_tile_border(cm, x, y)
    cm
}
add_tile_face <- function(cm, suit, rank, x, y, angle) {
    # rank symbol
    rs <- switch(rank, rss[1], rss[2], rss[3], rss[4], rss[5], rss[6])
    rs <- rotate(rs, angle)
    cm[y,x] <- rs
    # interior blanks
    cm[y+1,x-1] <- " "
    cm[y-1,x-1] <- " "
    cm[y-1,x+1] <- " "
    cm[y+1,x+1] <- " "
    cm[y,x+c(-1,1)] <- " "
    cm[y+c(-1,1),x] <- " "
    # suit symbol
    ss <- switch(suit, sss[1], sss[2], sss[3], sss[4], sss[5])
    ss <- rotate(ss, angle)
    if (angle == 0) {
        cm[y+1,x-1] <- ss
    } else if (angle == 90) {
        cm[y-1,x-1] <- ss
    } else if (angle == 180) {
        cm[y-1,x+1] <- ss
    } else if (angle == 270) {
        cm[y+1,x+1] <- ss
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
    bi <- char2bi[[cm[y,x]]]
    if(is.null(bi)) { bi <- c(0,0,0,0) }
    ind <- which(!is.na(box_info))
    for(ii in ind) {
        bi[ii] <- box_info[ii]
    }
    cm[y,x] <- box2char[[paste(bi, collapse="")]]
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

xrange <- function(df) {
    if(nrow(df) == 0) { return (c(NA_real_, NA_real_)) }
    xleft <- ifelse(grepl("tile", df$piece_side), df$x-1, df$x-0.5)
    xright <- ifelse(grepl("tile", df$piece_side), df$x+1, df$x+0.5)
    c(min(xleft), max(xright))
}
yrange <- function(df) {
    if(nrow(df) == 0) { return (c(NA_real_, NA_real_)) }
    yleft <- ifelse(grepl("tile", df$piece_side), df$y-1, df$y-0.5)
    yright <- ifelse(grepl("tile", df$piece_side), df$y+1, df$y+0.5)
    c(min(yleft), max(yright))
}
