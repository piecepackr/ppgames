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
    bi <- get_box_info(cm[y,x])
    ind <- which(!is.na(box_info))
    for(ii in ind) {
        bi[ii] <- box_info[ii]
    }
    cm[y,x] <- get_char(paste(bi, collapse=""))
    cm
}

# [top, right, bottom, left] 0-none 1-light 2-dark 
get_box_info <- function(char) {
    switch(char,
           "\u2500" = c(0,1,0,1),
           "\u2501" = c(0,2,0,2),
           "\u2502" = c(1,0,1,0),
           "\u2503" = c(2,0,2,0),
           "\u250c" = c(0,1,1,0),
           "\u250d" = c(0,2,1,0),
           "\u250e" = c(0,1,2,0),
           "\u250f" = c(0,2,2,0),
           "\u2510" = c(0,0,1,1),
           "\u2511" = c(0,0,1,2),
           "\u2512" = c(0,0,2,1),
           "\u2513" = c(0,0,2,2),
           "\u2514" = c(1,1,0,0),
           "\u2515" = c(1,2,0,0),
           "\u2516" = c(2,1,0,0),
           "\u2517" = c(2,2,0,0),
           "\u2518" = c(1,0,0,1),
           "\u2519" = c(1,0,0,2),
           "\u251a" = c(2,0,0,1),
           "\u251b" = c(2,0,0,2),
           "\u251c" = c(1,1,1,0),
           "\u251d" = c(1,2,1,0),
           "\u251e" = c(2,1,1,0),
           "\u251f" = c(1,1,2,0),
           "\u2520" = c(2,1,2,0),
           "\u2521" = c(2,2,1,0),
           "\u2522" = c(1,2,2,0),
           "\u2523" = c(2,2,2,0),
           "\u2524" = c(1,0,1,1),
           "\u2525" = c(1,0,1,2),
           "\u2526" = c(2,0,1,1),
           "\u2527" = c(1,0,2,1),
           "\u2528" = c(2,0,2,1),
           "\u2529" = c(2,0,1,2),
           "\u252a" = c(1,0,2,2),
           "\u252b" = c(2,0,2,2),
           "\u252c" = c(0,1,1,1),
           "\u252d" = c(0,1,1,2),
           "\u252e" = c(0,2,1,1),
           "\u252f" = c(0,2,1,2),
           "\u2530" = c(0,1,2,1),
           "\u2531" = c(0,1,2,2),
           "\u2532" = c(0,2,2,1),
           "\u2533" = c(0,2,2,2), 
           "\u2534" = c(1,1,0,1), 
           "\u2535" = c(1,1,0,2), 
           "\u2536" = c(1,2,0,1), 
           "\u2537" = c(1,2,0,2), 
           "\u2538" = c(2,1,0,1), 
           "\u2539" = c(2,1,0,2), 
           "\u253a" = c(2,2,0,1), 
           "\u253b" = c(2,2,0,2), 
           "\u253c" = c(1,1,1,1),
           "\u253d" = c(1,1,1,2),
           "\u253e" = c(1,2,1,1),
           "\u253f" = c(1,2,1,2),
           "\u2540" = c(2,1,1,1),
           "\u2541" = c(1,1,2,1),
           "\u2542" = c(2,1,2,1),
           "\u2543" = c(2,1,1,2),
           "\u2544" = c(2,2,1,1),
           "\u2545" = c(1,1,2,2),
           "\u2546" = c(1,2,2,1),
           "\u2547" = c(2,2,1,2),
           "\u2548" = c(1,2,2,2),
           "\u2549" = c(2,1,2,2),
           "\u254a" = c(2,2,2,1),
           "\u254b" = c(2,2,2,2),
           c(0,0,0,0))
}

get_char <- function(box_info) {
    switch(box_info,
           "0101" = "\u2500",
           "0202" = "\u2501",
           "1010" = "\u2502",
           "2020" = "\u2503",
           "0110" = "\u250c",
           "0210" = "\u250d",
           "0120" = "\u250e",
           "0220" = "\u250f",
           "0011" = "\u2510",
           "0012" = "\u2511",
           "0021" = "\u2512",
           "0022" = "\u2513",
           "1100" = "\u2514",
           "1200" = "\u2515",
           "2100" = "\u2516",
           "2200" = "\u2517",
           "1001" = "\u2518",
           "1002" = "\u2519",
           "2001" = "\u251a",
           "2002" = "\u251b",
           "1110" = "\u251c",
           "1210" = "\u251d",
           "2110" = "\u251e",
           "1120" = "\u251f",
           "2120" = "\u2520",
           "2210" = "\u2521",
           "1220" = "\u2522",
           "2220" = "\u2523",
           "1011" = "\u2524",
           "1012" = "\u2525",
           "2011" = "\u2526",
           "1021" = "\u2527",
           "2021" = "\u2528",
           "2012" = "\u2529",
           "1022" = "\u252a",
           "2022" = "\u252b",
           "0111" = "\u252c",
           "0112" = "\u252d",
           "0211" = "\u252e",
           "0212" = "\u252f",
           "0121" = "\u2530",
           "0122" = "\u2531",
           "0221" = "\u2532",
           "0222" = "\u2533", 
           "1101" = "\u2534", 
           "1102" = "\u2535", 
           "1201" = "\u2536", 
           "1202" = "\u2537", 
           "2101" = "\u2538", 
           "2102" = "\u2539", 
           "2201" = "\u253a", 
           "2202" = "\u253b", 
           "1111" = "\u253c",
           "1112" = "\u253d",
           "1211" = "\u253e",
           "1212" = "\u253f",
           "2111" = "\u2540",
           "1121" = "\u2541",
           "2121" = "\u2542",
           "2112" = "\u2543",
           "2211" = "\u2544",
           "1122" = "\u2545",
           "1221" = "\u2546",
           "2212" = "\u2547",
           "1222" = "\u2548",
           "2122" = "\u2549",
           "2221" = "\u254a",
           "2222" = "\u254b",
           NA)
}

rotate <- function(char, angle) {
    if (angle == 0) {
        char
    } else if (angle == 90) {
        rotate90(char)
    } else if (angle == 180) {
        rotate180(char)
    } else if (angle == 270) {
        rotate270(char)
    } else {
        warning_rotation(char, angle)
    }
}

rotate90 <- function(char) {
    switch(char,
           " " = " ",
           "\u2600" = "\u2600", # Suns
           "\u2609" = "\u2609",
           "\u263c" = "\u263c",
           "\u2665" = "\u2765", # Rotated Black Heart
           "\u2764" = "\u2765",
           "\u2654" = "\U1fa33", ## Chess (K)
           "\u2655" = "\U1fa34", # Q
           "\u2656" = "\U1fa35", # R
           "\u2657" = "\U1fa36", # B
           "\u2658" = "\U1fa37", # N
           "\u2659" = "\U1fa38", # P
           "\u265a" = "\U1fa39", # k
           "\u265b" = "\U1fa3a", # q
           "\u265c" = "\U1fa3b", # r
           "\u265d" = "\U1fa3c", # b
           "\u265e" = "\U1fa3d", # n
           "\u265f" = "\U1fa3e", # p
           warning_rotation(char, 90))
}
rotate180 <- function(char) {
    # https://en.wikipedia.org/wiki/Transformation_of_text#Reversed_text
    switch(char,
           " " = " ",
           "a" = "\u0250", # Letters/Numbers 
           "A" = "\u2200",
           "n" = "u",
           "N" = "N",
           "0" = "0",
           "1" = "\u21c2",
           "2" = "\u218a",
           "3" = "\u218b",
           "4" = "\u152d",
           # "4" = "\u3123",
           # "4" = "\u07c8",
           # "5" = "\u03da",
           "5" = "\u2185\u0332",
           "6" = "9",
           "\u2600" = "\u2600", ## Suns
           "\u2609" = "\u2609",
           "\u263c" = "\u263c",
           "\u263e" = "\u263d", 
           "\u263d" = "\u263e", ## Moons
           "\u2641" = "\u2640", ## Crowns (Earth)
           "\u2640" = "\u2641", ## Crowns (Venus)
           "\u2020" = "\u2e38", ## Swords (Dagger)
           "\u2e38" = "\u2020", ## Turned Dagger
           "\u2021" = "\u2021", ## Double Dagger
           "\u2641" = "\u2640", # Venus/Earth
           "\u2666" = "\u2666", # Diamond Suits (Black)
           "\u2662" = "\u2662", # (White)
           "\u260a" = "\u260b", # Ascending/Descending nodes
           "\u260b" = "\u260a", # Ascending/Descending nodes
           "\u2654" = "\U1fa1e", ## Chess (K)
           "\u2655" = "\U1fa1f", # Q
           "\u2656" = "\U1fa20", # R
           "\u2657" = "\U1fa21", # B
           "\u2658" = "\U1fa22", # N
           "\u2659" = "\U1fa23", # P
           "\u265a" = "\U1fa24", # k
           "\u265b" = "\U1fa25", # q
           "\u265c" = "\U1fa26", # r
           "\u265d" = "\U1fa27", # b
           "\u265e" = "\U1fa28", # n
           "\u265f" = "\U1fa29", # p
           warning_rotation(char, 180))
}
rotate270 <- function(char) {
    switch(char,
           " " = " ",
           "\u2600" = "\u2600", # Suns
           "\u2609" = "\u2609",
           "\u263c" = "\u263c",
           "\u2654" = "\U1fa09", ## Chess (K)
           "\u2655" = "\U1fa0a", # Q
           "\u2656" = "\U1fa0b", # R
           "\u2657" = "\U1fa0c", # B
           "\u2658" = "\U1fa0d", # N
           "\u2659" = "\U1fa0e", # P
           "\u265a" = "\U1fa0f", # k
           "\u265b" = "\U1fa10", # q
           "\u265c" = "\U1fa11", # r
           "\u265d" = "\U1fa12", # b
           "\u265e" = "\U1fa13", # n
           "\u265f" = "\U1fa14", # p
           warning_rotation(char, 270))
}

warning_rotation <- function(char, angle) {
    warning(paste("Can't rotate", char, angle, "degrees"))
    char
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
