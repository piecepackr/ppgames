# 24 tiles arranged 5x5 with hole in middle, center of bottom-left tile at (x0, y0)
df_donut_tiles <- function(seed = NULL, tiles = NULL, x0 = 1.5, y0 = 1.5, face = TRUE) {
    set.seed(seed)
    df_txy <- tibble(piece_side = ifelse(face, "tile_face", "tile_back"),
                     x = x0 + c(rep(seq(0, 8, 2), 2), 0, 2, 6, 8, rep(seq(0, 8, 2), 2)),
                     y = y0 + c(rep(8, 5), rep(6, 5), rep(4, 4), rep(2, 5), rep(0, 5)))
    if (is.null(tiles)) {
        df_tsr <- tibble(suit = rep(1:4, each = 6), rank = rep(1:6, 4))[sample.int(24), ]
    } else {
        df_tsr <- process_tiles(tiles)
    }
    bind_cols(df_txy, df_tsr)
}

process_angles <- function(angles) {
    angles <- gsub("\\^", "0", angles)
    angles <- gsub("<", "90", angles)
    angles <- gsub("v", "180", angles)
    angles <- gsub(">", "270", angles)
    as.numeric(angles)
}

process_ranks <- function(ranks) {
    if (is.character(ranks)) {
        ranks <- gsub("[[:space:]]", "", ranks)
        ranks <- gsub("[[:punct:]]", "", ranks)
        ranks <- gsub("n", "0", ranks)
        ranks <- gsub("a", "1", ranks)
        if (length(ranks) == 1) ranks <- stringr::str_split(ranks, "")[[1]]
    }
    as.integer(ranks) + 1
}

process_suits <- function(suits) {
    if (is.character(suits)) {
        suits <- gsub("[[:space:]]", "", suits)
        suits <- gsub("[[:punct:]]", "", suits)
        suits <- toupper(suits)
        suits <- gsub("S|R", "1", suits)
        suits <- gsub("M|K", "2", suits)
        suits <- gsub("C|G", "3", suits)
        suits <- gsub("A|B", "4", suits)
        if (length(suits) == 1) suits <- stringr::str_split(suits, "")[[1]]
    }
    as.integer(suits)
}

generate_sra <- function(df, filter_ = "^tile", which_ = "sra") {
    df <- dplyr::filter(df, str_detect(.data$piece_side, filter_))
    l <- purrr::pmap(df, generate_helper, which_ = which_)
    paste(unlist(l), collapse = "")
}
generate_helper <- function(suit, rank, angle = 0, which_ = "sra", ...) {
    s <- switch(suit, "S", "M", "C", "A")
    r <- rank - 1
    a <- "^"
    if (near(angle %% 360, 90)) a <- "<"
    if (near(angle %% 360, 180)) a <- "v"
    if (near(angle %% 360, 270)) a <- ">"
    switch(which_,
           sra = paste0(s, r, a),
           sr = paste(s, r),
           r = r)
}

process_tiles <- function(tiles, n_tiles = 24) {
    tiles <- gsub("[[:space:]]", "", tiles)
    tiles <- gsub("[/:;\\\\|]", "", tiles)
    tiles <- stringr::str_split(tiles, "")[[1]]
    if (length(tiles) == 2 * n_tiles) {
        suits <- tiles[which(seq(2 * n_tiles) %% 2 == 1)]
        needs_ranks <- str_detect(tiles[2], "[\\^<v>]")
        if (needs_ranks) {
            angles <- tiles[which(seq(2 * n_tiles) %% 2 == 0)]
            ranks <- integer(n_tiles)
        } else {
            ranks <- tiles[which(seq(2 * n_tiles) %% 2 == 0)]
            angles <- rep("^", n_tiles)
        }
    } else if (length(tiles) == 3 * n_tiles) {
        suits <- tiles[which(seq(3 * n_tiles) %% 3 == 1)]
        ranks <- tiles[which(seq(3 * n_tiles) %% 3 == 2)]
        angles <- tiles[which(seq(3 * n_tiles) %% 3 == 0)]
        needs_ranks <- FALSE
    } else {
        abort(paste("Don't know how to handle tiles string", tiles), class = "board_setup")
    }
    suits <- process_suits(suits)
    if (needs_ranks) {
        n_ranks <- n_tiles %/% 4
        ranks[which(suits==1)] <- sample.int(n_ranks)
        ranks[which(suits==2)] <- sample.int(n_ranks)
        ranks[which(suits==3)] <- sample.int(n_ranks)
        ranks[which(suits==4)] <- sample.int(n_ranks + n_tiles %% 4)
    } else {
        ranks <- process_ranks(ranks)
    }
    angles <- process_angles(angles)
    tibble(suit = suits, rank = ranks, angle = angles)
}

random_dice <- function(n_dice = 4, n_ranks = 6) {
    sample.int(n_ranks, n_dice, replace = TRUE)
}
