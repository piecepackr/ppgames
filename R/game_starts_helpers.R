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

process_tiles <- function(tiles, n_tiles = 24) {
    tiles <- gsub("[[:space:]]", "", tiles)
    tiles <- gsub("[/:;\\\\|]", "", tiles)
    tiles <- stringr::str_split(tiles, "")[[1]]
    if (length(tiles) == 2 * n_tiles) {
        suits <- tiles[which(seq(2 * n_tiles) %% 2 == 1)]
        angles <- tiles[which(seq(2 * n_tiles) %% 2 == 0)]
        ranks <- integer(n_tiles)
    } else if (length(tiles) == 3 * n_tiles) {
        suits <- tiles[which(seq(3 * n_tiles) %% 3 == 1)]
        ranks <- tiles[which(seq(3 * n_tiles) %% 3 == 2)]
        angles <- tiles[which(seq(3 * n_tiles) %% 3 == 0)]
    } else {
        stop(paste("Don't know how to handle tiles string", tiles))
    }
    suits <- process_suits(suits)
    if (length(tiles) == 2 * n_tiles) {
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
