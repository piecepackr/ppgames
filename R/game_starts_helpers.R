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
process_angles <- function(angles) {
    angles <- gsub("\\^", "0", angles)
    angles <- gsub("<", "90", angles)
    angles <- gsub("v", "180", angles)
    angles <- gsub(">", "270", angles)
    as.numeric(angles)
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
