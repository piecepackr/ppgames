#' Read ppn files
#'
#' Read in ppn files
#' @param file Filename
#' @param parse Logical of whether to parse the moves in the ppn file
#' @return A list, for each game in the file a list containing info about the game
#' @import stringr
#' @export
read_ppn <- function(file, parse = TRUE) {
    list_ppns <- parse_ppn_file(file)
    lapply(list_ppns, parse_ppn_game, parse = parse)
}

# Parse ppn files
#
# Parses ppn file
# @param file Filename
# @return A list, each element is a character vector containing the text of the PPN games within that file
parse_ppn_file <- function(file) {
    text <- readLines(file)
    game_starts <- grep("^-{3}", text)
    if (length(game_starts) == 0 || game_starts[1] != 1) {
        game_starts <- c(1, game_starts)
    }
    game_ends <- c(game_starts[-1]-1, length(text))
    contents <- list()
    for (ii in seq(game_starts)) {
        contents[[ii]] <- text[game_starts[ii]:game_ends[ii]]
    }
    contents
}

# Parse ppn game
#
# Parses (single) ppn game text to get Metadata and Movetext
# @param text Character vector of ppn game text
# @return A list with a named list element named \code{Metadata}
#         and character vector element named \code{Movetext}
parse_ppn_game <- function(text, parse = TRUE) {
    yaml_end <- grep("^\\.{3}", text)
    if (length(yaml_end) == 0) {
        yaml_end <- grep("^[[:blank:]]+|^$", text)
    }
    if (length(yaml_end) > 0) {
        metadata <- yaml::yaml.load(text[1:yaml_end[1]])
        if (yaml_end[1]<length(text)) {
            movetext <- text[(yaml_end[1]+1):length(text)]
        } else {
            movetext <- character()
        }
    } else {
        metadata <- list()
        movetext <- text
    }
    if (parse) {
        parse_movetext(movetext, metadata)
    } else {
        list(metadata = metadata, movetext = movetext)
    }
}

parse_movetext <- function(movetext, metadata) {
    parser <- metadata$MovetextParser
    if (is.null(parser)) {
        parser_default(movetext, metadata)
    } else {
        if (is.character(parser)) {
            parser_name <- parser
            .l <- list()
        } else if (is.list(parser)) {
            names(parser) <- to_varname(names(parser))
            i_name <- which("name" %in% names(parser))
            parser_name <- parser[["name"]]
            .l <- parser[-i_name]
        }
        fn <- ppn_get(paste0("parser_", to_varname(parser_name)))
        .l$movetext <- movetext
        .l$metadata <- metadata
        do.call(fn, .l)
    }
}

parser_default <- function(movetext = character(), metadata = list(), envir = NULL, scale_factor = NULL) {
    game_list <- list(metadata = metadata, movetext = movetext)
    df <- get_starting_df(metadata)
    if (!is.null(scale_factor)) attr(df, "scale_factor") <- scale_factor
    state <- create_state(df)
    move_list <- parse_moves(movetext, df = df, state = state)
    game_list <- c(game_list, move_list)
    game_list
}

df_none <- function() {
    tibble::tibble(piece_side = character(0), suit = numeric(0), rank = numeric(0),
                   cfg = character(0), x = numeric(0), y = numeric(0), angle = numeric(0))
}

get_starting_df <- function(metadata) {
    setup <- metadata$SetUp
    if (!is.null(setup)) {
        return(get_starting_df_from_field(setup))
    }
    game_type <- metadata$GameType
    if (!is.null(game_type)) {
        return(get_starting_df_from_field(game_type))
    }
    return(tibble::rowid_to_column(df_none(), "id"))
}

get_starting_df_from_field <- function(field) {
    if (is.character(field)) {
        df <- get_starting_df_from_name(field)
    } else if (is.list(field)) {
        names(field) <- to_varname(names(field))
        i_name <- match("name", names(field))
        i_system <- match("system", names(field), nomatch = 0)
        .l <- field[-c(i_name, i_system)]
        df <- get_starting_df_from_name(field[["name"]], .l, field[["system"]])
    }
    return(df)
}

to_varname <- function(string) {
    string <- gsub('"|\'|-', "", string) # e.g. The "In" Crowd -> the_in_crowd
    snakecase::to_snake_case(string)
}

get_ppn_package <- function(system) {
    if (is.null(system)) return(NULL)
    switch(to_varname(system),
           checkers = "tradgames",
           chess = "tradgames",
           icehouse = "piecenikr",
           icehouse_pieces = "piecenikr",
           looney_pyramids = "piecenikr",
           piecepack = "ppgames",
           stackpack = "ppgames",
           traditional = "tradgames",
           stop("Don't recognize system ", system))
}

get_starting_df_from_name <- function(game_name, .l = list(), system = NULL) {
    if (!is.null(system) && to_varname(system) == "stackpack")
        .l$has_subpack <- TRUE
    package <- get_ppn_package(system)
    fn_name <- paste0("df_", to_varname(game_name))
    fn <- ppn_get(fn_name, package)
    df <- do.call(fn, .l)
    df <- tibble::rowid_to_column(df, "id")
    if (is.null(df[["cfg"]])) df$cfg <- "piecepack"
    df
}

ppn_get <- function(name, package = NULL) {
    if (is.null(package)) {
        tryCatch(dynGet(name), error = function(e) get(name))
    } else {
        get(name, envir=getNamespace(package))
    }
}

# Parse Movetext by Move number
#
# Parse Movetext by Move number
# @param text Character vector of Movetext
# @param df Data frame containing starting state (inferred from Metadata)
# @return A list with element \code{moves} containing
#     named list (by move number) of move text and element \code{comments}
#     containing named list (by move number) of comments
parse_moves <- function(text, df = NULL, state = create_state(df)) {
    if (is.null(df)) df <- tibble::rowid_to_column(df_none(), "id")
    #### Convert # comments into braces?
    if (length(text)>0) {
        text <- str_squish(paste(text, collapse = " "))
        # (?![^\\{]*\\}) is a negative lookahead assertion to not capture moves in comment braces
        # (?![[:digit:]]) is a negative lookahead assertion to not capture dots followed by non-space
        move_number_token <- "(?<![[:alnum:][:punct:]])[[:alnum:]_]+\\.+(?![[:alnum:][:punct:]])(?![^\\{]*\\})"
        locations <- str_locate_all(text, move_number_token)[[1]]
        nr <- nrow(locations)
        moves_raw <- list()
        if (nr == 0) {
            moves_raw[[1]] <- text
        } else {
            i1 <- locations[1, 1]
            if (i1 > 1) {
                moves_raw[[1]] <- str_sub(text, 1L, i1 - 2L)
            }
            for (ii in seq(nr)) {
                is <- locations[ii, 1]
                ie <- locations[ii, 2]
                movenumber <- str_sub(text, is, ie)
                is <- locations[ii, 2] + 2L
                if (ii < nr)
                    ie <- locations[ii+1, 1] - 2L
                else
                    ie <- str_count(text)
                moves_raw[[movenumber]] <- str_sub(text, is, ie)
            }
        }
        moves <- lapply(moves_raw, remove_comments)
        comments <- lapply(moves_raw, extract_comments)
        dfs <- process_moves(df, moves, state = state)
    } else {
        moves <- NULL
        comments <- NULL
        dfs <- list(SetupFn.=df)
    }
    moves <- c(list(SetupFn.=""), moves)
    comments <- c(list(SetupFn.=""), comments)
    list(moves = moves, comments = comments, dfs = dfs)
}

comment_token <- "(?<![[:alnum:][:punct:]])\\{[^}]*\\}(?![[:alnum:][:punct:]])"
extract_comments <- function(text) {
    text <- paste(str_extract_all(text, comment_token)[[1]], collapse = " ")
    str_squish(str_remove_all(text, "\\{|\\}"))
}
remove_comments <- function(text) {
    str_squish(str_remove_all(text, comment_token))
}

parse_piece <- function(text) {
    df <- parse_simplified_piece(text)
    if (is.na(df$angle))
        df$angle <- 0
    if (is.na(df$piece)) {
        df$piece <- if (df$side %in% c("left", "right", "top")) {
            "pyramid"
        } else if (!is.na(df$suit) && !is.na(df$rank)) {
            "tile"
        } else if (!is.na(df$suit) || !is.na(df$rank)) {
            "coin"
        } else {
            "tile"
        }
    }
    if (is.na(df$cfg)) {
        if (df$piece == "pyramid" && str_detect(text, "[RKGBYW]")) {
            df$cfg <- "icehouse_pieces"
        } else {
            df$cfg <- "piecepack"
        }
    }
    if (is.na(df$side)) {
        df$side <- switch(df$piece,
               tile = ifelse(is.na(df$suit) || is.na(df$rank), "back", "face"),
               coin = ifelse(is.na(df$suit), "face", "back"),
               saucer = ifelse(is.na(df$suit), "face", "back"),
               pyramid = "top",
               "face")
    }
    if (df$cfg == "icehouse_pieces") {
        df$rank <- df$rank - 1
    }
    df$piece_side <- paste0(df$piece, "_", df$side)
    tibble::as_tibble(df[c("piece_side", "suit", "rank", "angle", "cfg")])
}

parse_simplified_piece <- function(text) {
    suit <- get_simplified_suit(text)
    rank <- get_simplified_rank(text)
    angle <- get_simplified_angle(text)
    piece <- get_simplified_piece(text)
    side <- get_simplified_side(text)
    cfg <- get_simplified_cfg(text)
    list(piece = piece, side = side, suit = suit, rank = rank, angle = angle, cfg = cfg)
}
get_simplified_cfg <- function(text) {
    if (str_detect(text, "\u25b3")) {
        "icehouse_pieces"
    } else if (str_detect(text, "\u2665|\u2660|\u2663|\u2666")) {
        "playing_cards_expansion"
    } else if (str_detect(text, "\u2661|\u2664|\u2667|\u2662")) {
        "dual_piecepacks_expansion"
    } else if (str_detect(text, "\u00b5|\u03bc|u")) {
        "subpack"
    } else if (str_detect(text, "\u2b22|\u2b21|\u2b23")) {
        "hexpack"
    } else {
        NA_character_
    }
}
get_simplified_suit <- function(text) {
    if (str_detect(text, "S|\u2665|R|\u2661")) {
        1
    } else if (str_detect(text, "M|\u2660|K|\u2664")) {
        2
    } else if (str_detect(text, "C|\u2663|G|\u2667")) {
        3
    } else if (str_detect(text, "A|\u2666|B|\u2662")) {
        4
    } else if (str_detect(text, "Y")) {
        5
    } else if (str_detect(text, "W")) {
        6
    } else {
        NA_integer_
    }
}
get_simplified_rank <- function(text) {
    if (str_detect(text, "n")) {
        1
    } else if (str_detect(text, "a")) {
        2
    } else if (str_detect(text, "[[:digit:]]")) {
        as.numeric(str_extract(text, "[[:digit:]]")) + 1
    } else {
        NA_integer_
    }
}
get_simplified_angle <- function(text) {
    if (str_detect(text, "\\^")) {
        0
    } else if (str_detect(text, "<")) {
        90
    } else if (str_detect(text, "v")) {
        180
    } else if (str_detect(text, ">")) {
        270
    } else {
        NA_integer_
    }
}
get_simplified_piece <- function(text) {
    if (str_detect(text, "t")) {
        "tile"
    } else if (str_detect(text, "c")) {
        "coin"
    } else if (str_detect(text, "d")) {
        "die"
    } else if (str_detect(text, "p")) {
        "pawn"
    } else if (str_detect(text, "m")) {
        "matchstick"
    } else if (str_detect(text, "s")) {
        "saucer"
    } else if (str_detect(text, "\u25b2|\u25b3")) { ####
        "pyramid"
    } else {
        NA_character_
    }
}
get_simplified_side <- function(text, suit, rank) {
    side <- if (str_detect(text, "f")) {
        "face"
    } else if (str_detect(text, "b")) {
        "back"
    } else if (str_detect(text, "l")) {
        "left"
    } else if (str_detect(text, "r")) {
        "right"
    } else if (str_detect(text, "x")) {
        "top"
    } else {
        NA_character_
    }
}

get_algebraic_x <- function(text) {
    ss <- str_extract(text, "[[:lower:]]+")
    ndigits <- str_count(ss)
    int <- 0
    for (ii in rev(seq(ndigits))) {
        int <- int + 26 ^ (ii - 1) * match(str_sub(ss, ii, ii), letters)
    }
    int
}
get_algebraic_y <- function(text) {
    as.numeric(str_extract(text, "[[:digit:]]+"))
}

get_id_from_piece_id <- function(piece_id, df, state = create_state(df)) {
    # nocov piece_id <- gsub("'", "", piece_id)
    if (str_detect(piece_id, "^\\^")) {
        piece_id <- str_sub(piece_id, 2)
        get_id_from_piece_id(piece_id, state$df_move_start, state)
    } else {
        if (str_detect(piece_id, "^[[:digit:]]+$")) {
            as.numeric(piece_id)
        } else if (str_detect(piece_id, "^[[:digit:]]+")) {
            n_pieces <- as.integer(gsub("(^[[:digit:]]+)(.*)", "\\1", piece_id))
            location <- gsub("(^[[:digit:]]+)(.*)", "\\2", piece_id)
            get_id_from_coords(df, location, n_pieces, state)
        } else {
            get_id_from_coords(df, piece_id, NULL, state)
        }
    }
}

#### get index for piece restriction
#' @importFrom dplyr near
#' @importFrom utils tail
get_id_from_coords <- function(df, coords, n_pieces = NULL, state = create_state(df)) {
    xy <- get_xy(coords, df, state)
    df <- mutate(df, dist_squared = round((.data$x - xy$x)^2 + (.data$y - xy$y)^2, 5))
    indices <- arrange(df, desc(.data$dist_squared))$id
    if (is.null(n_pieces)) {
        if (sum(near(df$dist_squared, 0)) < 1) stop(paste("Can't identify the piece at", coords))
        n_pieces <- 1
    }
    index <- utils::tail(indices, n_pieces)
    index
}

get_coords_from_piece_id <- function(piece_id, df, state) {
    indices <- get_indices_from_piece_id(piece_id, df, state)
    index <- tail(indices, 1)
    piecepackr:::Point$new(x=df$x[index], y=df$y[index])
}

process_submove <- function(df, text, state = create_state(df)) {
    if (text == "") {
        df
    } else if (str_detect(text, "@>")) {
        process_rotate_move(df, text, state)
    } else if (str_detect(text, "@")) {
        process_at_move(df, text, state)
    } else if (str_detect(text, "\\*")) {
        process_asterisk_move(df, text, state)
    } else if (str_detect(text, hyphen_token)) {
        process_hyphen_move(df, text, state)
    } else if (str_detect(text, "=")) {
        process_equal_move(df, text, state)
    } else if (str_detect(text, colon_token)) {
        process_colon_move(df, text, state)
    } else {
        stop(paste("Don't know how to handle move", text))
    }
}

process_rotate_move <- function(df, text, state = create_state(df), clockwise = TRUE) {
    id_angle <- str_split(text, "@>")[[1]]
    piece_id <- id_angle[1]
    angle <- as.numeric(id_angle[2])
    angle <- ifelse(clockwise, -angle, angle)

    indices <- get_indices_from_piece_id(piece_id, df, state)
    df$angle[indices] <- df$angle[indices] + angle
    df
}

process_at_move <- function(df, text, state = create_state(df)) {
    pc <- str_split(text, "@")[[1]]

    piece_spec <- pc[1]
    df_piece <- parse_piece(piece_spec)

    location <- pc[2]
    xy <- get_xy(location, df, state)

    df_piece$x <- xy$x
    df_piece$y <- xy$y
    df_piece$id <- state$max_id <- state$max_id + 1L
    #### get index for piece restriction
    index <- nrow(df)
    insert_df(df, df_piece, index)
}

get_indices_from_piece_id <- function(piece_id, df, state) {
    id_ <- get_id_from_piece_id(piece_id, df, state)
    which(match(df$id, id_, nomatch = 0) > 0)
}

process_asterisk_move <- function(df, text, state = create_state(df)) {
    piece_id <- gsub("\\*", "", text)
    indices <- get_indices_from_piece_id(piece_id, df, state)
    df[-indices,]
}

hyphen_token <- "-(?![[:digit:]]+)"  # a4-d4 but not (-4,-3)
process_hyphen_move <- function(df, text, state) {
    cc <- str_split(text, hyphen_token)[[1]]
    piece_id <- cc[1]
    indices <- get_indices_from_piece_id(piece_id, df, state)

    location <- cc[2]
    new_xy <- get_xy(location, df, state)

    df[indices, "x"] <- new_xy$x
    df[indices, "y"] <- new_xy$y
    insert_df(df[-indices, ], df[indices, ])
}

create_state <- function(df) {
    if (!is.null(attr(df, "scale_factor"))) {
        scale_factor <- attr(df, "scale_factor")
    } else {
        scale_factor <- 1.0
    }
    as.environment(list(df_move_start = df,
                        max_id = nrow(df),
                        scale_factor = as.numeric(scale_factor)))
}

process_equal_move <- function(df, text, state = create_state(df)) {
    cp <- str_split(text, "=")[[1]]

    piece_id <- cp[1]
    indices <- get_indices_from_piece_id(piece_id, df, state)

    piece_spec <- cp[2]
    df_piece <- parse_piece(piece_spec)

    new_id <- seq.int(state$max_id + 1, length.out = length(indices))
    state$max_id <- max(new_id)

    df[indices, "piece_side"] <- df_piece$piece_side
    df[indices, "suit"] <- df_piece$suit
    df[indices, "rank"] <- df_piece$rank
    df[indices, "angle"] <- df_piece$angle
    df[indices, "id"] <- new_id
    df
}

colon_token <- ":(?![^\\[]*])" # a4-d4 but not a4[1:4]
process_colon_move <- function(df, text, state = create_state(df)) {
    cc <- str_split(text, colon_token)[[1]]

    piece_id1 <- cc[1]
    piece_id2 <- cc[2]
    coords <- get_coords_from_piece_id(piece_id2, df, state)
    location <- stringr::str_glue("({coords$x/scale},{coords$y/scale})",
                                  coords = coords, scale = state$scale_factor)

    df <- process_asterisk_move(df, paste0("*", piece_id2), state)
    df <- process_hyphen_move(df, paste(piece_id1, location, sep = "-"), state)
    df
}

# Insert `df2` into `df1` after `index`
# index = 0 means instead at beginning
insert_df <- function(df1, df2, index = nrow(df1)) {
    if (index == 0) {
        bind_rows(df2, df1)
    } else if (index == nrow(df1)) {
        bind_rows(df1, df2)
    } else {
        bind_rows(df1[seq(index), ], df2, df1[-seq(index), ])
    }
}

get_xy <- function(coords, df, state = create_state(tibble())) {
    if (str_detect(coords, "^&")) {
        piece_id <- str_sub(coords, 2L)
        p <- get_coords_from_piece_id(piece_id, df, state)
    } else {
        p <- piecepackr:::Point$new(x = get_x(coords), y = get_y(coords))
    }
    p$dilate(state$scale_factor)
}

get_x <- function(coords) {
    if (str_detect(coords, ",")) {
        as.numeric(gsub("\\(|,.*", "", coords))
    } else {
        get_algebraic_x(coords)
    }
}
get_y <- function(coords) {
    if (str_detect(coords, ",")) {
        as.numeric(gsub(")|.*,", "", coords))
    } else {
        get_algebraic_y(coords)
    }
}

process_move <- function(df, text, state = create_state(df)) {
    state$df_move_start <- df
    text <- bracer::expand_braces(split_blanks(text))
    text <- str_trim(gsub("\\*", " *", text)) # allow a4-b5*c3*c4
    moves <- split_blanks(text)
    for (move in moves) {
        df <- process_submove(df, move, state = state)
    }
    df
}

# c("a b", "c d") -> c("a", "b", "c", "d")
split_blanks <- function(text) c(str_split(text, "[[:blank:]]+"), recursive = TRUE)

process_moves <- function(df, movelist, state = create_state(df)) {
    df_list <- list(SetupFn.=df)
    nms <- names(movelist)
    if (is.null(nms)) nms <- 1 + seq(movelist)
    for (ii in seq(movelist)) {
        df <- process_move(df, movelist[[ii]], state = state)
        df_list[[nms[ii]]] <- df
    }
    df_list
}
