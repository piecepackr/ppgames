#' Read PPN files
#'
#' Read/write Portable Piecepack Notation (PPN) files
#' @param file Filename, if "" will use \code{stdout()}
#' @param parse Logical of whether to parse the moves in the ppn file
#' @param games A list of parsed PPN games (as returned by \code{read_ppn()})
#' @return A list, for each game in the file a list containing info about the game
#' @import stringr
#' @examples
#'  list.files(system.file("ppn", package = "ppgames"))
#'  file <- system.file("ppn/tic-tac-toe.ppn", package = "ppgames")
#'  games <- read_ppn(file)
#'  tmp <- tempfile(fileext = ".ppn")
#'  write_ppn(games, tmp)
#'  unlink(tmp)
#' @export
#' @seealso [plot_move()], [animate_game()], and [cat_move()] for visualizing parsed ppn games.
read_ppn <- function(file, parse = TRUE) {
    list_ppns <- parse_ppn_file(file)
    lapply(list_ppns, parse_ppn_game, parse = parse)
}

#' @rdname read_ppn
#' @export
write_ppn <- function(games = list(), file = "") {
    ppn <- unlist(lapply(games, as_ppn))
    if (file == "") file <- stdout()
    writeLines(ppn, file)
}

# Parse ppn files
#
# Parses ppn file
# @param file Filename
# @return A list, each element is a character vector containing the text of the PPN games within that file
parse_ppn_file <- function(file) {
    text <- readlines_ppn(file)
    parse_contents(text)
}
parse_contents <- function(text) {
    game_starts <- grep("^-{3}", text)
    if (length(game_starts) == 0L || game_starts[1L] != 1L) {
        game_starts <- c(1L, game_starts)
    }
    game_ends <- c(game_starts[-1L]-1L, length(text))
    contents <- list()
    for (ii in seq(game_starts)) {
        contents[[ii]] <- text[game_starts[ii]:game_ends[ii]]
    }
    contents
}
readlines_ppn <- function(file) {
    tryCatch({
        readLines(file)
    }, warning = function(w) {
        abort(w$message, class = "readlines_ppn", parent = w)
    }, error = function(e) {
        msg <- paste("Couldn't read the file", file)
        msg <- c(msg, i = e$message)
        abort(msg, class = "readlines_ppn", parent = e)
    })
}

# Parse ppn game
#
# Parses (single) ppn game text to get Metadata and Movetext
# @param text Character vector of ppn game text
# @return A list with a named list element named \code{Metadata}
#         and character vector element named \code{Movetext}
parse_ppn_game <- function(text, parse = TRUE) {
    l <- extract_metadata_movetext(text)
    if (parse) {
        parse_movetext(l$movetext, l$metadata)
    } else {
        list(metadata = l$metadata, movetext = l$movetext)
    }
}

extract_metadata_movetext <- function(text) {
    yaml_end <- grep("^\\.{3}", text)
    if (length(yaml_end) == 0L) {
        yaml_end <- grep("^[[:blank:]]+|^$", text)
    }
    if (length(yaml_end) > 0L) {
        metadata <- yaml_load(text[1L:yaml_end[1L]])
        if (yaml_end[1]<length(text)) {
            movetext <- text[(yaml_end[1L]+1L):length(text)]
        } else {
            movetext <- character()
        }
    } else {
        metadata <- list()
        movetext <- text
    }
    if (is.null(metadata))
        metadata <- list()
    if (!is.list(metadata)) {
        msg <- paste0("The PPN metadata does not appear to be a YAML dictionary:\n",
                     yaml::as.yaml(metadata))
        abort(msg, class = "extract_metadata")
    }
    list(metadata = metadata, movetext = movetext)
}

yaml_load <- function(text) {
    tryCatch(yaml::yaml.load(text),
             warning = function(w) {
                abort(w$message, class = "yaml_load")
             },
             error = function(e) {
                text <- paste(paste(" ", text), collapse = "\n")
                msg <- c("YAML parsing error:", i = e$message,
                         i = paste("Failed to parse the following YAML text:\n", text))
                abort(msg, class = "yaml_load")
             })
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

parser_default <- function(movetext = character(), metadata = list(), scale_factor = NULL) {
    game_list <- list(metadata = metadata, movetext = movetext)
    df <- get_starting_df(metadata)
    if (!is.null(scale_factor)) attr(df, "scale_factor") <- scale_factor
    state <- create_state(df, metadata)
    move_list <- parse_moves(movetext, df = df, state = state)
    game_list <- c(game_list, move_list)
    game_list
}

df_none <- function() {
    tibble::tibble(piece_side = character(0L),
                   suit = integer(0L), rank = integer(0L),
                   cfg = character(0),
                   x = numeric(0), y = numeric(0), angle = numeric(0))
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
    return(initialize_df(df_none()))
}

get_starting_df_from_field <- function(field) {
    field0 <- field
    df <- tryCatch({
        if (is.character(field)) {
            df <- get_starting_df_from_name(field)
        } else if (is.list(field)) {
            names(field) <- to_varname(names(field))
            i_name <- match("name", names(field))
            i_system <- match("system", names(field), nomatch = 0)
            .l <- field[-c(i_name, i_system)]
            df <- get_starting_df_from_name(field[["name"]], .l, field[["system"]])
        }
        df
    }, error = function(e) {
        if (is.list(field))
            msg <- paste0("Couldn't process SetUp/GameType:\n", yaml::as.yaml(field0))
        else
            msg <- paste("Couldn't process SetUp/GameType:", field0)
        msg <- c(msg, i = e$message)
        abort(msg, class = "initialize_setup", parent = e)
    })
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
           abort(paste("Don't recognize System:", system), class = "board_setup"))
}

get_starting_df_from_name <- function(game_name, .l = list(), system = NULL) {
    if (!is.null(system) && to_varname(system) == "stackpack")
        .l$has_subpack <- TRUE
    package <- get_ppn_package(system)
    fn_name <- paste0("df_", to_varname(game_name))
    fn <- ppn_get(fn_name, package)
    df <- do.call(fn, .l)
    df <- initialize_df(df)
    df
}

initialize_df <- function(df) {
    df$id <- as.character(seq_len(nrow(df)))
    if (!hasName(df, "angle")) df$angle <- 0
    df$angle <- ifelse(is.na(df$angle), 0, df$angle)
    df$rank <- ifelse(is.na(df$rank), 1L, df$rank)
    df$suit <- ifelse(is.na(df$suit), 1L, df$suit)
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
    if (is.null(df)) df <- initialize_df(df_none())
    #### Convert # comments into braces?
    if (length(text) > 0) {
        text <- parse_braces(text)
        moves_raw <- parse_movenumbers(text)
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
    if (any(duplicated(names(dfs)))) warning("Non-unique MoveNumbers")
    names(moves) <- names(dfs)
    names(comments) <- names(dfs)
    for (i in seq_along(dfs)) attr(dfs[[i]], "scale_factor") <- state$scale_factor
    list(moves = moves, comments = comments, dfs = dfs)
}

parse_braces <- function(text) {
    text <- split_blanks(str_squish(paste(text, collapse = " "))) # vector of tokens
    text <- unlist(lapply(text, exp_braces)) # vector of expanded tokens
    paste(text, collapse = " ")
}
exp_braces <- function(string) {
    if (str_sub(string, 1L, 1L) == "{" && str_sub(string, -1L, -1L) == "}") # no preamble/postfix
        string
    else
        bracer::expand_braces(string, engine = "r")
}

parse_movenumbers <- function(text) {
    # (?![^\\{]*\\}) is a negative lookahead assertion to not capture moves in comment braces
    # (?![[:digit:]]) is a negative lookahead assertion to not capture dots followed by non-space
    move_number_semicolon <- ";(?![^\\{]*\\})"
    text <- str_replace_all(text, move_number_semicolon, " . ")
    move_number_token <- "(?<![[:alnum:][:punct:]])[[:alnum:]_\\.]*\\.+(?![[:alnum:][:punct:]])(?![^\\{]*\\})"
    locations <- str_locate_all(text, move_number_token)[[1]]
    nr <- nrow(locations)
    moves_raw <- list()
    if (nr == 0L) {
        moves_raw[[1L]] <- text
    } else {
        i1 <- locations[1L, 1L]
        if (i1 > 1L) {
            moves_raw <- vector("list", nr + 1)
            moves_names <- vector("character", nr + 1)
            offset <- 1L
            moves_raw[[1]] <- str_sub(text, 1L, i1 - 2L)
        } else {
            offset <- 0L
            moves_names <- vector("character", nr)
            moves_raw <- vector("list", nr)
        }
        for (ii in seq(nr)) {
            is <- locations[ii, 1L]
            ie <- locations[ii, 2L]
            moves_names[ii + offset] <- str_sub(text, is, ie)
            is <- locations[ii, 2L] + 2L
            if (ii < nr)
                ie <- locations[ii+1L, 1L] - 2L
            else
                ie <- str_count(text)
            moves_raw[[ii + offset]] <- str_sub(text, is, ie)
        }
        names(moves_raw) <- moves_names
        if (names(moves_raw)[1] == "") names(moves_raw)[1] <- "SetupFn.."
    }
    moves_raw
}

comment_token <- "(?<![[:alnum:][:punct:]])\\{[^}]*\\}(?![[:alnum:][:punct:]])"
extract_comments <- function(text) {
    text <- paste(str_extract_all(text, comment_token)[[1L]], collapse = " ")
    str_squish(str_remove_all(text, "\\{|\\}"))
}
remove_comments <- function(text) {
    str_squish(str_remove_all(text, comment_token))
}

parse_piece_incomplete <- function(std_piece_spec) {
    df <- parse_simplified_piece(std_piece_spec$simple)
    if (!is.na(std_piece_spec$angle)) {
        if (!is.na(df$angle))
            df$angle <- (df$angle + std_piece_spec$angle) %% 360
        else
            df$angle <- std_piece_spec$angle
    }
    if (!is.na(std_piece_spec$rank))
        df$rank <- std_piece_spec$rank
    if (!is.na(std_piece_spec$suit))
        df$suit <- std_piece_spec$suit
    if (!is.na(std_piece_spec$cfg))
        df$cfg <- std_piece_spec$cfg
    df
}

index_rank_by_one <- c("checkers1", "checkers2", "chess1", "chess2", "go", "dice", "icehouse_pieces",
                       "playing_cards", "playing_cards_colored", "playing_cards_tarot")
character_class <- function(characters) {
    paste(characters, collapse = "|")
}
standardize_piece_spec <- function(piece_spec) {
    simple_complex <- str_split(piece_spec, ",")[[1L]]
    # complex
    elements <- tail(simple_complex, -1L)
    angle <- grep("^a-*[[:digit:]]", elements, value = TRUE)
    angle <- ifelse(length(angle), as.numeric(str_sub(angle, 2L)) %% 360, NA_real_)
    rank <- grep("^r[[:digit:]]", elements, value = TRUE)
    rank <- ifelse(length(rank), as.integer(str_sub(rank, 2L)) + 1L, NA_integer_) # index by 0
    suit <- grep("^s[[:digit:]]", elements, value = TRUE)
    suit <- ifelse(length(suit), as.integer(str_sub(suit, 2L)) + 1L, NA_integer_) # index by 0
    cfg <- grep("'$", elements, value = TRUE)
    cfg <- ifelse(length(cfg), str_sub(cfg, 1L, -2L), NA_character_)

    # simple
    x <- simple_complex[1]
    x <- gsub("\u00b5|\u03bc", "u", x) # micro sign
    x <- gsub("/\\\\", "\u25b2", x) # triangle
    x <- gsub("\\[]", "\U0001f0a0", x) # card back
    x <- gsub("\\()", "\u25cf", x) # circle
    x <- gsub("\u25cb", "W\u25cf", x) # white circle
    x <- gsub("\\[X]", "\u25a0", x) # square
    # piecepack ranks
    x <- gsub("n", "0", x)
    x <- gsub("a", "1", x)
    # checkers
    if (str_detect(x, "\u26c2|\u26c0") && str_detect(x, "[RKGBYW]")) {
        x <- gsub("\u26c2|\u26c0", "c", x)
    } else if (str_detect(x, "\u26c2")) {
        x <- gsub("\u26c2", "Kc", x)
    } else if (str_detect(x, "\u26c0")) {
        x <- gsub("\u26c0", "Wc", x)
    }
    # go
    if (!is.na(board <- str_extract(x, "\\[#]|\u25a6"))) {
        if (str_detect(x, "[RKGBYW]")) {
            x <- gsub(board, "\u25a0", x)
        } else {
            x <- gsub(board, "K\u25a0", x)
        }
        cfg <- "go"
    }
    if (!is.na(die <- str_extract(x, character_class(unicode_dice)))) {
        rank <- str_which(unicode_dice, die) + 1L
        if (str_detect(x, "[RKGBYW]")) {
            x <- gsub(die, "d", x)
        } else {
            x <- gsub(die, "Wd", x)
        }
    }
    if (!is.na(card <- str_extract(x, character_class(unicode_cards)))) {
        rank <- card2rank[[card]] + 1L
        suit <- card2suit[[card]]
        x <- gsub(card, "\U0001f0a0", x)
    }
    if (!is.na(piece <- str_extract(x, character_class(unicode_chess_black)))) {
        rank <- str_which(unicode_chess_black, piece) + 1
        if (!str_detect(x, "[RKGBYW]")) {
            suit <- 2
        }
        cfg <- "chess2"
        if (str_detect(x, "b")) {
            x <- gsub(piece, "\u25cf", x)
        } else {
            x <- gsub(piece, "f\u25cf", x)
        }
    }
    if (!is.na(piece <- str_extract(x, character_class(unicode_chess_white)))) {
        rank <- str_which(unicode_chess_white, piece) + 1
        if (!str_detect(x, "[RKGBYW]")) {
            suit <- 6
        }
        cfg <- "chess2"
        if (str_detect(x, "b")) {
            x <- gsub(piece, "\u25cf", x)
        } else {
            x <- gsub(piece, "f\u25cf", x)
        }
    }
    if (!is.na(tile <- str_extract(x, character_class(unicode_dominoes)))) {
        if (tile %in% c("\U0001f030", "\U0001f062"))
            x <- gsub(tile, "b", x)
        else
            x <- gsub(tile, "", x)
        rank <- tile2rank[[tile]] + 1L
        suit <- tile2suit[[tile]] + 1L
        if (!is.na(angle))
            angle <- (angle + tile2angle[[tile]]) %% 360
        else
            angle <- tile2angle[[tile]]
        if (!is.na(col <- str_extract(x, "[RKGBYW]"))) {
            x <- gsub(col, "", x)
            if (is.na(cfg)) {
                cfg <- paste0("dominoes_",
                              switch(col, R = "red", K = "black", G = "green", B = "blue", Y = "yellow", W = "white"))
            }
        } else {
            if (is.na(cfg)) cfg <- "dominoes"
        }
    }
    list(simple = x, suit = suit, rank = rank, angle = angle, cfg = cfg)
}

complete_piece <- function(df, std_piece_spec) {
    simple <- std_piece_spec$simple
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
        if (str_detect(simple, "[RKGBYW]")) {
            df$cfg <- switch(df$piece,
                             bit = switch(str_extract(simple, "[smc]"),
                                          s = "go", c = "checkers2", m = "meeples",
                                          abort(str_glue("Don't know proper cfg for piece '{simple}'"),
                                                class = "infer_piece")),
                             die = "dice",
                             pyramid = "icehouse_pieces",
                             tile = paste0("dominoes_",
                                           switch(str_extract(simple, "[RKGBYW]"),
                                                  R = "red", K = "black", G = "green",
                                                  B = "blue", Y = "yellow", W = "white")),
                             abort(str_glue("Don't know proper cfg for piece '{simple}'"),
                                   class = "infer_piece")
            )
        } else if (df$piece == "card") {
            df$cfg <- "playing_cards_tarot"
        } else if (df$piece == "bit") {
            df$cfg <- "go"
        } else if (df$piece == "board") {
            df$cfg <- "checkers2"
        } else {
            df$cfg <- "piecepack"
        }
    }
    if (str_detect(simple, "u")) {
        df$cfg <- switch(df$cfg,
                         chess2 = "chess1",
                         checkers2 = "checkers1",
                         piecepack = "subpack",
                         abort(paste("Don't know miniature version of cfg", df$cfg),
                               class = "infer_piece")
        )
    }
    if (is.na(df$side)) {
        df$side <- switch(df$piece,
               tile = ifelse(is.na(df$suit) || is.na(df$rank), "back", "face"),
               coin = ifelse(is.na(df$suit), "face", "back"),
               saucer = ifelse(is.na(df$suit), "face", "back"),
               pyramid = "top",
               bit = "back",
               card = ifelse(is.na(df$suit) || is.na(df$rank), "back", "face"),
               "face")
    }
    if (is.na(df$suit)) {
        df$suit <- switch(df$cfg,
                          go = 2L,
                          checkers1 = 3L,
                          checkers2 = 3L,
                          dice = 6L,
                          1L)
    }
    if (is.na(df$rank)) {
        if (df$piece == "board") df$rank <- switch(df$cfg, go = 19L, 8L)
        df$rank <- 1L
    } else if (df$cfg %in% index_rank_by_one) {
        df$rank <- df$rank - 1L
    }
    df$piece_side <- paste0(df$piece, "_", df$side)
    tibble::as_tibble(df[c("piece_side", "suit", "rank", "angle", "cfg")])
}

parse_piece <- function(text) {
    std_piece_spec <- standardize_piece_spec(text)
    df <- parse_piece_incomplete(std_piece_spec)
    df <- complete_piece(df, std_piece_spec)
    df
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
    if (str_detect(text, "\u2665|\u2660|\u2663|\u2666")) {
        "playing_cards_expansion"
    } else if (str_detect(text, "\u2661|\u2664|\u2667|\u2662")) {
        "dual_piecepacks_expansion"
    } else if (str_detect(text, "\u2b22")) {
        "hexpack"
    } else {
        NA_character_
    }
}
get_simplified_suit <- function(text) {
    if (str_detect(text, "S|\u2665|R|\u2661")) {
        1L
    } else if (str_detect(text, "M|\u2660|K|\u2664")) {
        2L
    } else if (str_detect(text, "C|\u2663|G|\u2667")) {
        3L
    } else if (str_detect(text, "A|\u2666|B|\u2662")) {
        4L
    } else if (str_detect(text, "Y")) {
        5L
    } else if (str_detect(text, "W")) {
        6L
    } else {
        NA_integer_
    }
}
get_simplified_rank <- function(text) {
    if (str_detect(text, "[[:digit:]]")) {
        as.integer(str_extract(text, "[[:digit:]]")) + 1L
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
        NA_real_
    }
}
get_simplified_piece <- function(text) {
    if (str_detect(text, "t")) {
        "tile"
    } else if (str_detect(text, "c")) {
        if (str_detect(text, "[RKGBYW]"))
            "bit"
        else
            "coin"
    } else if (str_detect(text, "d")) {
        "die"
    } else if (str_detect(text, "p")) {
        "pawn"
    } else if (str_detect(text, "m")) {
        if (str_detect(text, "[RKGBYW]"))
            "bit"
        else
            "matchstick"
    } else if (str_detect(text, "s")) {
        if (str_detect(text, "[RKGBYW]"))
            "bit"
        else
            "saucer"
    } else if (str_detect(text, "\U0001f0a0")) {
        "card"
    } else if (str_detect(text, "\u25b2")) {
        "pyramid"
    } else if (str_detect(text, "\u25cf")) {
        "bit"
    } else if (str_detect(text, "\u25a0")) {
        "board"
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
    if (piece_id == "") {
        id <- state$active_id
        if (!length(id)) abort("Couldn't find any active pieces", class = "identify_piece")
        id
    } else if (str_detect(piece_id, "^\\^")) { # ^b4
        piece_id <- str_sub(piece_id, 2L)
        get_id_from_piece_id(piece_id, state$df_move_start, state)
    } else {
        if (str_detect(piece_id, "^[[:digit:].]+$")) { # 15
            as.character(piece_id)
        } else if (str_detect(piece_id, "^[[:digit:]]+")) { # 2b4
            n_pieces <- as.integer(gsub("(^[[:digit:]]+)(.*)", "\\1", piece_id))
            location <- gsub("(^[[:digit:]]+)(.*)", "\\2", piece_id)
            get_id_from_coords(df, location, n_pieces, state)
        } else if (str_detect(piece_id, "^\\?")) { # ?S4
            piece_spec <- str_sub(piece_id, 2L)
            non_greedy_match(df, piece_spec)
        } else if (str_detect(piece_id, "^/")) { # /S4
            piece_spec <- str_sub(piece_id, 2L)
            greedy_match(df, piece_spec)
        } else if (str_detect(piece_id, "\\[.*\\]$")) { # b4[2:3] # nolint
            brackets <- gsub(".*\\[(.*)\\]$", "\\1", piece_id)
            coords <- gsub("(.*)\\[.*\\]$", "\\1", piece_id)
            sub_indices <- get_indices_from_brackets(brackets)
            indices <- get_id_from_coords(df, coords, Inf, state)
            sub_indices <- length(indices) - sub_indices + 1L
            indices[sub_indices]
        } else { # b4
            get_id_from_coords(df, piece_id, NULL, state)
        }
    }
}

get_indices_from_brackets <- function(bracket_contents) {
    indices <- str_split(bracket_contents, ",")[[1]]
    indices <- gsub(":", "..", indices)
    indices <- paste0("{", indices, "}")
    indices <- bracer::expand_braces(indices, engine = "r")
    indices <- gsub("[\\{\\}]", "", indices)
    rev(as.integer(indices))
}

#' @importFrom dplyr arrange desc near
#' @importFrom utils tail
get_id_from_coords <- function(df, coords, n_pieces = NULL, state = create_state(df)) {
    xy <- get_xy(coords, df, state)
    df <- mutate(df, dist_squared = round((.data$x - xy$x)^2 + (.data$y - xy$y)^2, 5))
    indices <- arrange(df, desc(.data$dist_squared))$id
    if (is.null(n_pieces)) {
        if (sum(near(df$dist_squared, 0)) < 1)
            abort(paste("Can't identify the piece at", coords), class = "identify_piece")
        n_pieces <- 1L
    } else if (is.infinite(n_pieces)) {
        n_pieces <- sum(near(df$dist_squared, 0))
    }
    index <- utils::tail(indices, n_pieces)
    index
}

get_coords_from_piece_id <- function(piece_id, df, state = create_state(df)) {
    if (str_detect(piece_id, "^[[:digit:]]+")) {
        coords <- gsub("^[[:digit:]]+", "", piece_id)
        get_xy(coords, df, state)
    } else {
        indices <- get_indices_from_piece_id(piece_id, df, state)
        index <- tail(indices, 1L)
        piecepackr:::Point2D$new(x=df$x[index], y=df$y[index])
    }
}

process_submove <- function(df, text, state = create_state(df)) {
    if (text == "") {
        df
    } else if (str_detect(text, "\\$>")) {
        process_dollar_rotate_move(df, text, state)
    } else if (str_detect(text, "@>")) {
        process_rotate_move(df, text, state)
    } else if (str_detect(text, "@%")) {
        process_at_percent_move(df, text, state)
    } else if (str_detect(text, "@")) {
        process_at_move(df, text, state)
    } else if (str_detect(text, "-%")) {
        process_hyphen_percent_move(df, text, state)
    } else if (str_detect(text, hyphen_token)) {
        process_hyphen_move(df, text, state)
    } else if (str_detect(text, "\\*")) {
        process_asterisk_move(df, text, state)
    } else if (str_detect(text, plus_token)) {
        process_plus_move(df, text, state)
    } else if (str_detect(text, "~")) {
        process_tilde_move(df, text, state)
    } else if (str_detect(text, "=")) {
        process_equal_move(df, text, state)
    } else if (str_detect(text, colon_token)) {
        process_colon_move(df, text, state)
    } else if (str_detect(text, "_%")) {
        process_underscore_percent_move(df, text, state)
    } else if (str_detect(text, "_")) {
        process_underscore_move(df, text, state)
    } else if (str_detect(text, "\\\\%")) {
        process_backslash_percent_move(df, text, state)
    } else if (str_detect(text, "\\\\")) {
        process_backslash_move(df, text, state)
    } else if (str_detect(text, "#")) {
        process_hash_move(df, text, state)
    } else if (str_detect(text, "!")) {
        process_exclamation_move(df, text, state)
    } else {
        abort(paste("Don't know how to handle move", text), class = "identify_move")
    }
}

process_rotate_move <- function(df, text, state = create_state(df), clockwise = TRUE) {
    id_angle <- str_split(text, "@>")[[1]]
    piece_id <- id_angle[1L]
    indices <- get_indices_from_piece_id(piece_id, df, state)
    angle <- id_angle[2L]
    if (str_detect(angle, "\\|")) {
        angle_coords <- str_split(angle, "\\|", n=2L)[[1L]]
        angle <- as.numeric(angle_coords[1L])
        location <- get_xy(angle_coords[2L], df, state)
    } else if (str_detect(angle, "\\$")) {
        angle_coords <- str_split(angle, "\\$", n=2L)[[1L]]
        angle <- as.numeric(angle_coords[1L])
        location <- get_xy(paste0("&", angle_coords[2L]), df, state)
    } else {
        angle <- as.numeric(angle)
        location <- NULL
    }
    angle <- ifelse(clockwise, -angle, angle)
    if (!is.null(location)) {
        p <- piecepackr:::Point2D$new(x = df$x[indices], y = df$y[indices])
        p <- p$translate(-location$x, -location$y)$rotate(angle)$translate(location$x, location$y)
        df$x[indices] <- p$x
        df$y[indices] <- p$y
    }
    df$angle[indices] <- (df$angle[indices] + angle)
    state$active_id <- df$id[indices]
    df
}

process_exclamation_move <- function(df, text, state = create_state(df)) {
    piece_id <- str_sub(text, 2L, 2L)
    process_hyphen_move(df, paste0(piece_id, "-<0,0>"), state)
}

process_dollar_rotate_move <- function(df, text, state = create_state(df)) {
    pa <- str_split(text, "\\$>")[[1L]]
    piece_spec <- pa[1L]
    angle <- pa[2L]
    text <- str_glue("{piece_spec}@>{angle}${piece_spec}")
    process_rotate_move(df, text, state)
}

process_hash_move <- function(df, text, state = create_state(df)) {
    id1_id2 <- str_split(text, "#")[[1]]
    piece_id1 <- id1_id2[1L]
    piece_id2 <- id1_id2[2L]
    coords1 <- get_coords_from_piece_id(piece_id1, df, state)
    coords2 <- get_coords_from_piece_id(piece_id2, df, state)
    indices1 <- get_indices_from_piece_id(piece_id1, df, state)
    indices2 <- get_indices_from_piece_id(piece_id2, df, state)
    df1 <- df[indices1, ]
    df1$x <- coords2$x
    df1$y <- coords2$y
    df2 <- df[indices2, ]
    df2$x <- coords1$x
    df2$y <- coords1$y
    dfo <- df[-c(indices1, indices2), ]
    state$active_id <- df$id[indices1]
    bind_rows(dfo, df1, df2)
}

process_at_move <- function(df, text, state = create_state(df)) {
    pc <- str_split(text, "@")[[1L]]
    piece_spec <- pc[1L]
    l_i <- get_location_index(pc[2L], df, state)
    df_piece <- parse_piece(piece_spec)
    xy <- get_xy(l_i$location, df, state)
    df_piece$x <- xy$x
    df_piece$y <- xy$y
    df_piece$id <- compute_new_id(state)

    state$active_id <- df_piece$id
    insert_df(df, df_piece, l_i$index)
}
process_at_percent_move <- function(df, text, state = create_state(df)) {
    pi <- str_split(text, "@%")[[1L]]

    piece_spec <- pi[1L]
    id_ <- pi[2L]
    text <- str_glue("{piece_spec}@&{id_}%{id_}")
    process_at_move(df, text, state)
}

process_backslash_move <- function(df, text, state = create_state(df)) {
    pc <- str_split(text, "\\\\")[[1L]]
    piece_spec <- pc[1L]
    l_i <- get_location_index(pc[2L], df, state)
    df_piece <- parse_piece(piece_spec)
    xy <- get_xy(l_i$location, df, state)
    df_piece$x <- xy$x
    df_piece$y <- xy$y
    df_piece$id <- compute_new_id(state)
    if (is.null(l_i$id))
        index <- 0L
    else
        index <- l_i$index - 1L

    state$active_id <- df_piece$id
    insert_df(df, df_piece, index)
}
process_backslash_percent_move <- function(df, text, state = create_state(df)) {
    pi <- str_split(text, "\\\\%")[[1L]]

    piece_spec <- pi[1L]
    id_ <- pi[2L]
    text <- str_glue("{piece_spec}\\&{id_}%{id_}")
    process_backslash_move(df, text, state)
}

get_location_index <- function(text, df, state) {
    if (str_detect(text, "%")) {
        c_id <- str_split(text, "%")[[1L]]
        location <- c_id[1L]
        index <- get_indices_from_piece_id(c_id[2L], df, state)
        id <- df$id[index]
    } else {
        location <- text
        index <- nrow(df)
        id <- NULL
    }
    list(location = location, index = index, id = id)
}

get_indices_from_piece_id <- function(piece_id, df, state) {
    id_ <- get_id_from_piece_id(piece_id, df, state)
    which(match(df$id, id_, nomatch = 0) > 0)
}

process_asterisk_move <- function(df, text, state = create_state(df)) {
    piece_id <- sub("\\*", "", text)
    indices <- get_indices_from_piece_id(piece_id, df, state)
    state$active_id <- setdiff(state$active_id, df$id[indices])
    df[-indices,]
}
plus_token <- "(?<![(<,])\\+"  # +d4 but not (+4,-3) or <-4,+3>
process_plus_move <- function(df, text, state) {
    piece_id <- sub("\\+", "", text)
    indices <- get_indices_from_piece_id(piece_id, df, state)
    df$piece_side[indices] <- flip_ps(df$piece_side[indices])
    df$rank[indices] <- ifelse(df$piece_side[indices] == "die_face",
                               (df$rank[indices] + 2) %% 6 + 1,
                               df$rank[indices])
    df$id[indices] <- compute_plus_id(df$id[indices])
    df
}

process_underscore_move <- function(df, text, state) {
    cc <- str_split(text, "_")[[1L]]
    piece_id <- cc[1L]
    indices <- get_indices_from_piece_id(piece_id, df, state)
    df_moving <- df[indices, ]
    df_rest <- df[-indices, ]

    l_i <- get_location_index(cc[2L], df, state)
    location <- l_i$location
    new_xy <- get_xy(location, df, state, indices)
    if (is.null(l_i$id))
        index <- 0L
    else
        index <- which(df_rest$id %in% l_i$id) - 1L

    df_moving$x <- new_xy$x
    df_moving$y <- new_xy$y
    state$active_id <- df_moving$id
    insert_df(df_rest, df_moving, index)
}

process_underscore_percent_move <- function(df, text, state) { # nolint
    pi <- str_split(text, "_%")[[1L]]

    piece_spec <- pi[1L]
    id_ <- pi[2L]
    text <- str_glue("{piece_spec}_&{id_}%{id_}")
    process_underscore_move(df, text, state)
}

hyphen_token <- "(?<![(<,])-"  # a4-d4 and a4-2E but not (-4,-3) or <-4,-3>
process_hyphen_move <- function(df, text, state) {
    cc <- str_split(text, hyphen_token)[[1L]]
    piece_id <- cc[1L]
    indices <- get_indices_from_piece_id(piece_id, df, state)
    df_moving <- df[indices, ]
    df_rest <- df[-indices, ]

    l_i <- get_location_index(cc[2L], df, state)
    location <- l_i$location
    new_xy <- get_xy(location, df, state, indices)
    if (is.null(l_i$id))
        index <- nrow(df_rest)
    else
        index <- which(df_rest$id %in% l_i$id)
    df_moving$x <- new_xy$x
    df_moving$y <- new_xy$y
    state$active_id <- df_moving$id
    insert_df(df_rest, df_moving, index)
}
process_hyphen_percent_move <- function(df, text, state) {
    pi <- str_split(text, "-%")[[1L]]

    piece_spec <- pi[1L]
    id_ <- pi[2L]
    text <- str_glue("{piece_spec}-&{id_}%{id_}")
    process_hyphen_move(df, text, state)
}

create_state <- function(df, metadata = list()) {
    if (!is.null(attr(df, "scale_factor"))) {
        scale_factor <- attr(df, "scale_factor")
    } else {
        scale_factor <- 1.0
    }
    as.environment(list(df_move_start = df,
                        macros = c(metadata$Macros, attr(df, "macros"), macros),
                        max_id = nrow(df),
                        active_id = character(),
                        scale_factor = as.numeric(scale_factor)))
}

ngm_helper <- function(na_check, value) {
    if (is.na(na_check))
        TRUE
    else
        value
}

greedy_match <- function(df, piece_spec) {
    std_piece_spec <- standardize_piece_spec(piece_spec)
    dfi <- parse_piece_incomplete(std_piece_spec)
    with_incomplete <- which(ngm_helper(dfi$piece, str_detect(df$piece_side, paste0("^", dfi$piece))) &
                             ngm_helper(dfi$side, str_detect(df$piece_side, paste0(dfi$side, "$"))) &
                             ngm_helper(dfi$suit, df$suit == dfi$suit) &
                             ngm_helper(dfi$rank, df$rank == dfi$rank) &
                             ngm_helper(dfi$cfg, df$cfg == dfi$cfg) &
                             ngm_helper(dfi$angle, df$angle == dfi$angle))
    df$id[with_incomplete]
}

non_greedy_match <- function(df, piece_spec) {
    std_piece_spec <- standardize_piece_spec(piece_spec)
    dfi <- parse_piece_incomplete(std_piece_spec)
    with_incomplete <- which(ngm_helper(dfi$piece, str_detect(df$piece_side, paste0("^", dfi$piece))) &
                             ngm_helper(dfi$side, str_detect(df$piece_side, paste0(dfi$side, "$"))) &
                             ngm_helper(dfi$suit, df$suit == dfi$suit) &
                             ngm_helper(dfi$rank, df$rank == dfi$rank) &
                             ngm_helper(dfi$cfg, df$cfg == dfi$cfg) &
                             ngm_helper(dfi$angle, df$angle == dfi$angle))
    if (length(with_incomplete) == 1) return(df$id[with_incomplete])
    dff  <- complete_piece(dfi, std_piece_spec)
    with_angle <- which(df$piece_side == dff$piece_side &
                        ngm_helper(dff$suit, df$suit == dff$suit) &
                        ngm_helper(dff$rank, df$rank == dff$rank) &
                        df$cfg == dff$cfg &
                        near(df$angle, dff$angle))
    if (length(with_angle)) return(df$id[tail(with_angle, 1L)])
    without_angle <- which(df$piece_side == dff$piece_side &
                           ngm_helper(dff$suit, df$suit == dff$suit) &
                           ngm_helper(dff$rank, df$rank == dff$rank) &
                           df$cfg == dff$cfg)
    if (length(without_angle)) return(df$id[tail(without_angle, 1L)])
    abort("Couldn't find a match", class = "identify_piece")
}

process_tilde_move <- function(df, text, state = create_state(df)) {
    cp <- str_split(text, "~")[[1]]
    piece_id <- cp[1]
    indices <- get_indices_from_piece_id(piece_id, df, state)
    to_change_id <- rep(0L, length(indices))

    std_piece_spec <- standardize_piece_spec(cp[2])
    dfp <- parse_piece_incomplete(std_piece_spec)

    if (!is.na(dfp$angle)) df[indices, "angle"] <- dfp$angle
    if (!is.na(dfp$piece) || !is.na(dfp$side)) {
        psm <- str_split_fixed(df$piece_side, "_", 2L)
        if (!is.na(dfp$side)) { # change side means 1L
            to_change <- psm[indices, 2L] != dfp$side
            to_change_id[to_change] <- 1L
            psm[indices, 2L] <- dfp$side
        }
    }
    # 2L if cfg change
    if (!is.na(dfp$cfg)) {
        to_change <- is.na(df$cfg[indices]) | df$cfg[indices] != dfp$cfg
        to_change_id[to_change] <- 2L
        df$cfg[indices] <- dfp$cfg
    }
    if (!is.na(dfp$rank)) { # if change rank 1L if die_face and not 2L already else 2L
        to_change <- is.na(df$rank[indices]) ||
            df$rank[indices] != ifelse(df$cfg[indices] %in% index_rank_by_one, dfp$rank - 1L, dfp$rank)
        to_change_id[to_change] <- ifelse(df$piece_side[indices[to_change]] == "die_face" &&
                                          to_change_id[to_change] < 2L,
                                          1L, 2L)
        df$rank[indices] <- ifelse(df$cfg[indices] %in% index_rank_by_one, dfp$rank - 1L, dfp$rank)
    }
    # 2L if different piece, suit
    if (!is.na(dfp$piece) || !is.na(dfp$side)) {
        if (!is.na(dfp$piece)) {
            to_change <- psm[indices, 1] != dfp$piece
            to_change_id[to_change] <- 2L
            psm[indices, 1L] <- dfp$piece
        }
        df$piece_side <- paste0(psm[, 1L], "_", psm[, 2L])
    }
    if (!is.na(dfp$suit)) {
        to_change <- is.na(df$suit[indices]) | df$suit[indices] != dfp$suit
        to_change_id[to_change] <- 2L
        df$suit[indices] <- dfp$suit
    }

    to_plus_id <- which(to_change_id == 1L)
    df$id[indices[to_plus_id]] <- compute_plus_id(df$id[indices[to_plus_id]])
    to_equal_id <- which(to_change_id == 2L)
    df$id[indices[to_equal_id]] <- compute_equal_id(df$id[indices[to_equal_id]])
    state$active_id <- df$id[indices]
    df
}

process_equal_move <- function(df, text, state = create_state(df)) {
    cp <- str_split(text, "=")[[1L]]

    piece_id <- cp[1L]
    indices <- get_indices_from_piece_id(piece_id, df, state)
    #### Don't downgrade to 1 if already 2
    to_change_id <- rep(0L, length(indices))

    piece_spec <- cp[2L]
    df_piece <- parse_piece(piece_spec)

    df$angle[indices] <- df_piece$angle

    # change side means 1L
    side <- str_extract(df_piece$piece_side, "^[a-z]+")
    old_side <- str_extract(df$piece_side[indices], "^[a-z]+")
    to_change_id[old_side != side] <- 1L

    # change rank 1L if die_face else 2L
    to_change <- df$rank[indices] != df_piece$rank
    to_change_id[to_change] <- ifelse(df$piece_side[indices] == "die_face", 1L, 2L)
    df$rank[indices] <- df_piece$rank

    # 2L if different piece, suit, or cfg
    piece <- str_extract(df_piece$piece_side, "^[a-z]+")
    old_piece <- str_extract(df$piece_side[indices], "^[a-z]+")
    to_change_id[old_piece != piece] <- 2L
    df$piece_side[indices] <- df_piece$piece_side

    to_change_id[df$suit[indices] != df_piece$suit] <- 2L
    df$suit[indices] <- df_piece$suit
    to_change_id[df$cfg[indices] != df_piece$cfg] <- 2L
    df$cfg[indices] <- df_piece$cfg

    to_plus_id <- which(to_change_id == 1L)
    df$id[indices[to_plus_id]] <- compute_plus_id(df$id[indices[to_plus_id]])
    to_equal_id <- which(to_change_id == 2L)
    df$id[indices[to_equal_id]] <- compute_equal_id(df$id[indices[to_equal_id]])
    state$active_id <- df$id[indices]
    df
}

# as a side effect updates state$max_id
compute_new_id <- function(state = create_state(initialize_df()), n = 1L) {
    new_id <- seq.int(state$max_id + 1L, length.out = n)
    state$max_id <- max(new_id)
    as.character(new_id)
}
compute_plus_id <- function(id) {
    if (length(id) == 0) return(id)
    id <- str_replace(id, "\\.{2}", "=")
    id <- str_split(id, "\\.", simplify=TRUE)
    if (ncol(id) == 1) id <- cbind(id, "")
    id[, 2] <- ifelse(id[, 2] == "", "0", id[, 2])
    id[, 2] <- as.character(as.integer(id[, 2]) + 1L)
    id <- paste0(id[, 1], ".", id[, 2])
    id <- str_replace(id, "=", "..")
    id
}
compute_equal_id <- function(id) {
    if (length(id) == 0) return(id)
    id <- str_replace(id, "\\.{2}", "=")
    id <- str_remove(id, "\\.[:digit:]*")
    id <- str_split(id, "=", simplify=TRUE)
    if (ncol(id) == 1) id <- cbind(id, "")
    id[, 2] <- ifelse(id[, 2] == "", "0", id[, 2])
    id[, 2] <- as.character(as.integer(id[, 2]) + 1L)
    paste0(id[, 1], "..", id[, 2])
}

colon_token <- ":(?![^\\[]*])" # a4-d4 but not a4[1:4]
process_colon_move <- function(df, text, state = create_state(df)) {
    cc <- str_split(text, colon_token)[[1]]

    piece_id1 <- cc[1L]
    piece_id2 <- cc[2L]

    location <- get_location_from_piece_id(piece_id2, df, state)

    df <- process_asterisk_move(df, paste0("*", piece_id2), state)
    df <- process_hyphen_move(df, paste(piece_id1, location, sep = "-"), state)
    df
}

get_location_from_piece_id <- function(piece_id, df, state) {
    coords <- get_coords_from_piece_id(piece_id, df, state)
    stringr::str_glue("({coords$x/scale},{coords$y/scale})",
                      coords = coords, scale = state$scale_factor)
}

# Insert `df2` into `df1` after `index`
# index = 0 means instead at beginning
insert_df <- function(df1, df2, index = nrow(df1)) {
    if (index == 0L) {
        bind_rows(df2, df1)
    } else if (index == nrow(df1)) {
        bind_rows(df1, df2)
    } else {
        bind_rows(df1[seq(index), ], df2, df1[-seq(index), ])
    }
}

get_xy <- function(coords, df, state = create_state(tibble()), anchor_indices = NULL) {
    xy <- if (coords == "") {
        get_xy("&", df, state, anchor_indices)
    } else if (str_detect(coords, "^&")) {
        piece_id <- str_sub(coords, 2L)
        get_coords_from_piece_id(piece_id, df, state)
    } else if (str_detect(coords, "^<")) { # relative moves
        if (str_detect(coords, "\\|")) {
            coords_anchor <- str_split(coords, "\\|", n=2)[[1]]
            coords <- coords_anchor[1]
            location <- get_xy(coords_anchor[2], df, state, anchor_indices)
        } else if (str_detect(coords, "\\$")) {
            coords_anchor <- str_split(coords, "\\$", n=2)[[1]]
            coords <- coords_anchor[1]
            location <- get_xy(paste0("&", coords_anchor[2]), df, state, anchor_indices)
        } else {
            location <- NULL
        }
        xy <- as.numeric(str_extract_all(coords, "[0-9.-]+")[[1]]) * state$scale_factor
        if (is.null(location)) {
            if (is.null(anchor_indices))
                abort("Don't know where this location is relative to", class = "infer_location")
            list(x = xy[1] + df$x[anchor_indices], y = xy[2] + df$y[anchor_indices])
        } else {
            list(x = xy[1] + location$x, y = xy[2] + location$y)
        }
    } else if (str_detect(coords, "^[0-9]")) { # alternative relative moves
        coords <- convert_relative(coords)
        get_xy(coords, df, state, anchor_indices)
    } else {
        p <- piecepackr:::Point2D$new(x = get_x(coords), y = get_y(coords))
        p$dilate(state$scale_factor)
    }
    if (any(is.na(xy$x) | is.na(xy$y)))
        abort(paste("Failed to parse coordinates:", coords), class = "infer_location")
    xy
}

convert_relative <- function(coords) {
    if (str_detect(coords, "\\$")) {
        coords_anchor <- str_split(coords, "\\$", n=2)[[1]]
        coords <- convert_relative_helper(coords_anchor[1])
        paste0(coords, "$", coords_anchor[2])
    } else if (str_detect(coords, "\\|")) {
        coords_anchor <- str_split(coords, "\\|", n=2)[[1]]
        coords <- convert_relative_helper(coords_anchor[1])
        paste0(coords, "|", coords_anchor[2])
    } else {
        convert_relative_helper(coords)
    }
}
convert_relative_helper <- function(coords) {
    number <- as.numeric(str_extract(coords, "[0-9.]+"))
    direction <- str_extract(coords, "[A-Z]+")
    multiplier <- switch(EXPR = direction,
                         N = c(0, 1), E = c(1, 0), S = c(0, -1), W = c(-1, 0),
                         U = c(0, 1), R = c(1, 0), D = c(0, -1), L = c(-1, 0),
                         NE = c(1, 1), SE = c(1, -1), SW = c(-1, -1), NW = c(-1, 1),
                         UR = c(1, 1), DR = c(1, -1), DL = c(-1, -1), UL = c(-1, 1),
                         NNE = hex_xy(60, number), ENE = hex_xy(30, number),
                         ESE = hex_xy(330, number), SSE = hex_xy(300, number),
                         SSW = hex_xy(240, number), WSW = hex_xy(210, number),
                         WNW = hex_xy(150, number), NNW = hex_xy(120, number),
                         UUR = hex_xy(60, number), RUR = hex_xy(30, number),
                         RDR = hex_xy(330, number), DDR = hex_xy(300, number),
                         DDL = hex_xy(240, number), LDL = hex_xy(210, number),
                         LUL = hex_xy(150, number), UUL = hex_xy(120, number),
                         abort(str_glue("Don't know direction {direction}"),
                               class = "infer_location"))
    paste0("<", number * multiplier[1], ",", number * multiplier[2], ">")
}
hex_xy <- function(angle, number) c(to_x(angle, number), to_y(angle, number))

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

replace_macros <- function(df, text, state = create_state(df)) {
    ml <- str_locate_all(text, "`[^']+'")[[1]]
    for (r in rev(seq_len(nrow(ml)))) {
        mtext <- str_sub(text, ml[r, 1] + 1, ml[r, 2] - 1)
        if (is.null(state$macros[[mtext]]))
            abort(paste("Macro", mtext, "is unknown"), class = "identify_macro")
        str_sub(text, ml[r, 1], ml[r, 2]) <- state$macros[[mtext]]
    }
    text
}

process_move <- function(df, text, state = create_state(df)) {
    state$df_move_start <- df
    text <- replace_macros(df, text, state)
    text <- split_blanks(text)
    text <- str_trim(gsub("\\*", " *", text)) # allow a4-b5*c3*c4
    text <- str_trim(gsub("\\+", " +", text)) # allow a4-b5+
    text <- str_trim(gsub("!", " !", text)) # allow 3?Sa$>90!
    text <- gsub("\u035c|\u203f", "_", text) # convert underties to underscore
    text <- gsub("([0-9]+)\\?", "\\1&?", text) # convert n? to n&?
    moves <- split_blanks(text)
    for (move in moves) {
        df <- process_submove(df, move, state = state)
    }
    df
}

# nocov c("a b", "c d") -> c("a", "b", "c", "d")
split_blanks <- function(text) c(str_split(text, "[[:blank:]]+"), recursive = TRUE)

process_moves <- function(df, movelist, state = create_state(df)) {
    nms <- vector("character", 1L + length(movelist))
    nms[1] <- "SetupFn."
    if (!is.null(names(movelist))) nms[seq_along(movelist) + 1L] <- names(movelist)
    nms <- clean_comments(nms)

    df_list <- vector("list", 1L + length(movelist))
    df_list[[1L]] <- df
    for (i in seq_along(movelist)) {
        df <- tryCatch(process_move(df, movelist[[i]], state = state),
                       error = function(e) {
                           msg <- c(str_glue("Couldn't process move `{movenumber} {move}`",
                                             movenumber = nms[i + 1L], move = movelist[[i]]),
                                    i = e$message)
                           abort(msg, class = "process_move", parent = e)
                       })
        df_list[[i + 1L]] <- df
    }
    names(df_list) <- nms

    df_list
}

clean_comments <- function(nms) {
    for (i in which(str_detect(nms, "^\\.$"))) {
        nms[[i]] <- paste0(nms[[i-1L]], ".")
    }
    nms
}

flip_ps <- function(piece_side) {
    new <- piece_side
    p_s <- str_split(piece_side, "_")
    piece <- sapply(p_s, function(x) x[1])
    side <- sapply(p_s, function(x) x[2])
    new <- ifelse(side == "face", paste0(piece, "_back"), new)
    new <- ifelse(side == "back", paste0(piece, "_face"), new)
    new <- ifelse(side == "left", paste0(piece, "_right"), new)
    new <- ifelse(side == "right", paste0(piece, "_left"), new)
    new <- ifelse(side == "top", paste0(piece, "_base"), new)
    new <- ifelse(side == "base", paste0(piece, "_top"), new)
    new <- ifelse(piece == "pyramid", "pyramid_top", new)
    new <- ifelse(piece_side == "pyramid_top", "pyramid_face", new)
    new <- ifelse(piece_side == "die_face", "die_face", new)
    new
}
