#' Read ppn files
#'
#' Read in ppn files
#' @param file Filename
#' @param parse Logical of whether to parse the moves in the ppn file
#' @return A list, for each game in the file a list containing info about the game
read_ppn <- function(file, parse=TRUE) {
    list_ppns <- parse_ppn_file(file)
    lapply(list_ppns, parse_ppn_game, parse=parse)
}

# pmp <- function(x) { grid.newpage(); pmap_piece(x, default.units="in") }
# saveGIF({lapply(pmt1$dfs, pmp)}, movie.name="test.gif")

#' Animate a ppn game
#'
#' Animate a ppn game
#' @param game A list containing a parsed ppn game (as parsed by \code{read_ppn})
#' @param file Filename to save animation
#' @param ... Arguments to \code{pmap_piece}
#' @return Nothing, as a side effect saves an animation of ppn game
animate_game <- function(game, file, ...) {
    xranges <- lapply(game$dfs, xrange)
    # xmin <- min(sapply(xranges, function(x) x[1]), na.rm=TRUE)
    xmax <- max(sapply(xranges, function(x) x[2]), na.rm=TRUE)
    yranges <- lapply(game$dfs, yrange)
    # ymin <- min(sapply(yranges, function(y) y[1]), na.rm=TRUE)
    ymax <- max(sapply(yranges, function(y) y[2]), na.rm=TRUE)
    #### Adjust if xmin under 0
    #### Adjust for oblique projection
    #### Add grid and comment annotations
    m <- max(xmax, ymax) + 0.5
    res <- round(600 / m, 0)
    height <- res * (ymax+0.5)
    width <- res * (xmax+0.5) 
    plot_fn <- function(df, ...) {
        grid::grid.newpage()
        pmap_piece(df, default.units="in", ...)
    }
    animation::saveGIF({lapply(game$dfs, plot_fn, ...)}, movie.name=file,
        ani.height=height, ani.width=width, ani.res=res, ani.dev="png", ani.type="png")
    invisible(NULL)
}

#### Option to generate postcard?

#' Plot game move
#'
#' Plot game move
#' @param game A list containing a parsed ppn game (as parsed by \code{read_ppn})
#' @param move Which move to plot game state (will use \code{game$dfs[[move]]}).  
#'             If \code{NULL} will plot the game state after the last move.
#' @param file Filename to save graphic to.  If \dev{NULL} open new graphics device.
#' @param bg Background color (\code{"transparent")} for transparent
#' @param res For bitmap image formats the resolution 
#' @param ... Arguments to \code{pmap_piece}
#' @return Nothing, as a side effect saves a graphic
plot_move <- function(game, move=NULL, file=NULL,  bg="white", res=72, ...) {
    if (is.null(move)) {
        df <- tail(game$dfs, 1)[[1]]
    } else {
        df <- game$dfs[[move]]
    }
    width <- xrange(df)[2] + 0.5
    height <- yrange(df)[2] + 0.5

    if (is.null(file)) {
        dev.new(width=width, height=height, unit="in", noRstudioGD=TRUE)
        pmap_piece(df, default.units="in", ...)
    } else {
        format <- tools::file_ext(file)
        switch(format,
               bmp = bmp(file, width, height, "in", res=res, bg=bg),
               jpeg = jpeg(file, width, height, "in", res=res, bg=bg),
               pdf = cairo_pdf(file, width, height, bg=bg),
               png = png(file, width, height, "in", res=res, bg=bg),
               ps = cairo_ps(file, width, height, bg=bg),
               svg = svg(file, width, height, bg=bg),
               tiff = tiff(file, width, height, "in", res=res, bg=bg))
        pmap_piece(df, default.units="in", ...)
        dev.off()
    }
    invisible(NULL)
}

#' Parse ppn files
#'
#' Parses ppn file 
#' @param file Filename 
#' @return A list, each element is a character vector containing the text of the PPN games within that file
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

#' Parse ppn game
#'
#' Parses (single) ppn game text to get Metadata and Movetext
#' @param text Character vector of ppn game text
#' @return A list with a named list element named \code{Metadata} 
#'         and character vector element named \code{Movetext}
parse_ppn_game <- function(text, parse=TRUE) {
    yaml_end <- grep("^\\.{3}", text)
    if (length(yaml_end) == 0) {
        yaml_end <- grep("^[[:blank:]]+|^$", text)
    }
    if (length(yaml_end) > 0) {
        metadata <- yaml::yaml.load(text[1:yaml_end[1]])
        movetext <- text[(yaml_end[1]+1):length(text)]
    } else {
        metadata <- list()
        movetext <- text
    }
    game_list <- list(metadata=metadata, movetext=movetext)
    if (parse) {
        #### Get starting df
        df <- tibble::tibble()
        move_list <- parse_moves(movetext, df=df)
        game_list <- c(game_list, move_list)
    }
    game_list
}


#' Parse Movetext by Move number
#'
#' Parse Movetext by Move number
#' @param text Character vector of Movetext
#' @param df Data frame containing starting state (inferred from Metadata)
#' @return A list with element \code{moves} containing 
#'     named list (by move number) of move text and element \code{comments}
#'     containing named list (by move number) of comments
parse_moves <- function(text, df=tibble::tibble()) {
    #### Convert # comments into braces?
    text <- stringr::str_squish(paste(text, collapse=" "))
    # (?![^\\{]*\\}) is a negative lookahead assertion to not capture moves in comment braces
    # (?![[:digit:]]) is a negative lookahead assertion to not capture floating numbers
    token <- "[[:alnum:]_]+\\.+(?![[:digit:]])(?![^\\{]*\\})"
    locations <- stringr::str_locate_all(text, token)[[1]]
    nr <- nrow(locations)
    moves_raw <- list()
    if (nr == 0) {
        moves_raw[[1]] <- text
    } else {
        i1 <- locations[1,1] 
        if (i1 > 1) {
            moves_raw[[1]] <- stringr::str_sub(text, 1, i1-2)
        } 
        for (ii in seq(nr)) {
            is <- locations[ii,1]
            ie <- locations[ii,2]
            movenumber <- stringr::str_sub(text, is, ie)
            is <- locations[ii,2]+2
            if(ii < nr) 
                ie <- locations[ii+1,1]-2
            else
                ie <- stringr::str_count(text)
            moves_raw[[movenumber]] <- stringr::str_sub(text, is, ie) 
        }
    }
    moves <- lapply(moves_raw, remove_comments)
    comments <- lapply(moves_raw, extract_comments)
    dfs <- process_moves(df, moves)
    moves <- c(list(SetupFn.=""), moves)
    comments <- c(list(SetupFn.=""), comments)
    list(moves=moves, comments=comments, dfs=dfs)
}

ctoken <- "\\{[^}]*\\}+?"
extract_comments <- function(text) {
    text <- paste(stringr::str_extract_all(text, ctoken)[[1]], collapse=" ")
    stringr::str_squish(stringr::str_remove_all(text, "\\{|\\}"))
}
remove_comments <- function(text) {
    stringr::str_squish(stringr::str_remove_all(text, ctoken))
}

parse_simplified_piece <- function(text) {
    suit <- get_simplified_suit(text)
    rank <- get_simplified_rank(text)
    angle <- get_simplified_angle(text)
    ps <- get_simplified_ps(text, suit, rank)
    tibble(piece_side=ps, suit=suit, rank=rank, angle=angle)
}
get_simplified_suit <- function(text) {
    if (grepl("S", text)) {
        1
    } else if (grepl("M", text)) {
        2 
    } else if (grepl("C", text)) {
        3
    } else if (grepl("A", text)) {
        4
    } else {
        NA_integer_
    }
}
get_simplified_rank <- function(text) {
    if (grepl("n", text)) {
        1
    } else if (grepl("a", text)) {
        2
    } else if (grepl("2", text)) {
        3
    } else if (grepl("3", text)) {
        4
    } else if (grepl("4", text)) {
        5
    } else if (grepl("5", text)) {
        6
    } else {
        NA_integer_
    }
}
get_simplified_angle <- function(text) {
    if (grepl("<", text)) {
        90
    } else if (grepl("v", text)) {
        180
    } else if (grepl(">", text)) {
        270
    } else {
        0
    }
}
get_simplified_piece <- function(text, suit, rank) {
    if (grepl("t", text)) {
        "tile"
    } else if (grepl("c", text)) {
        "coin"
    } else if (grepl("d", text)) {
        "die"
    } else if (grepl("p", text)) {
        "pawn"
    } else {
        if(!is.na(suit) && !is.na(rank)) {
            "tile"
        } else if (!is.na(suit) || !is.na(rank)) {
            "coin"
        } else {
            "tile"
        }
    }
}
get_simplified_ps <- function(text, suit, rank) {
    piece <- get_simplified_piece(text, suit, rank)
    side <- if (grepl("f", text)) {
        "face"
    } else if (grepl("b", text)) {
        "back"
    } else {
        switch(piece,
               tile = ifelse(is.na(suit) || is.na(rank), "back", "face"),
               coin = ifelse(is.na(suit), "face", "back"),
               "face")
    }
    paste(piece, side, sep="_")
}

get_algebraic_x <- function(text) {
    ss <- stringr::str_extract(text, "[[:lower:]]+")   
    ndigits <- stringr::str_count(ss)
    int <- 0
    for (ii in rev(seq(ndigits))) {
        int <- int + 26^(ii-1) * match(stringr::str_sub(ss, ii, ii), letters)
    }
    int
}
get_algebraic_y <- function(text) {
    as.numeric(stringr::str_extract(text, "[[:digit:]]+"))
}

process_move <- function(df, text) {
    if (text == "") {
        df
    } else if (grepl("@", text)) {
        process_at_move(df, text)
    } else if (grepl("'", text)) {
        process_apostrophe_move(df, text)
    } else if (grepl("*", text)) {
        process_asterisk_move(df, text)
    } else if (grepl("-", text)) {
        process_hyphen_move(df, text)
    } else {
        stop(paste("Don't know how to handle move", text))
    }
}

process_at_move <- function(df, text) {
    pc <- stringr::str_split(text, "@")[[1]]
    piece <- pc[1]
    coords <- pc[2]
    #### handle complicated piece and coords
    df_piece <- parse_simplified_piece(piece)
    xy <- get_xy(coords)
    df_piece$x <- xy[1]
    df_piece$y <- xy[2]
    #### get index for piece restriction
    index <- nrow(df)
    insert_df(df, df_piece, index)
}

aef <- function(x,y) { isTRUE(all.equal(x,y)) }
process_apostrophe_move <- function(df, text) {
    coords <- gsub("'", "", text)
    index <- get_index_from_coords(df, coords)
    df[-index,]
}
get_index_from_coords <- function(df, coords) {
    xy <- get_xy(coords)
    indices <- sapply(df$x, aef, xy[1]) & sapply(df$y, aef, xy[2])
    #### get index for piece restriction
    index <- tail(which(indices), 1)
    index
}

process_hyphen_move <- function(df, text) {
    cc <- stringr::str_split(text, "-")[[1]]
    index <- get_index_from_coords(df, cc[1])
    new_xy <- get_xy(cc[2])
    df[index,"x"] <- new_xy[1]
    df[index,"y"] <- new_xy[2]
    df
}

process_asterisk_move <- function(df, text) {
    cc <- stringr::str_split(text, "\\*")[[1]]
    df <- process_apostrophe_move(df, paste0(cc[2], "'"))
    df <- process_hyphen_move(df, gsub("\\*", "-", text))
    df
}

# Insert `df2` into `df1` after `index`
# index = 0 means instead at beginning
insert_df <- function(df1, df2, index=nrow(df1)) {
    if (index == 0) {
        bind_rows(df2, df1)
    } else if (index == nrow(df1)) {
        bind_rows(df1, df2)
    } else {
        bind_rows(df1[seq(index),], df2, df1[-seq(index),])
    }
}

get_xy <- function(coords) {
    c(get_x(coords), get_y(coords))
}

get_x <- function(coords) {
    if (grepl(",", coords)) {
        as.numeric(gsub("\\(|,.*", "", coords))
    } else {
        get_algebraic_x(coords)
    }
}
get_y <- function(coords) {
    if (grepl(",", coords)) {
        as.numeric(gsub(")|.*,", "", coords))
    } else {
        get_algebraic_y(coords)
    }
}

process_moveline <- function(df, text) {
    moves <- stringr::str_split(text, "[[:blank:]]+")[[1]]
    for(move in moves) {
        df <- process_move(df, move)
    }
    df
}

process_moves <- function(df, movelist) {
    df_list <- list(SetupFn.=df)
    nms <- names(movelist)
    if(is.null(nms)) { nms <- 1 + seq(movelist) }
    for(ii in seq(movelist)) {
        df <- process_moveline(df, movelist[[ii]])
        df_list[[nms[ii]]] <- df
    }
    df_list
}
