#' @importFrom knitr knit opts_chunk knit_hooks
#' @import grid
#' @import piecepackr
#' @importFrom tibble tibble tribble
#' @importFrom tools file_ext


xelatex <- function(tex, quietly = TRUE) {
    stdout <- if (quietly) NULL else ""
    pdf <- sub(paste0(file_ext(tex), "$"), "pdf", tex)
    system2("xelatex", c(tex), stdout = stdout)
    system2("xelatex", c(tex), stdout = stdout)
    pdf
}

# nolint start
# to_tex <- function(kf) {
#     tex <- sub(paste0(file_ext(kf), "$"), "tex", kf)
#     system2("pandoc", args = c("-o", tex, kf))
#     tex
# }
# to_pdf <- function(kf) {
#     pdf <- sub(paste0(file_ext(kf), "$"), "pdf", kf)
#     system2("pandoc", args = c("-o", pdf, "--pdf-engine=xelatex", kf))
#     pdf
# }
# nolint end

set_knitr_opts <- function(name) {
    hook_plot <- knit_hooks$get("plot")
    knit_hooks$set(plot = function(x, options) {
      x <- paste(c(x, "whatever"), collapse = ".")
      hook_plot(x, options)
    })
    opts_chunk$set(dev = "cairo_pdf",
                  fig.align = "center",
                  fig.path = paste0(name, "-"),
                  fig.pos = "ht!",
                  fig.lp = paste0("fig:", name, "-"))
}

clean_game <- function(game) {
    gsub("_", "-", to_varname(game))
}

#' Save ruleset and/or rulebook
#'
#' \code{save_ruleset} save ruleset of a game,
#' \code{save_pamphlet} is a variant that saves the ruleset as a pamphlet.
#' \code{save_rulebook} saves a rulebook.
#'
#' @param game Game name
#' @param gk A \code{game_kit} R6 object.
#' @param output Path to the output file.
#'        If \code{NULL} the function will guess a default.
#' @param size Paper size (either "letter", or "A4")
#' @param quietly Whether to hide document compilation output.
#' @param duplex_edge String specifying the desired duplex printing edge.
#'       If "short" match the second page along its short edge (second page flipped up, easier to preview on computer)
#'       and if "long" match along its long edge (second page flipped upside down, usual printer default).
#' @rdname save_ruleset
#' @export
save_ruleset <- function(game, gk = game_kit(), output = NULL,
                         quietly = TRUE, size = "letter") {

    game <- clean_game(game)
    if (is.null(output)) output <- paste0(game, ".pdf")
    if (!exists(output)) file.create(output)
    output <- normalizePath(output)

    wd <- getwd()
    on.exit(setwd(wd))
    setwd(tempdir())

    knit_game(game, gk, quietly, size)

    of <- system.file("extdata/ruleset.Rtex", package = "ppgames")
    tex <- knit(of, quiet = quietly)
    pdf <- xelatex(tex, quietly)

    file.copy(pdf, output, overwrite = TRUE)
    invisible(NULL)
}

#' @rdname save_ruleset
#' @export
save_pamphlet <- function(game, gk = game_kit(), output = NULL,
                          quietly = TRUE, size = "letter", duplex_edge = "short") {

    game <- clean_game(game)
    if (is.null(output)) output <- paste0(game, ".pdf")
    if (!exists(output)) file.create(output)
    output <- normalizePath(output)

    # create canonical image
    cfile <- file.path(tempdir(), paste0(gsub("-", "_", game), "_canonical.pdf"))
    cwhf <- plot_canonical_image(game, gk, cfile)

    wd <- getwd()
    on.exit(setwd(wd))
    setwd(tempdir())

    knit_game(game, gk, quietly, size, is_pamphlet = TRUE)

    of <- system.file("extdata/pamphlet.Rtex", package = "ppgames")
    tex <- knit(of, quiet = quietly)
    pdf <- xelatex(tex, quietly)

    file.copy(pdf, output, overwrite = TRUE)
    invisible(NULL)
}

game_info <- yaml::yaml.load_file(system.file("extdata", "game_info.yaml", package = "ppgames"))

knit_chapter <- function(game, gk = game_kit(), quietly = TRUE, size = "letter") {
    output <- paste0(game, "-chapter.tex")

    wd <- getwd()
    on.exit(setwd(wd))
    setwd(tempdir())

    knit_game(game, gk, quietly, size)

    of <- system.file("extdata/chapter.Rtex", package = "ppgames")
    tex <- knit(of, quiet = quietly)

    file.copy(tex, output, overwrite = TRUE)
    invisible(NULL)
}

knit_game <- function(game, gk, quietly = TRUE, size = "letter", is_pamphlet = FALSE) {
    game_files <- list.files(system.file("games", package = "ppgames"),
                             full.names = TRUE)
    of <- game_files[grep(game, game_files)]
    knit(of, quiet = quietly)
}

#' @param book Book name
#' @rdname save_ruleset
#' @export
save_rulebook <- function(book = "The Historical Piecepacker", gk = game_kit(), output = NULL,
                          quietly = TRUE, size = "letter") {

    book <- clean_game(book)
    if (is.null(output)) output <- paste0(book, ".pdf")
    if (!exists(output)) file.create(output)
    output <- normalizePath(output)

    wd <- getwd()
    on.exit(setwd(wd))
    setwd(tempdir())

    games <- list.files(system.file("games", package = "ppgames"), pattern = ".Rtex")
    games <- gsub(".Rtex", "", games)
    for (game in games) {
        knit_chapter(game, gk, quietly, size)
    }

    of <- system.file(sprintf("books/%s.Rtex", book), package = "ppgames")
    tex <- knit(of, quiet = quietly)
    pdf <- xelatex(tex, quietly)

    file.copy(pdf, output, overwrite = TRUE)
    invisible(NULL)
}

clean_fn <- function(cleaned, x) {
    if (cleaned$prev == -1) # initialize
        return(list(prev = x, val = as.character(x)))
    if (near(x - cleaned$prev, 1)) { # sequence
        if (str_sub(cleaned$val, -2L, -2L) != "-") {
            return(list(prev = x, val = paste0(cleaned$val, "--", x)))
        } else {
            str_sub(cleaned$val, -1L, -1L) <- x
            cleaned$prev <- x
            return(cleaned)
        }
    }
    list(prev = x, val = paste0(cleaned$val, ", ", x))
}

clean_n_players <- function(players) {
    players <- unique(players)
    cleaned <- list(prev = -1, val = "")
    Reduce(clean_fn, players, cleaned)$val
}

game_data <- function(game) {
    info <- get_game_info(game)
    items <- list()
    items$Players <- clean_n_players(info$players)
    items$Length <- game_length(info$length)
    equipment <- if (is.null(info$equipment)) "one standard piecepack" else info$equipment
    items$Equipment <- equipment
    items$Version <- sprintf("%s (%s)", info$version, info$version_date)
    cat(paste(c("\\begin{description}",
                sprintf("\\item[%s] %s", names(items), as.character(items)),
                "\\end{description}\n"),
          collapse="\n"))
}

game_credits <- function(game) {
    info <- get_game_info(game)
    items <- list()
    if ("author" %in% names(info)) items$`Written by:` <- info$author
    items$`Game design:` <- info$designer
    license <- if (is.null(info$license)) "CC-BY-SA-4" else info$license
    license_text <- sprintf("\\href{%s}{%s}", license_urls[[license]], license_names[[license]])
    copyright <- paste0("\\copyright~", info$copyright, "\\newline")
    license <- paste(copyright, "Some Rights Reserved.\\newline",
                     paste("License: ", license_text, "\\newline"),
                     collapse="\n")
    cat(paste(c("\\begin{description}",
                sprintf("\\item[%s] %s", names(items), as.character(items)),
                "\\end{description}\n"),
          collapse="\n"))
    cat(license, "\n")
}

game_length <- function(gl) {
    if (length(gl) == 2)
        sprintf("%s--%s minutes", gl[1], gl[2])
    else
        paste(gl, "minutes")
}

get_game_info <- function(game) {
    if (is.null(game)) return(list())
    game <- gsub("-", "_", game)
    game_info[[game]]
}

title <- function(game) {
    if (is.null(game)) return(NULL)
    info <- get_game_info(game)
    if (is.null(info$title)) {
        stringr::str_to_title(gsub("_|-", " ", game))
    } else {
        info$title
    }
}
author <- function(game) {
    info <- get_game_info(game)
    if (is.null(info$author)) {
        "Trevor L. Davis"
    } else {
        info$author
    }
}
keywords <- function(game) {
    info <- get_game_info(game)
    if (is.null(info$author)) {
        "piecepack,board games"
    } else {
        info$keywords
    }
}
subject <- function(game) {
    info <- get_game_info(game)
    if (is.null(info$subject)) {
        paste0("Learn how to play the board game ", title(game), ".")
    } else {
        info$subject
    }
}

pdf_key <- function(name, value) {
    if (is.null(value)) {
        NULL
    } else {
        paste0(name, "={", value, "},%")
    }
}

pdf_metadata <- function(game = NULL, ...,
                         pdftitle = NULL, pdfauthor = NULL,
                         pdfsubject = NULL, pdfkeywords = NULL) {
    if (is.null(pdftitle)) pdftitle <- title(game)
    if (is.null(pdfauthor)) pdfauthor <- author(game)
    if (is.null(pdfsubject)) pdfsubject <- subject(game)
    if (is.null(pdfkeywords)) pdfkeywords <- keywords(game)
cat(paste0("\\hypersetup{", pdf_key("pdftitle", pdftitle)),
    pdf_key("pdfauthor", pdfauthor),
    pdf_key("pdfcreator", paste0("ppgames (v", packageVersion("ppgames"), ")")),
    pdf_key("pdfsubject", pdfsubject),
    pdf_key("pdfkeywords", pdfkeywords),
    "}", sep = "\n")
}

external_links <- function(game, list_type = "itemize") {
    links <- list()
    info <- get_game_info(game)
    if ("ppwiki" %in% names(info)) {
        links[["Piecepack Wiki"]] <- paste0("http://www.ludism.org/ppwiki/", info$ppwiki)
    }
    if ("boardgamegeek" %in% names(info)) {
        links[["BoardGameGeek"]] <- paste0("https://boardgamegeek.com/boardgame/", info$boardgamegeek)
    }
    if ("chessvariants" %in% names(info)) {
        links[["The Chess Variants Pages"]] <- paste0("https://www.chessvariants.com/", info$chessvariants)
    }
    if ("cyningstan" %in% names(info)) {
        links[["Cyningstan"]] <- paste0("http://www.cyningstan.com/game/", info$cyningstan)
    }
    if ("wikipedia" %in% names(info)) {
        links[["Wikipedia"]] <- paste0("https://en.wikipedia.org/wiki/", info$wikipedia)
    }
    if ("external_links" %in% names(info)) {
        for (n in names(info$external_links))
            links[[n]] <- info$external_links[[n]]
    }
    if (list_type == "description") {
        items <- paste("\\item[%s] \\url{%s}", names(links), as.character(links))
        cat(paste(c("\\begin{description}", items, "\\end{description}\n"),
              collapse="\n"))
    } else {
        items <- paste("\\item", href(as.character(links)))
        cat(paste(c("\\begin{itemize}", items, "\\end{itemize}\n"),
              collapse="\n"))
    }
}

latex_url_name <- function(url) {
    name <- gsub("https*://", "", url)
    name <- gsub("%", "\\\\%", name)
    name <- gsub("#", "\\\\#", name)
    name <- gsub("_", "\\\\_", name)
    name
}

# url <- function(url) sprintf("\\url{%s}", name) # nolint
href <- function(url, name=NULL) {
    if (is.null(name)) name <- latex_url_name(url)
    sprintf("\\href{%s}{%s}", url, name)
}
