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

to_latex <- function(kf) {
    stopifnot(Sys.which("pandoc") != "")
    tex <- sub(paste0(file_ext(kf), "$"), "tex", kf)
    system2("pandoc", args = c("-o", tex, kf))
    tex
}

to_output <- function(lf, output, cmd_options = c("--standalone", "--self-contained")) {
    stopifnot(Sys.which("pandoc") != "")
    system2("pandoc", args = c(cmd_options,  "-o", output, lf))
    output
}

# nolint start
# to_pdf <- function(kf) {
#     pdf <- sub(paste0(file_ext(kf), "$"), "pdf", kf)
#     system2("pandoc", args = c("-o", pdf, "--pdf-engine=xelatex", kf))
#     pdf
# }
# nolint end

#' @importFrom knitr knit
set_knitr_opts <- function(name, output_ext = "pdf", wd = getwd()) {
    # nolint start
    # hook_plot <- knit_hooks$get("plot")
    # knit_hooks$set(plot = function(x, options) {
    #   x <- paste(c(x, "whatever"), collapse = ".")
    #   hook_plot(x, options)
    # })
    # nolint end
    if (output_ext == "pdf")
        dev <- "cairo_pdf"
    else
        dev <- "png"
    knitr::opts_chunk$set(dev = dev,
                          echo = FALSE,
                          fig.align = "center",
                          fig.path = paste0(name, "-"),
                          fig.pos = "ht!",
                          fig.lp = paste0("fig:", name, "-"))
    invisible(NULL)
}

#' Save ruleset
#'
#' \code{save_ruleset} save ruleset of a game.
#' \code{save_pamphlet} is a variant that saves the ruleset as a (tri-fold) pamphlet
#' while \code{save_pocketmod} is a variant that saves the rulest as a \dQuote{pocketmod} booklet.
#'
#' @param game Game name to generate ruleset for.  See [names_rulesets()].
#'             Will be normalized by [normalize_name()].
#' @param gk A \code{game_kit} R6 object.
#' @param output Path to the output file.
#'        If \code{NULL} the function will guess a default.
#' @param quietly Whether to hide document compilation output.
#' @param size Paper size (either "letter", or "A4").
#' @param ... Ignored
#' @param duplex_edge String specifying the desired duplex printing edge.
#'       If "short" match the second page along its short edge (second page flipped up, easier to preview on computer)
#'       and if "long" match along its long edge (second page flipped upside down, usual printer default).
#' @param game_info List with game info.  If `NULL` then we use
#'                  `yaml::yaml.load_file(system.file("extdata/game_info.yaml", package = "ppgames"))`.
#' @param game_files Character vector of (full path to) "Rtex" game rules.  If `NULL` then we use
#'                  `list.files(system.file("rules", package = "ppgames"), full.names = TRUE)`.
#' @param cmd_options Options passed to `pandoc` when using non-pdf output formats.
#'                    If `NULL` we try to guess a good set of options.
#' @rdname save_ruleset
#' @seealso See <https://pocketmod.com/> for more information about \dQuote{pocketmod} booklets including folding instructions.
#' @export
save_ruleset <- function(game, gk = game_kit(), output = NULL,
                         quietly = TRUE, size = "letter",
                         ..., game_info = NULL, game_files = NULL,
                         cmd_options = NULL) {

    cmd_options <- cmd_options %||% c("--standalone", "--self-contained", "--metadata=lang:en-US")

    game_info <- game_info %||%
        yaml::yaml.load_file(system.file("extdata/game_info.yaml", package = "ppgames"))
    game_files <- game_files %||%
        list.files(system.file("rules", package = "ppgames"), full.names = TRUE)
    game_files <- normalizePath(game_files)

    game <- normalize_name(game, sep = "-")
    size <- tolower(size)
    if (is.null(output)) output <- paste0(game, ".pdf")
    if (!exists(output)) file.create(output)
    output <- normalizePath(output)
    output_ext <- file_ext(output)

    dir <- setup_tempdir(output)
    wd <- setwd(dir)
    on.exit(setwd(wd))
    # on.exit(unlink(dir, recursize = TRUE))

    knit_game(game, gk, quietly, size,
              game_files = game_files, output_ext = output_ext, wd = dir)

    of <- system.file("templates/ruleset.Rtex", package = "ppgames")
    tex <- knit(of, quiet = quietly)
    if (output_ext == "pdf") {
        pdf <- xelatex(tex, quietly)
        embed_xmp(pdf, game, game_info)
        file.copy(pdf, output, overwrite = TRUE)
    } else {
        if (quietly)
            cmd_options <- c(cmd_options, "--quiet")
        else
            cmd_options <- c(cmd_options, "--verbose")
        to_output(tex, output, cmd_options)
    }
    invisible(NULL)
}

setup_tempdir <- function(output) {
    dir <- file.path(tempdir(), paste0(basename(output), "_tempdir"))
    unlink(dir, recursive = TRUE)
    dir.create(dir)
    dir
}

#' @param save_promo_fn A function with arguments `game`, `gk`, and `file`
#'                        that saves a promo image for `game`.
#'                        Defaults to [save_promo_image()].
#' @rdname save_ruleset
#' @export
save_pamphlet <- function(game, gk = game_kit(), output = NULL,
                          quietly = TRUE, size = "letter",
                          duplex_edge = "short",
                          ...,
                          game_info = NULL,
                          game_files = NULL,
                          save_promo_fn = save_promo_image) {

    game_info <- game_info %||%
        yaml::yaml.load_file(system.file("extdata/game_info.yaml", package = "ppgames"))
    game_files <- game_files %||%
        list.files(system.file("rules", package = "ppgames"), full.names = TRUE)
    game_files <- normalizePath(game_files)

    game_under <- normalize_name(game, sep = "_")
    game <- normalize_name(game, sep = "-")
    size <- tolower(size)
    if (is.null(output)) output <- paste0(game, ".pdf")
    if (!exists(output)) file.create(output)
    output <- normalizePath(output)
    output_ext <- file_ext(output)

    # create promo image
    cfile <- file.path(tempdir(), paste0(game_under, "_promo.pdf"))
    cwhf <- save_promo_fn(game_under, gk, cfile)

    dir <- setup_tempdir(output)
    wd <- setwd(dir)
    on.exit(setwd(wd))
    # on.exit(unlink(dir, recursize = TRUE))

    knit_game(game, gk, quietly, size,
              is_pamphlet = TRUE, game_files = game_files, output_ext = "pdf", wd = dir)

    of <- system.file("templates/pamphlet.Rtex", package = "ppgames")
    tex <- knit(of, quiet = quietly)
    if (output_ext == "pdf") {
        pdf <- xelatex(tex, quietly)
        embed_xmp(pdf, game, game_info)
        file.copy(pdf, output, overwrite = TRUE)
    } else {
        abort(str_glue('Can\'t handle "{output_ext}" output yet.'))
    }
    invisible(NULL)
}

#' @rdname save_ruleset
#' @export
save_pocketmod <- function(game, gk = game_kit(), output = NULL,
                          quietly = TRUE, size = "letter",
                          duplex_edge = "short",
                          ...,
                          game_info = NULL,
                          game_files = NULL,
                          save_promo_fn = save_promo_image) {

    game_info <- game_info %||%
        yaml::yaml.load_file(system.file("extdata/game_info.yaml", package = "ppgames"))
    game_files <- game_files %||%
        list.files(system.file("rules", package = "ppgames"), full.names = TRUE)
    game_files <- normalizePath(game_files)

    game_under <- normalize_name(game, sep = "_")
    game <- normalize_name(game, sep = "-")
    size <- tolower(size)
    stopifnot(size %in% c("a4", "letter"))
    if (is.null(output)) output <- paste0(game, ".pdf")
    if (!exists(output)) file.create(output)
    output <- normalizePath(output)
    output_ext <- file_ext(output)

    # create promo image
    cfile <- file.path(tempdir(), paste0(game_under, "_promo.pdf"))
    cwhf <- save_promo_fn(game_under, gk, cfile)

    dir <- setup_tempdir(output)
    wd <- setwd(dir)
    on.exit(setwd(wd))
    # on.exit(unlink(dir, recursize = TRUE))

    knit_game(game, gk, quietly, size,
              is_pamphlet = TRUE, game_files = game_files,
              game_info = game_info, output_ext = output_ext, wd = dir)

    # We'll use three passes to generate "pocketmod" booklet
    # See https://nilisnotnull.blogspot.com/2013/07/latex-pocketmod.html
    of <- system.file("templates/pocketmod1.Rtex", package = "ppgames")
    tex <- knit(of, quiet = quietly)
    pdf <- xelatex(tex, quietly)

    of <- system.file("templates/pocketmod2.Rtex", package = "ppgames")
    tex <- knit(of, quiet = quietly)
    pdf <- xelatex(tex, quietly)

    of <- system.file("templates/pocketmod3.Rtex", package = "ppgames")
    tex <- knit(of, quiet = quietly)
    pdf <- xelatex(tex, quietly)

    if (output_ext == "pdf") {
        embed_xmp(pdf, game, game_info)
        file.copy(pdf, output, overwrite = TRUE)
    } else {
        abort(str_glue('Can\'t handle "{output_ext}" output yet.'))
    }

    invisible(NULL)
}

knit_chapter <- function(game, gk = game_kit(), quietly = TRUE, size = "letter",
                         ..., game_info = NULL, game_files = NULL, output_ext = "pdf") {
    # game should have been pre-cleaned by `normalize_name(game, sep = "-")`
    output <- paste0(game, "-chapter.tex")

    game_files <- game_files %||%
        list.files(system.file("rules", package = "ppgames"), full.names = TRUE)
    game_files <- normalizePath(game_files)

    knit_game(game, gk, quietly, size,
              game_info = game_info, game_files = game_files, output_ext = output_ext,
              wd = getwd())

    of <- system.file("templates/chapter.Rtex", package = "ppgames")
    tex <- knit(of, quiet = quietly)

    file.copy(tex, output, overwrite = TRUE)
    invisible(NULL)
}

knit_game <- function(game, gk, quietly = TRUE, size = "letter",
                      ...,
                      is_pamphlet = FALSE, game_info = NULL,
                      game_files = NULL, output_ext = "pdf", wd = getwd()) {
    # game should have been pre-cleaned by `normalize_name(game, sep = "-")`
    stopifnot(any(grepl(game, game_files)))
    of <- game_files[grep(game, game_files)]
    knitr::opts_knit$set(root.dir = wd)
    on.exit(knitr::opts_knit$set(root.dir = NULL))
    filename <- knit(of, quiet = quietly)
    if (file_ext(filename) != "tex")
        filename <- to_latex(filename)
    invisible(filename)
}

#' Save rulebook
#'
#' \code{save_rulebook} saves a rulebook.
#'
#' @inheritParams save_ruleset
#' @param book Book name.  Currently only supports "The Historical Piecepacker".
#' @export
save_rulebook <- function(book = "The Historical Piecepacker", gk = game_kit(), output = NULL,
                          quietly = TRUE, size = "letter",
                          cmd_options = NULL) {

    cmd_options <- cmd_options %||% c("--standalone", "--self-contained", "--metadata=lang:en-US",
                                          "--table-of-contents", "--toc-depth=3")

    game_info <- NULL # later make a function argument?
    game_info <- game_info %||%
        yaml::yaml.load_file(system.file("extdata/game_info.yaml", package = "ppgames"))
    game_files <- NULL # later make a function argument
    game_files <- game_files %||%
        list.files(system.file("rules", package = "ppgames"), full.names = TRUE)
    game_files <- normalizePath(game_files)

    size <- tolower(size)
    book_hyphen <- normalize_name(book, sep = "-")
    if (is.null(output)) output <- paste0(book_hyphen, ".pdf")
    if (!exists(output)) file.create(output)
    output <- normalizePath(output)
    output_ext <- file_ext(output)

    dir <- setup_tempdir(output)
    wd <- setwd(dir)
    on.exit(setwd(wd))
    # on.exit(unlink(dir, recursize = TRUE))

    ## games <- list.files(system.file("rules", package = "ppgames"), pattern = ".Rtex")
    ## games <- gsub(".Rtex", "", games)
    games <- names_rulesets(book = book) %>% normalize_name(sep = "-")
    for (game_hyphen in games) {
        knit_chapter(game_hyphen, gk, quietly, size,
                     game_info = game_info, output_ext = output_ext)
    }

    ## new argument book files?
    of <- system.file(str_glue("books/{book_hyphen}.Rtex"), package = "ppgames")
    tex <- knit(of, quiet = quietly)
    if (output_ext == "pdf") {
        pdf <- xelatex(tex, quietly)
        file.copy(pdf, output, overwrite = TRUE)
    } else {
        if (quietly)
            cmd_options <- c(cmd_options, "--quiet")
        else
            cmd_options <- c(cmd_options, "--verbose")
        to_output(tex, output, cmd_options)
    }
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

game_data <- function(game, game_info = NULL) {
    info <- get_game_info(game, game_info)
    items <- list()
    items$Players <- clean_n_players(info$players)
    items$Length <- game_length(info$length)
    equipment <- if (is.null(info$equipment)) "one standard piecepack" else info$equipment
    items$Equipment <- equipment
    items$Version <- str_glue("{info$version} ({info$version_date})")
    cat(paste(c("\\begin{description}",
                str_glue("\\item[{names(items)}] {as.character(items)}"),
                "\\end{description}\n"),
          collapse="\n"))
}

game_credits <- function(game, game_info = NULL) {
    info <- get_game_info(game, game_info)
    items <- list()
    if ("author" %in% names(info)) items$`Written by:` <- info$author
    items$`Game design:` <- info$designer
    license <- info$license %||% "CC-BY-SA-4.0"
    stopifnot(license %in% piecepackr::spdx_license_list$id)
    license_name <- piecepackr::spdx_license_list[license, "name"]
    license_url <- piecepackr::spdx_license_list[license, "url_alt"]
    if (is.na(license_url))
        license_url <- piecepackr::spdx_license_list[license, "url"]
    license_text <- str_glue("\\href{{{license_url}}}{{{license_name}}}")
    copyright <- paste0(info$copyright, "\\newline")
    license <- paste(copyright,
                     paste("License: ", license_text, "\\newline"),
                     collapse="\n")
    cat(paste(c("\\begin{description}",
                str_glue("\\item[{names(items)}] {as.character(items)}"),
                "\\end{description}\n"),
          collapse="\n"))
    cat(license, "\n")
}

embed_xmp <- function(file, game, game_info = NULL) {
    info <- get_game_info(game, game_info)
    x <- xmpdf::xmp()
    x$creator <- author(game, game_info)
    x$description <- subject(game, game_info)
    x$rights <- info$copyright
    x$spdx_id <- info$license %||% "CC-BY-SA-4.0"
    x$subject <- strsplit(keywords(game, game_info), ", ")[[1]]
    x$title <- title(game, game_info)

    if (requireNamespace("xmpdf", quietly = TRUE)) {
        xmpdf::set_xmp(x, file)
    } else if (!isFALSE(getOption("piecepackr.metadata.inform"))) {
        msg <- c(x = "Unable to embed pdf XMP metadata",
                 xmpdf::enable_feature_message("set_xmp"),
                 i = "These messages can be disabled via `options(piecepackr.metadata.inform = FALSE)`.")
        rlang::inform(msg, class = "piecepackr_embed_metadata")
    }
}

game_length <- function(gl) {
    if (length(gl) == 2)
        str_glue("{gl[1]}--{gl[2]} minutes")
    else
        paste(gl, "minutes")
}

get_game_info <- function(game, game_info = NULL) {
    if (is.null(game)) return(list())
    game <- gsub("-", "_", game)
    game_info <- game_info %||%
        yaml::yaml.load_file(system.file("extdata/game_info.yaml", package = "ppgames"))
    game_info[[game]]
}

title <- function(game, game_info = NULL) {
    if (is.null(game)) return(NULL)
    info <- get_game_info(game, game_info)
    if (is.null(info$title)) {
        stringr::str_to_title(gsub("_|-", " ", game))
    } else {
        info$title
    }
}
author <- function(game, game_info = NULL) {
    info <- get_game_info(game, game_info)
    if (is.null(info$author)) {
        "Trevor L. Davis"
    } else {
        info$author
    }
}
keywords <- function(game, game_info = NULL) {
    info <- get_game_info(game, game_info)
    if (is.null(info$keywords)) {
        "piecepack, board games"
    } else {
        info$keywords
    }
}
subject <- function(game, game_info = NULL) {
    info <- get_game_info(game, game_info)
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

pdf_metadata <- function(game = NULL, game_info = NULL, ...,
                         pdftitle = NULL, pdfauthor = NULL,
                         pdfsubject = NULL, pdfkeywords = NULL) {
    if (is.null(pdftitle)) pdftitle <- title(game, game_info)
    if (is.null(pdfauthor)) pdfauthor <- author(game, game_info)
    if (is.null(pdfsubject)) pdfsubject <- subject(game, game_info)
    if (is.null(pdfkeywords)) pdfkeywords <- keywords(game, game_info)
cat(paste0("\\hypersetup{", pdf_key("pdftitle", pdftitle)),
    pdf_key("pdfauthor", pdfauthor),
    pdf_key("pdfcreator", paste0("ppgames (v", packageVersion("ppgames"), ")")),
    pdf_key("pdfsubject", pdfsubject),
    pdf_key("pdfkeywords", pdfkeywords),
    pdf_key("pdflang", "en-US"),
    "}", sep = "\n")
}

external_links <- function(game, list_type = "itemize", game_info = NULL) {
    links <- character(0)
    info <- get_game_info(game, game_info)
    if (hasName(info, "ppwiki"))
        links <- c(links, paste0("http://www.ludism.org/ppwiki/", info$ppwiki))
    if (hasName(info, "boardgamegeek"))
        links <- c(links, paste0("https://boardgamegeek.com/boardgame/", info$boardgamegeek))
    if (hasName(info, "chessvariants"))
        links <- c(links, paste0("https://www.chessvariants.com/", info$chessvariants))
    if (hasName(info, "cyningstan"))
        links <- c(links, paste0("http://www.cyningstan.com/game/", info$cyningstan))
    if (hasName(info, "wikipedia"))
        links <- c(links, paste0("https://en.wikipedia.org/wiki/", info$wikipedia))
    if (hasName(info, "external_links"))
        links <- c(links, info$external_links)

    links <- paste("\\item", href(as.character(links)))

    if (hasName(info, "books"))
        books <- paste("\\item", info$books)
    else
        books <- character(0)

    cat(paste(c("\\begin{itemize}", links, books, "\\end{itemize}\n"), collapse="\n"))
}

latex_url_name <- function(url) {
    name <- gsub("https*://", "", url)
    name <- gsub("%", "\\\\%", name)
    name <- gsub("#", "\\\\#", name)
    name <- gsub("_", "\\\\_", name)
    name
}

# url <- function(url) str_glue("\\url{{{name}}}") # nolint
href <- function(url, name=NULL) {
    if (is.null(name)) name <- latex_url_name(url)
    str_glue("\\href{{{url}}}{{{name}}}")
}
