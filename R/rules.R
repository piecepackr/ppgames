#' @importFrom knitr knit opts_chunk knit_hooks
#' @import grid
#' @import piecepackr
#' @importFrom tibble tibble tribble
#' @importFrom tools file_ext


xelatex <- function(tex, quietly = TRUE) {
    if (quietly)
        stdout <- NULL
    else
        stdout <- ""
    pdf <- sub(paste0(file_ext(tex), "$"), "pdf", tex)
    system2("xelatex", c("-papersize=letter", tex), stdout = stdout)
    system2("xelatex", c("-papersize=letter", tex), stdout = stdout)
    pdf
}

to_tex <- function(kf) {
    tex <- sub(paste0(file_ext(kf), "$"), "tex", kf)
    system2("pandoc", args = c("-o", tex, kf))
    tex
}
to_pdf <- function(kf) {
    pdf <- sub(paste0(file_ext(kf), "$"), "pdf", kf)
    system2("pandoc", args = c("-o", pdf, "--pdf-engine=xelatex", kf))
    pdf
}

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

#' Save ruleset and/or rulebook
#'
#' Save ruleset and/or rulebook
#'
#' @param game Game name
#' @param gk A \code{game_kit} R6 object.
#' @param output_dir Directory to save ruleset/rulebook to.
#' @rdname save_ruleset
#' @export
save_ruleset <- function(game, gk = game_kit(), output_dir = getwd(), quietly = TRUE) {

    force(output_dir)
    wd <- getwd()
    on.exit(setwd(wd))
    setwd(tempdir())

    game_files <- list.files(system.file("games", package = "ppgames"),
                             full.names = TRUE)
    of <- game_files[grep(game, game_files)]
    kf <- knit(of, quiet = quietly)

    if (is.null(output_dir)) {
        if (file_ext(kf) != "tex") {
            to_tex(kf)
        }
    } else {
        output_dir <- normalizePath(output_dir)
        pdf <- to_pdf(kf)
        file.copy(pdf, output_dir, overwrite = TRUE)
    }
    invisible(NULL)
}

#' @param book Book name
#' @rdname save_ruleset
#' @param quietly Whether to hide xelatex output.
#' @export
save_rulebook <- function(book = "the-historical-piecepacker", gk = game_kit(), output_dir = getwd(), quietly = TRUE) {

    output_dir <- normalizePath(output_dir)

    wd <- getwd()
    on.exit(setwd(wd))
    setwd(tempdir())

    games <- list.files(system.file("games", package = "ppgames"), pattern = ".Rtex")
    games <- gsub(".Rtex", "", games)
    for (game in games) {
        save_ruleset(game, gk, NULL, quietly)
    }

    of <- system.file(sprintf("books/%s.Rtex", book), package = "ppgames")
    tex <- knit(of, quiet = quietly)
    pdf <- xelatex(tex, quietly)
    file.copy(pdf, output_dir, overwrite = TRUE)

    invisible(NULL)
}

game_data <- function(game) {
    info <- game_info[[game]]
    items <- list()
    items$Players <- paste(info$players, collapse=", ")
    items$Length <- game_length(info$length)
    items$Equipment <- info$equipment
    items$Designer <- info$designer
    if ("author" %in% names(info)) items$Author <- info$author
    items$Version <- info$version
    items[["Version date"]] <- info$version_date
    items$License <- sprintf("\\href{%s}{%s}", license_urls[[info$license]], license_names[[info$license]])
    cat(paste(c("\\begin{description}",
                sprintf("\\item[%s] %s", names(items), sapply(items, identity)),
                "\\end{description}\n"),
          collapse="\n"))
}

game_length <- function(gl) {
    if (length(gl) == 2)
        sprintf("%s-%s minutes", gl[1], gl[2])
    else
        paste(gl, "minutes")
}

external_links <- function(game) {
    links <- character(0)
    info <- game_info[[game]]
    if ("boardgamegeek" %in% names(info)) {
        link <- sprintf("\\item[BoardGameGeek] \\url{https://boardgamegeek.com/boardgame/%s}",
                        info$boardgamegeek)
        links <- append(links, link)
    }
    if ("chessvariants" %in% names(info)) {
        link <- sprintf("\\item[The Chess Variants Pages] \\url{https://www.chessvariants.com/%s}",
                        info$chessvariants)
        links <- append(links, link)
    }
    if ("cyningstan" %in% names(info)) {
        link <- sprintf("\\item[Cyningstan] \\url{http://www.cyningstan.com/%s}",
                        info$cyningstan)
        links <- append(links, link)
    }
    if ("wikipedia" %in% names(info)) {
        link <- sprintf("\\item[Wikipedia] \\url{https://en.wikipedia.org/wiki/%s}",
                        info$wikipedia)
        links <- append(links, link)
    }
    cat(paste(c("\\begin{description}", links, "\\end{description}\n"),
          collapse="\n"))
}
