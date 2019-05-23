#' @importFrom knitr knit opts_chunk knit_hooks
#' @import grid
#' @import piecepackr
#' @importFrom tibble tibble tribble
#' @importFrom tools file_ext


xelatex <- function(tex, quietly=TRUE) {
    if (quietly) 
        stdout <- NULL
    else
        stdout <- ""
    pdf <- sub(paste0(file_ext(tex), "$"), "pdf", tex)
    system2("xelatex", c("-papersize=letter", tex), stdout=stdout)
    system2("xelatex", c("-papersize=letter", tex), stdout=stdout)
    pdf
}

to_tex <- function(kf) {
    tex <- sub(paste0(file_ext(kf), "$"), "tex", kf)
    system2("pandoc", args=c("-o", tex, kf))
    tex
}
to_pdf <- function(kf) {
    pdf <- sub(paste0(file_ext(kf), "$"), "pdf", kf)
    system2("pandoc", args=c("-o", pdf, "--pdf-engine=xelatex", kf))
    pdf
}

set_knitr_opts <- function(name) {
    hook_plot = knit_hooks$get('plot')
    knit_hooks$set(plot = function(x, options) {
      x = paste(c(x, 'whatever'), collapse = '.')
      hook_plot(x, options)
    })
    opts_chunk$set(dev='cairo_pdf', 
                  fig.align='center',
                  fig.path=paste0(name, "-"), 
                  fig.pos='ht!',
                  fig.lp=paste0("fig:", name, "-"))
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
save_ruleset <- function(game, gk=game_kit(), output_dir=getwd(), quietly=TRUE) {

    wd <- getwd()
    on.exit(setwd(wd))
    setwd(tempdir())

    game_files <- list.files(system.file("games", package="ppgames"), 
                             full.names=TRUE)
    of <- game_files[grep(game, game_files)]
    kf <- knit(of, quiet=quietly)

    if(is.null(output_dir)) {
        if (file_ext(kf) != "tex") {
            tex <- to_tex(kf)
        }
    } else {
        output_dir <- normalizePath(output_dir)
        pdf <- to_pdf(kf)
        file.copy(pdf, output_dir, overwrite=TRUE)
    }
    invisible(NULL)
}

#' @param book Book name
#' @rdname save_ruleset
#' @param quietly Whether to hide xelatex output.
#' @export
save_rulebook <- function(book="the_historical_piecepacker", gk=game_kit(), output_dir=getwd(), quietly=TRUE) {

    output_dir <- normalizePath(output_dir)

    wd <- getwd()
    on.exit(setwd(wd))
    setwd(tempdir())

    games <- list.files(system.file("games", package="ppgames"), pattern=".Rtex")
    games <- gsub(".Rtex", "", games)
    for (game in games) {
        save_ruleset(game, gk, NULL, quietly)
    }

    of <- system.file(sprintf("books/%s.Rtex", book), package="ppgames")
    tex <- knit(of, quiet=quietly)
    pdf <- xelatex(tex, quietly)
    file.copy(pdf, output_dir, overwrite=TRUE)

    invisible(NULL)
}
