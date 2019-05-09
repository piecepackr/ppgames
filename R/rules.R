#' @importFrom knitr knit opts_chunk knit_hooks
#' @import grid
#' @import piecepackr
#' @importFrom tibble tibble tribble
#' @importFrom tools file_ext


xelatex <- function(tex) {
    pdf <- sub(paste0(file_ext(tex), "$"), "pdf", tex)
    system2("xelatex", c("-papersize=letter", tex), stdout=NULL)
    system2("xelatex", c("-papersize=letter", tex), stdout=NULL)
    pdf
}

to_tex <- function(kf) {
    tex <- sub(paste0(file_ext(kf), "$"), "tex", kf)
    system2("pandoc", args=c("-o", tex, 
                             kf))
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
                  fig.path=paste0(name, "-"), 
                  fig.lp=paste0("fig:", name, "-"))
}

make_rule <- function(game, cfg=NULL) {
    game_files <- list.files(system.file("games", package="ppgames"), 
                             full.names=TRUE)
    cfg <- cfgs[[game]]
    if(is.null(cfg)) { cfg <- pp_cfg() }
    of <- game_files[grep(game, game_files)]

    kf <- knit(of, quiet=TRUE)
    if (file_ext(kf) == "tex") {
        tex <- kf
    } else {
        tex <- to_tex(kf)
    }

    to_pdf(kf)
}

make_rules <- function(cfg=NULL, output_dir=getwd()) {

    output_dir <- normalizePath(output_dir)

    wd <- getwd()
    on.exit(setwd(wd))
    setwd(tempdir())

    for (game in c("backgammon", "tablut")) {
        pdf <- make_rule(game, cfg)
        file.copy(pdf, output_dir, overwrite=TRUE)
    }

    of <- system.file("books/book.Rtex", package="ppgames")
    tex <- knit(of, quiet=TRUE)
    pdf <- xelatex(tex)
    file.copy(pdf, output_dir, overwrite=TRUE)

    invisible(NULL)
}
