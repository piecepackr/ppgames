#' Animate a ppn game
#'
#' Animate a ppn game
#' @param game A list containing a parsed ppn game (as parsed by \code{read_ppn})
#' @param file Filename to save animation
#' @param annotate If \code{TRUE} annotate the plot
#' @param ... Arguments to \code{pmap_piece}
#' @param cfg A piecepackr configuration list
#' @param envir Environment (or named list) of piecepackr configuration lists
#' @return Nothing, as a side effect saves an animation of ppn game
#' @export
animate_game <- function(game, file = "animation.gif", annotate = TRUE, ...,
                         cfg = NULL, envir = NULL) {

    ce <- piecepackr:::default_cfg_envir(cfg, envir)
    cfg <- ce$cfg
    envir <- ce$envir

    dfs <- game$dfs

    ranges <- lapply(dfs, range_true, cfg = cfg, envir = envir, ...)
    xmax_op <- max(sapply(ranges, function(x) x$xmax_op), na.rm = TRUE)
    ymax_op <- max(sapply(ranges, function(y) y$ymax_op), na.rm = TRUE)
    xmax <- max(sapply(ranges, function(x) x$xmax), na.rm = TRUE)
    ymax <- max(sapply(ranges, function(y) y$ymax), na.rm = TRUE)
    #### Adjust if xmin under 0
    #### Add grid and comment annotations
    m <- max(xmax, ymax) + 0.5
    res <- round(600 / m, 0)
    height <- res * (ymax_op+0.5)
    width <- res * (xmax_op+0.5)
    plot_fn <- function(df, ...) {
        grid::grid.newpage()
        pmap_piece(df, default.units = "in", ..., envir = envir)
        if (annotate) annotate_plot(xmax, ymax)
    }
    animation::saveGIF(lapply(dfs, plot_fn, ...), movie.name = file,
        ani.height = height, ani.width = width, ani.res = res, ani.dev = "png", ani.type = "png")
    invisible(NULL)
}

#### Option to generate postcard?

get_df_from_move <- function(game, move = NULL) {
    if (is.null(move)) {
        utils::tail(game$dfs, 1)[[1]]
    } else {
        game$dfs[[move]]
    }
}

#' Plot game move
#'
#' Plot game move
#' @param game A list containing a parsed ppn game (as parsed by \code{read_ppn})
#' @param file Filename to save graphic to unless \code{NULL} and \code{new_device==TRUE}
#'        in which case it opens a new graphics device.
#' @param move Which move to plot game state (after the move, will use \code{game$dfs[[move]]})
#'             unless \code{NULL} in which case will plot the game state after the last move.
#' @param annotate If \code{TRUE} annotate the plot
#' @param bg Background color (\code{"transparent")} for transparent
#' @param res For bitmap image formats the resolution
#' @param ... Arguments to \code{pmap_piece}
#' @param cfg A piecepackr configuration list
#' @param envir Environment (or named list) of piecepackr configuration lists
#' @param new_device If \code{file} is \code{NULL} should we open up a new graphics device?
#' @return Nothing, as a side effect saves a graphic
#' @import grDevices
#' @export
plot_move <- function(game, file = NULL,  move = NULL, annotate = TRUE, ..., bg = "white", res = 72,
                      cfg = NULL, envir = NULL, new_device = TRUE) {

    ce <- piecepackr:::default_cfg_envir(cfg, envir)
    cfg <- ce$cfg
    envir <- ce$envir

    df <- get_df_from_move(game, move)
    dfr <- range_true(df, cfg = cfg, envir = envir, ...)
    width <- dfr$xmax_op + 0.5
    height <- dfr$ymax_op + 0.5

    if (is.null(file)) {
        if (new_device) dev.new(width = width, height = height, unit = "in", noRstudioGD = TRUE)
    } else {
        format <- tools::file_ext(file)
        switch(format,
               bmp = bmp(file, width, height, "in", res = res, bg = bg),
               jpeg = jpeg(file, width, height, "in", res = res, bg = bg),
               pdf = cairo_pdf(file, width, height, bg = bg),
               png = png(file, width, height, "in", res = res, bg = bg),
               ps = cairo_ps(file, width, height, bg = bg),
               svg = svg(file, width, height, bg = bg),
               tiff = tiff(file, width, height, "in", res = res, bg = bg))
    }
    pmap_piece(df, default.units = "in", ..., envir = envir)
    if (annotate) annotate_plot(dfr$xmax, dfr$ymax)
    if (!is.null(file)) dev.off()
    invisible(NULL)
}

annotate_plot <- function(xmax, ymax) {
        gp <- gpar(fontsize = 18, fontface = "bold")
        x_indices <- seq(floor(xmax))
        l <- letters[x_indices]
        l <- stringr::str_pad(l, max(stringr::str_count(l)))
        grid.text(l, x = x_indices, y = 0.25, default.units = "in", gp = gp)
        y_indices <- seq(floor(ymax))
        n <- as.character(y_indices)
        n <- stringr::str_pad(n, max(stringr::str_count(n)))
        grid.text(n, x = 0.25, y = y_indices, default.units = "in", gp = gp)
        invisible(NULL)
}
