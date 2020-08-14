#' Animate a ppn game
#'
#' Animate a ppn game
#' @param game A list containing a parsed ppn game (as parsed by \code{read_ppn})
#' @param file Filename to save animation unless \code{NULL}
#'             in which case it uses the current graphics device.
#' @param annotate If \code{TRUE} annotate the plot
#' @param ... Arguments to \code{pmap_piece}
#' @param cfg A piecepackr configuration list
#' @param envir Environment (or named list) of piecepackr configuration lists
#' @param n_transitions Integer, if over zero (the default)
#'                how many transition frames to add between moves.
#' @param n_pauses Integer, how many paused frames per completed move.
#' @param fps Double, frames per second.
#' @param new_device If \code{file} is \code{NULL} should we open up a new graphics device?
#' @return Nothing, as a side effect saves an animation of ppn game
#' @export
animate_game <- function(game, file = "animation.gif", annotate = TRUE, ...,
                         cfg = NULL, envir = NULL,
                         n_transitions = 0L, n_pauses = 1L, fps = n_transitions + n_pauses,
                         new_device = TRUE) {

    if (n_transitions > 0L && !requireNamespace("tweenr", quietly = TRUE)) {
        stop("You need to install the suggested package 'tweenr' to use 'n_transitions > 0'.",
             "Use 'install.packages(\"tweenr\")'")
    }

    ce <- piecepackr:::default_cfg_envir(cfg, envir)
    cfg <- ce$cfg
    envir <- ce$envir

    dfs <- game$dfs
    ranges <- lapply(dfs, range_true, cfg = cfg, envir = envir, ...)
    if (n_transitions > 0L)
        dfs <- get_tweenr_dfs(dfs, n_transitions, ..., cfg = cfg, envir = envir)

    #### Add grid and comment annotations
    xmax_op <- max(sapply(ranges, function(x) x$xmax_op), na.rm = TRUE)
    ymax_op <- max(sapply(ranges, function(x) x$ymax_op), na.rm = TRUE)
    xmin_op <- min(sapply(ranges, function(x) x$xmin_op), na.rm = TRUE)
    ymin_op <- min(sapply(ranges, function(x) x$ymin_op), na.rm = TRUE)
    xmax <- max(sapply(ranges, function(x) x$xmax), na.rm = TRUE)
    ymax <- max(sapply(ranges, function(x) x$ymax), na.rm = TRUE)
    if (xmin_op < 0.25) {
        xoffset <- 0.25 - xmin_op
    } else {
        xoffset <- 0
    }
    if (ymin_op < 0.25) {
        yoffset <- 0.25 - ymin_op
    } else {
        yoffset <- 0
    }
    width <- xmax_op + xoffset + 0.25
    height <- ymax_op + yoffset + 0.25
    m <- max(width, height)
    res <- round(600 / m, 0)
    height <- res * height
    width <- res * width
    plot_fn <- function(df, ...) {
        df$x <- df$x + xoffset
        df$y <- df$y + yoffset
        grid::grid.newpage()
        pmap_piece(df, default.units = "in", ..., envir = envir)
        if (annotate) annotate_plot(xmax, ymax, xoffset, yoffset)
    }
    animation_fn(file, new_device)(lapply(dfs, plot_fn, ...), file, width, height, 1 / fps, res)
    invisible(NULL)
}
#### How to handle empty tibbles??
animation_fn <- function(file, new_device = TRUE) {
    if (is.null(file)) {
        function(expr, file, width, height, delay, res) {
            if (new_device) dev.new(width = width / res, height = height / res, unit = "in", noRstudioGD = TRUE)
            devAskNewPage(TRUE)
            eval(expr)
            devAskNewPage(getOption("device.ask.default"))
        }
    } else if (grepl(".html$", file)) {
        if (!requireNamespace("animation")) stop("You need to install the suggested package 'animation'")
        function(expr, file, width, height, delay, res) {
            animation::saveHTML(expr, htmlfile = file, interval = delay,
                                ani.height = height, ani.width = width, ani.res = res,
                                ani.dev = "png", ani.type = "png",
                                title = "Animated game", verbose = FALSE)
        }
    } else if (requireNamespace("gifski", quietly = TRUE)) {
        function(expr, file, width, height, delay, res) {
            gifski::save_gif(expr, file, width, height, delay, res = res, progress = FALSE)
        }
    } else if (requireNamespace("animation", quietly = TRUE)) {
        function(expr, file, width, height, delay, res) {
            animation::saveGIF(expr, movie.name = file, interval = delay,
                               ani.height = height, ani.width = width, ani.res = res,
                               ani.dev = "png", ani.type = "png")
        }
    } else {
        stop("You need to install either the suggested package 'animation' or 'gifski' to use 'animate_game'.",
             "Use 'install.packages(\"gifski\")' and/or 'install.packages(\"animation\")'")
    }
}

#' @importFrom dplyr bind_rows left_join matches select
to_zero <- function(df) {
    # df$alpha <- 0 nocov
    df$scale <- 0
    df
}
get_tweenr_dfs <- function(dfs, n_transitions = 0L, n_pauses = 1L, ...) {
    df_id_cfg <- get_id_cfg(dfs)
    dfs <- rev(lapply(dfs, get_tweenr_df, ...)) # better transitions with 'rev'
    dfs[[1]] <- keep_state(dfs[[1L]], n_pauses)
    df <- Reduce(tweenr_reducer(n_transitions, n_pauses), dfs)
    df <- left_join(df, df_id_cfg, by = "id")
    id_frames <- as.list(seq.int(max(df$.frame)))
    rev(lapply(id_frames, function(id_frame) dplyr::filter(df, .data$.frame == id_frame)))
}
get_id_cfg <- function(dfs) {
    df <- do.call(bind_rows, dfs)
    df <- select(df, .data$id, .data$cfg)
    unique(df)
}
get_tweenr_df <- function(df, ...) {
    df$scale <- 1
    df$alpha <- 1
    df <- select(df, .data$id, .data$piece_side, .data$suit, .data$rank,
                 .data$x, .data$y, matches("^z$"), .data$angle,
                 .data$scale, .data$alpha)
    as.data.frame(df)
}
#' @importFrom utils packageVersion
tweenr_reducer <- function(n_transitions = 0L, n_pauses = 1L) {
    function(df1, df2) {
        df <- tween_state(df1, df2, n_transitions)
        keep_state(df, n_pauses)
    }
}
has_name <- function(df, name) name %in% names(df)
tween_state <- function(df1, df2, n_transitions = 0L) {
    # https://github.com/thomasp85/tweenr/issues/44
    if (has_name(df1, ".frame") && packageVersion("tweenr") <= package_version("1.0.1"))
        n_transitions <- n_transitions - 1L
    tweenr::tween_state(df1, df2, ease = "cubic-in-out", nframes = n_transitions + 2L,
                              id = .data$id, enter = to_zero, exit = to_zero)
}
keep_state <- function(df, n_pauses = 1L) {
    if (has_name(df, ".frame") && packageVersion("tweenr") <= package_version("1.0.1"))
        n_pauses <- n_pauses - 1L
    tweenr::keep_state(df, nframes = n_pauses)
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
#' @param file Filename to save graphic to unless \code{NULL}
#'             in which case it uses the current graphics device.
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
    if (dfr$xmin_op < 0.25) {
        xoffset <- 0.25 - dfr$xmin_op
    } else {
        xoffset <- 0
    }
    if (dfr$ymin_op < 0.25) {
        yoffset <- 0.25 - dfr$ymin_op
    } else {
        yoffset <- 0
    }
    width <- dfr$xmax_op + xoffset + 0.25
    height <- dfr$ymax_op + yoffset + 0.25
    df$x <- df$x + xoffset
    df$y <- df$y + yoffset

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
    if (annotate) annotate_plot(dfr$xmax, dfr$ymax, xoffset, yoffset)
    if (!is.null(file)) dev.off()
    invisible(NULL)
}

annotate_plot <- function(xmax, ymax, xoffset = 0, yoffset = 0) {
        gp <- gpar(fontsize = 18, fontface = "bold")
        x_indices <- seq(floor(xmax))
        l <- letters[x_indices]
        l <- stringr::str_pad(l, max(stringr::str_count(l)))
        grid.text(l, x = x_indices + xoffset, y = 0.25, default.units = "in", gp = gp)
        y_indices <- seq(floor(ymax))
        n <- as.character(y_indices)
        n <- stringr::str_pad(n, max(stringr::str_count(n)))
        grid.text(n, x = 0.25, y = y_indices + yoffset, default.units = "in", gp = gp)
        invisible(NULL)
}
