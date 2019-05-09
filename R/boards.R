even <- function(x) { x %% 2 == 0 }

#' Various game boards
#' 
#' \code{grid.board_rect_tiles} draws a rectangular board using the back of piecepack tiles, 
#' it applies \code{pmap_piece} to the \code{tibble} data frame returned by \code{df_rect_board_tiles}.
#' \code{grid.board_rect_cells} draws a rectangular board where pieces are placed in the cells of the board.
#' \code{grid.board_rect_points} draws a rectangular board where pieces are placed in the points of the board.
#' 
#' @param nrows Number of rows in game board
#' @param ncols Number of columns in game board
#' @param x0 X coordinate for the center of the first cell/point
#' @param y0 Y coordinate for the center of the first cell/point
#' @param max_tiles Maximum number of tiles that can be used
#' @param cfg Piecepack configuration list or \code{pp_cfg} object, 
#'        a list of \code{pp_cfg} objects,
#'        or a character vector of \code{pp_cfg} objects
#' @param envir Environment (or named list) containing configuration list(s).
#' @param name A character identifier (for grid)
#' @param gp An object of class ‘gpar’, typically the output from a call
#'        to the function ‘gpar’.  This is basically a list of
#'        graphical parameter settings.
#' @param draw A logical value indicating whether graphics output should be produced.
#' @param vp A \code{grid} viewport object (or NULL).
#' 
#' @rdname boards
#' @name boards
NULL

#' @export
grid.board_rect_cells <- function(nrows=8, ncols=8, x0=1, y0=1, 
				  default.units="inches", 
				  draw=TRUE, name=NULL, gp=gpar(), vp=NULL) {
    fill <- gp$fill
    x <- seq(x0, length.out=ncols)
    y <- seq(y0, length.out=nrows)
    gl <- gList()
    for (ii in seq(y)) {
	rgp <- gp
        rgp$fill <- cycle_elements(fill, ii-1)
        gl[[ii]] <- rectGrob(x, y[ii], width=1, height=1, gp=rgp, default.units="inches")
    }
    grob <- gTree(children=gl, name=name, vp=vp)
    if (draw)
        grid.draw(grob)
    invisible(grob)
}

#' @export
grid.board_rect_points <- function(nrows=8, ncols=8, x0=1, y0=1, 
				  default.units="inches", 
				  draw=TRUE, name=NULL, gp=gpar(), vp=NULL) {
    grid.board_rect_cells(nrows-1, ncols-1, x0+0.5, y0+0.5, 
			  default.units, draw, name, gp, vp)
}

cycle_elements <- function(x, n=1) {
    l <- length(x)
    if (l < 2 || n == l || n == 0) {
	x
    } else if (n < l) {
	c(x[(n+1):l], x[1:n])
    } else {
	cycle_elements(cycle_elements(x, l), n-l)
    }
}

#' @export
grid.board_rect_tiles <- function(nrows=8, ncols=8, x0=1, y0=1, max_tiles=24,
				cfg=pp_cfg(), envir=NULL,
				draw=TRUE, name=NULL, gp=NULL, vp=NULL){
    df <- df_rect_board_tiles(nrows, ncols, x0, y0, max_tiles)
    pmap_piece(df, cfg=cfg, envir=envir, default.units="in",
	       draw=draw, name=name, gp=gp, vp=vp)
}

#' @export
df_rect_board_tiles <- function(nrows=8, ncols=8, x0=1, y0=1, max_tiles=24) {
    if (can_use_squares(nrows, ncols, max_tiles)) {
        x <- seq(0.5, by=2, length.out=ncols/2)
        y <- seq(0.5, by=2, length.out=nrows/2)
    } else if (can_use_lines(nrows, ncols, max_tiles)) {
        ntiles <- 0
        for (rr in as.integer(even(nrows)):(nrows %/% 3 - 1)) {
            for (rc in as.integer(even(ncols)):(ncols %/% 3 - 1)) {
                tr <- n_tiles(nrows, rr)
                tc <- n_tiles(ncols, rc)
                if (n_lines(tr, rr) == nrows &&
                    n_lines(tc, rc) == ncols &&
                    tr * tc > ntiles &&
                    tr * tc <= max_tiles) {
                    ntiles <- tr * tc

                    x3 <- rep(3, rc)
                    x2 <- rep(2, tc-rc-1)
                    y3 <- rep(3, rr)
                    y2 <- rep(2, tr-rr-1)
                    x <- 1 + cumsum(c(0, adjust_rivers(c(x3, x2)))) 
                    y <- 1 + cumsum(c(0, adjust_rivers(c(y3, y2))))
                }
            }
        } 
    } else {
       stop("don't know how to draw this board")
    }
    xr <- x0 + rep(x, length(y))
    yr <- y0 + rep(y, each=length(x))
    tibble(piece_side="tile_back", suit=NA, rank=NA, x=xr, y=yr)
}

can_use_squares <- function(nrows, ncols, max_tiles) {
    even(nrows) && even(ncols) && (nrows*ncols/4 <= max_tiles)
}

n_lines <- function(tiles, rivers) {
    2 * tiles + 1 + rivers
}
n_tiles <- function(n, rivers) {
    ceiling((n - rivers - 1)/2) #### Not quite right for illegal n, river values
}
n_rivers <- function(n, tiles) {
    n - 2*tiles - 1
}

min_line_tiles <- function(n) {
    mod3 <- n %% 3
    if (mod3 == 0) {
        n / 3
    } else {
        n %/% 3 + 1
    }
}

line_score <- function(x) {
    prev <- Inf
    score <- 0
    streak <- 0
    for (e in x) {
        if (e == prev) {
            streak <- streak + 1
            score <- score + streak * e
        } else {
            streak <- 0
        }
        prev <- e
    }
    score
}

# #' @importFrom gtools permutations
adjust_rivers <- function(x) {
    if (length(x) < 3) return(x)
    score <- Inf
    new_score <- line_score(x)
    n <- length(x)
    while (new_score < score) {
	score <- new_score
        for (ii in seq(n)) {
            for (jj in seq(ii-1)) {
                 if(ii > 1 && x[ii] != x[jj] && new_score == score) {
		      xp = swap(x, ii, jj)
                      sp = line_score(xp)
		      if (sp < score) {
		          new_score <- sp
		          x <- xp
		      }
		 }
	    }
	}
    }
    # n <- length(x)
    # p <- unique(permutations(n, n, x, FALSE))
    # s <- apply(p, 1, line_score)
    # p[which.min(s),]
    x
}

swap <- function(x, ii, jj) {
    tmp <- x[ii]
    x[ii] <- x[jj]
    x[jj] <- tmp
    x
}

# max_line_tiles <- function(n) {
#     if(odd(n)) {
#         (n-1)/2
#     } else {
#         (n-2)/2
#     }
# }

can_use_lines <- function(nrows, ncols, max_tiles) {
    minr <- min_line_tiles(nrows)
    minc <- min_line_tiles(ncols)
    !(nrows %in% c(2,4)) && 
        !(ncols %in% c(2,4)) &&
        minr * minc <= max_tiles
}
