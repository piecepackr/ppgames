range_heuristic <- function(df) {
    if (nrow(df) == 0) return(list(xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_))
    if (!isTRUE(attr(df, "was_cleaned"))) df <- clean_df(df)

    # piecepack
    is_tile <- grepl("tile", df$piece_side)
    xleft <- ifelse(is_tile, df$x-1, df$x-0.5)
    xright <- ifelse(is_tile, df$x+1, df$x+0.5)
    ybot <- ifelse(is_tile, df$y-1, df$y-0.5)
    ytop <- ifelse(is_tile, df$y+1, df$y+0.5)

    # subpack
    is_subpack <- is_tile & df$cfg == "subpack"
    xleft <- ifelse(is_subpack, df$x-0.5, xleft)
    xright <- ifelse(is_subpack, df$x+0.5, xright)
    ybot <- ifelse(is_subpack, df$y-0.5, ybot)
    ytop <- ifelse(is_subpack, df$y+0.5, ytop)

    # dominoes
    is_dominoes_horizontal <- is_tile & grepl("dominoes", df$cfg) & (df$angle == 90 | df$angle == 270)
    ybot <- ifelse(is_dominoes_horizontal, df$y-0.5, ybot)
    ytop <- ifelse(is_dominoes_horizontal, df$y+0.5, ytop)
    is_dominoes_vertical <- is_tile & grepl("dominoes", df$cfg) & (df$angle == 0 | df$angle == 180)
    xleft <- ifelse(is_dominoes_vertical, df$x-0.5, xleft)
    xright <- ifelse(is_dominoes_vertical, df$x+0.5, xright)

    # boards
    is_board <- grepl("board", df$piece_side)
    xleft <- ifelse(is_board, df$x-0.5*df$rank, xleft)
    xright <- ifelse(is_board, df$x+0.5*df$rank, xright)
    ybot <- ifelse(is_board, df$y-0.5*df$rank, ybot)
    ytop <- ifelse(is_board, df$y+0.5*df$rank, ytop)

    is_board2 <- is_board & grepl("2", df$cfg)
    xleft <- ifelse(is_board2, df$x-df$rank, xleft)
    xright <- ifelse(is_board2, df$x+df$rank, xright)
    ybot <- ifelse(is_board2, df$y-df$rank, ybot)
    ytop <- ifelse(is_board2, df$y+df$rank, ytop)

    list(xmin = min(xleft), xmax = max(xright), ymin = min(ybot), ymax = max(ytop))
}

range_true <- function(df, cfg = pp_cfg(), envir = NULL, op_scale = 0, op_angle = 45, ...) {
    if (nrow(df) == 0) {
        return(list(xmin = NA_real_, xmax = NA_real_,
                    ymin = NA_real_, ymax = NA_real_,
                    xmin_op = NA_real_, xmax_op = NA_real_,
                    ymin_op = NA_real_, ymax_op = NA_real_))
    }
    aabb <- aabb_piece(df, cfg = cfg, envir = envir, op_scale = op_scale, op_angle = op_angle, ...)
    list(xmin = aabb$x[1], xmax = aabb$x[2], ymin = aabb$y[1], ymax = aabb$y[2],
         xmin_op = aabb$x_op[1], xmax_op = aabb$x_op[2], ymin_op = aabb$y_op[1], ymax_op = aabb$y_op[2])
}
