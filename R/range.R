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

#' @importFrom rlang .data
range_true <- function(df, cfg = pp_cfg(), envir = NULL, op_scale = 0, op_angle = 45, ...) {
    if (nrow(df) == 0) {
        return(list(xmin = NA_real_, xmax = NA_real_,
                    ymin = NA_real_, ymax = NA_real_,
                    xmin_op = NA_real_, xmax_op = NA_real_,
                    ymin_op = NA_real_, ymax_op = NA_real_))
    }
    df <- piecepackr:::add_3d_info(df, cfg = cfg, envir = envir)
    llb <- piecepackr:::Point3D$new(df$xll, df$yll, df$zb)$project_op(op_angle, op_scale)
    llt <- piecepackr:::Point3D$new(df$xll, df$yll, df$zt)$project_op(op_angle, op_scale)
    ulb <- piecepackr:::Point3D$new(df$xul, df$yul, df$zb)$project_op(op_angle, op_scale)
    ult <- piecepackr:::Point3D$new(df$xul, df$yul, df$zt)$project_op(op_angle, op_scale)
    lrb <- piecepackr:::Point3D$new(df$xlr, df$ylr, df$zb)$project_op(op_angle, op_scale)
    lrt <- piecepackr:::Point3D$new(df$xlr, df$ylr, df$zt)$project_op(op_angle, op_scale)
    urb <- piecepackr:::Point3D$new(df$xur, df$yur, df$zb)$project_op(op_angle, op_scale)
    urt <- piecepackr:::Point3D$new(df$xur, df$yur, df$zt)$project_op(op_angle, op_scale)
    df$xllb <- llb$x
    df$yllb <- llb$y
    df$xulb <- ulb$x
    df$yulb <- ulb$y
    df$xlrb <- lrb$x
    df$ylrb <- lrb$y
    df$xurb <- urb$x
    df$yurb <- urb$y
    df$xllt <- llt$x
    df$yllt <- llt$y
    df$xult <- ult$x
    df$yult <- ult$y
    df$xlrt <- lrt$x
    df$ylrt <- lrt$y
    df$xurt <- urt$x
    df$yurt <- urt$y
    xmin = min(df$xl, df$xr)
    xmax = max(df$xl, df$xr)
    ymin = min(df$yb, df$yt)
    ymax = max(df$yb, df$yt)
    xmin_op = min(df$xllb, df$xllt, df$xulb, df$xult,
                  df$xlrb, df$xlrt, df$xurb, df$xurt)
    xmax_op = max(df$xllb, df$xllt, df$xulb, df$xult,
                  df$xlrb, df$xlrt, df$xurb, df$xurt)
    ymin_op = min(df$yllb, df$yllt, df$yulb, df$yult,
                  df$ylrb, df$ylrt, df$yurb, df$yurt)
    ymax_op = max(df$yllb, df$yllt, df$yulb, df$yult,
                  df$ylrb, df$ylrt, df$yurb, df$yurt)
    list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
         xmin_op = xmin_op, xmax_op = xmax_op, ymin_op = ymin_op, ymax_op = ymax_op)
}
