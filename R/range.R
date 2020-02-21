range_heuristic <- function(df) {
    if (nrow(df) == 0) return(c(xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_))

    is_tile <- grepl("tile", df$piece_side)
    xleft <- ifelse(is_tile, df$x-1, df$x-0.5)
    xright <- ifelse(is_tile, df$x+1, df$x+0.5)
    yleft <- ifelse(is_tile, df$y-1, df$y-0.5)
    yright <- ifelse(is_tile, df$y+1, df$y+0.5)

    #### dominoes

    is_board <- grepl("board", df$piece_side)
    xleft <- ifelse(is_board, df$x-0.5*df$rank, xleft)
    xright <- ifelse(is_board, df$x+0.5*df$rank, xright)
    yleft <- ifelse(is_board, df$y-0.5*df$rank, yleft)
    yright <- ifelse(is_board, df$y+0.5*df$rank, yright)

    is_board2 <- is_board & grepl("2", df$cfg)
    xleft <- ifelse(is_board2, df$x-df$rank, xleft)
    xright <- ifelse(is_board2, df$x+df$rank, xright)
    yleft <- ifelse(is_board2, df$y-df$rank, yleft)
    yright <- ifelse(is_board2, df$y+df$rank, yright)

    list(xmin = min(xleft), xmax = max(xright), ymin = min(yleft), ymax = max(yright))
}

#' @importFrom rlang .data
range_true <- function(df, cfg = pp_cfg(), envir = NULL, op_scale = 0, op_angle = 45, ...) {
    if (nrow(df) == 0) {
        return(tibble::tibble(xmin = NA, xmax = NA, ymin = NA, ymax = NA,
                              xmin_op = NA, xmax_op = NA, ymin_op = NA, ymax_op = NA))
    }
    df <- piecepackr:::add_3d_info(df, cfg, envir)
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
    dfr <- dplyr::summarize(df,
                            xmin = min(.data$xl, .data$xr),
                            xmax = max(.data$xl, .data$xr),
                            ymin = min(.data$yb, .data$yt),
                            ymax = max(.data$yb, .data$yt),
                            xmin_op = min(.data$xllb, .data$xllt, .data$xulb, .data$xult,
                                        .data$xlrb, .data$xlrt, .data$xurb, .data$xurt),
                            xmax_op = max(.data$xllb, .data$xllt, .data$xulb, .data$xult,
                                        .data$xlrb, .data$xlrt, .data$xurb, .data$xurt),
                            ymin_op = min(.data$yllb, .data$yllt, .data$yulb, .data$yult,
                                        .data$ylrb, .data$ylrt, .data$yurb, .data$yurt),
                            ymax_op = max(.data$yllb, .data$yllt, .data$yulb, .data$yult,
                                        .data$ylrb, .data$ylrt, .data$yurb, .data$yurt))
    dfr
}
