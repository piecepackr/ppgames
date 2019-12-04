range_heuristic <- function(df) {
    if (nrow(df) == 0) return(c(xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_))
    xleft <- ifelse(grepl("tile", df$piece_side), df$x-1, df$x-0.5)
    xright <- ifelse(grepl("tile", df$piece_side), df$x+1, df$x+0.5)
    yleft <- ifelse(grepl("tile", df$piece_side), df$y-1, df$y-0.5)
    yright <- ifelse(grepl("tile", df$piece_side), df$y+1, df$y+0.5)
    list(xmin = min(xleft), xmax = max(xright), ymin = min(yleft), ymax = max(yright))
}

#' @importFrom rlang .data
range_true <- function(df, cfg = pp_cfg(), envir = NULL, op_scale = 0, op_angle = 90, ...) {
    if (nrow(df) == 0) {
        return(tibble::tibble(xmin = NA, xmax = NA, ymin = NA, ymax = NA,
                              xmin_op = NA, xmax_op = NA, ymin_op = NA, ymax_op = NA))
    }
    df <- piecepackr:::add_3d_info(df, cfg, envir)
    df <- dplyr::mutate(df,
                        xllb = piecepackr:::op_x(.data$xll, .data$yll, .data$zb, op_angle, op_scale),
                        xllt = piecepackr:::op_x(.data$xll, .data$yll, .data$zt, op_angle, op_scale),
                        xulb = piecepackr:::op_x(.data$xul, .data$yul, .data$zb, op_angle, op_scale),
                        xult = piecepackr:::op_x(.data$xul, .data$yul, .data$zt, op_angle, op_scale),
                        xlrb = piecepackr:::op_x(.data$xlr, .data$ylr, .data$zb, op_angle, op_scale),
                        xlrt = piecepackr:::op_x(.data$xlr, .data$ylr, .data$zt, op_angle, op_scale),
                        xurb = piecepackr:::op_x(.data$xur, .data$yur, .data$zb, op_angle, op_scale),
                        xurt = piecepackr:::op_x(.data$xur, .data$yur, .data$zt, op_angle, op_scale),
                        yllb = piecepackr:::op_y(.data$xll, .data$yll, .data$zb, op_angle, op_scale),
                        yllt = piecepackr:::op_y(.data$xll, .data$yll, .data$zt, op_angle, op_scale),
                        yulb = piecepackr:::op_y(.data$xul, .data$yul, .data$zb, op_angle, op_scale),
                        yult = piecepackr:::op_y(.data$xul, .data$yul, .data$zt, op_angle, op_scale),
                        ylrb = piecepackr:::op_y(.data$xlr, .data$ylr, .data$zb, op_angle, op_scale),
                        ylrt = piecepackr:::op_y(.data$xlr, .data$ylr, .data$zt, op_angle, op_scale),
                        yurb = piecepackr:::op_y(.data$xur, .data$yur, .data$zb, op_angle, op_scale),
                        yurt = piecepackr:::op_y(.data$xur, .data$yur, .data$zt, op_angle, op_scale))
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
