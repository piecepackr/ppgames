range_heuristic <- function(df) {
    if(nrow(df) == 0) { return (c(xmin=NA_real_, xmax=NA_real_, ymin=NA_real_, ymax=NA_real_)) }
    xleft <- ifelse(grepl("tile", df$piece_side), df$x-1, df$x-0.5)
    xright <- ifelse(grepl("tile", df$piece_side), df$x+1, df$x+0.5)
    yleft <- ifelse(grepl("tile", df$piece_side), df$y-1, df$y-0.5)
    yright <- ifelse(grepl("tile", df$piece_side), df$y+1, df$y+0.5)
    list(xmin=min(xleft), xmax=max(xright), ymin=min(yleft), ymax=max(yright))
}

range_true <- function(df, cfg=pp_cfg(), envir=NULL, op_scale=0, op_angle=90, ...) {
    if(nrow(df) == 0) { return (c(NA_real_, NA_real_)) }
    df <- piecepackr:::add_3d_info(df, cfg, envir)
    df <- dplyr::mutate(df,
                        xllb = piecepackr:::op_x(xll, yll, zb, op_angle, op_scale),
                        xllt = piecepackr:::op_x(xll, yll, zt, op_angle, op_scale),
                        xulb = piecepackr:::op_x(xul, yul, zb, op_angle, op_scale),
                        xult = piecepackr:::op_x(xul, yul, zt, op_angle, op_scale),
                        xlrb = piecepackr:::op_x(xlr, ylr, zb, op_angle, op_scale),
                        xlrt = piecepackr:::op_x(xlr, ylr, zt, op_angle, op_scale),
                        xurb = piecepackr:::op_x(xur, yur, zb, op_angle, op_scale),
                        xurt = piecepackr:::op_x(xur, yur, zt, op_angle, op_scale),
                        yllb = piecepackr:::op_y(xll, yll, zb, op_angle, op_scale),
                        yllt = piecepackr:::op_y(xll, yll, zt, op_angle, op_scale),
                        yulb = piecepackr:::op_y(xul, yul, zb, op_angle, op_scale),
                        yult = piecepackr:::op_y(xul, yul, zt, op_angle, op_scale),
                        ylrb = piecepackr:::op_y(xlr, ylr, zb, op_angle, op_scale),
                        ylrt = piecepackr:::op_y(xlr, ylr, zt, op_angle, op_scale),
                        yurb = piecepackr:::op_y(xur, yur, zb, op_angle, op_scale),
                        yurt = piecepackr:::op_y(xur, yur, zt, op_angle, op_scale))
    dfr <- dplyr::summarize(df, 
                            xmin=min(xl, xr), xmax=max(xl, xr),
                            ymin=min(yb, yt), ymax=max(yb, yt),
                            xmin_op=min(xllb, xllt, xulb, xult, xlrb, xlrt, xurb, xurt),
                            xmax_op=max(xllb, xllt, xulb, xult, xlrb, xlrt, xurb, xurt),
                            ymin_op=min(yllb, yllt, yulb, yult, ylrb, ylrt, yurb, yurt),
                            ymax_op=max(yllb, yllt, yulb, yult, ylrb, ylrt, yurb, yurt))
    dfr
}
