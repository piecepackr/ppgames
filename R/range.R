xrange_heuristic <- function(df) {
    if(nrow(df) == 0) { return (c(NA_real_, NA_real_)) }
    xleft <- ifelse(grepl("tile", df$piece_side), df$x-1, df$x-0.5)
    xright <- ifelse(grepl("tile", df$piece_side), df$x+1, df$x+0.5)
    c(min(xleft), max(xright))
}
yrange_heuristic <- function(df) {
    if(nrow(df) == 0) { return (c(NA_real_, NA_real_)) }
    yleft <- ifelse(grepl("tile", df$piece_side), df$y-1, df$y-0.5)
    yright <- ifelse(grepl("tile", df$piece_side), df$y+1, df$y+0.5)
    c(min(yleft), max(yright))
}
xrange <- function(df) {
    if(nrow(df) == 0) { return (c(NA_real_, NA_real_)) }
    xleft <- ifelse(grepl("tile", df$piece_side), df$x-1, df$x-0.5)
    xright <- ifelse(grepl("tile", df$piece_side), df$x+1, df$x+0.5)
    c(min(xleft), max(xright))
}
yrange <- function(df) {
    if(nrow(df) == 0) { return (c(NA_real_, NA_real_)) }
    yleft <- ifelse(grepl("tile", df$piece_side), df$y-1, df$y-0.5)
    yright <- ifelse(grepl("tile", df$piece_side), df$y+1, df$y+0.5)
    c(min(yleft), max(yright))
}
