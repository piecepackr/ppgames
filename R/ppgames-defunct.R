#' Defunct functions
#'
#' These functions are Defunct in this release of ppgames,
#' they have been removed.
#'
#' \enumerate{
#' \item{For \code{grid.board_rect_cells)}}
#' \item{For \code{grid.board_rect_points}}
#' \item{For \code{grid.board_rect_tiles}}
#' }
#' @param ... Not used
#' @name ppgames-defunct
NULL

#' @rdname ppgames-defunct
#' @export
grid.board_rect_cells <- function(...) {
    .Defunct("game_systems", "piecpackr",
             msg = paste("use the board pieces from ``piecepackr::game_systems()$checkers1``",
                         "with ``piecepacrk::grid.piece()`` instead"))
}

#' @rdname ppgames-defunct
#' @export
grid.board_rect_points <- function(...) {
    .Defunct("game_systems", "piecepackr",
             msg = paste("use the board pieces from ``piecepackr::game_systems()$go``",
                        "with ``piecepacrk::grid.piece()`` instead"))
}

#' @rdname ppgames-defunct
#' @export
grid.board_rect_tiles <- function(...) {
    .Defunct("pmap_piece", "piecepackr",
             "use piecepackr::pmap_piece(df_rect_board_tiles()) instead")
}
