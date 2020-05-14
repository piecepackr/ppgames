library("piecepackr")
library("vdiffr")
cfg <- pp_cfg()
pmp <- function(df) {
    function() {
        pmap_piece(df, default.units = "in", cfg = cfg)
        grid.piece("coin_face", rank = 1:6, x = 1:6, y = 1:6, cfg = cfg, default.units = "in")
    }
}

context("test rectangular boards")
test_that("rectangular boards works as expected", {
    expect_equal(min_line_tiles(6), 2)
    expect_equal(min_line_tiles(7), 3)
    expect_equal(min_line_tiles(8), 3)
    expect_equal(n_lines(1, 0), 3)
    expect_equal(n_lines(2, 1), 6)
    expect_equal(n_lines(2, 0), 5)
    expect_equal(n_rivers(6, 2), 1)
    expect_equal(n_rivers(5, 2), 0)
    expect_equal(n_tiles(6, 1), 2)
    expect_equal(n_tiles(5, 0), 2)
    expect_equal(n_tiles(7, 0), 3)
    expect_equal(n_tiles(10, 1), 4)
    expect_equal(line_score(c(2, 2, 3)), 2)
    expect_equal(line_score(c(2, 3, 3)), 3)
    expect_equal(line_score(c(3, 3, 2, 2, 2)), 9)
    expect_equal(line_score(c(2, 3, 2, 3, 2)), 0)
    expect_equal(line_score(c(2, 2, 2, 3)), 6)

    expect_error(df_rect_board_tiles(2, 3), "don't know how to draw this board")

    skip_on_ci()
    df <- df_rect_board_tiles(nr = 8, nc = 8)
    expect_doppelganger("8x8", pmp(df))

    df <- df_rect_board_tiles(nr = 10, nc = 10)
    expect_doppelganger("10x10", pmp(df))

    df <- df_rect_board_tiles(nr = 8, nc = 4)
    expect_doppelganger("4x8", pmp(df))

    df <- df_rect_board_tiles(nr = 6, nc = 2)
    expect_doppelganger("2x6", pmp(df))

    df <- df_rect_board_tiles(nr = 5, nc = 3)
    expect_doppelganger("3x5", pmp(df))

    df <- df_rect_board_tiles(nr = 5, nc = 7)
    expect_doppelganger("7x5", pmp(df))

    df <- df_rect_board_tiles(nr = 5, nc = 6)
    expect_doppelganger("6x5", pmp(df))

    df <- df_rect_board_tiles(nr = 8, nc = 8, max_tiles = 12)
    expect_doppelganger("8x8_12t", pmp(df))

    expect_doppelganger("8x8_grid", grid.board_rect_tiles)
    expect_doppelganger("8x8_cells", grid.board_rect_cells)
    expect_doppelganger("5x5_cells_checkers", function()
            grid.board_rect_cells(5, 5, gp = gpar(fill = c("black", "red"), col = NA)))
    expect_doppelganger("8x8_points", grid.board_rect_points)

})
