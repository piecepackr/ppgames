context("test rectangular boards")
test_that("rectangular boards works as expected", {
    add_coins <- function(df) {
        dfc <- tibble(piece_side = "coin_face", x=1:6, y = 1:6, angle = 0, suit = 1, rank = 1:6)
        rbind(df, dfc)
    }
    cpiece <- function(df) cat_piece(add_coins(df), color = FALSE)

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

    expect_error(df_rect_board_tiles(2, 3), "Don't know how to form a 3x2 board with 24 tiles")

    df <- df_rect_board_tiles(nr = 8, nc = 8)
    verify_output("../text_diagrams/8x8.txt", cpiece(df))

    df <- df_rect_board_tiles(nr = 10, nc = 10)
    verify_output("../text_diagrams/10x10.txt", cpiece(df))

    df <- df_rect_board_tiles(nr = 8, nc = 4)
    verify_output("../text_diagrams/4x8.txt", cpiece(df))

    df <- df_rect_board_tiles(nr = 6, nc = 2)
    verify_output("../text_diagrams/2x6.txt", cpiece(df))

    df <- df_rect_board_tiles(nr = 5, nc = 3)
    verify_output("../text_diagrams/3x5.txt", cpiece(df))

    df <- df_rect_board_tiles(nr = 5, nc = 7)
    verify_output("../text_diagrams/7x5.txt", cpiece(df))

    df <- df_rect_board_tiles(nr = 5, nc = 6)
    verify_output("../text_diagrams/6x5.txt", cpiece(df))

    df <- df_rect_board_tiles(nr = 8, nc = 8, max_tiles = 12)
    verify_output("../text_diagrams/8x8_12t.txt", cpiece(df))


    expect_error(grid.board_rect_cells(), "use the board pieces")
    expect_error(grid.board_rect_points(), "use the board pieces")
    expect_error(grid.board_rect_tiles(), "use piecepackr::pmap_piece")
})
