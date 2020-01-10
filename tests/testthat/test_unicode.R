library("tibble")
context("test unicode")
test_that("dimensions", {
    df <- df_four_field_kono()
    expect_equal(range_heuristic(df)$xmin, 0.5)
    expect_equal(range_heuristic(df)$xmax, 4.5)
    expect_equal(range_heuristic(df)$ymin, 0.5)
    expect_equal(range_heuristic(df)$ymax, 4.5)
})
test_that("text diagrams", {
    expect_warning(rotate("$", 90))
    expect_warning(rotate("&", 180))
    expect_warning(rotate("&", 270))
    expect_warning(rotate("&", 45))

    # nolint start
    # df <- tibble(piece_side = "tile_face", x = c(1, 3, 3, 1), y = c(3, 3, 1, 1),
    #              suit = 1, rank = 4, angle = c(0, 90, 180, 270))
    # nolint end
})
