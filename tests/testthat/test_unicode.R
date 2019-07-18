library("tibble")
context("test unicode")
test_that("dimensions", {
    df <- df_four_field_kono()
    expect_equal(xrange(df), c(0.5, 4.5))
    expect_equal(yrange(df), c(0.5, 4.5))
})
test_that("text diagrams", {
    expect_output(cat_piece(df_fide_chess()), "☀⃝┃☽⃝│")
    expect_output(cat_piece(df_xiangqi()), "♛⃝━╋━⸸⃝")
})
