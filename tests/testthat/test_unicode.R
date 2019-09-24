library("tibble")
context("test unicode")
test_that("dimensions", {
    df <- df_four_field_kono()
    expect_equal(xrange(df), c(0.5, 4.5))
    expect_equal(yrange(df), c(0.5, 4.5))
})
test_that("text diagrams", {
    skip_on_appveyor() 
    expect_warning(rotate("$", 90))
    expect_warning(rotate("&", 180))
    expect_warning(rotate("&", 270))
    expect_warning(rotate("&", 45))
    expect_output(cat_piece(df_fide_chess()),
                  "\u2600\u20dd\u2503\u263d\u20dd\u2502")
    expect_output(cat_piece(df_xiangqi()), 
                  "\u265b\u20dd\u2501\u254b\u2501\u2e38\u20dd")
})
