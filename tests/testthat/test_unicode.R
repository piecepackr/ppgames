library("tibble")
context("test unicode")
test_that("dimensions", {
    df <- df_four_field_kono()
    df$cfg <- "piecepack"
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

    dft <- tibble(piece_side = "board_back", x=seq(1.5, 5.5, 2), y=1.5, rank=2, cfg="checkers1")
    dfb <- tibble(piece_side = "bit_back", x=1:6, y=1, suit=1:6, cfg="checkers1")
    dfd <- tibble(piece_side = "die_face", x=1:6, y=2, suit=1:6, rank=1:6, cfg="dice")
    df <- dplyr::bind_rows(dft, dfb, dfd)
    verify_output("../text_diagrams/some_checkers1.txt", cat_piece(df))

    df <- dplyr::mutate(df, cfg = gsub("checkers1", "checkers2", cfg),
                        x = 2 * x, y = 2 * y)
    verify_output("../text_diagrams/some_checkers2.txt", cat_piece(df))
})
