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
    expect_null(cat_piece(tibble()))

    # checkers
    dft <- tibble(piece_side = "board_back", x=seq(1.5, 5.5, 2), y=1.5, rank=2, cfg="checkers1")
    dfb <- tibble(piece_side = "bit_back", x=1:6, y=1, suit=1:6, cfg="checkers1")
    dfd <- tibble(piece_side = "die_face", x=1:6, y=2, suit=1:6, rank=1:6, cfg="dice")
    df <- dplyr::bind_rows(dft, dfb, dfd)
    verify_output("../text_diagrams/some_checkers1.txt", cat_piece(df))

    df <- dplyr::mutate(df, cfg = gsub("checkers1", "checkers2", cfg),
                        x = 2 * x, y = 2 * y)
    verify_output("../text_diagrams/some_checkers2.txt", cat_piece(df))

    # icehouse
    dfb <- tibble(piece_side = "board_face", x=c(2.5,6.5), y=2, rank=4, cfg="checkers1")
    dfpt <- tibble(piece_side = "pyramid_top", x=1:8, y=4,
                   rank=rep(1:3, length.out=8), suit=c(1:6, 1:2),
                   angle=seq(0, by=45, length.out=8), cfg="icehouse_pieces")
    dfpf <- tibble(piece_side = "pyramid_face", x=rep(1:8, 3), y=rep(1:3, each=8),
                   rank=rep(1:3, each=8), suit=rep(1:6, 4),
                   angle=rep(seq(0, by=45, length.out=8), 3),
                   cfg="icehouse_pieces")
    df <- dplyr::bind_rows(dfb, dfpt, dfpf)
    verify_output("../text_diagrams/icehouse.txt", cat_piece(df))

    # misc
    dft <- tibble(piece_side = "tile_face", x=c(1.5, 3.5), y=1.5,
                  suit = 1, rank = 4, angle = c(90, 270))
    dfpb <- tibble(piece_side = "pawn_back", x=1:2, y=1,
                  suit=2:1, angle = c(0, 45))
    dfpf <- tibble(piece_side = "pawn_face", x=1, y=2,
                  suit=1, angle = 45)
    dfbf <- tibble(piece_side = "bit_face", x=3, y=1, suit=3, cfg="checkers1")
    df <- dplyr::bind_rows(dft, dfpb, dfpf, dfbf)
    verify_output("../text_diagrams/misc.txt", cat_piece(df))

})
