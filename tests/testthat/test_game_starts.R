library("piecepackr")
library("vdiffr")
cfg <- pp_cfg()

context("test game diagrams")
test_that("game diagrams work as expected", {
    expect_doppelganger("alien_city", function() {
        df <- df_alien_city(seed=42)
        pmap_piece(df, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("backgammon", function() {
        df <- df_backgammon()
        pmap_piece(df, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("cribbage", function() {
        df <- df_cribbage_board()
        pmap_piece(df, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("shogi", function() {
        df <- df_shogi()
        ee <- list(piecepack = cfg)
        pmap_piece(df, default.units = "in", envir = ee)
    })
    expect_doppelganger("tablut", function() {
        df <- df_tablut(cfg$get_width("die_face"))
        pmap_piece(df, cfg = cfg, default.units = "in")
    })

    verify_output("../text_diagrams/everest.txt", cat_piece(df_everest()))
    verify_output("../text_diagrams/san_andreas.txt", cat_piece(df_san_andreas()))
    verify_output("../text_diagrams/the_in_crowd.txt", cat_piece(df_the_in_crowd()))

})

    # chess
    # checkers
    # xiangqi (without palaces?)
    # alice chess (twice)
    # cribbage (twice)
    # shogi (twice)
    # tablut
