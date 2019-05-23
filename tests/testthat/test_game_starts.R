library("piecepackr")
library("vdiffr")
cfg <- pp_cfg()

context("test game diagrams")
test_that("game diagrams work as expected", {
    expect_doppelganger("backgammon", function() {
        df <- df_backgammon(cfg)
	pmap_piece(df, cfg=cfg, default.units="in")
    })
    expect_doppelganger("cribbage", function() {
        df <- df_cribbage_board(cfg)
        pmap_piece(df, cfg=cfg, default.units="in")
    })
    expect_doppelganger("shogi", function() {
        df <- df_shogi(cfg)
	ee <- list(cfg1=cfg, cfg2=cfg)
        pmap_piece(df, default.units="in", envir=ee)
    })
    expect_doppelganger("tablut", function() {
        df <- df_tablut(cfg)
	pmap_piece(df, cfg=cfg, default.units="in")
    })
})

    # chess
    # checkers
    # xiangqi (without palaces?)
    # alice chess (twice)
    # cribbage (twice)
    # shogi (twice)
    # tablut
