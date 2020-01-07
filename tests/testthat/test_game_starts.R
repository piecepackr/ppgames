library("piecepackr")
library("vdiffr")
cfg <- pp_cfg()

context("test game diagrams")
test_that("game diagrams work as expected", {
    expect_doppelganger("alien_city", function() {
        df <- df_alien_city(seed=42)
        pmap_piece(df, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("alien_city_tiles", function() {
        tiles <- "G^R^K^R^/R<B<GvB^/B<R^K<B</GvR^K>B>/G>K>G>K<"
        df <- df_alien_city(seed=42, tiles=tiles)
        pmap_piece(df, cfg = cfg, default.units = "in")
    })
    expect_doppelganger("alien_city_tiles", function() {
        tiles <- "G3^Rn^K3^R4^/R3<Ba<GnvB4^/B2<R2^Ka<Bn</G4vRa^K4>B3>/Ga>Kn>G2>K2<"
        df <- df_alien_city(seed=42, tiles=tiles)
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

    verify_output("../text_diagrams/alice_chess", cat_piece(df_alice_chess()))
    verify_output("../text_diagrams/checkers.txt", cat_piece(df_checkers()))
    verify_output("../text_diagrams/everest.txt", cat_piece(df_everest()))
    verify_output("../text_diagrams/ice_floe.txt", cat_piece(df_ice_floe()))
    verify_output("../text_diagrams/international_chess.txt", cat_piece(df_international_chess()))
    verify_output("../text_diagrams/plans_of_action_seed.txt", cat_piece(df_plans_of_action(seed=42)))
    coins <- "ASSCCM/CAMSMS/AAMCSS/ACAMMC"
    verify_output("../text_diagrams/plans_of_action_coins.txt", cat_piece(df_plans_of_action(coins=coins)))
    verify_output("../text_diagrams/relativity_seed.txt", cat_piece(df_relativity(seed=42)))
    coins <- "3ann4a/524253/345n34/a2na52"
    verify_output("../text_diagrams/relativity_coins.txt", cat_piece(df_relativity(coins=coins)))
    verify_output("../text_diagrams/salta.txt", cat_piece(df_salta()))
    verify_output("../text_diagrams/san_andreas.txt", cat_piece(df_san_andreas()))
    verify_output("../text_diagrams/the_in_crowd.txt", cat_piece(df_the_in_crowd()))
    verify_output("../text_diagrams/wormholes.txt", cat_piece(df_wormholes()))
    verify_output("../text_diagrams/xiangqi.txt", cat_piece(df_xiangqi()))
})
