library("dplyr")
library("piecepackr")
library("vdiffr")
cfg <- pp_cfg()

context("test game diagrams")
test_that("game diagrams work as expected", {
    verify_output("../text_diagrams/alice_chess.txt", cat_piece(df_alice_chess()))
    verify_output("../text_diagrams/alien_city.txt",
                  cat_piece(df_alien_city(seed=42), reorient="symbols"))
    tiles <- "G^R^K^R^/R<B<GvB^/B<R^K<B</GvR^K>B>/G>K>G>K<"
    df <- df_alien_city(seed=42, tiles=tiles)
    verify_output("../text_diagrams/alien_city_tiles.txt", cat_piece(df, reorient="symbols"))
    tiles <- "G3^Rn^K3^R4^/R3<Ba<GnvB4^/B2<R2^Ka<Bn</G4vRa^K4>B3>/Ga>Kn>G2>K2<"
    df <- df_alien_city(seed=42, tiles=tiles)
    verify_output("../text_diagrams/alien_city_tiles.txt", cat_piece(df, reorient="symbols"))
    verify_output("../text_diagrams/alquerque.txt", cat_piece(df_alquerque()))
    verify_output("../text_diagrams/backgammon.txt", cat_piece(df_backgammon()))
    verify_output("../text_diagrams/breakthrough.txt", cat_piece(df_breakthrough()))
    verify_output("../text_diagrams/checkers.txt", cat_piece(df_checkers()))
    verify_output("../text_diagrams/chinese_checkers.txt", cat_piece(df_chinese_checkers(), reorient="all"))
    verify_output("../text_diagrams/cribbage.txt", cat_piece(df_cribbage_board()))
    verify_output("../text_diagrams/desfases_seed.txt",
                  cat_piece(df_desfases(seed=42), reorient="symbols"))
    verify_output("../text_diagrams/evade.txt", cat_piece(df_evade()))
    verify_output("../text_diagrams/everest.txt", cat_piece(df_everest()))
    verify_output("../text_diagrams/froggy_bottom.txt", cat_piece(df_froggy_bottom()))
    verify_output("../text_diagrams/grasshopper.txt", cat_piece(df_grasshopper()))
    verify_output("../text_diagrams/iceberg.txt", cat_piece(df_iceberg()))
    verify_output("../text_diagrams/ice_floe.txt", cat_piece(df_ice_floe()))
    verify_output("../text_diagrams/international_chess.txt", cat_piece(df_international_chess()))
    verify_output("../text_diagrams/japan.txt", cat_piece(df_japan(seed=42)))
    verify_output("../text_diagrams/jul_gono.txt", cat_piece(df_jul_gono()))
    verify_output("../text_diagrams/ley_lines.txt", cat_piece(df_ley_lines()))
    verify_output("../text_diagrams/lines_of_action.txt",
                  cat_piece(df_lines_of_action(), reorient="all"))
    verify_output("../text_diagrams/nine_mens_morris_matchsticks.txt",
                  cat_piece(df_nine_mens_morris(has_matchsticks = TRUE)))
    verify_output("../text_diagrams/pass_the_food.txt", cat_piece(df_pass_the_food()))
    verify_output("../text_diagrams/piecepackmen.txt",
                  cat_piece(df_piecepackman(seed = 42)))
    expect_error(df_piecepackman(seed = 42, variant = 2))
    verify_output("../text_diagrams/plans_of_action_seed.txt", cat_piece(df_plans_of_action(seed=42)))
    coins <- "ASSCCM/CAMSMS/AAMCSS/ACAMMC"
    verify_output("../text_diagrams/plans_of_action_coins.txt", cat_piece(df_plans_of_action(coins=coins)))
    verify_output("../text_diagrams/relativity_seed.txt", cat_piece(df_relativity(seed=42)))
    coins <- "3ann4a/524253/345n34/a2na52"
    verify_output("../text_diagrams/relativity_coins.txt", cat_piece(df_relativity(coins=coins)))
    verify_output("../text_diagrams/salta.txt", cat_piece(df_salta()))
    verify_output("../text_diagrams/san_andreas.txt", cat_piece(df_san_andreas()))
    verify_output("../text_diagrams/the_in_crowd.txt", cat_piece(df_the_in_crowd()))
    verify_output("../text_diagrams/tablut.txt",
                  cat_piece(df_tablut(), reorient="all"))
    verify_output("../text_diagrams/tower_of_babel_seed.txt", cat_piece(df_tower_of_babel(seed=42)))
    verify_output("../text_diagrams/triactor.txt",
                  cat_piece(df_triactor(), reorient="all"))
    verify_output("../text_diagrams/tula.txt", cat_piece(df_tula()))
    verify_output("../text_diagrams/turkish_draughts.txt", cat_piece(df_turkish_draughts()))
    verify_output("../text_diagrams/wormholes.txt", cat_piece(df_wormholes()))
    verify_output("../text_diagrams/xiangqi.txt", cat_piece(df_xiangqi(), annotate="cartesian"))

    # subpack
    verify_output("../text_diagrams/chaturaji_subpack.txt",
                  cat_piece(df_chaturaji(TRUE), reorient="all"))
    verify_output("../text_diagrams/four_seasons_chess_subpack.txt",
                  cat_piece(df_four_seasons_chess(TRUE), reorient="all"))
    verify_output("../text_diagrams/international_chess_subpack.txt",
                  cat_piece(df_international_chess(TRUE)))
    verify_output("../text_diagrams/salta_subpack.txt",
                  cat_piece(df_salta(TRUE)))
    verify_output("../text_diagrams/shogi_subpack.txt",
                  cat_piece(df_shogi(TRUE), reorient="all"))
    verify_output("../text_diagrams/ultima_subpack.txt",
                  cat_piece(df_ultima(TRUE)))
    verify_output("../text_diagrams/xiangqi_subpack.txt",
                  cat_piece(df_xiangqi(TRUE)))

    expect_error(process_tiles("&^&&"))

    # graphic checks
    skip_on_ci()
    expect_doppelganger("cell_management", function() {
        text <- "---\nGameType:\n  Name: Cell Management\n  Seed: 42\n..."
        game <- read_ppn(textConnection(text))[[1]]
        plot_move(game, new_device=FALSE)
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
})
