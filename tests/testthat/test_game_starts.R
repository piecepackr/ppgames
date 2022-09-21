cat_piece <- function(df, ..., color = FALSE) ppgames::cat_piece(df, ..., color = color)

test_that("game diagrams work as expected", {
    skip_if_not_installed("dplyr")
    skip_if_not_installed("piecepackr")
    skip_if_not_installed("vdiffr")
    skip_on_os("windows")

    library("piecepackr")
    library("vdiffr")

    cfg <- pp_cfg()

    expect_snapshot(cat_piece(df_alice_chess()))
    expect_snapshot(cat_piece(df_alien_city(seed=42), reorient="symbols"))
    expect_snapshot({
        tiles <- "G^R^K^R^/R<B<GvB^/B<R^K<B</GvR^K>B>/G>K>G>K<"
        cat_piece(df_alien_city(seed=42, tiles=tiles),
                  reorient="symbols")
    })

    expect_snapshot({
        tiles <- "G3^Rn^K3^R4^/R3<Ba<GnvB4^/B2<R2^Ka<Bn</G4vRa^K4>B3>/Ga>Kn>G2>K2<"
        cat_piece(df_alien_city(seed=42, tiles=tiles),
                  reorient="symbols")
    })
    expect_snapshot(cat_piece(df_alquerque()))
    expect_snapshot(cat_piece(df_alquerque(TRUE)))
    expect_snapshot(cat_piece(df_backgammon()))
    expect_snapshot(cat_piece(df_brandubh(), reorient="all"))
    expect_snapshot(cat_piece(df_breakthrough()))
    expect_snapshot(cat_piece(df_checkers()))
    expect_snapshot(cat_piece(df_chinese_checkers(), reorient="all"))
    expect_snapshot(cat_piece(df_coin_collectors(seed = 15)))
    expect_snapshot(cat_piece(df_cribbage_board()))
    expect_snapshot({
        df <- df_desfases(seed=42)
        tiles <- generate_sra(df)
        dice <- generate_sra(df, "^die", "r")
        df <- df_desfases(tiles=tiles, dice=dice)
        cat_piece(df, reorient="symbols")
    })
    expect_snapshot(cat_piece(df_easy_slider(seed=71)))
    expect_snapshot(cat_piece(df_evade()))
    expect_snapshot(cat_piece(df_everest()))
    expect_snapshot(cat_piece(df_froggy_bottom()))
    expect_snapshot(cat_piece(df_four_blind_mice()))
    expect_snapshot(cat_piece(df_grasshopper()))
    expect_snapshot({
        df <- df_iceberg(seed=42)
        tiles <- generate_sra(df)
        df <- df_iceberg(tiles = tiles)
        cat_piece(df)
    })
    expect_snapshot(cat_piece(df_ice_floe()))
    expect_snapshot(cat_piece(df_international_chess()))
    expect_snapshot(cat_piece(df_japan(seed=42)))
    expect_snapshot(cat_piece(df_jul_gono()))
    expect_snapshot(cat_piece(df_landlocked(seed=42)))
    expect_snapshot(cat_piece(df_ley_lines()))
    expect_snapshot(cat_piece(df_lines_of_action(), reorient="all"))
    expect_snapshot(cat_piece(df_mathrix(seed=72)))
    expect_snapshot(cat_piece(df_nine_mens_morris(has_matchsticks = TRUE)))
    expect_snapshot(cat_piece(df_pass_the_food()))
    expect_snapshot(cat_piece(df_piece_gaps(seed = 23)))
    expect_snapshot(cat_piece(df_piece_packing_pirates(seed = 42)))
    expect_snapshot(cat_piece(df_piecepack_klondike(seed = 42)))
    expect_snapshot(cat_piece(df_piecepackman(seed = 42)))
    expect_error(df_piecepackman(seed = 42, variant = 2))
    expect_snapshot(cat_piece(df_plans_of_action(seed=42)))
    expect_snapshot({
        coins <- "ASSCCM/CAMSMS/AAMCSS/ACAMMC"
        cat_piece(df_plans_of_action(coins=coins))}
    )
    expect_snapshot(cat_piece(df_quatri(), color=NULL))
    expect_snapshot(cat_piece(df_relativity(seed=42)))
    expect_snapshot({
        coins <- "3ann4a/524253/345n34/a2na52"
        cat_piece(df_relativity(coins=coins))
    })
    expect_snapshot(cat_piece(df_salta()))
    expect_snapshot(cat_piece(df_san_andreas()))
    expect_snapshot(cat_piece(df_skyscrapers(seed=23)))
    expect_snapshot(cat_piece(df_slides_of_action()))
    expect_snapshot(cat_piece(df_the_in_crowd()))
    expect_snapshot(cat_piece(df_the_magic_bag(seed=27)))
    expect_snapshot(cat_piece(df_tablut(), reorient="all"))
    expect_snapshot({
        df <- df_tower_of_babel(seed=42)
        tiles <- generate_sra(df, "^tile", "sr")
        df <- df_tower_of_babel(tiles = tiles)
        cat_piece(df)
    })
    expect_snapshot(cat_piece(df_triactor(), reorient="all"))
    expect_snapshot({
        df <- df_tula(seed=42)
        tiles <- generate_sra(df)
        df <- df_tula(tiles = tiles)
        cat_piece(df)
    })
    expect_snapshot(cat_piece(df_turkish_draughts()))
    expect_snapshot(cat_piece(df_wormholes()))
    expect_snapshot(cat_piece(df_xiangqi(), annotate="cartesian"))

    # subpack
    expect_snapshot(cat_piece(df_chaturaji(TRUE), reorient="all"))
    expect_snapshot(cat_piece(df_four_seasons_chess(TRUE), reorient="all"))
    expect_snapshot(cat_piece(df_international_chess(TRUE)))
    expect_snapshot(cat_piece(df_salta(TRUE)))
    expect_snapshot(cat_piece(df_shogi(TRUE), reorient="all"))
    expect_snapshot(cat_piece(df_ultima(TRUE)))
    expect_snapshot(cat_piece(df_xiangqi(TRUE)))

    expect_error(process_tiles("&^&&"))

    # graphic checks
    skip_on_ci()
    ee <- list(piecepack = cfg)
    expect_doppelganger("awithlaknannai_mosona", function() {
        df <- df_awithlaknannai_mosona(TRUE)
        pmap_piece(df, default.units = "in", envir = ee)
    })
    expect_doppelganger("cell_management", function() {
        text <- "---\nGameType:\n  Name: Cell Management\n  Seed: 42\n..."
        game <- read_ppn(textConnection(text))[[1]]
        plot_move(game, new_device=FALSE)
    })
    expect_doppelganger("ludo", function() {
        df <- df_ludo()
        pmap_piece(df, default.units = "in", envir = ee)
    })
    expect_doppelganger("shogi", function() {
        df <- df_shogi()
        pmap_piece(df, default.units = "in", envir = ee)
    })
    expect_doppelganger("tablut", function() {
        df <- df_tablut(cfg$get_width("die_face"))
        pmap_piece(df, cfg = cfg, default.units = "in")
    })
})
