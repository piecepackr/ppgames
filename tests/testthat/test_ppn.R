library("piecepackr")
library("vdiffr")
context("test ppn")
ppn1 <- read_ppn(system.file("extdata/ex1.ppn", package="ppgames"))
ppn2 <- read_ppn(system.file("extdata/ex2.ppn", package="ppgames"))
ppn3 <- read_ppn(system.file("extdata/ex3.ppn", package="ppgames"))
ppn4 <- read_ppn(system.file("extdata/ex4.ppn", package="ppgames"))
g1 <- ppn1[[1]]
g2 <- ppn2[[1]]
g3a <- ppn3[[1]]
g3b <- ppn3[[2]]
g4 <- ppn4[[1]]
cfg <- pp_cfg()
pp <- function(df) { function() { pmap_piece(df, cfg=cfg, default.units="in") } }
test_that("parsing ppn files works as expected", {
    expect_true(any(grepl("2. S\\@c1 2... M\\@a3", g1$movetext)))
    expect_equal(g1$moves[["setup."]], "t@b2")
    expect_equal(g1$moves[["1..."]], "M@a2")
    expect_equal(g1$comments[["1..."]], "? (1... M@a1)")
    df1 <- tail(g1$dfs, 1)[[1]]
    expect_doppelganger("tic-tac-toe", pp(df1))

    df2 <- tail(g2$dfs, 1)[[1]]
    expect_doppelganger("four-field-kono", pp(df2))

    df3a <- tail(g3a$dfs, 1)[[1]]
    expect_equal(g3a$metadata$GameType, "Ultima")
    expect_doppelganger("ultima-chess", pp(df3a))

    expect_equal(g3b$metadata$Event, "Example 3 Game B")
    expect_equal(g3b$movetext, "0. t@b4 cA@c3")

    expect_equal(g4$metadata, list())
    expect_equal(g4$movetext, "0. c5@b3 t@(2.5,2.5)")
})

test_that("parsing simplified piece notation works as expected", {
    t <- parse_simplified_piece("t")
    expect_true(is.na(t$suit))
    expect_true(is.na(t$rank))
    expect_equal(t$angle, 0)
    expect_equal(t$piece_side, "tile_back")
    cC <- parse_simplified_piece("cC^")
    expect_equal(cC$suit, 3)
    expect_true(is.na(cC$rank))
    expect_equal(cC$angle, 0)
    expect_equal(cC$piece_side, "coin_back")
    dSn270 <- parse_simplified_piece("dSn>")
    expect_equal(dSn270$suit, 1)
    expect_equal(dSn270$rank, 1)
    expect_equal(dSn270$angle, 270)
    expect_equal(dSn270$piece_side, "die_face")
    c5 <- parse_simplified_piece("c5v")
    expect_true(is.na(c5$suit))
    expect_equal(c5$rank, 6)
    expect_equal(c5$angle, 180)
    expect_equal(c5$piece_side, "coin_face")
    pM <- parse_simplified_piece("pMb")
    expect_equal(pM$suit, 2)
    expect_true(is.na(pM$rank))
    expect_equal(pM$angle, 0)
    expect_equal(pM$piece_side, "pawn_back")
})

test_that("parsing algebraic coordinates works as expected", {
    expect_equal(get_algebraic_x("c4"), 3)
    expect_equal(get_algebraic_y("d5"), 5)
    expect_equal(get_algebraic_x("aa12"), 27)
    expect_equal(get_algebraic_y("aa12"), 12)
})