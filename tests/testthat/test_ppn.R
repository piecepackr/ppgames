library("dplyr")
library("piecepackr")
library("vdiffr")
context("test ppn")
ppn1 <- read_ppn(system.file("ppn/tic-tac-toe.ppn", package = "ppgames"))
ppn2 <- read_ppn(system.file("ppn/four-field-kono.ppn", package = "ppgames"))
ppn3 <- read_ppn(system.file("extdata/ex3.ppn", package = "ppgames"))
ppn4 <- read_ppn(system.file("extdata/ex4.ppn", package = "ppgames"))
g1 <- ppn1[[1]]
g2 <- ppn2[[1]]
g3a <- ppn3[[1]]
g3b <- ppn3[[2]]
g4 <- ppn4[[1]]
pp <- function(df) {
    function() pmap_piece(df, envir = list(piecepack = pp_cfg()), default.units = "in")
}
test_that("parsing ppn files works as expected", {
    expect_true(any(grepl("2. S\\@c1 2... M\\@a3", g1$movetext)))
    expect_equal(g1$moves[["setup."]], "t@b2")
    expect_equal(g1$moves[["1..."]], "M@a2")
    expect_equal(g1$comments[["1..."]], "? (1... M@a1)")
    df1 <- tail(g1$dfs, 1)[[1]]
    expect_doppelganger("tic-tac-toe", pp(df1))

    df2 <- tail(g2$dfs, 1)[[1]]
    expect_doppelganger("four-field-kono", pp(df2))

    verify_output("../text_diagrams/ppn-four-field-kono.txt", cat_move(g2))

    df3a <- tail(g3a$dfs, 1)[[1]]
    expect_equal(g3a$metadata$GameType, "Ultima")

    expect_equal(g3b$metadata$Event, "Example 3 Game B")
    expect_equal(g3b$movetext, "0. t@b4 cA@c3")

    expect_equal(g4$metadata, list())
    expect_equal(g4$movetext, "0. c5@b3 t@(2.5,2.5)")

    no_parse <- "---\nSetUp: Chess\n...\n1. b2-d2 1... g2-e2"
    g <- read_ppn(textConnection(no_parse), parse = FALSE)[[1]]
    expect_length(g, 2)
    expect_equal(g$metadata, list(SetUp = "Chess"))
    expect_equal(g$movetext, "1. b2-d2 1... g2-e2")

    null <- "---\nSetUp: Chess\nMovetextParser: 'Null'\n...\n1. b2-d2 1... g2-e2"
    parser_null <- function(...) list()
    g <- read_ppn(textConnection(null))[[1]]
    expect_length(g, 0)

    null <- "---\nSetUp: Chess\nMovetextParser: Default\n...\n1. b2-d2 1... g2-e2"
    parser_default <- function(...) list()
    g <- read_ppn(textConnection(null))[[1]]
    expect_length(g, 0)
})

test_that("parsing simplified piece notation works as expected", {
    t <- parse_piece("t")
    expect_true(is.na(t$suit))
    expect_true(is.na(t$rank))
    expect_equal(t$angle, 0)
    expect_equal(t$piece_side, "tile_back")
    cC <- parse_piece("cC^")
    expect_equal(cC$suit, 3)
    expect_true(is.na(cC$rank))
    expect_equal(cC$angle, 0)
    expect_equal(cC$piece_side, "coin_back")
    dSn270 <- parse_piece("dSn>")
    expect_equal(dSn270$suit, 1)
    expect_equal(dSn270$rank, 1)
    expect_equal(dSn270$angle, 270)
    expect_equal(dSn270$piece_side, "die_face")
    c5 <- parse_piece("c5v")
    expect_true(is.na(c5$suit))
    expect_equal(c5$rank, 6)
    expect_equal(c5$angle, 180)
    expect_equal(c5$piece_side, "coin_face")
    expect_equal(c5$cfg, "piecepack")
    cn <- parse_piece("n")
    cn2 <- parse_piece("0")
    expect_equal(cn, cn2)
    ca <- parse_piece("af<")
    expect_equal(ca$rank, 2)
    expect_equal(ca$angle, 90)
    expect_equal(ca$piece_side, "coin_face")
    pM <- parse_piece("pMb")
    expect_equal(pM$suit, 2)
    expect_true(is.na(pM$rank))
    expect_equal(pM$angle, 0)
    expect_equal(pM$piece_side, "pawn_back")
    pt <- parse_piece("▲S")
    expect_equal(pt$piece_side, "pyramid_top")
    pl <- parse_piece("▲Sl")
    expect_equal(pl$piece_side, "pyramid_left")
    pr <- parse_piece("▲Sr")
    expect_equal(pr$piece_side, "pyramid_right")
    mM <- parse_piece("mM")
    expect_equal(mM$piece_side, "matchstick_face")
    sM <- parse_piece("sM")
    expect_equal(sM$piece_side, "saucer_back")
    s <- parse_piece("s")
    expect_equal(s$piece_side, "saucer_face")
    # playing cards expansion
    H <- parse_piece("\u2665")
    expect_equal(H$piece_side, "coin_back")
    expect_equal(H$cfg, "playing_cards_expansion")
    expect_equal(H$suit, 1)
    S <- parse_piece("\u2660d")
    expect_equal(S$piece_side, "die_face")
    expect_equal(S$cfg, "playing_cards_expansion")
    expect_equal(S$suit, 2)
    C <- parse_piece("\u2663p")
    expect_equal(C$piece_side, "pawn_face")
    expect_equal(C$cfg, "playing_cards_expansion")
    expect_equal(C$suit, 3)
    D <- parse_piece("\u26665")
    expect_equal(D$piece_side, "tile_face")
    expect_equal(D$cfg, "playing_cards_expansion")
    expect_equal(D$suit, 4)
    # dual piecepacks expansion
    D <- parse_piece("\u26625")
    expect_equal(D$piece_side, "tile_face")
    expect_equal(D$cfg, "dual_piecepacks_expansion")
    expect_equal(D$suit, 4)
    # icehouse pieces
    I <- parse_piece("\u25b3Y2")
    expect_equal(I$piece_side, "pyramid_top")
    expect_equal(I$cfg, "icehouse_pieces")
    expect_equal(I$rank, 2)
    expect_equal(I$suit, 5)
    I <- parse_piece("xW1")
    expect_equal(I$piece_side, "pyramid_top")
    expect_equal(I$cfg, "icehouse_pieces")
    expect_equal(I$rank, 1)
    expect_equal(I$suit, 6)
    # subpack
    c5 <- parse_piece("\u03bcc5v")
    expect_true(is.na(c5$suit))
    expect_equal(c5$rank, 6)
    expect_equal(c5$angle, 180)
    expect_equal(c5$piece_side, "coin_face")
    expect_equal(c5$cfg, "subpack")
    c5 <- parse_piece("\u00b5c5v")
    expect_true(is.na(c5$suit))
    expect_equal(c5$rank, 6)
    expect_equal(c5$angle, 180)
    expect_equal(c5$piece_side, "coin_face")
    expect_equal(c5$cfg, "subpack")
    c5 <- parse_piece("uc5v")
    expect_true(is.na(c5$suit))
    expect_equal(c5$rank, 6)
    expect_equal(c5$angle, 180)
    expect_equal(c5$piece_side, "coin_face")
    expect_equal(c5$cfg, "subpack")
    # hexpack
    ht <- parse_piece("\u2b22")
    expect_equal(ht$piece_side, "tile_back")
    expect_equal(ht$cfg, "hexpack")
})

test_that("parsing algebraic coordinates works as expected", {
    expect_equal(get_algebraic_x("c4"), 3)
    expect_equal(get_algebraic_y("d5"), 5)
    expect_equal(get_algebraic_x("aa12"), 27)
    expect_equal(get_algebraic_y("aa12"), 12)
})

test_that("process_move works as expected", {
    expect_equal(process_move(tibble(), ""), tibble())

    df <- df_four_field_kono()
    df$cfg <- "piecepack"
    df <- tibble::rowid_to_column(df, "id")
    expect_equal(nrow(df), 20)

    df <- process_move(df, "*b4")
    df <- process_move(df, "*d2")
    expect_equal(nrow(df), 18)

    df <- process_move(df, "b2=dC4")
    expect_equal(nrow(df), 18)
    expect_true(any(grepl("die_face", df$piece_side)))

    expect_error(process_move(df, "!"))
    expect_error(get_id_from_coords(df, "e5"))
})

test_that("parse_moves works as expected", {
    l <- parse_moves(c("S@b4", "M@b2"))
    df1 <- l$dfs[[2]]
    expect_equal(nrow(df1), 2)

    l <- parse_moves(c("S@b4", "2. M@b2"))
    df2 <- l$dfs[[3]]
    expect_equal(nrow(df2), 2)
    expect_equal(df1, df2)

    df <- insert_df(df1, df2, 1)
    expect_equal(nrow(df), 4)
    expect_equal(df$suit, rep(1:2, each = 2))
})

test_that("# notation works as expected", {
    df <- tibble::rowid_to_column(df_none(), "id")
    state <- create_state(df)
    df <- process_move(tibble(), "S@b2", state)
    expect_true(near(df$x, 2))
    expect_equal(nrow(df), 1)
    expect_equal(df$suit, 1)
    df <- process_move(df, "#1=M", state)
    expect_equal(df$suit, 2)
    df <- process_move(df, "#2-c3", state)
    expect_true(near(df$x, 3))
    expect_equal(nrow(df), 1)
    df <- process_move(df, "*#2", state)
    expect_equal(nrow(df), 0)
})

test_that("Move multiple pieces works as expected", {
    df <- tail(process_moves(tibble(), c("S@b2", "S@b2", "S@b2")), 1)[[1]]
    expect_equal(sum(near(df$x, 2)), 3)
    df <- process_move(df, "2b2-c3")
    expect_equal(sum(near(df$x, 2)), 1)
    expect_equal(sum(near(df$x, 3)), 2)
    expect_equal(nrow(df), 3)
    df <- process_move(df, "b2:2c3")
    expect_equal(sum(near(df$x, 2)), 0)
    expect_equal(sum(near(df$x, 3)), 1)
    expect_equal(nrow(df), 1)
    df <- tail(process_moves(tibble(), c("S@b2", "S@b2", "S@b2")), 1)[[1]]
    df <- process_move(df, "3b2-c3")
    expect_equal(sum(near(df$x, 2)), 0)
    expect_equal(sum(near(df$x, 3)), 3)
    df <- process_move(df, "c3-b2")
    expect_equal(sum(near(df$x, 2)), 1)
    expect_equal(sum(near(df$x, 3)), 2)
    df <- process_move(df, "2c3:b2")
    expect_equal(sum(near(df$x, 2)), 2)
    expect_equal(sum(near(df$x, 3)), 0)
    expect_equal(nrow(df), 2)
})

test_that("move multiple pieces works as expected", {
    checkers <- read_ppn(system.file("ppn/checkers.ppn", package = "ppgames"))[[1]]
    verify_output("../text_diagrams/ppn_checkers.txt", cat_move(checkers))
})

test_that("scale_factor works as expected", {
    scale <- "MovetextParser:\n  Name: Default\n  ScaleFactor: 2\n...\n1. S@a2"
    df <- tail(read_ppn(textConnection(scale))[[1]]$dfs, 1)[[1]]
    expect_true(near(df$x, 2))
    expect_true(near(df$y, 4))
    scale <- "MovetextParser:\n  Name: Default\n  ScaleFactor: 2\n...\n1. S@a2 a2-b3 M@d3 b3:d3"
    df <- tail(read_ppn(textConnection(scale))[[1]]$dfs, 1)[[1]]
    expect_true(near(df$x, 8))
    expect_true(near(df$y, 6))
})

test_that("Setup and GameType work as expected", {
    chess1 <- "GameType: Chess\n"
    df1 <- read_ppn(textConnection(chess1))[[1]]$dfs[[1]]
    chess2 <- "GameType:\n  Name: Chess\n"
    df2 <- read_ppn(textConnection(chess1))[[1]]$dfs[[1]]
    expect_true(identical(df1, df2))
    chess3 <- "SetUp: Chess\n\nMovetextParser:\n  Name: Default\n...\n"
    df3 <- read_ppn(textConnection(chess3))[[1]]$dfs[[1]]
    expect_true(identical(df1, df3))
    chess4 <- "SetUp:\n  Name: Chess\nMovetextParser: Default\n...\n"
    df4 <- read_ppn(textConnection(chess4))[[1]]$dfs[[1]]
    expect_true(identical(df1, df4))
    chess5 <- "SetUp:\n  Name: Chess\n  System: Piecepack\nGameType: Hostage Chess\n...\n"
    df5 <- read_ppn(textConnection(chess5))[[1]]$dfs[[1]]
    expect_true(identical(df1, df5))
    chess6 <- "---\nSetUp:\n  Chess\nGameType: Hostage Chess\n...\n"
    df6 <- read_ppn(textConnection(chess6))[[1]]$dfs[[1]]
    expect_true(identical(df1, df6))
    chess7 <- "GameType:\n  Name: Chess\n  System: Piecepack\n...\n"
    df7 <- read_ppn(textConnection(chess7))[[1]]$dfs[[1]]
    expect_true(identical(df1, df7))
    chess8 <- "GameType:\n  Name: Chess\n  System: Stackpack\n...\n"
    df8 <- read_ppn(textConnection(chess8))[[1]]$dfs[[1]]
    expect_false(identical(df1, df8))
    chess9 <- "GameType:\n  Name: Chess\n  Has Subpack: true\n...\n"
    df9 <- read_ppn(textConnection(chess9))[[1]]$dfs[[1]]
    expect_true(identical(df8, df9))

    none1 <- ""
    df1 <- read_ppn(textConnection(none1))[[1]]$dfs[[1]]
    expect_equal(nrow(df1), 0)
    none2 <- "GameType: None\n...\n"
    df2 <- read_ppn(textConnection(none2))[[1]]$dfs[[1]]
    expect_equal(nrow(df2), 0)
    none3 <- "GameType: Tak\nSetUp:  None\n...\n"
    df3 <- read_ppn(textConnection(none3))[[1]]$dfs[[1]]
    expect_equal(nrow(df3), 0)

    expect_equal(get_ppn_package("Piecepack"), "ppgames")
    expect_equal(get_ppn_package("Stackpack"), "ppgames")
    expect_equal(get_ppn_package("Chess"), "tradgames")
    expect_equal(get_ppn_package("Checkers"), "tradgames")
    expect_equal(get_ppn_package("Traditional"), "tradgames")
    expect_equal(get_ppn_package("Looney Pyramids"), "piecenikr")
    expect_equal(get_ppn_package("Icehouse Pieces"), "piecenikr")
    expect_equal(get_ppn_package("Icehouse"), "piecenikr")
    expect_error(get_ppn_package("Realm"), "Don't recognize system Realm")
})
