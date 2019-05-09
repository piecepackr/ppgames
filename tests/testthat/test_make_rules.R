library("piecepackr")
cfg <- pp_cfg()

context("test make rules")
test_that("game rules work as expected", {
    dir <- tempfile()
    dir.create(dir)
    make_rules(NULL, dir)

    expect_true(file.exists(file.path(dir, "backgammon.pdf")))
    expect_true(file.exists(file.path(dir, "tablut.pdf")))

    expect_true(file.exists(file.path(dir, "book.pdf")))
    unlink(dir)
})
