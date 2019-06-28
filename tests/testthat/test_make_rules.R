context("test make rules")
gk <- game_kit()
dir <- tempfile()
on.exit(unlink(dir))
dir.create(dir)
test_that("game rules work as expected", {
    skip_if(Sys.which("pandoc") == "", "Doesn't have pandoc binary")
    skip_if(Sys.which("xelatex") == "", "Doesn't have xelatex binary")

    save_ruleset("backgammon", gk, dir)
    expect_true(file.exists(file.path(dir, "backgammon.pdf")))
    save_ruleset("tablut", gk, dir)
    expect_true(file.exists(file.path(dir, "tablut.pdf")))
})
test_that("game books work as expected", {
    skip_if(Sys.which("xelatex") == "", "Doesn't have xelatex binary")

    save_rulebook("the-historical-piecepacker", gk, dir)
    expect_true(file.exists(file.path(dir, "the-historical-piecepacker.pdf")))
})
