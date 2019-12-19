context("test make rules")
gk <- game_kit()
dir <- tempfile()
on.exit(unlink(dir))
dir.create(dir)
test_that("game rules work as expected", {
    skip_if(Sys.which("pandoc") == "", "Doesn't have pandoc binary")
    skip_if(Sys.which("xelatex") == "", "Doesn't have xelatex binary")

    output <- file.path(dir, "backgammon.pdf")
    save_ruleset("backgammon", gk, output)
    expect_true(file.exists(output))
    output <- file.path(dir, "tablut.pdf")
    save_pamphlet("tablut", gk, output)
    expect_true(file.exists(file.path(dir, "tablut.pdf")))
})
test_that("game books work as expected", {
    skip_if(Sys.which("xelatex") == "", "Doesn't have xelatex binary")

    output <- file.path(dir, "the-historical-piecepacker.pdf")
    save_rulebook("the-historical-piecepacker", gk, output)
    expect_true(file.exists(output))
})
