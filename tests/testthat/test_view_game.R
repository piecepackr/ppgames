context("test view game")
test_that("view_game works as expected", {
    ppn <- read_ppn(system.file("ppn/tic-tac-toe.ppn", package = "ppgames"))
    game <- ppn[[1]]

    tmp <- tempfile(fileext = ".ppn")
    write_ppn(list(game), file = tmp)
    game2 <- read_ppn(tmp)[[1]]
    expect_equal(game$moves, game2$moves)
    expect_output(write_ppn(list(game2), ""), "Example Tic-Tac-Toe Game")
    unlink(tmp)

    move <- tail(names(game$moves), 1)
    prev <- prev_move(game, move)
    expect_equal(move, "4.")
    expect_equal(prev, "3...")
    expect_equal(next_move(game, prev), "4.")

    print_screen <- function(...) ppgames:::print_screen(..., color = FALSE)
    verify_output("../text_diagrams/print_screen.txt",
                  print_screen(game, move, clear = FALSE))

    g <- list(metadata = NULL, movetext = character())
    g <- append_to_ppn(g, "1. S@b2")
    expect_equal(nrow(g$dfs[["1."]]), 1)

    skip_if_not_installed("argparse")
    p <- get_parser()
    expect_output(p$print_help(), "subcommands")
})
