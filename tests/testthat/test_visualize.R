library("piecepackr")
ppn <- read_ppn(system.file("ppn/tic-tac-toe.ppn", package = "ppgames"))
game <- ppn[[1]]

skip_if_not_installed("gifski")
filename <- "animation.gif"
on.exit(unlink("animation.gif"))
animate_game(game)
size_frames <- file.size(filename)
expect_true(size_frames > 0)

filename <- "tictactoe.png"
on.exit(unlink("tictactoe.png"))
expect_false(file.exists(filename))
plot_move(game, file=filename)
expect_true(file.exists(filename))
