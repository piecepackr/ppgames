library("piecepackr")
context("test visualize")
ppn <- read_ppn(system.file("ppn/tic-tac-toe.ppn", package = "ppgames"))
game <- ppn[[1]]

filename <- "animation.gif"
on.exit(unlink(filename))
animate_game(game)
size_noframes <- file.size(filename)
animate_game(game, n_transitions=3, trans=op_transform, op_scale=0.5)
size_frames <- file.size(filename)
expect_true(size_frames > size_noframes)

filename <- "tictactoe_rayrender.png"
on.exit(unlink(filename))
expect_false(file.exists(filename))
plot_move(game, file=filename, .f=piece)
expect_true(file.exists(filename))
