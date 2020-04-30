library("piecepackr")
context("test visualize")
ppn <- read_ppn(system.file("ppn/tic-tac-toe.ppn", package = "ppgames"))
game <- ppn[[1]]

filename <- "animation.gif"
on.exit(unlink(filename))
animate_game(game)
size_noframes <- file.size(filename)
animate_game(game, nframes=5, trans=op_transform, op_scale=0.5)
size_frames <- file.size(filename)
expect_true(size_frames > size_noframes)
