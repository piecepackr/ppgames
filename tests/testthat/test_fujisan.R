library("piecepackr")
library("vdiffr")
context("test fujisan solver")
test_that("fujisan solver works as expected", {

    puzzle2 <- matrix(c(4,4,4,5,2,0,2,4,0,3,1,1,
                        1,2,5,3,3,5,3,2,5,1,0,0), nrow=2, byrow=TRUE)
    s2 <- solve_fujisan(coins=puzzle2)
    g2 <- read_ppn(textConnection(s2$ppn))[[1]]

    expect_doppelganger("fujisan", function() {
        pmap_piece(g2$dfs[[1]], envir=list(piecepack=pp_cfg()), default.units="in")
    })

})
