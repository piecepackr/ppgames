library("piecepackr")
library("vdiffr")
context("test fujisan solver")
test_that("fujisan solver works as expected", {

    puzzle2 <- matrix(c(4,4,4,5,2,0,2,4,0,3,1,1,
                        1,2,5,3,3,5,3,2,5,1,0,0), nrow = 2, byrow = TRUE)
    s2 <- solve_fujisan(coins = puzzle2)
    g2 <- read_ppn(textConnection(s2$ppn))[[1]]

    expect_doppelganger("fujisan", function() {
        pmap_piece(g2$dfs[[1]], envir = list(piecepack = pp_cfg()), default.units = "in")
    })

    coins <- "235334140030554141221205"
    dice <- "0000"
    s <- solve_fujisan(coins=coins, dice=dice)
    expect_length(s$shortest_path, 0)

    verify_output("../text_diagrams/fujisan_seed.txt",
                  cat_piece(df_fujisan(seed=42)))
    coins <- "44nna233nna4/5342352a2a55"
    verify_output("../text_diagrams/fujisan_dice1.txt",
                  cat_piece(df_fujisan(seed=42, coins=coins)))
    verify_output("../text_diagrams/fujisan_dice2.txt",
                  cat_piece(df_fujisan(seed=42, coins=coins, dice="3n/n5")))
})
