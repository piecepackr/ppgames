cat_piece <- function(df, ...) ppgames::cat_piece(df, ..., color = FALSE)
cat_move <- function(df, ...) ppgames::cat_move(df, ..., color = FALSE)
test_that("fujisan solver works as expected", {
    coins <- "235334140030554141221205"
    dice <- "0000"
    s <- solve_fujisan(coins=coins, dice=dice)
    expect_length(s$shortest_path, 0)
    expect_true(is.na(s$shortest_distance))

    coins <- "2nnna244naa5/335a45235432"
    dice <- "a2/55"
    s <- solve_fujisan(coins=coins, dice=dice)
    expect_length(s$shortest_path, 13)
    expect_equal(s$shortest_distance, 12)

    skip_on_os("windows")

    expect_snapshot(cat_piece(df_fujisan(seed=42)))
    coins <- "44nna233nna4/5342352a2a55"
    expect_snapshot(cat_piece(df_fujisan(seed=42, coins=coins)))
    expect_snapshot(cat_piece(df_fujisan(seed=42, coins=coins, dice="3n/n5")))

    puzzle2 <- matrix(c(4,4,4,5,2,0,2,4,0,3,1,1,
                        1,2,5,3,3,5,3,2,5,1,0,0), nrow = 2, byrow = TRUE)
    s2 <- solve_fujisan(coins = puzzle2)
    saved_ppn <- paste(readLines(system.file("ppn/fujisan.ppn", package = "ppgames")),
                       collapse = "\n")
    expect_equal(str_wrap(s2$ppn), str_wrap(saved_ppn))
    g2 <- read_ppn(textConnection(s2$ppn))[[1]]
    expect_snapshot(cat_move(g2, move = "SetupFn."))
})
