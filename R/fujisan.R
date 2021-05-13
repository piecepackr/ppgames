state_env <- new.env()
get_states <- function() {
    if (exists("states", envir = state_env)) {
        states <- get("states", envir = state_env)
    } else {
        states <- utils::combn(28, 4) # gives all possible states of (28 C 4)
        assign("states", states, envir = state_env)
    }
    states
}

state2string <- function(s) paste(s, collapse = "_")

state2vector <- function(x) {
    v <- numeric(28)
    v[x] <- 1
    v
}
state2string <- function(s) paste(s, collapse = "_")
matrix2string <- function(m) {
    state2string(matrix2state(m))
}

matrix2state <- function(m) {
    which(c(m[1, ], m[2, ]) > 0)
}

states2edges <- function(state, coins, dice) {
    v <- state2vector(state)
    ps <- character(0)
    indices <- which(v > 0)
    ics <- (indices-1) %% 14 + 1
    irs <- (indices-1) %/% 14 + 1
    ors <- irs %% 2 + 1
    sm <- matrix(v, nrow = 2, byrow = TRUE)
    for (ii in seq(4)) {
        ic <- ics[ii]
        ir <- irs[ii]
        or <- ors[ii]
        # vertical moves
        if (sm[or, ic] == 0 && ic > 1 && ic < 14) {
            os <- sm
            os[ir, ic] <- 0
            os[or, ic] <- 1
            ps <- append(ps, matrix2string(os))
        }
        # horizontal moves on top of mountain
        if (ic == 7 && sm[ir, 8] == 0) {
            os <- sm
            os[ir, 7] <- 0
            os[ir, 8] <- 1
            ps <- append(ps, matrix2string(os))
        }
        if (ic == 8 && sm[ir, 7] == 0) {
            os <- sm
            os[ir, 8] <- 0
            os[ir, 7] <- 1
            ps <- append(ps, matrix2string(os))
        }
        # normal moves
        if (ic < 7 || ic > 8) {
            dist <- get_distances(sm, ir, ic, coins) # dice
            for (im in which(dist[ir, ]==coins[ir, ] & coins[ir, ] > 0)) {
                im <- im+1
                os <- sm
                os[ir, ic] <- 0
                os[ir, im] <- 1
                ps <- append(ps, matrix2string(os))
            }
        }
    }
    # initial dice moves
    n <- state2string(state)
    if (length(ps) == 0 && n == "1_14_15_28") {
        if (dice[1] > 0) {
            os <- sm
            os[1, 1] <- 0
            os[1, 1+dice[1]] <- 1
            ps <- append(ps, matrix2string(os))
        }
        if (dice[2] > 0) {
            os <- sm
            os[1, 14] <- 0
            os[1, 14-dice[2]] <- 1
            ps <- append(ps, matrix2string(os))
        }
        if (dice[3] > 0) {
            os <- sm
            os[2, 1] <- 0
            os[2, 1+dice[3]] <- 1
            ps <- append(ps, matrix2string(os))
        }
        if (dice[4] > 0) {
            os <- sm
            os[2, 14] <- 0
            os[2, 14-dice[4]] <- 1
            ps <- append(ps, matrix2string(os))
        }
    }
    ans <- matrix(n, nrow = length(ps), ncol = 2)
    ans[, 2] <- ps
    ans
}

get_distances <- function(sm, ir, ic, coins) {
    distances <- matrix(0, nrow = 2, ncol = 14)
    ris <- which(1:14 > ic & sm[ir, 1:14] == 0)
    distances[ir, ris] <- cumsum(rep(1, length(ris)))
    lis <- which(1:14 < ic & sm[ir, 1:14] == 0)
    distances[ir, lis] <- rev(cumsum(rep(1, length(lis))))
    distances[, 2:13]
}

random_fujisan_coins <- function(...) {
    coins <- integer(24)
    coins[which(1:24 %% 4 == 0)] <- sample(0:5)
    coins[which(1:24 %% 4 == 1)] <- sample(0:5)
    coins[which(1:24 %% 4 == 2)] <- sample(0:5)
    coins[which(1:24 %% 4 == 3)] <- sample(0:5)
    coins <- matrix(coins, nrow = 2, byrow = TRUE)
    coins
}

path2movetext <- function(p) {
    if (length(p) == 0) return("")
    p <- stringr::str_split(p, "_")
    p <- lapply(p, n2alg)
    movetext <- character(length(p)-1)
    for (ii in seq(length(p)-1)) {
        b <- p[[ii]]
        a <- p[[ii+1]]
        i <- intersect(b, a)
        movetext[ii] <- str_glue("{movenumber}. {from}-{to}",
                                 movenumber = ii,
                                 from = b[!(b %in% i)],
                                 to = a[!(a %in% i)])
    }
    movetext
}
path2counter_intuitive <- function(p) {
    if (length(p) == 0) return(NA_integer_)
    p <- stringr::str_split(p, "_")
    p <- lapply(p, n2alg)
    n_counter_intuitive <- 0
    for (ii in seq(length(p)-1)) {
        b <- p[[ii]]
        a <- p[[ii+1]]
        i <- intersect(b, a)
        bl <- alg2level(b[!(b %in% i)])
        al <- alg2level(a[!(a %in% i)])
        if (al < bl) n_counter_intuitive <- n_counter_intuitive + 1
    }
    n_counter_intuitive
}
alg2level <- function(alg) {
    file <- substr(alg, 1, 1)
    switch(file, a = 0, b = 1, c = 2, d = 3, e = 4, f = 5, g = 6, h = 6, i = 5, j = 4, k = 3, l = 2, m = 1, n = 0)
}

coins2string <- function(coins, sep = "/") {
    coins <- c(coins[1, ], coins[2, ])
    coins <- as.character(coins)
    coins <- gsub("0", "n", coins)
    coins <- gsub("1", "a", coins)
    paste0(paste(coins[1:12], collapse = ""), sep, paste(coins[13:24], collapse = ""))
}

dice2string <- function(dice, sep = "/") {
    if (any(is.na(dice))) return(NA_character_)
    dice <- as.character(dice)
    dice <- gsub("0", "n", dice)
    dice <- gsub("1", "a", dice)
    paste0(paste(dice[1:2], collapse = ""), sep, paste(dice[3:4], collapse = ""))
}
dice2ppn <- function(coins, dice) {
    if (first_move_needs_dice(coins)) {
        str_glue('\n  Dice: "{dice}"\n', dice = dice2string(dice, "/"))
    } else {
        ""
    }
}

sol2ppn <- function(sol) {
    movetext <- paste(path2movetext(sol$shortest_path), collapse = "\n")
    metadata <- str_glue('---\nGameType:\n  Name: Fujisan\n  Coins: "{coins}"{dice}\n...',
                         coins = coins2string(sol$coins, "/"),
                         dice = dice2ppn(sol$coins, sol$dice))
    paste0(metadata, "\n", movetext, "\n")
}

n2alg <- function(n) {
    n <- as.numeric(n)
    ic <- (n-1) %% 14 + 1
    ir <- (n-1) %/% 14 + 1
    paste0(letters[ic], 3-ir)
}

first_move_needs_dice <- function(coins) {
    !((1 %in% coins[, c(1, 12)]) ||
      (2 %in% coins[, c(2, 11)]) ||
      (3 %in% coins[, c(3, 10)]) ||
      (4 %in% coins[, c(4, 9)]) ||
      (5 %in% coins[, c(5, 8)]))
}

#' Solve Fujisan game
#'
#' Solves a game of Fujisan (if possible).
#' @param coins A vector or matrix of Fujisan coin layout.  Default is a random layout.
#' @param dice  A vector of Fujisan dice layout.  Default is random dice.  Usually not needed.
#' @return A list with solution of Fujisan solution, its length, coin layout, dice (if needed),
#'         and portable piecepack notation.
#' @rdname game_solvers
#' @examples
#'  puzzle2 <- matrix(c(4, 4, 4, 5, 2, 0, 2, 4, 0, 3, 1, 1,
#'                      1, 2, 5, 3, 3, 5, 3, 2, 5, 1, 0, 0), nrow = 2, byrow = TRUE)
#'  s2 <- solve_fujisan(coins = puzzle2)
#'  g2 <- read_ppn(textConnection(s2$ppn))[[1]]
#' @export
solve_fujisan <- function(coins = random_fujisan_coins(), dice = random_dice() - 1) {
    piecepackr:::assert_suggested("igraph")
    if (is.vector(coins)) {
        coins <- process_ranks(coins) - 1
        coins <- matrix(coins, nrow = 2, byrow = TRUE)
    }
    dice <- process_ranks(dice) - 1

    edges <- do.call(rbind, apply(get_states(), 2, states2edges, coins, dice))
    g <- igraph::graph_from_edgelist(edges, directed = TRUE)
    te <- try({
        initial <- igraph::V(g)["1_14_15_28"]
        final <- igraph::V(g)["7_8_21_22"]
        d <- igraph::distances(g, v = initial, to = final, mode = "out")
        d <- as.integer(d)
    }, silent=TRUE)
    if (inherits(te, "try-error")) {
        d <- NA_integer_
        p <- character(0)
    } else {
        p <- names(igraph::shortest_paths(g, from = initial, to = final)$vpath[[1]])
    }
    if (!first_move_needs_dice(coins)) dice <- NA_integer_
    sol <- list(shortest_distance = d, shortest_path = p, coins = coins, dice = dice,
         coin_string = coins2string(coins), dice_string = dice2string(dice),
         n_counter_intuitive = path2counter_intuitive(p))
    sol$ppn <- sol2ppn(sol)
    sol
}
