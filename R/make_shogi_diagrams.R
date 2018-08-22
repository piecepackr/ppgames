library("grid")
suppressPackageStartupMessages(library("piecepackr"))

cfg_orthodox <- read_configuration("--file=configurations/orthodox1.json")
cfg_chess5 <- read_configuration("--file=configurations/chess5.json")
cfg_chess6 <- read_configuration("--file=configurations/chess6.json")

## Shogi Board (standard piecepack)
draw_shogi_board_orthodox <- function() {
    cfg <- cfg_orthodox
    # tiles
    pushViewport(viewport(width=inch(10), height=inch(10), name="board"))
    seekViewport("board")
    for (xx in seq(2, 8, by=2)) {
        for (yy in seq(2, 8, by=2)) {
            draw_component("tile_back", cfg, x=inch(xx), y=inch(yy))
        }
    }
    # pawns
    for (xx in 1:9) {
        i_s <- xx %% 4 + 1
        draw_component("coin_back", cfg, i_s=i_s, x=inch(xx), y=inch(3))
        draw_component("coin_back", cfg, i_s=i_s, x=inch(xx), y=inch(7), angle=180)
    }
    # bishops
    draw_component("coin_face", cfg, i_r=3, x=inch(2), y=inch(2))
    draw_component("coin_face", cfg, i_r=3, x=inch(10-2), y=inch(10-2), angle=180)
    # rooks
    draw_component("coin_face", cfg, i_r=4, x=inch(8), y=inch(2))
    draw_component("coin_face", cfg, i_r=4, x=inch(10-8), y=inch(10-2), angle=180)
    # lances
    for (xx in c(1, 9)) {
        draw_component("coin_face", cfg, i_r=5, x=inch(xx), y=inch(1))
        draw_component("coin_face", cfg, i_r=5, x=inch(10-xx), y=inch(10-1), angle=180)
    }
    # knights
    for (xx in c(2, 8)) {
        draw_component("coin_face", cfg, i_r=2, x=inch(xx), y=inch(1))
        draw_component("coin_face", cfg, i_r=2, x=inch(10-xx), y=inch(10-1), angle=180)
    }
    # silvers
    for (xx in c(3, 7)) {
        draw_component("coin_face", cfg, i_r=6, x=inch(xx), y=inch(1))
        draw_component("coin_face", cfg, i_r=6, x=inch(10-xx), y=inch(10-1), angle=180)
    }
    # golds
    xxs <- c(4, 6)
    for (ii in 1:2) {
        xx <- xxs[ii]
        draw_component("ppdie_face", cfg, i_s=ii, i_r=6, x=inch(xx), y=inch(1))
        draw_component("ppdie_face", cfg, i_s=ii+2, i_r=6, x=inch(xx), y=inch(10-1), angle=180)
    }
    # kings
    draw_component("pawn_face", cfg, i_s=4, x=inch(5), y=inch(1))
    draw_component("pawn_face", cfg, i_s=3, x=inch(5), y=inch(10-1), angle=180)
}

## Shogi Board ("chess" piecepack)
draw_shogi_board_chess <- function() {
    # tiles
    pushViewport(viewport(width=inch(10), height=inch(10), name="board"))
    seekViewport("board")
    for (xx in seq(2, 8, by=2)) {
        for (yy in c(4, 6)) {
            draw_component("tile_back", cfg_chess6, x=inch(xx), y=inch(yy))
        }
        for (yy in c(2, 8)) {
            draw_component("tile_back", cfg_chess5, x=inch(xx), y=inch(yy))
        }
    }
    # pawns
    for (xx in 1:9) {
        i_s <- xx %% 6 + 1
        draw_component("coin_back", cfg_chess5, i_s=i_s, x=inch(xx), y=inch(3))
        draw_component("coin_back", cfg_chess5, i_s=i_s, x=inch(xx), y=inch(7), angle=180)
    }
    # bishops
    draw_component("coin_face", cfg_chess6, i_r=3, x=inch(2), y=inch(2))
    draw_component("coin_face", cfg_chess6, i_r=3, x=inch(10-2), y=inch(10-2), angle=180)
    # rooks
    draw_component("coin_face", cfg_chess6, i_r=4, x=inch(8), y=inch(2))
    draw_component("coin_face", cfg_chess6, i_r=4, x=inch(10-8), y=inch(10-2), angle=180)
    # lances
    for (xx in c(1, 9)) {
        draw_component("coin_face", cfg_chess6, i_r=5, x=inch(xx), y=inch(1))
        draw_component("coin_face", cfg_chess6, i_r=5, x=inch(10-xx), y=inch(10-1), angle=180)
    }
    # knights
    for (xx in c(2, 8)) {
        draw_component("coin_face", cfg_chess6, i_r=2, x=inch(xx), y=inch(1))
        draw_component("coin_face", cfg_chess6, i_r=2, x=inch(10-xx), y=inch(10-1), angle=180)
    }
    # silvers
    for (xx in c(3, 7)) {
        draw_component("coin_face", cfg_chess6, i_r=6, x=inch(xx), y=inch(1))
        draw_component("coin_face", cfg_chess6, i_r=6, x=inch(10-xx), y=inch(10-1), angle=180)
    }
    # golds
    xxs <- c(4, 6)
    for (ii in 1:2) {
        xx <- xxs[ii]
        draw_component("ppdie_face", cfg_chess5, i_s=ii, i_r=6, x=inch(xx), y=inch(1))
        draw_component("ppdie_face", cfg_chess5, i_s=ii+2, i_r=6, x=inch(10-xx), y=inch(10-1), angle=180)
    }
    # kings
    draw_component("pawn_face", cfg_chess5, i_s=5, x=inch(5), y=inch(1))
    draw_component("pawn_face", cfg_chess5, i_s=6, x=inch(5), y=inch(10-1), angle=180)
}

# invisible(try(dev.off()))
# dev.new(width=10, height=10)
cairo_pdf("diagrams/shogi_traditional.pdf", width=10, height=10)
draw_shogi_board_orthodox()
invisible(dev.off())

# invisible(try(dev.off()))
# dev.new(width=10, height=10)
cairo_pdf("diagrams/shogi_chess.pdf", width=10, height=10)
draw_shogi_board_chess()
invisible(dev.off())
