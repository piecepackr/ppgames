include_piece <- function(piece_side, suit, rank, cfg, angle = 0, height = "1\\baselineskip") {
    dir <- file.path(tempdir(), "ppgames_cache")
    if(!dir.exists(dir)) dir.create(dir)

    file <- file.path(dir, sprintf("%s_%s_%s.pdf", piece_side, suit, rank))
    if(!file.exists(file)) {
        grDevices::cairo_pdf(file,
                             width = cfg$get_width(piece_side, suit, rank),
                             height = cfg$get_height(piece_side, suit, rank),
                             bg = "transparent")
        grid.piece(piece_side, suit, rank, cfg, angle = angle)
        grDevices::dev.off()
    }
    sprintf("\\includegraphics[height=%s]{%s}", height, file)
}

IncludePieces <- R6Class("include_pieces",
    public = list(
        initialize = function(cfg) private$cfg <- cfg
    ),
    active = list(
        cS = function(value) include_piece("coin_back", 1, 1, private$cfg),
        cM = function(value) include_piece("coin_back", 2, 1, private$cfg),
        cC = function(value) include_piece("coin_back", 3, 1, private$cfg),
        cA = function(value) include_piece("coin_back", 4, 1, private$cfg),
        cn = function(value) include_piece("coin_face", 1, 1, private$cfg),
        ca = function(value) include_piece("coin_face", 1, 2, private$cfg),
        c2 = function(value) include_piece("coin_face", 1, 3, private$cfg),
        c3 = function(value) include_piece("coin_face", 1, 4, private$cfg),
        c4 = function(value) include_piece("coin_face", 1, 5, private$cfg),
        c5 = function(value) include_piece("coin_face", 1, 6, private$cfg),
        dSn = function(value) include_piece("die_face", 1, 1, private$cfg),
        dSa = function(value) include_piece("die_face", 1, 2, private$cfg),
        dS2 = function(value) include_piece("die_face", 1, 3, private$cfg),
        dS3 = function(value) include_piece("die_face", 1, 4, private$cfg),
        dS4 = function(value) include_piece("die_face", 1, 5, private$cfg),
        dS5 = function(value) include_piece("die_face", 1, 6, private$cfg),
        dMn = function(value) include_piece("die_face", 2, 1, private$cfg),
        dMa = function(value) include_piece("die_face", 2, 2, private$cfg),
        dM2 = function(value) include_piece("die_face", 2, 3, private$cfg),
        dM3 = function(value) include_piece("die_face", 2, 4, private$cfg),
        dM4 = function(value) include_piece("die_face", 2, 5, private$cfg),
        dM5 = function(value) include_piece("die_face", 2, 6, private$cfg),
        dCn = function(value) include_piece("die_face", 3, 1, private$cfg),
        dCa = function(value) include_piece("die_face", 3, 2, private$cfg),
        dC2 = function(value) include_piece("die_face", 3, 3, private$cfg),
        dC3 = function(value) include_piece("die_face", 3, 4, private$cfg),
        dC4 = function(value) include_piece("die_face", 3, 5, private$cfg),
        dC5 = function(value) include_piece("die_face", 3, 6, private$cfg),
        dAn = function(value) include_piece("die_face", 4, 1, private$cfg),
        dAa = function(value) include_piece("die_face", 4, 2, private$cfg),
        dA2 = function(value) include_piece("die_face", 4, 3, private$cfg),
        dA3 = function(value) include_piece("die_face", 4, 4, private$cfg),
        dA4 = function(value) include_piece("die_face", 4, 5, private$cfg),
        dA5 = function(value) include_piece("die_face", 4, 6, private$cfg),
        pS = function(value) include_piece("pawn_face", 1, 1, private$cfg),
        pM = function(value) include_piece("pawn_face", 2, 1, private$cfg),
        pC = function(value) include_piece("pawn_face", 3, 1, private$cfg),
        pA = function(value) include_piece("pawn_face", 4, 1, private$cfg),
        tMn = function(value) include_piece("tile_face", 2, 1, private$cfg),
        tMa = function(value) include_piece("tile_face", 2, 2, private$cfg),
        tM2 = function(value) include_piece("tile_face", 2, 3, private$cfg),
        tM3 = function(value) include_piece("tile_face", 2, 4, private$cfg),
        tM4 = function(value) include_piece("tile_face", 2, 5, private$cfg),
        tM5 = function(value) include_piece("tile_face", 2, 6, private$cfg),
        tCn = function(value) include_piece("tile_face", 3, 1, private$cfg),
        tCa = function(value) include_piece("tile_face", 3, 2, private$cfg),
        tC2 = function(value) include_piece("tile_face", 3, 3, private$cfg),
        tC3 = function(value) include_piece("tile_face", 3, 4, private$cfg),
        tC4 = function(value) include_piece("tile_face", 3, 5, private$cfg),
        tC5 = function(value) include_piece("tile_face", 3, 6, private$cfg),
        tAn = function(value) include_piece("tile_face", 4, 1, private$cfg),
        tAa = function(value) include_piece("tile_face", 4, 2, private$cfg),
        tA2 = function(value) include_piece("tile_face", 4, 3, private$cfg),
        tA3 = function(value) include_piece("tile_face", 4, 4, private$cfg),
        tA4 = function(value) include_piece("tile_face", 4, 5, private$cfg),
        tA5 = function(value) include_piece("tile_face", 4, 6, private$cfg),
        tb = function(value) include_piece("tile_back", 1, 1, private$cfg)
    ),
    private = list(cfg = NULL)
)

