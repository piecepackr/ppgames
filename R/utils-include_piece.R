include_piece <- function(piece_side, suit, rank, cfg, angle = 0, height = "1\\baselineskip") {
    dir <- file.path(tempdir(), "ppgames_cache")
    if(!dir.exists(dir)) dir.create(dir)

    file <- file.path(dir, sprintf("%s_%s_%s_%s.pdf", piece_side, suit, rank, angle))
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
        initialize = function(cfg) {
            private$cfg <- cfg
        },
        resize = function(height) {
            pieces <- self$clone()
            pieces$height <- height
            return(pieces)
        },
        rotate = function(angle) {
            pieces <- self$clone()
            pieces$angle <- angle
            return(pieces)
        }
    ),
    active = list(
        cS  = function(value) include_piece("coin_back", 1, 1, private$cfg, self$angle, self$height),
        cM  = function(value) include_piece("coin_back", 2, 1, private$cfg, self$angle, self$height),
        cC  = function(value) include_piece("coin_back", 3, 1, private$cfg, self$angle, self$height),
        cA  = function(value) include_piece("coin_back", 4, 1, private$cfg, self$angle, self$height),
        cn  = function(value) include_piece("coin_face", 1, 1, private$cfg, self$angle, self$height),
        ca  = function(value) include_piece("coin_face", 1, 2, private$cfg, self$angle, self$height),
        c2  = function(value) include_piece("coin_face", 1, 3, private$cfg, self$angle, self$height),
        c3  = function(value) include_piece("coin_face", 1, 4, private$cfg, self$angle, self$height),
        c4  = function(value) include_piece("coin_face", 1, 5, private$cfg, self$angle, self$height),
        c5  = function(value) include_piece("coin_face", 1, 6, private$cfg, self$angle, self$height),
        dSn = function(value) include_piece("die_face",  1, 1, private$cfg, self$angle, self$height),
        dSa = function(value) include_piece("die_face",  1, 2, private$cfg, self$angle, self$height),
        dS2 = function(value) include_piece("die_face",  1, 3, private$cfg, self$angle, self$height),
        dS3 = function(value) include_piece("die_face",  1, 4, private$cfg, self$angle, self$height),
        dS4 = function(value) include_piece("die_face",  1, 5, private$cfg, self$angle, self$height),
        dS5 = function(value) include_piece("die_face",  1, 6, private$cfg, self$angle, self$height),
        dMn = function(value) include_piece("die_face",  2, 1, private$cfg, self$angle, self$height),
        dMa = function(value) include_piece("die_face",  2, 2, private$cfg, self$angle, self$height),
        dM2 = function(value) include_piece("die_face",  2, 3, private$cfg, self$angle, self$height),
        dM3 = function(value) include_piece("die_face",  2, 4, private$cfg, self$angle, self$height),
        dM4 = function(value) include_piece("die_face",  2, 5, private$cfg, self$angle, self$height),
        dM5 = function(value) include_piece("die_face",  2, 6, private$cfg, self$angle, self$height),
        dCn = function(value) include_piece("die_face",  3, 1, private$cfg, self$angle, self$height),
        dCa = function(value) include_piece("die_face",  3, 2, private$cfg, self$angle, self$height),
        dC2 = function(value) include_piece("die_face",  3, 3, private$cfg, self$angle, self$height),
        dC3 = function(value) include_piece("die_face",  3, 4, private$cfg, self$angle, self$height),
        dC4 = function(value) include_piece("die_face",  3, 5, private$cfg, self$angle, self$height),
        dC5 = function(value) include_piece("die_face",  3, 6, private$cfg, self$angle, self$height),
        dAn = function(value) include_piece("die_face",  4, 1, private$cfg, self$angle, self$height),
        dAa = function(value) include_piece("die_face",  4, 2, private$cfg, self$angle, self$height),
        dA2 = function(value) include_piece("die_face",  4, 3, private$cfg, self$angle, self$height),
        dA3 = function(value) include_piece("die_face",  4, 4, private$cfg, self$angle, self$height),
        dA4 = function(value) include_piece("die_face",  4, 5, private$cfg, self$angle, self$height),
        dA5 = function(value) include_piece("die_face",  4, 6, private$cfg, self$angle, self$height),
        pS  = function(value) include_piece("pawn_face", 1, 1, private$cfg, self$angle, self$height),
        pM  = function(value) include_piece("pawn_face", 2, 1, private$cfg, self$angle, self$height),
        pC  = function(value) include_piece("pawn_face", 3, 1, private$cfg, self$angle, self$height),
        pA  = function(value) include_piece("pawn_face", 4, 1, private$cfg, self$angle, self$height),
        tMn = function(value) include_piece("tile_face", 2, 1, private$cfg, self$angle, self$height),
        tMa = function(value) include_piece("tile_face", 2, 2, private$cfg, self$angle, self$height),
        tM2 = function(value) include_piece("tile_face", 2, 3, private$cfg, self$angle, self$height),
        tM3 = function(value) include_piece("tile_face", 2, 4, private$cfg, self$angle, self$height),
        tM4 = function(value) include_piece("tile_face", 2, 5, private$cfg, self$angle, self$height),
        tM5 = function(value) include_piece("tile_face", 2, 6, private$cfg, self$angle, self$height),
        tCn = function(value) include_piece("tile_face", 3, 1, private$cfg, self$angle, self$height),
        tCa = function(value) include_piece("tile_face", 3, 2, private$cfg, self$angle, self$height),
        tC2 = function(value) include_piece("tile_face", 3, 3, private$cfg, self$angle, self$height),
        tC3 = function(value) include_piece("tile_face", 3, 4, private$cfg, self$angle, self$height),
        tC4 = function(value) include_piece("tile_face", 3, 5, private$cfg, self$angle, self$height),
        tC5 = function(value) include_piece("tile_face", 3, 6, private$cfg, self$angle, self$height),
        tAn = function(value) include_piece("tile_face", 4, 1, private$cfg, self$angle, self$height),
        tAa = function(value) include_piece("tile_face", 4, 2, private$cfg, self$angle, self$height),
        tA2 = function(value) include_piece("tile_face", 4, 3, private$cfg, self$angle, self$height),
        tA3 = function(value) include_piece("tile_face", 4, 4, private$cfg, self$angle, self$height),
        tA4 = function(value) include_piece("tile_face", 4, 5, private$cfg, self$angle, self$height),
        tA5 = function(value) include_piece("tile_face", 4, 6, private$cfg, self$angle, self$height),
        tb  = function(value) include_piece("tile_back", 1, 1, private$cfg, self$angle, self$height),
        angle = function(x) {
            if (missing(x)) return(private$angle_)
            else private$angle_ <- x
        },
        height = function(x) {
            if (missing(x)) return(private$height_)
            else private$height_ <- x
        }
    ),
    private = list(angle_ = 0, cfg = NULL, height_ = "1\\baselineskip"),
)
