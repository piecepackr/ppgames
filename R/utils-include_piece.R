include_piece <- function(piece_side, suit, rank, cfg, angle = 0, height = "1.0em", file_ext = "pdf") {
    if (file_ext == "pdf") {
        file <- sprintf("%s_%s_%s_%s.pdf", piece_side, suit, rank, angle)
        if(!file.exists(file)) {
            grDevices::cairo_pdf(file,
                                 width = cfg$get_width(piece_side, suit, rank),
                                 height = cfg$get_height(piece_side, suit, rank),
                                 bg = "transparent")
            grid.piece(piece_side, suit, rank, cfg, angle = angle)
            grDevices::dev.off()
        }
    } else {
        file <- sprintf("%s_%s_%s_%s.png", piece_side, suit, rank, angle)
        if(!file.exists(file)) {
            grDevices::png(file,
                           width = cfg$get_width(piece_side, suit, rank),
                           height = cfg$get_height(piece_side, suit, rank),
                           units = "in", res = 60,
                           bg = "transparent", type = "cairo")
            grid.piece(piece_side, suit, rank, cfg, angle = angle)
            grDevices::dev.off()
        }
    }
    sprintf("\\includegraphics[height=%s]{%s}", height, file)
}

IncludePieces <- R6Class("include_pieces",
    public = list(
        initialize = function(cfg, file_ext = "pdf") {
            private$cfg <- cfg
            private$file_ext <- file_ext
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
        cS  = function(value) include_piece("coin_back", 1, 1, private$cfg, self$angle, self$height, private$file_ext),
        cM  = function(value) include_piece("coin_back", 2, 1, private$cfg, self$angle, self$height, private$file_ext),
        cC  = function(value) include_piece("coin_back", 3, 1, private$cfg, self$angle, self$height, private$file_ext),
        cA  = function(value) include_piece("coin_back", 4, 1, private$cfg, self$angle, self$height, private$file_ext),
        cn  = function(value) include_piece("coin_face", 1, 1, private$cfg, self$angle, self$height, private$file_ext),
        ca  = function(value) include_piece("coin_face", 1, 2, private$cfg, self$angle, self$height, private$file_ext),
        c2  = function(value) include_piece("coin_face", 1, 3, private$cfg, self$angle, self$height, private$file_ext),
        c3  = function(value) include_piece("coin_face", 1, 4, private$cfg, self$angle, self$height, private$file_ext),
        c4  = function(value) include_piece("coin_face", 1, 5, private$cfg, self$angle, self$height, private$file_ext),
        c5  = function(value) include_piece("coin_face", 1, 6, private$cfg, self$angle, self$height, private$file_ext),
        dSn = function(value) include_piece("die_face",  1, 1, private$cfg, self$angle, self$height, private$file_ext),
        dSa = function(value) include_piece("die_face",  1, 2, private$cfg, self$angle, self$height, private$file_ext),
        dS2 = function(value) include_piece("die_face",  1, 3, private$cfg, self$angle, self$height, private$file_ext),
        dS3 = function(value) include_piece("die_face",  1, 4, private$cfg, self$angle, self$height, private$file_ext),
        dS4 = function(value) include_piece("die_face",  1, 5, private$cfg, self$angle, self$height, private$file_ext),
        dS5 = function(value) include_piece("die_face",  1, 6, private$cfg, self$angle, self$height, private$file_ext),
        dMn = function(value) include_piece("die_face",  2, 1, private$cfg, self$angle, self$height, private$file_ext),
        dMa = function(value) include_piece("die_face",  2, 2, private$cfg, self$angle, self$height, private$file_ext),
        dM2 = function(value) include_piece("die_face",  2, 3, private$cfg, self$angle, self$height, private$file_ext),
        dM3 = function(value) include_piece("die_face",  2, 4, private$cfg, self$angle, self$height, private$file_ext),
        dM4 = function(value) include_piece("die_face",  2, 5, private$cfg, self$angle, self$height, private$file_ext),
        dM5 = function(value) include_piece("die_face",  2, 6, private$cfg, self$angle, self$height, private$file_ext),
        dCn = function(value) include_piece("die_face",  3, 1, private$cfg, self$angle, self$height, private$file_ext),
        dCa = function(value) include_piece("die_face",  3, 2, private$cfg, self$angle, self$height, private$file_ext),
        dC2 = function(value) include_piece("die_face",  3, 3, private$cfg, self$angle, self$height, private$file_ext),
        dC3 = function(value) include_piece("die_face",  3, 4, private$cfg, self$angle, self$height, private$file_ext),
        dC4 = function(value) include_piece("die_face",  3, 5, private$cfg, self$angle, self$height, private$file_ext),
        dC5 = function(value) include_piece("die_face",  3, 6, private$cfg, self$angle, self$height, private$file_ext),
        dAn = function(value) include_piece("die_face",  4, 1, private$cfg, self$angle, self$height, private$file_ext),
        dAa = function(value) include_piece("die_face",  4, 2, private$cfg, self$angle, self$height, private$file_ext),
        dA2 = function(value) include_piece("die_face",  4, 3, private$cfg, self$angle, self$height, private$file_ext),
        dA3 = function(value) include_piece("die_face",  4, 4, private$cfg, self$angle, self$height, private$file_ext),
        dA4 = function(value) include_piece("die_face",  4, 5, private$cfg, self$angle, self$height, private$file_ext),
        dA5 = function(value) include_piece("die_face",  4, 6, private$cfg, self$angle, self$height, private$file_ext),
        pS  = function(value) include_piece("pawn_face", 1, 1, private$cfg, self$angle, self$height, private$file_ext),
        pM  = function(value) include_piece("pawn_face", 2, 1, private$cfg, self$angle, self$height, private$file_ext),
        pC  = function(value) include_piece("pawn_face", 3, 1, private$cfg, self$angle, self$height, private$file_ext),
        pA  = function(value) include_piece("pawn_face", 4, 1, private$cfg, self$angle, self$height, private$file_ext),
        tMn = function(value) include_piece("tile_face", 2, 1, private$cfg, self$angle, self$height, private$file_ext),
        tMa = function(value) include_piece("tile_face", 2, 2, private$cfg, self$angle, self$height, private$file_ext),
        tM2 = function(value) include_piece("tile_face", 2, 3, private$cfg, self$angle, self$height, private$file_ext),
        tM3 = function(value) include_piece("tile_face", 2, 4, private$cfg, self$angle, self$height, private$file_ext),
        tM4 = function(value) include_piece("tile_face", 2, 5, private$cfg, self$angle, self$height, private$file_ext),
        tM5 = function(value) include_piece("tile_face", 2, 6, private$cfg, self$angle, self$height, private$file_ext),
        tCn = function(value) include_piece("tile_face", 3, 1, private$cfg, self$angle, self$height, private$file_ext),
        tCa = function(value) include_piece("tile_face", 3, 2, private$cfg, self$angle, self$height, private$file_ext),
        tC2 = function(value) include_piece("tile_face", 3, 3, private$cfg, self$angle, self$height, private$file_ext),
        tC3 = function(value) include_piece("tile_face", 3, 4, private$cfg, self$angle, self$height, private$file_ext),
        tC4 = function(value) include_piece("tile_face", 3, 5, private$cfg, self$angle, self$height, private$file_ext),
        tC5 = function(value) include_piece("tile_face", 3, 6, private$cfg, self$angle, self$height, private$file_ext),
        tAn = function(value) include_piece("tile_face", 4, 1, private$cfg, self$angle, self$height, private$file_ext),
        tAa = function(value) include_piece("tile_face", 4, 2, private$cfg, self$angle, self$height, private$file_ext),
        tA2 = function(value) include_piece("tile_face", 4, 3, private$cfg, self$angle, self$height, private$file_ext),
        tA3 = function(value) include_piece("tile_face", 4, 4, private$cfg, self$angle, self$height, private$file_ext),
        tA4 = function(value) include_piece("tile_face", 4, 5, private$cfg, self$angle, self$height, private$file_ext),
        tA5 = function(value) include_piece("tile_face", 4, 6, private$cfg, self$angle, self$height, private$file_ext),
        tb  = function(value) include_piece("tile_back", 1, 1, private$cfg, self$angle, self$height, private$file_ext),
        angle = function(x) {
            if (missing(x)) return(private$angle_)
            else private$angle_ <- x
        },
        height = function(x) {
            if (missing(x)) return(private$height_)
            else private$height_ <- x
        }
    ),
    private = list(angle_ = 0, cfg = NULL, height_ = "1.0em", file_ext = NULL),
)
