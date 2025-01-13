.onLoad <- function(libname, pkgname) {
    rlang::inform(c(
        "{ppgames} is now superseded in favor of the newer {ppcli}, {ppdf}, {ppgamer}, {ppn}, and {pprules} spinoff packages", 
        i = "These can all be installed with `piecepackr::install_ppverse()`",
        i = 'See <https://piecepackr.r-universe.dev/builds> for more info on the various "ppverse" packages',
        i = "Use `ppcli::cat_piece()` for `cat_piece()`",
        i = "Use similar functions from `{ppdf}` for the various `df_*()` game setup functions",
        i = "Use `ppgamer::solve_fujisan()` for `solve_fujisan()`",
        i = "Use `{ppn}` for various `*_game()`, `*_ppn()`, and `*_move()` PPN parsing and game visualization functions",
        i = "Use `{pprules}` for various `save_*()` piecepack ruleset generators"
    ), class = "packageStartupMessage")
}
