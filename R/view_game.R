#' View/edit game
#'
#' Basic command-line program to view/edit (PPN) games
#'
#' @inheritParams cat_move
#' @param editor usually a character string naming (or giving the path to) the text editor you want to use.
#' @param ... Passed to \code{plot_move()}
#' @param fps Frames per second.  Passed to \code{cat_game()}.
#' @export
view_game <- function(game, ...,
                      editor = getOption("editor"),
                      reorient = "none", annotate = FALSE, fps = 1) {
    piecepackr:::assert_suggested("argparse")
    move <- tail(names(game$moves), 1)
    parser <- get_parser()
    print_screen(game, move, reorient, annotate)
    a <- get_input(game, parser)
    while (a$command != "q") {
        if (a$command == "g") {
            move <- a$move
            if (is.null(move)) move <- tail(names(game$moves), 1)
        } else if (a$command == "n") {
            move <- next_move(game, move)
        } else if (a$command == "p") {
            move <- prev_move(game, move)
        } else if (a$command == "r") {
            plot_move(game, ..., annotate = annotate)
        } else if (a$command == "c") {
            cat_game(game, reorient = reorient, annotate = annotate)
        } else if (a$command == "e") {
            game <- edit_game(game, editor)
            move <- tail(names(game$moves), 1)
        } else if (a$command == "a") {
            game <- append_to_ppn(game, a$ppn)
            move <- tail(names(game$moves), 1)
        }
        print_screen(game, move, reorient, annotate)
        a <- get_input(game, parser)
    }
    invisible(game)
}

prev_move <- function(game, move) {
    i <- match(move, names(game$moves))
    if (i > 0) move <- names(game$moves)[i - 1]
    move
}

next_move <- function(game, move) {
    i <- match(move, names(game$moves))
    if (i < length(game$moves)) move <- names(game$moves)[i + 1]
    move
}

print_screen <- function(game, move, reorient = "none", annotate = FALSE, clear = TRUE) {
    ppn <- as_ppn(game)
    diagram <- cat_move(game, move, reorient = reorient, annotate = annotate, file = NULL)
    pmove_info <- paste("Prev move:", move, game$moves[[move]])
    nmove <- next_move(game, move)
    if (move != nmove)
        nmove_info <- paste("Next move:", nmove, game$moves[[nmove]])
    else
        nmove_info <- ""
    if (clear) clear_screen()
    cat(str_wrap(ppn), pmove_info, nmove_info, diagram, sep = "\n")
}

get_input <- function(game, parser) {
    while (TRUE) {
        prompt <- "Enter command ('h' for help, 'q' to quit):"
        ans <- str_trim(readline(prompt))
        args <- str_split(ans, " ")[[1]]
        a <- try(parser$parse_args(args))
        if (inherits(a, "try-error")) {
            NULL
        } else if (a$command == "h") {
            parser$print_help()
        } else if (a$command == "l") {
            cat(str_wrap(paste(names(game$moves), collapse = ", ")), "\n")
        } else {
            break
        }
    }
    a
}

as_ppn <- function(game) {
    if (is.null(game$metadata)) {
        meta <- c("---", "...\n")
    } else {
        meta <- yaml::as.yaml(game$metadata)
        meta <- c("---", utils::head(str_split(meta, "\n")[[1]], -1L), "...\n")
    }
    c(meta, game$movetext)
}

edit_game <- function(game, editor = getOption("editor")) {
    file <- tempfile(fileext = ".ppn")
    on.exit(unlink(file))
    write_ppn(list(game), file = file)
    utils::file.edit(file, editor = editor)
    read_ppn(file)[[1]]
}

append_to_ppn <- function(game, to_append) {
    ppn <- as_ppn(game)
    n <- length(ppn)
    ppn[n] <- paste(ppn[n], paste(to_append, collapse = " "))
    read_ppn(textConnection(ppn))[[1]]
}

get_parser <- function() {
    parser <- argparse::ArgumentParser(prog = "View game", add_help=FALSE)
    subparsers <- parser$add_subparsers(help = "subcommands", metavar="", dest="command")
    a <- subparsers$add_parser("a", help = "append to ppn", add_help=FALSE)
    a$add_argument("ppn", nargs = "+", help = "additional ppn")
    c <- subparsers$add_parser("c", help = "animate game via cat_game()", add_help=FALSE)
    e <- subparsers$add_parser("e", help = "edit ppn", add_help=FALSE)
    g <- subparsers$add_parser("g", help = "go to move", add_help=FALSE)
    g$add_argument("move", nargs = "?", default = NULL, help = "which move to view")
    h <- subparsers$add_parser("h", help = "help", add_help=FALSE)
    l <- subparsers$add_parser("l", help = "list moves", add_help=FALSE)
    n <- subparsers$add_parser("n", help = "next move", add_help=FALSE)
    p <- subparsers$add_parser("p", help = "previous move", add_help=FALSE)
    q <- subparsers$add_parser("q", help = "quit", add_help=FALSE)
    r <- subparsers$add_parser("r", help = "render current move via plot_move()", add_help=FALSE)
    parser
}
