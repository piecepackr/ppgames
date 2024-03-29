% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_promo_image.R
\name{save_promo_image}
\alias{save_promo_image}
\title{Save promo image of game}
\usage{
save_promo_image(game, gk = game_kit(), file = NULL, ...)
}
\arguments{
\item{game}{Game name.  Will be normalized by \code{\link[=normalize_name]{normalize_name()}}.}

\item{gk}{A \code{game_kit} R6 object.}

\item{file}{Filename for the image.  If \code{NULL} we'll generate
a pdf image with an appropriate name given \code{game}.}

\item{...}{Passed to \code{\link[piecepackr:render_piece]{piecepackr::render_piece()}}.}
}
\value{
A list with the width, height, and filename of the promo image.
As a side effect we save an image to disk.
}
\description{
\code{save_promo_image()} saves a promo image of a game.
This is usually just a starting diagram for the game but
sometimes we may create a custom image.
}
\details{
If we haven't created a custom image for this game then basically
we'll convert the game name to snake case, prepend a \code{"df_"}
in front and then \code{dynGet()} a function with that name and use
the resulting data frame with \code{\link[piecepackr:render_piece]{piecepackr::render_piece()}}.
See \link{df_game} for list of games directly supported by this package.
}
\examples{
file <- tempfile(fileext = ".pdf")
whf <- save_promo_image("Fuji-san", gk = game_kit(), file = file)
print(whf)
unlink(whf$file)
}
