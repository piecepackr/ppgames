library("ppgames")
library("shiny")
if (requireNamespace("piecenikr", quietly = TRUE)) library("piecenikr")

txt <- ""

ui <- fluidPage(
    tags$head(tags$style(HTML("
        pre.shiny-text-output {
            font-family: FreeMono, mono;
            line-height: 100%;
            font-size: 24px;
        }"))),
    fluidRow(column(5, gameUI("game", txt), hr(), moveUI("move")),
             column(7, plotUI("plot")))
)

server <- function(input, output, session) {
    game <- gameServer("game")
    move <- moveServer("move", game)
    plotServer("plot", game, move)
}

shinyApp(ui, server)
