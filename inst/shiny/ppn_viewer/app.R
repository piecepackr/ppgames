library("ppgames")
library("shiny")
if (has_piecenikr) library("piecenikr")

txt <- ""

ui <- fluidPage(
    tags$head(tags$style(HTML("
        pre.diagram {
            font-family: FreeMono, mono;
            line-height: 100%;
            font-size: 24px;
        }"))),
    fluidRow(column(5, gameUI("game", txt), hr(), moveUI("move")),
             column(7, plotUI("plot")))
)

server <- function(input, output, session) {
    if (!has_animation)
        showNotification("Neither 'animation' or 'gifski' packages installed.  GIF animation disabled.",
                         type = "warning")
    if (!has_fansi)
        showNotification("'fansi' package not installed. 'cat_piece' 'color' option disabled.",
                         type = "warning")
    if (!has_piecenikr)
        showNotification("'piecenickr' package not installed.  'icehouse_pieces' support disabled.",
                         type = "warning")
    if (!has_rgl)
        showNotification("'rgl' package not installed. webGL support disabled.",
                         type = "warning")
    if (!has_tradgames)
        showNotification("'tradgames' package not installed.  Some checker games disabled.",
                         type = "warning")

    game <- gameServer("game")
    move <- moveServer("move", game)
    plotServer("plot", game, move)
}

shinyApp(ui, server)
