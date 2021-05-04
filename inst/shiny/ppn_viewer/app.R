library("ppgames")
library("shiny")

txt <- ""

ui <- fluidPage(
    gameUI("game", txt),
    hr(),
    moveUI("move"),
    hr(),
    plotUI("plot")
)

server <- function(input, output, session) {
    game <- gameServer("game")
    move <- moveServer("move", game)
    plotServer("plot", game, move)
}

shinyApp(ui, server)
