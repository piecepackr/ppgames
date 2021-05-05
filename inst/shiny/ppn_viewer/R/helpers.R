# parse PPN game
ppn_link <- a(href="https://trevorldavis.com/piecepackr/portable-piecepack-notation.html", "PPN")
gameUI <- function(id, ppn_text = "") {
    ns <- NS(id)
    tagList("Enter the ", ppn_link, "for a game:",
        textAreaInput(ns("ppn_text"), "", ppn_text,
                      width = "100%", rows=24),
        actionButton(ns("parse_ppn"), "Parse PPN"),
        verbatimTextOutput(ns("parse_error"))
    )
}
gameServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        game <- eventReactive(input$parse_ppn, {
            ppn <- textConnection(input$ppn_text)
            try(ppgames::read_ppn(ppn)[[1]])
        })

        output$parse_error <- renderText(if (inherits(game(), "try-error")) game() else NULL)

        game
    })
}

# select move within game
moveUI <- function(id) {
    ns <- NS(id)
    tagList(selectInput(ns("move"), "Select move", character(0)),
            fluidRow(column(2, actionButton(ns("prev_move"), "Prev.")),
                     column(2, actionButton(ns("next_move"), "Next"))),
            br(),
            uiOutput(ns("move_label")))
}
moveServer <- function(id, game) {
    moduleServer(id, function(input, output, session) {
        observeEvent(game(), {
            req(game())
            g <- game()
            n <- length(g$dfs)
            l <- names(g$moves)
            updateSelectInput(session, "move", choices = l, selected = l[n])
        })

        observeEvent(input$prev_move, {
            req(game(), input$move)
            l <- names(game()$moves)
            i <- match(input$move, l)
            if (i > 1) updateSelectInput(session, "move", selected = l[i - 1])
        })

        observeEvent(input$next_move, {
            req(game(), input$move)
            l <- names(game()$moves)
            i <- match(input$move, l)
            if (i < length(l)) updateSelectInput(session, "move", selected = l[i + 1])
        })

        output$move_label <- renderUI({
            req(game())
            g <- game()
            n <- input$move
            if (is.na(n)) n <- tail(names(g$moves), 1)
            move_text <- g$moves[[n]]
            tagList(strong(n), move_text)
        })
        reactive(input$move)
    })
}

# plot move with a specified renderer
envir <- piecepackr::game_systems("dejavu")
envir3d <- piecepackr::game_systems("dejavu3d")
renderers <- c("grid.piece", "cat_piece")
if (requireNamespace("rgl")) {
    renderers <- append(renderers, "piece3d")
}
# Support Looney Pyramids if available
#### Have trans use a larger pt_thickness argument?
if (requireNamespace("piecenikr")) {
    pyramids <- piecenikr::looney_pyramids()$icehouse_pieces
    envir$icehouse_pieces <- pyramids
    envir3d$icehouse_pieces <- pyramids
}

plotUI <- function(id) {
    ns <- NS(id)
    tagList(selectInput(ns("renderer"), "Select renderer", renderers),
            uiOutput(ns("plot")))
}
plotServer <- function(id, game, move) {
    moduleServer(id, function(input, output, session) {

        observeEvent(input$renderer, {
            if (input$renderer == "cat_piece") {
                output$plot <- plaintextUI(session$ns("cat_piece"))
            } else if (input$renderer == "grid.piece") {
                output$plot <- gridUI(session$ns("grid.piece"))
            } else if (input$renderer == "piece3d") {
                output$plot <- rglUI(session$ns("piece3d"))
            }
        })

        plaintextServer("cat_piece", game, move)
        gridServer("grid.piece", game, move)
        rglServer("piece3d", game, move)
    })
}

annotateInput <- function(id) {
    options <- c("none", "algebraic", "cartesian")
    selectInput(id, "annotate", options)
}
reorientInput <- function(id) {
    options <- c("none", "symbols", "all")
    selectInput(id, "reorient", options)
}

# cat_piece
plaintextUI <- function(id) {
    ns <- NS(id)
    renderUI(tagList(fluidRow(column(3, annotateInput(ns("annotate"))),
                              column(3, reorientInput(ns("reorient")))),
                     verbatimTextOutput(ns("plaintext"))))
}
plaintextServer <- function(id, game, move) {
    moduleServer(id, function(input, output, session) {
        output$plaintext <- renderText({
            req(game(), move(), input$annotate, input$reorient)
            f <- textConnection("diagram", "w")
            on.exit(close(f))
            cat_move(game(), move(), file = f,
                     annotate = input$annotate,
                     reorient = input$reorient)
            paste(diagram, collapse="\n")
        })
    })
}

# grid.piece
gridUI <- function(id) {
    ns <- NS(id)
    renderUI(tagList(fluidRow(column(3, numericInput(ns("op_angle"), "op_angle", 90, step = 15)),
                              column(3, numericInput(ns("op_scale"), "op_scale", 0, min = 0, step = 0.1)),
                              column(3, annotateInput(ns("annotate")))),
                     hr(),
                     fluidRow(column(3, actionButton(ns("animate"), "Create GIF!")),
                              column(3, numericInput(ns("n_transitions"), "n_transitions", 0, min = 0, step = 1)),
                              column(2, numericInput(ns("n_pauses"), "n_pauses", 1, min = 1, step = 1)),
                              column(3, numericInput(ns("fps"), "fps", 1.5, min = 0.5, step = 0.5))),

                     imageOutput(ns("grid"))))
}
gridServer <- function(id, game, move) {
    moduleServer(id, function(input, output, session) {

        v <- reactiveValues(should_animate = FALSE)
        observeEvent(move(), v$should_animate <- FALSE)
        observeEvent(game(), v$should_animate <- FALSE)

        observeEvent(input$op_scale, v$should_animate <- FALSE)
        observeEvent(input$op_angle, v$should_animate <- FALSE)
        observeEvent(input$annotate, v$should_animate <- FALSE)

        observeEvent(input$n_transitions, {
                         v$should_animate <- FALSE
                         fps <- 1.5 * (input$n_transitions + input$n_pauses)
                         updateNumericInput(session, "fps", value = fps)
                     })
        observeEvent(input$n_pauses, {
                         v$should_animate <- FALSE
                         fps <- 1.5 * (input$n_transitions + input$n_pauses)
                         updateNumericInput(session, "fps", value = fps)
                     })
        observeEvent(input$fps, v$should_animate <- FALSE)

        observeEvent(input$animate, v$should_animate <- TRUE)

        output$grid <- renderImage({
            if (v$should_animate) {
                req(game(), input$op_scale, input$op_angle, input$annotate,
                    input$n_transitions, input$n_pauses, input$fps)
                id <- showNotification("I'm drawing animation frames as fast as I can.  Please be patient.",
                                       type = "message", duration = NULL, closeButton = FALSE)
                on.exit(removeNotification(id), add = TRUE)
                f <- tempfile(fileext = ".gif")
                if (input$op_scale > 0) {
                    trans <- shiny_trans
                } else {
                    trans <- function(x, ...) x
                }
                animate_game(game(), file = f, envir = envir,
                             op_scale = input$op_scale,
                             op_angle = input$op_angle,
                             trans = shiny_trans,
                             annotate = input$annotate,
                             n_transitions = input$n_transitions,
                             n_pauses = input$n_pauses,
                             fps = input$fps)
                list(src = f)
            } else {
                req(game(), move(), input$op_scale, input$op_angle, input$annotate)
                id <- showNotification("I'm now painting the game diagram.  Please be patient.",
                                       type = "message", duration = NULL, closeButton = FALSE)
                on.exit(removeNotification(id), add = TRUE)
                f <- tempfile(fileext = ".png")
                if (input$op_scale > 0) {
                    trans <- shiny_trans
                } else {
                    trans <- function(x, ...) x
                }
                dim_image <- plot_move(game(), f, move(), annotate = input$annotate,
                          envir = envir, trans = trans,
                          op_scale = input$op_scale,
                          op_angle = input$op_angle)
                h <- dim_image$height
                w <- dim_image$width
                max_pixels <- min(600, max(h, w))
                if (h < w) {
                    height <- round((h / w) * max_pixels, 0)
                    width <- max_pixels
                } else {
                    height <- max_pixels
                    width <- round((w / h) * max_pixels, 0)
                }
                list(src = f, width = width, height = height)
            }}, deleteFile = TRUE)
    })
}

shiny_trans <- function(df, ...) piecepackr::op_transform(df, ..., pt_thickness = 0.30)

# piece3d
rglUI <- function(id) {
    ns <- NS(id)
    renderUI(tagList(rgl::rglwidgetOutput(ns("rgl"), width = "600px", height = "600px")))
}
rglServer <- function(id, game, move) {
    moduleServer(id, function(input, output, session) {
        output$rgl <- rgl::renderRglwidget({
            req(game(), move())
            id <- showNotification("Crafting rotatable, zoomable 3D model.  Please be patient.",
                                   type = "message", duration = NULL, closeButton = FALSE)
            on.exit(removeNotification(id), add = TRUE)
            try(rgl::close3d())
            rgl::open3d(useNULL = TRUE)
            rgl::view3d(0, 0)
            trans <- shiny_trans
            df <- game()$dfs[[move()]]
            piecepackr::pmap_piece(df, piecepackr::piece3d,
                                   envir=envir3d, trans=trans, scale = 0.98)
            rgl::rglwidget()
        })
    })
}
