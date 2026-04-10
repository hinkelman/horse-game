library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyjs)

source("functions.R")

ui <- page_fluid(
  title = "Horse Game",

  useShinyjs(),

  tags$head(tags$style(HTML(
    "
    /* ---- General compactness ---- */
    .card-body   { padding: 0.5rem !important; }
    .card-header { padding: 0.35rem 0.6rem !important; font-size: 0.85rem; font-weight: 600; }

    /* ---- Setup bar (base value + scratches combined) ---- */
    .setup-bar {
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 0.5rem 1rem;
    }
    .setup-item {
      display: flex;
      align-items: center;
      gap: 0.35rem;
      white-space: nowrap;
    }
    .setup-label {
      font-size: 0.78rem;
      font-weight: 600;
      color: #555;
      text-transform: uppercase;
      letter-spacing: 0.03em;
    }
    .setup-bar .form-group { margin-bottom: 0 !important; }
    /* Strip wrapper padding so inputs sit on same baseline */
    .setup-bar .form-group,
    .setup-bar .shiny-input-container { margin: 0 !important; padding: 0 !important; }
    .setup-bar input[type='number'] {
      height: 28px !important;
      font-size: 0.85rem;
      padding: 2px 6px;
      width: 62px !important;
      box-sizing: border-box;
    }
    .setup-bar input[type='text'],
    .scratch-box {
      height: 28px !important;
      font-size: 0.85rem;
      padding: 2px 4px !important;
      width: 38px !important;
      box-sizing: border-box;
      text-align: center;
      flex-shrink: 0;
    }
    .setup-divider {
      width: 1px; height: 24px;
      background: #ddd; flex-shrink: 0;
    }
    @media (max-width: 480px) { .setup-divider { display: none; } }

    /* ---- Roll buttons ---- */
    .roll-btn {
      min-width: 40px;
      font-size: 0.88rem;
      padding: 4px 5px;
      margin: 2px;
    }

    /* ---- Setup and Roll cards stack vertically ---- */
    .setup-card, .rolls-card { margin-bottom: 0.5rem; }

    /* ---- Roll history badge strip ---- */
    .roll-history-wrap {
      display: flex;
      flex-wrap: nowrap;
      overflow-x: auto;
      gap: 5px;
      padding: 4px 2px;
      align-items: center;
      min-height: 34px;
      scrollbar-width: thin;
    }
    .roll-badge {
      display: inline-block;
      padding: 2px 10px;
      border-radius: 12px;
      font-size: 0.82rem;
      font-weight: 700;
      color: #fff;
      white-space: nowrap;
      flex-shrink: 0;
    }
    .roll-badge.faded { opacity: 0.4; }
    .roll-empty { color: #aaa; font-size: 0.8rem; font-style: italic; }

    /* ---- Value boxes: 2x2 on mobile, 4-in-a-row on md+ ---- */
    @media (max-width: 767px) {
      .vbox-row { display: grid; grid-template-columns: 1fr 1fr; gap: 0.5rem; }
    }
    @media (min-width: 768px) {
      .vbox-row { display: grid; grid-template-columns: repeat(4, 1fr); gap: 0.5rem; }
    }
  "
  ))),

  br(),

  div(
    class = "setup-card",
    card(
      card_header("Setup"),
      div(
        class = "setup-bar",
        div(
          class = "setup-item",
          tags$span(class = "setup-label", "Base $"),
          numericInput(
            "base_value",
            label = NULL,
            min = 0.25,
            max = 5,
            step = 0.25,
            value = 0.25,
            width = "75px"
          )
        ),
        div(class = "setup-divider"),
        div(
          class = "setup-item",
          tags$span(class = "setup-label", "Scratches:"),
          tags$input(
            id = "x1",
            type = "text",
            class = "form-control scratch-box",
            value = "6"
          ),
          tags$input(
            id = "x2",
            type = "text",
            class = "form-control scratch-box",
            value = "7"
          ),
          tags$input(
            id = "x3",
            type = "text",
            class = "form-control scratch-box",
            value = "8"
          ),
          tags$input(
            id = "x4",
            type = "text",
            class = "form-control scratch-box",
            value = "9"
          )
        )
      )
    )
  ),
  uiOutput("validation_msg"),
  div(
    class = "rolls-card",
    card(
      card_header("Roll"),
      div(
        style = "display:flex; flex-wrap:wrap;",
        actionButton("2", "2", class = "roll-btn btn-outline-secondary"),
        actionButton("3", "3", class = "roll-btn btn-outline-secondary"),
        actionButton("4", "4", class = "roll-btn btn-outline-secondary"),
        actionButton("5", "5", class = "roll-btn btn-outline-secondary"),
        actionButton("6", "6", class = "roll-btn btn-outline-secondary"),
        actionButton("7", "7", class = "roll-btn btn-outline-secondary"),
        actionButton("8", "8", class = "roll-btn btn-outline-secondary"),
        actionButton("9", "9", class = "roll-btn btn-outline-secondary"),
        actionButton("10", "10", class = "roll-btn btn-outline-secondary"),
        actionButton("11", "11", class = "roll-btn btn-outline-secondary"),
        actionButton("12", "12", class = "roll-btn btn-outline-secondary")
      )
    )
  ),

  # Roll history badge strip
  card(
    # card_header("Roll History"),
    card_header(
      "Roll History",
      div(
        style = "display: flex; justify-content: space-between; align-items: right;",
        actionButton(
          "undo",
          "↶ Undo",
          class = "btn-sm btn-outline-secondary",
          style = "padding: 1px 8px; font-size: 0.75rem;"
        )
      )
    ),
    div(class = "roll-history-wrap", uiOutput("roll_history"))
  ),

  # Value boxes
  div(
    class = "vbox-row",
    uiOutput("place1_box"),
    uiOutput("place2_box"),
    uiOutput("place3_box"),
    value_box(
      title = "Current Kitty",
      value = textOutput("kitty_value"),
      showcase = bsicons::bs_icon("piggy-bank"),
      theme = value_box_theme(bg = "#fff", fg = "#000")
    )
  )
)

server <- function(session, input, output) {
  rv <- reactiveValues(rolls = c())

  observeEvent(input[["2"]], {
    rv$rolls = c(rv$rolls, 2)
  })
  observeEvent(input[["3"]], {
    rv$rolls = c(rv$rolls, 3)
  })
  observeEvent(input[["4"]], {
    rv$rolls = c(rv$rolls, 4)
  })
  observeEvent(input[["5"]], {
    rv$rolls = c(rv$rolls, 5)
  })
  observeEvent(input[["6"]], {
    rv$rolls = c(rv$rolls, 6)
  })
  observeEvent(input[["7"]], {
    rv$rolls = c(rv$rolls, 7)
  })
  observeEvent(input[["8"]], {
    rv$rolls = c(rv$rolls, 8)
  })
  observeEvent(input[["9"]], {
    rv$rolls = c(rv$rolls, 9)
  })
  observeEvent(input[["10"]], {
    rv$rolls = c(rv$rolls, 10)
  })
  observeEvent(input[["11"]], {
    rv$rolls = c(rv$rolls, 11)
  })
  observeEvent(input[["12"]], {
    rv$rolls = c(rv$rolls, 12)
  })

  rolls <- reactive({
    factor(rv$rolls, levels = 2:12)
  })

  scratches <- reactive({
    as.numeric(c(input$x1, input$x2, input$x3, input$x4))
  })

  check_scratches <- function() {
    s <- scratches()
    if (length(unique(s)) != 4) {
      return("Scratches must be unique")
    }
    if (!all(s %in% 2:12)) {
      return("Scratches must be integers from 2 to 12")
    }
    NULL
  }

  output$validation_msg <- renderUI({
    err <- check_scratches()
    if (is.null(err)) {
      return(NULL)
    }
    div(class = "alert alert-warning", style = "margin-bottom: 0.5rem;", err)
  })

  game_over <- reactive({
    req(
      length(unique(scratches())) == 4,
      all(scratches() %in% 2:12),
      req(rolls())
    )
    sr <- get_steps_remain(scratches(), rolls())
    sr[as.character(scratches())] <- NA
    any(sr < 1, na.rm = TRUE)
  })

  observe({
    if (game_over()) {
      lapply(2:12, function(h) shinyjs::disable(as.character(h)))
    } else {
      lapply(2:12, function(h) shinyjs::enable(as.character(h)))
    }
  })

  observeEvent(input$undo, {
    if (length(rv$rolls) > 0) {
      rv$rolls <- rv$rolls[-length(rv$rolls)]
    }
  })

  observe({
    toggleState("undo", condition = length(rv$rolls) > 0)
  })

  # ---- Roll history badge strip ----
  # Shows last 20 rolls newest->oldest (left→right); most recent is fully opaque
  output$roll_history <- renderUI({
    rls <- rv$rolls
    if (length(rls) == 0) {
      return(tags$span(class = "roll-empty", "No rolls yet"))
    }
    recent <- rev(tail(rls, 20))
    lapply(seq_along(recent), function(i) {
      horse <- as.character(recent[i])
      color <- horse_colors[horse]
      faded <- if (i > 1) "faded" else ""
      tags$span(
        class = paste("roll-badge", faded),
        style = paste0("background-color:", color, ";"),
        horse
      )
    })
  })

  # ---- Win probability rankings ----
  current_probs <- reactive({
    req(length(unique(scratches())) == 4, all(scratches() %in% 2:12))
    sim_win_prob(scratches(), rolls()) |>
      sort(decreasing = TRUE)
  })

  fmt_horse <- function(horse) {
    if (is.null(horse)) "\u2014" else paste("Horse", horse)
  }
  fmt_prob <- function(prob) {
    if (is.null(prob)) "\u2014" else paste0(round(prob * 100, 1), "%")
  }

  make_place_box <- function(title, n) {
    renderUI({
      place <- current_probs()[n] # reactive read happens here, inside renderUI
      horse <- names(place)
      prob <- unname(place)
      color_bg <- horse_colors[horse]
      color_fg <- if (as.numeric(horse) %in% 2:6) "#000" else "#fff"
      value_box(
        title = title,
        value = fmt_horse(horse),
        showcase = tags$span(style = "font-size:1.1rem;", fmt_prob(prob)),
        theme = value_box_theme(
          bg = color_bg,
          fg = color_fg
        )
      )
    })
  }

  output$place1_box <- make_place_box("1st Place", 1)
  output$place2_box <- make_place_box("2nd Place", 2)
  output$place3_box <- make_place_box("3rd Place", 3)

  # ---- Kitty ----
  output$kitty_value <- renderText({
    req(length(unique(scratches())) == 4, all(scratches() %in% 2:12))
    kitty_val <- get_kitty(input$base_value, scratches(), rolls())
    paste0("$", formatC(kitty_val, format = "f", digits = 2))
  })
}

shinyApp(ui, server)
