library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)

source("functions.R")

horse_colors = c("2" = "#a6cee3",
                 "12" = "#1f78b4",
                 "3" = "#b2df8a",
                 "11" = "#33a02c",
                 "4" = "#fb9a99",
                 "10" = "#e31a1c",
                 "5" = "#fdbf6f",
                 "9" = "#ff7f00",
                 "6" = "#cab2d6",
                 "8" = "#6a3d9a",
                 "7" = "#b15928")

ui <- page_fluid(
  title = "Horse Game",
  br(),
  layout_columns(
    card(
      card_header("Base Value"),
      radioGroupButtons(inputId = "base_value", label = "", choices = c("$0.05", "$0.10", "$0.25", "$1.00"), 
                        selected = "$0.25", justified = TRUE)
    ),
    card(
      card_header("Scratches"),
      layout_columns(
        pickerInput("x1", label = "x1", choices = 2:12, selected = 6,
                    options = list(container = "body", liveSearch = TRUE)),
        pickerInput("x2", label = "x2", choices = 2:12, selected = 7,
                    options = list(container = "body", liveSearch = TRUE)),
        pickerInput("x3", label = "x3", choices = 2:12, selected = 8,
                    options = list(container = "body", liveSearch = TRUE)),
        pickerInput("x4", label = "x4", choices = 2:12, selected = 9,
                    options = list(container = "body", liveSearch = TRUE))
      )
    ),
    col_widths = c(4, 8)
  ),
  card(
    card_header("Click Button for Each Roll"),
    layout_columns(actionBttn("2", "2"), 
                   actionBttn("3", "3"), 
                   actionBttn("4", "4"),
                   actionBttn("5", "5"),
                   actionBttn("6", "6"), 
                   actionBttn("7", "7"), 
                   actionBttn("8", "8"),
                   actionBttn("9", "9",),
                   actionBttn("10", "10"), 
                   actionBttn("11", "11"),
                   actionBttn("12", "12"))
  ),
  layout_columns(
    card(
      card_header("Win Probability"),
      plotlyOutput("probs_plot")
    ),
    card(
      card_header("Kitty"), 
      plotlyOutput("kitty_plot")
    ),
    col_widths = c(7, 5)
  )
)

server <- function(session, input, output) {
  rv <- reactiveValues(rolls = c())
  
  observeEvent(input[["2"]],{ rv$rolls = c(rv$rolls, 2) })
  observeEvent(input[["3"]],{ rv$rolls = c(rv$rolls, 3) })
  observeEvent(input[["4"]],{ rv$rolls = c(rv$rolls, 4) })
  observeEvent(input[["5"]],{ rv$rolls = c(rv$rolls, 5) })
  observeEvent(input[["6"]],{ rv$rolls = c(rv$rolls, 6) })
  observeEvent(input[["7"]],{ rv$rolls = c(rv$rolls, 7) })
  observeEvent(input[["8"]],{ rv$rolls = c(rv$rolls, 8) })
  observeEvent(input[["9"]],{ rv$rolls = c(rv$rolls, 9) })
  observeEvent(input[["10"]],{ rv$rolls = c(rv$rolls, 10) })
  observeEvent(input[["11"]],{ rv$rolls = c(rv$rolls, 11) })
  observeEvent(input[["12"]],{ rv$rolls = c(rv$rolls, 12) })
  
  base_value <- reactive({
    as.numeric(sub("\\$", "", "$0.25"))
  })
  
  scratches <- reactive({
    as.numeric(c(input$x1, input$x2, input$x3, input$x4))
  })
  
  rolls <- reactive({
    factor(rv$rolls, levels = 2:12)
  })
  
  steps <- reactive({
    0:length(rv$rolls)
  })
  
  probs <- reactive({
    lapply(steps(), function(n){
      rls = if (n == 0) NULL else rolls()[1:n]
      get_win_prob(scratches(), rls)
    }) |> 
      bind_rows(.id = "Roll") |> 
      mutate(Roll = as.numeric(Roll) - 1)
  })
  
  output$probs_plot <- renderPlotly({
    validate(need(length(unique(scratches())) == 4, "At least one scratch is duplicated"))
    p = ggplot(probs(), aes(x = Roll, y = WinProb, col = Horse)) +
      geom_point() + 
      geom_line() +
      scale_color_manual(values = horse_colors) +
      labs(y = "Win Probability") +
      theme_classic()
    ggplotly(p)
  })
  
  kitty <- reactive({
    get_kitty(base_value(), scratches(), rolls())
  })
  
  output$kitty_plot <- renderPlotly({
    validate(need(length(unique(scratches())) == 4, "At least one scratch is duplicated"))
    df = data.frame(Roll = steps(),
                    Kitty = kitty())
    p = ggplot(df, aes(x = Roll, y = Kitty)) +
      geom_point() +
      geom_line() +
      theme_classic()
    ggplotly(p)
  })
}

shinyApp(ui, server)