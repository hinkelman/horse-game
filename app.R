library(shiny)
library(bslib)
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
  card(
    card_header("Base Value"),
    sliderInput(inputId = "base_value", label = "", min = 0.05, max = 1, 
                step = 0.05, value = 0.25, pre = "$", width = "100%")
  ),
  card(
    card_header("Scratches"),
    layout_column_wrap(
      width = "40px",
      textInput("x1", label = "x1", value = "6"),
      textInput("x2", label = "x2", value = "7"),
      textInput("x3", label = "x3", value = "8"),
      textInput("x4", label = "x4", value = "9")
    )
  ),
  card(
    card_header("Click Button for Each Roll"),
    layout_column_wrap(
      width = "80px",
      actionButton("2", "2"), 
      actionButton("3", "3"), 
      actionButton("4", "4"),
      actionButton("5", "5"),
      actionButton("6", "6"), 
      actionButton("7", "7"), 
      actionButton("8", "8"),
      actionButton("9", "9"),
      actionButton("10", "10"), 
      actionButton("11", "11"),
      actionButton("12", "12")
    )
  ),
  card(
    card_header("Win Probability"),
    plotlyOutput("probs_plot")
  ),
  card(
    card_header("Kitty"), 
    plotlyOutput("kitty_plot")
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
      bind_rows(.id = "roll") |> 
      mutate(roll = as.numeric(roll) - 1)
  })
  
  output$probs_plot <- renderPlotly({
    validate(need(length(unique(scratches())) == 4, "At least one scratch is duplicated"))
    p = ggplot(probs(), aes(x = roll, y = win_prob, col = horse)) +
      geom_point() + 
      geom_line() +
      scale_color_manual(values = horse_colors) +
      labs(y = "Win Probability", color = "Horse") +
      theme_classic()
    ggplotly(p)
  })
  
  kitty <- reactive({
    get_kitty(input$base_value, scratches(), rolls())
  })
  
  output$kitty_plot <- renderPlotly({
    validate(need(length(unique(scratches())) == 4, "At least one scratch is duplicated"))
    df = data.frame(roll = steps(),
                    kitty = kitty())
    p = ggplot(df, aes(x = roll, y = kitty)) +
      geom_point() +
      geom_line() +
      labs(x = "Roll", y = "Kitty") +
      theme_classic()
    ggplotly(p)
  })
}

shinyApp(ui, server)