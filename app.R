library(shinydashboard)
library(bigrquery)
library(lubridate)
library(plotly)
library(dygraphs)
library(dplyr)
library(reshape2)
library(prophet)
library(gtrendsR)
library(CausalImpact)
library(quantmod)
library(gridExtra)

dashboard_header <- dashboardHeader(
  title = "Instrumentum Analytica",
  tags$li(a(href = "https://www.picturatechnica.com", target = "_blank", img(src = "logo.png", title = "Logo", height = "30px"), style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown"),
  titleWidth = 250
)

ui <- dashboardPage(
  skin = "blue",
  dashboard_header,
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Data Preview", tabName = "home", icon = icon("table")),
      menuItem("Forecast", tabName = "forecast", icon = icon("cloud")),
      menuItem("Causality", tabName = "causality", icon = icon("lightbulb")),
      menuItemOutput("selectable_data_sets"),
      hr(),
      menuItemOutput("menuitem"),
      selectizeInput(
        "symbols_2",
        label = p("Demo data (Yahoo Finance)"),
        choices = list(
          "Xero (XRO.AX)" = "XRO.AX",
          "Intuit (INTU)" = "INTU",
          "Alphabet (GOOG)" = "GOOG"
        ),
        options = list(create = TRUE, maxItems = 1)
      ),
      # selectInput(
      #   "symbols",
      #   label = p("Demo data (stock prices)"),
      #   choices = list(
      #     "Xero (XRO.AX)" = "XRO.AX",
      #     "Intuit (INTU)" = "INTU",
      #     "Alphabet (GOOG)" = "GOOG",
      #     "Apple (AAPL)" = "AAPL"
      #   ),
      #   selected = NULL,
      #   multiple = F
      # ),
      actionButton("fetch_symbols", label = "Load demo data"),
      hr()
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(
        "home",
        h3("Data Preview"),
        DT::DTOutput("data_set")
      ),
      tabItem(
        "causality",
        fluidRow(
          column(
            width = 2,
            box(
              title = "Help",
              solidHeader = T, collapsible = T,  width = NULL,
              helpText("To support your experimental time series (the one with intervention), also select one or more control time series."),
              hr(),
              helpText("An assumption of causal inference is that we assume that there is a set of control time series that were themselves not affected by the intervention."),
              hr(),
              tagList(p("Download a sample dataset: "), a("Beer Stock Prices", href = "https://gist.github.com/jeremyyeo/e4dcc3dd148428784e689546d151afbc", target = "_blank"))
            ),
            uiOutput("causality_inputs")
          ),
          column(
            width = 10,
              tabBox(
                width = NULL,
                height = NULL,
                tabPanel(
                  "Causality",
                  fluidRow(
                    box(width = 8, solidHeader = T, plotOutput("causality_plot")),
                    box(width = 4, solidHeader = T, textOutput("causality_report"))
                  )
                ),
                tabPanel(
                  "Preview selected time series",
                  box(
                    solidHeader = T,
                    dygraphOutput("causality_preview"),
                    width = NULL
                  )
                )
              )
          )
        )
      ),
      tabItem(
        "forecast",
        fluidRow(
          column(
            width = 2,
            box(
              title = "Help",
              solidHeader = T, collapsible = T,  width = NULL,
              helpText("Your data set needs to have at least a date column (in the format of YYYY-MM-DD) and a value to forecast."),
            ),
            uiOutput("forecast_inputs")
          ),
          column(
            width = 10,
            box(dygraphOutput("chart"), width = NULL),
            box(plotOutput("chart_components"), width = NULL)
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$menuitem <- renderMenu({
    fileInput(
      "file1", "Choose CSV File",
      multiple = F,
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
    )
  })

  reactive_data <- reactiveValues()
  values <- reactiveValues()
  
  observeEvent(input$file1, {
    in_file <- input$file1
    if (is.null(in_file)) {
      return(NULL)
    } else {
      user_uploaded_csv <- read.csv(in_file$datapath)
      reactive_data[[as.character(in_file$name)]] <- data.frame(user_uploaded_csv)
    }
  })

  observeEvent(input$fetch_symbols, {
    withProgress(message = "Downloading...", value = 0.5, {
      data_symbols <- getSymbols(input$symbols_2, auto.assign = F)
      incProgress(0.25)
      data_symbols <- data.frame(date = index(data_symbols), coredata(data_symbols))
      incProgress(0.25)
      reactive_data[[input$symbols_2]] <- data_symbols
    })
  })

  output$debugger <- renderPrint({
   post_period()
  })

  output$selectable_data_sets <- renderMenu({
    # print(names(reactive_data)) # character(0) if nothing.
    if (identical(names(reactive_data), character(0))) {
      NULL
    } else {
      selectInput(
        "selected_data_set",
        label = p("Main data set"),
        choices = names(reactive_data),
        multiple = F,
        selected = NULL
      )
    }
  })

  data_input <- eventReactive(input$selected_data_set, {
    if (is.null(input$selected_data_set)) {
      return(NULL)
    }
    reactive_data[[input$selected_data_set]]
  })

  data_input_dates <- reactive({
    x <- data_input()['date']
    x$date <- as.Date(x$date, format = "%Y-%m-%d")
    x$date
  })

  # output$debugger <- renderPrint({
  #  causality_intervention_date
  # })

  output$data_set <- DT::renderDT({
    req(input$selected_data_set)
    reactive_data[[input$selected_data_set]]
  })

  output$forecast_inputs <- renderUI({
    req(input$selected_data_set)
    # field_names <- colnames(reactive_data[[input$selected_data_set]])
    field_names <- colnames(data_input())
    tagList(
      actionButton("forecast", label = h4("Forecast"), width = "100%"),
      # selectInput(
      #   "time_scale",
      #   label = h4("Time scale of your data"),
      #   choices = list("Daily" = "day", "Monthly" = "month"),
      #   selected = "day",
      #   multiple = F
      # ),
      selectInput(
        "date_field",
        label = h4("Date column"),
        choices = field_names,
        multiple = F
      ),
      selectInput(
        "value_field",
        label = h4("Value column"),
        choices = field_names,
        multiple = F
      ),
      numericInput(
        "forecasting_periods",
        label = h4("How many periods ahead to forecast?"),
        value = 365
      )
    )
  })

  observe({
    selected_date_field <- input$date_field
    selectable_value_fields <- colnames(data_input())[!(colnames(data_input()) %in% selected_date_field)]
    if (!is.null(selected_date_field)) {
      updateSelectInput(
        session,
        "value_field",
        choices = selectable_value_fields
      )
    }
  })

  m <- eventReactive(input$forecast, {
    tidy_data <- data_input()
    names(tidy_data)[names(tidy_data) == input$date_field]  <- "ds"
    names(tidy_data)[names(tidy_data) == input$value_field] <- "y"
    withProgress(message = "Modelling time series...", value = 0.5, {
      x <- prophet(tidy_data, daily.seasonality = T)
      incProgress(0.5)
    })
    x
  })

  output$chart <- renderDygraph({
    req(m())
    withProgress(message = "Forecasting...", value = 0.5, {
      future   <- make_future_dataframe(m(), periods = input$forecasting_periods)
      incProgress(0.25)
      values$forecast <- predict(m(), future)
      incProgress(0.25)
    })
    dyplot.prophet(m(), values$forecast)
  })

  output$chart_components <- renderPlot({
    req(m())
    all_plots <- prophet_plot_components(m(), values$forecast)
    grid.arrange(grobs = all_plots, ncol = 2)
  })

  # Causality components ----

  output$causality_inputs <- renderUI({
    req(input$selected_data_set)
    field_names <- colnames(data_input())
    tagList(
      actionButton("analyse", label = h4("Analyse intervention"), width = "100%"),
      selectInput(
        "causality_date_field",
        label = h4("Date column"),
        choices = field_names,
        multiple = F
      ),
      selectInput(
        "causality_experiment_field",
        label = h4("Experimental time series"),
        choices = field_names,
        multiple = F
      ),
      selectInput(
        "causality_controls_field",
        label = h4("Control time series"),
        choices = field_names,
        multiple = T
      ),
      dateInput(
        "causality_intervention_date",
        label = h4("Start date of intervention"),
        value = max(data_input_dates()) - 30,
        min = min(data_input_dates()),
        max = max(data_input_dates()),
      )
    )
  })

  observe({
    selected_date_field <- input$causality_date_field
    selectable_value_fields <- colnames(data_input())[!(colnames(data_input()) %in% selected_date_field)]
    if (!is.null(selected_date_field)) {
      updateSelectInput(
        session,
        "causality_experiment_field",
        choices = selectable_value_fields
      )
    }
  })

  observe({
    selected_meta_fields <- c(input$causality_date_field, input$causality_experiment_field)
    selectable_value_fields <- colnames(data_input())[!(colnames(data_input()) %in% selected_meta_fields)]
    if (!is.null(selected_meta_fields)) {
      updateSelectInput(
        session,
        "causality_controls_field",
        choices = selectable_value_fields
      )
    }
  })
  
  pre_period <- reactive({
    c(min(data_input_dates()), input$causality_intervention_date)
  })
  
  post_period <- reactive({
    c(input$causality_intervention_date + days(1), max(data_input_dates()))
  })
  
  causal_impact <- eventReactive(input$analyse, {
    withProgress(message = "Sciencing...", value = 0.5, {
      selected_data <- data_input() %>%
        rename(date = input$causality_date_field) %>%
        dplyr::select(c(date, input$causality_experiment_field, input$causality_controls_field))
      all_dates  <- data.frame(date = seq.Date(pre_period()[1], post_period()[2], by = "day"))
      incProgress(0.1)
      rownames(selected_data) <- selected_data[['date']]
      selected_data <- selected_data %>% dplyr::select(-c(date)) %>% as.xts()
      selected_data <- zoo(selected_data, all_dates$date)
      incProgress(0.1)
      x <- CausalImpact(selected_data, pre_period(), post_period())
      incProgress(0.2)
    })
    x
  })
  
  output$causality_plot <- renderPlot({
    plot(causal_impact())
  })
  
  output$causality_report <- renderText({
    causal_impact()$report
  })
  
  output$causality_preview <- renderDygraph({
    selected_data <- data_input() %>% 
      rename(date = input$causality_date_field) %>%
      dplyr::select(c(date, input$causality_experiment_field, input$causality_controls_field))
    rownames(selected_data) <- selected_data[['date']]
    selected_data <- selected_data %>% dplyr::select(-c(date)) %>% as.xts()
    dygraph(selected_data) %>% 
      dyShading(from = input$causality_intervention_date, to = post_period()[2]) %>%
      dyRangeSelector()
  })

}

shinyApp(ui, server)
