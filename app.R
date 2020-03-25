library(shinydashboard)
library(lubridate)
library(dygraphs)
library(dplyr)
library(reshape2)
library(prophet)
library(CausalImpact)
library(quantmod)
library(gridExtra)

dashboard_header <- dashboardHeader(
  title = "Machina",
  tags$li(a(href = "https://www.picturatechnica.com", target = "_blank", img(src = "logo.png", title = "Logo", height = "30px"), style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown"),
  titleWidth = 250
)

ui <- function(request) {
  dashboardPage(
    skin = "blue",
    dashboard_header,
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "sidebar",
        menuItem("Data Preview", tabName = "home", icon = icon("table")),
        menuItem("Forecast", tabName = "forecast", icon = icon("cloud")),
        menuItem("Causality", tabName = "causality", icon = icon("lightbulb")),
        menuItemOutput("selectable_data_sets"),
        menuItemOutput("menuitem"),
        selectizeInput(
          "symbols_2",
          label = p("Yahoo Finance Symbols"),
          choices = list(
            "Xero (XRO.AX)" = "XRO.AX",
            "Alphabet (GOOG)" = "GOOG",
            "Facebook (FB)" = "FB",
            "Tesla (TSLA)" = "TSLA"
          ),
          options = list(create = TRUE, maxItems = 1)
        ),
        actionButton("fetch_symbols", label = "Load Data"),
        hr(),
        bookmarkButton(label = "Bookmark Results"),
        menuItem("Source Code", icon = icon("file-code-o"), href = "https://github.com/jeremyyeo/machina")
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        includeScript("www/ga.js")
      ),
      tabItems(
        tabItem(
          "home",
          fluidRow(
            column(
              width = 12,
              box(
                width = NULL,
                solidHeader = T,
                status = "primary",
                title = "Data Preview",
                span(textOutput("on_load_help"), style = "color:#BF616A"),
                DT::DTOutput("data_set")
              )
            )
          )
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
                tagList(p("Library documentation:"), tags$a(href = "https://google.github.io/CausalImpact/", "CausalImpact (Google)")),
                hr(),
                tagList(
                  tags$p("Load the", code("BUD, RI.PA, HEINY"), "stock price sample data, select", code("2020-01-01"), "as the 'Start date of intervention' and", code("RI.PA"), "and", code("HEINY"), "as the controls."),
                  a("Data documentation", href = "https://gist.github.com/jeremyyeo/e4dcc3dd148428784e689546d151afbc", target = "_blank")
                )
              ),
              actionButton("load", label = h4("Load Sample Data"), width = "100%"),
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
                      box(width = 12, solidHeader = T, dygraphOutput("causality_plot"))
                    ),
                    fluidRow(
                      box(width = 9, solidHeader = T, textOutput("causality_report")),
                      box(width = 3, solidHeader = T, valueBoxOutput("causality_significance", width = NULL))
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
                hr(),
                tagList(p("Library documentation:"), tags$a(href = "https://facebook.github.io/prophet/", "Prophet (Facebook)")),
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
}
server <- function(input, output, session) {
  output$on_load_help <- renderText({
    if (identical(names(reactive_data), character(0))) {
      "Upload a CSV or load some data..."
    } else {
      NULL
    }
  })
  
  
  output$menuitem <- renderMenu({
    fileInput(
      "file1", "Choose CSV File",
      multiple = F,
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
    )
  })

  reactive_data <- reactiveValues()
  values <- reactiveValues()
  
  observeEvent(input$load, {
    withProgress(message = "Downloading...", value = 0.5, {
      source("data/load-beer-stocks.R", local = TRUE)
      incProgress(0.5)
    })
    reactive_data[["BUD, RI.PA, HEINY"]] <- all_xts
    removeUI("#load")
  })
  
  observeEvent(input$file1, {
    in_file <- input$file1
    if (is.null(in_file)) {
      return(NULL)
    } else {
      user_uploaded_csv <- read.csv(in_file$datapath)
      reactive_data[[as.character(in_file$name)]] <- data.frame(user_uploaded_csv)
    }
  })
  
  observeEvent(input$symbols_2, {
    updateActionButton(
      session,
      "fetch_symbols",
      label = paste0("Load ", input$symbols_2, " data")
    )
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
        label = p("Selected Data"),
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
        value = 30,
        max = 365
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
  
  future <- eventReactive(input$forecast, {
    req(m())
    make_future_dataframe(m(), periods = input$forecasting_periods)
  })
  
  output$chart <- renderDygraph({
    req(future())
    withProgress(message = "Forecasting...", value = 0.5, {
      future   <- future()
      incProgress(0.25)
      values$forecast <- predict(m(), future)
      incProgress(0.25)
    })
    dyplot.prophet(m(), values$forecast)
  })

  output$chart_components <- renderPlot({
    req(future())
    all_plots <- prophet_plot_components(m(), values$forecast)
    grid.arrange(grobs = all_plots, ncol = 2)
  })

  # Causality components ----
  
  output$causality_inputs <- renderUI({
    req(input$selected_data_set)
    field_names <- colnames(data_input())
    tagList(
      actionButton("analyse", label = h4("Analyse Intervention"), width = "100%"),
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
  
  output$causality_plot <- renderDygraph({
    req(causal_impact())
    dygraph(causal_impact()$series[, c("response", "point.pred", "point.pred.lower", "point.pred.upper")]) %>%
      dySeries("response", label = "Actual", color = "#5E81AC") %>%
      dySeries(c("point.pred.lower", "point.pred", "point.pred.upper"), label = "Prediction", color = "#BF616A") %>%
      dyShading(from = input$causality_intervention_date, to = post_period()[2]) %>%
      dyRangeSelector()
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
  
  output$causality_significance <- renderValueBox({
    req(causal_impact())
    p_val <- max(causal_impact()$summary$p)
    icon_name  <- ifelse(p_val <= 0.05, "thumbs-up", "thumbs-down")
    color_name <- ifelse(p_val <= 0.05, "green", "red")
    valueBox(
      paste0(round((1 - p_val) * 100, 2), "%"),
      "Probability of a causal effect",
      icon = icon(icon_name, lib = "glyphicon"),
      color = color_name
    )
  })

}

shinyApp(ui, server, enableBookmarking = "server")
