## 1. Business Logic Section ----------------
# State Migration and Economic Indicators App

### Enter Business Logic after this line
###

## Library Packages
library(shiny)
library(tidyverse)
library(DT)
library(broom)
library(ggplot2)

# Optional packages if needed:
# library(maps)
library(sf)
library(tigris)
library(scales)
# library(plotly)
# library(scales)
# library(bslib)

## Read in Master State-Year Dataset
# This app assumes one merged dataset where:
# - each row = one state-year
# - each column = one migration or economic variable
#

app_data <- read_csv("../data/master_state_year_data.csv")

## Clean Variable Types
# Make sure:
# - state is character or factor
# - year is numeric/integer
# - analysis variables are numeric

## Create Derived Variables
# Create any shared variables the full app depends on.
# Example:
# - net_migration
# - net_migration_rate

## Create UI Choice Vectors
# These define reusable dropdown/checkbox options for the app.
#
# - state_choices and year_choices come directly from the dataset
#   and are used to filter rows (which states / which year).
#
# - For variables, we use named vectors where:
#     * Left side = label shown to the user
#     * Right side = actual column name in the dataset
#
#   Example:
#     "Average Wages" = "avg_wages"
#     → User sees "Average Wages"
#     → Server receives "avg_wages"
#
# These are passed into the `choices = ...` argument in UI inputs.
# Example:
#   selectInput("econ_x_var", "X variable:", choices = economic_choices)
#
# Key distinction:
#   - state/year choices → filter rows
#   - other choices → select variables (columns)

state_choices <- sort(unique(app_data$state))
year_choices  <- sort(unique(app_data$year))

migration_choices <- c(
  "Inbound Migration" = "inbound_migration",
  "Outbound Migration" = "outbound_migration",
  "Net Migration" = "net_migration",
  "Net Migration Rate (per 1,000)" = "net_migration_rate"
)

economic_choices <- c(
  "Average Wages" = "avg_wages",
  "Average Rent" = "avg_rent",
  "Housing Cost" = "housing_cost",
  "Tax Rate" = "tax_rate",
  "Unemployment Rate" = "unemployment_rate"
)

model_outcome_choices <- c(
  "Net Migration Rate (per 1,000)" = "net_migration_rate",
  "Net Migration" = "net_migration",
  "Inbound Migration" = "inbound_migration",
  "Outbound Migration" = "outbound_migration"
)

model_predictor_choices <- c(
  "Average Wages" = "avg_wages",
  "Average Rent" = "avg_rent",
  "Unemployment Rate" = "unemployment_rate",
  "Housing Cost" = "housing_cost",
  "Tax Rate" = "tax_rate"
)

pretty_variable_names <- c(
  inbound_migration = "Inbound Migration",
  outbound_migration = "Outbound Migration",
  net_migration = "Net Migration",
  net_migration_rate = "Net Migration Rate (per 1,000)",
  avg_wages = "Average Wages",
  avg_rent = "Average Rent",
  housing_cost = "Housing Cost",
  tax_rate = "Tax Rate",
  unemployment_rate = "Unemployment Rate (%)"
)
# Optional combined choices for tables or flexible displays
# all_variable_choices <- c(migration_choices, economic_choices)

## Create Helper Functions
# Add only helper functions that the whole app will reuse.
# Examples might include:
# - label formatting
# - summary table formatting
# - model output formatting

###
### Enter Business Logic before this line
###
###
## Begin User Interface Section ----------------

ui <- fluidPage(
  
  ## Optional Theme
  # theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  ## App Title
  titlePanel("State Migration and Economic Indicators Explorer"),
  
  ## Main Tabs
  tabsetPanel(
    
    ## -------------------------------------------------------------
    ## Migration Overview Tab
    ## -------------------------------------------------------------
    tabPanel(
      "Migration Overview",
      sidebarLayout(
        sidebarPanel(
          
          ## Inputs
          selectInput(
            inputId = "migration_states",
            label = "Select state(s):",
            choices = state_choices,
            multiple = TRUE, 
            selected = state_choices[1:2]
          ),
          
          selectInput(
            inputId = "migration_year",
            label = "Select year:",
            choices = year_choices
          ),
          
          selectInput(
            inputId = "migration_measure",
            label = "Migration measure:",
            choices = migration_choices,
            selected = "net_migration_rate"
          )
          
        ),
        
        mainPanel(
          
          ## Outputs
          plotOutput("migration_map", height = "650px"),
          tableOutput("migration_summary_table")
          
        )
      )
    ),
    
    ## -------------------------------------------------------------
    ## Economic Indicators Tab
    ## -------------------------------------------------------------
    tabPanel(
      "Economic Indicators",
      sidebarLayout(
        sidebarPanel(
          
          ## Inputs
          selectInput(
            inputId = "econ_states",
            label = "Select state(s):",
            choices = state_choices,
            multiple = TRUE
          ),
          
          sliderInput(
            inputId = "econ_year_range",
            label = "Select year:",
            min=2015,
            max=2025,
            value=c(2015, 2016),
            sep=""
          ),
          
          selectInput(
            inputId = "econ_overtime_var",
            label = "Over-Time Variable",
            choices = c(economic_choices, migration_choices)
          ),
          
          selectInput(
            inputId = "econ_x_var",
            label = "X variable:",
            choices = economic_choices
          ),
          
          selectInput(
            inputId = "econ_y_var",
            label = "Y variable:",
            choices = migration_choices
          ),
          
          selectInput(
            inputId = "econ_summarize_var",
            label = 'Variable to Summarize',
            choices= c(economic_choices, migration_choices)
          ),
          
          checkboxInput(
            inputId = "econ_color_var",
            label = "Color points by state?",
            value = TRUE
          ),
          
          checkboxInput(
            inputId = "econ_add_smoother",
            label = "Add linear smoother",
            value = FALSE
          )
          
        ),
        
        mainPanel(
          
          ## Outputs
          plotOutput("overtime_plot"),
          plotOutput("economic_scatterplot"),
          plotOutput("tax_barplot"),
          fluidRow(column(12, align='center', htmlOutput("econ_summary_table_title"))),
          fluidRow(column(12, align='center',tableOutput("summary_stats_table")))
          
        )
      )
    ),
    
    ## -------------------------------------------------------------
    ## Modeling Tab
    ## -------------------------------------------------------------
    tabPanel(
      "Modeling",
      sidebarLayout(
        sidebarPanel(
          
          ## Inputs
          selectInput(
            inputId = "model_year",
            label = "Select model year:",
            choices = year_choices
          ),
          
          selectInput(
            inputId = "model_outcome",
            label = "Outcome variable:",
            choices = model_outcome_choices,
            selected = "net_migration_rate"
          ),
          
          checkboxGroupInput(
            inputId = "model_predictors",
            label = "Select predictor(s):",
            choices = model_predictor_choices,
            selected = c("avg_wages", "unemployment_rate")
          ),
          
          checkboxInput(
            inputId = "model_log_transform",
            label = "Log-transform eligible continuous variables",
            value = FALSE
          ),
          
          actionButton(
            inputId = "run_model",
            label = "Run Model"
          )
          
        ),
        
        mainPanel(
          
          ## Outputs
          verbatimTextOutput("model_formula_text"),
          tableOutput("regression_summary_table"),
          plotOutput("coefficient_plot"),
          plotOutput("predictor_vs_outcome_plot")
          
        )
      )
    ),
    
    ## -------------------------------------------------------------
    ## Data Table Tab
    ## -------------------------------------------------------------
    tabPanel(
      "Data Table",
      sidebarLayout(
        sidebarPanel(
          
          ## Inputs
          selectInput(
            inputId = "table_states",
            label = "Select state(s):",
            choices = state_choices,
            multiple = TRUE
          ),
          
          selectInput(
            inputId = "table_year",
            label = "Select year:",
            choices = year_choices
          ),
          
          checkboxGroupInput(
            inputId = "table_variables",
            label = "Variables to display:",
            choices = c(migration_choices, economic_choices),
            selected = c("net_migration", "net_migration_rate", "avg_wages", "avg_rent")
          )
          
        ),
        
        mainPanel(
          
          ## Outputs
          DTOutput("data_table")
          
        )
      )
    )
    
  )
)

### End User Interface Section ----------------
##
### Begin Server Section ----------------

server <- function(input, output, session) {
  ###
  ### Enter Server Code After this line
  ###
  
  ## -------------------------------------------------------------
  ## Shared reactive datasets
  # Each reactive filters app_data differently depending on the tab:
  # - Migration → selected states + one year
  # - Economic → selected states (+ possibly multiple years)
  # - Modeling → all states + one year
  # - Data table → selected states + one year
  #
  # These are reused in outputs to avoid repeating filter logic.
  ## -------------------------------------------------------------
  
  # req() prevents code from running if inputs are missing 
  
  migration_filtered <- reactive({
    req(input$migration_states, input$migration_year)
    
    app_data |>
      filter(
        state %in% input$migration_states,
        year == input$migration_year
      )
  })
  
  migration_map_data <- reactive({
    req(input$migration_year)
    
    app_data |>
      filter(year == input$migration_year)
  })
  
  econ_filtered <- reactive({
    # Filter app_data for the Economic Indicators tab
    # Add time-trend logic here if needed
    app_data |> 
      filter(state %in% input$econ_states & (year >= input$econ_year_range[1] & year <= input$econ_year_range[2])) 
  })
  
model_filtered <- reactive({
  req(input$model_year)
  
  app_data |>
    filter(year == input$model_year)
})
  
  table_filtered <- reactive({
    req(input$table_states, input$table_year)
    
    app_data |>
      filter(
        state %in% input$table_states,
        year == input$table_year
      )
  })
  
  ## -------------------------------------------------------------
  ## Migration Overview Outputs
  ## -------------------------------------------------------------
  
  # validate(need()) displays user-friendly messages in outputs
  
  output$migration_map <- renderPlot({
    
    validate(
      need(input$migration_measure %in% colnames(migration_map_data()),
           "Selected migration variable is not available.")
    )
    
    states_sf <- tigris::states(cb = TRUE, year = 2022) |>
      filter(!STUSPS %in% c("PR", "VI", "MP", "GU", "AS")) |>
      mutate(
        state = case_when(
          NAME == "District of Columbia" ~ "District Of Columbia",
          TRUE ~ NAME
        )
      )
    
    map_data <- states_sf |>
      left_join(migration_map_data(), by = "state") |>
      tigris::shift_geometry()
    
    fill_scale <- if (input$migration_measure == "net_migration") {
      scale_fill_gradient2(
        low = "firebrick3",
        mid = "white",
        high = "steelblue4",
        midpoint = 0,
        name = pretty_variable_names[input$migration_measure],
        labels = scales::comma,
        na.value = "grey90"
      )
    } else if (input$migration_measure == "net_migration_rate") {
      scale_fill_gradient2(
        low = "firebrick3",
        mid = "white",
        high = "steelblue4",
        midpoint = 0,
        name = pretty_variable_names[input$migration_measure],
        labels = scales::label_number(accuracy = 0.1),
        na.value = "grey90"
      )
    } else {
      scale_fill_viridis_c(
        name = pretty_variable_names[input$migration_measure],
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        na.value = "grey90"
      )
    }
    
    ggplot(map_data) +
      geom_sf(aes(fill = .data[[input$migration_measure]]), color = "white", linewidth = 0.2) +
      geom_sf(
        data = map_data |> filter(state %in% input$migration_states),
        fill = NA,
        color = "black",
        linewidth = 0.6
      ) +
      fill_scale +
      labs(
        title = paste(pretty_variable_names[input$migration_measure], "by State —", input$migration_year),
        subtitle = "Selected states outlined in black"
      ) +
      coord_sf(datum = NA) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      )
  })
  
  output$migration_summary_table <- renderTable({
    
    validate(
      need(length(input$migration_states) > 0, "Please select at least one state."),
      need(input$migration_measure %in% colnames(migration_filtered()),
           "Selected migration variable is not available.")
    )
    
    migration_vars <- c(
      "inbound_migration",
      "outbound_migration",
      "net_migration",
      "net_migration_rate"
    )
    
    table_vars <- c(
      input$migration_measure,
      setdiff(migration_vars, input$migration_measure)
    )
    
    migration_filtered() |>
      select(state, year, all_of(table_vars)) |>
      arrange(desc(.data[[input$migration_measure]])) |>
      rename_with(
        ~ pretty_variable_names[.x],
        -c(state, year)
      )
  })
  
  ## -------------------------------------------------------------
  ## Economic Indicators Outputs
  ## -------------------------------------------------------------
  
  output$overtime_plot <- renderPlot({
    # Build time trend plot here
    
    overtime_plot <- econ_filtered() |> 
      drop_na(input$econ_overtime_var) |> 
      ggplot(mapping=aes(x=year, y=.data[[input$econ_overtime_var]], color=state)) +
      geom_line() +
      scale_x_continuous(breaks=seq(input$econ_year_range[1], input$econ_year_range[2], by=1)) +
      labs(title=paste0(pretty_variable_names[input$econ_overtime_var], ' Trend Over Time (',input$econ_year_range[1],'-',input$econ_year_range[2],')' ),
           x='Year',
           y=pretty_variable_names[input$econ_overtime_var],
           color='State') 
    overtime_plot + 
      theme_bw() +
      theme(legend.position = 'top',
                          plot.title=element_text(hjust=0.5)) 

  })
  
  output$economic_scatterplot <- renderPlot({
    # Build economic scatterplot here
    #add validation saying need two continuous
    if (input$econ_color_var == TRUE) {
    plot <- econ_filtered() |> 
      drop_na(all_of(c(input$econ_x_var, input$econ_y_var))) |> 
      ggplot(mapping=aes(x=.data[[input$econ_x_var]], y=.data[[input$econ_y_var]])) +
      geom_point(aes(color=state)) +
      labs(title=paste0(pretty_variable_names[input$econ_y_var],' Versus ',pretty_variable_names[input$econ_x_var]),
           x=pretty_variable_names[input$econ_x_var],
           y=pretty_variable_names[input$econ_y_var],
           color='State')} else {
             
             plot <- econ_filtered() |> 
               drop_na(all_of(c(input$econ_x_var, input$econ_y_var))) |> 
               ggplot(mapping=aes(x=.data[[input$econ_x_var]], y=.data[[input$econ_y_var]])) +
               geom_point() +
               labs(title=paste0(pretty_variable_names[input$econ_y_var],' Versus ',pretty_variable_names[input$econ_x_var]),
                    x=pretty_variable_names[input$econ_x_var],
                    y=pretty_variable_names[input$econ_y_var])}
             
           
    
    if (input$econ_add_smoother==TRUE) {
      plot <- plot + geom_smooth(method='lm', se=FALSE)
    }
     plot +
       theme_bw() +
       theme(legend.position = 'top',
                  plot.title=element_text(hjust=0.5))  
  })
  
  output$tax_barplot <- renderPlot({
    # Build tax barplot here
    econ_filtered() |> 
      drop_na(tax_rate) |> 
      ggplot(mapping=aes(x=as.factor(year), y=tax_rate, fill=state)) +
      geom_col(position =position_dodge2(padding=0)) +
      labs(title=paste0('Tax Rate Compared Across States (',input$econ_year_range[1],'-',input$econ_year_range[2],')'),
           x='Year',
           y=pretty_variable_names['tax_rate'],
           fill='State') +
      theme_bw() +
      theme(legend.position='top',
            plot.title = element_text(hjust=0.5))
  })
  
  output$econ_summary_table_title <- renderUI({
    paste0('Summary Statistics for ', pretty_variable_names[input$econ_summarize_var])})
  
  output$summary_stats_table <- renderTable({
    # Build summary statistics table here
    econ_filtered() |> 
      drop_na(all_of(c(input$econ_summarize_var))) |> 
      group_by(state) |> 
      summarise(min=min(.data[[input$econ_summarize_var]], na.rm=TRUE),
                median=median(.data[[input$econ_summarize_var]], na.rm=TRUE),
                max=max(.data[[input$econ_summarize_var]], na.rm=TRUE),
                mean=mean(.data[[input$econ_summarize_var]], na.rm=TRUE),
                sd=sd(.data[[input$econ_summarize_var]], na.rm=TRUE)) |> 
      rename('Minimum Value' = 'min',
             'Median' = 'median',
             'Maximum Value' = 'max',
             'Mean' = 'mean',
             'Standard Deviation' = 'sd',
             'State' = 'state')
  })
  
  ## -------------------------------------------------------------
  ## Modeling Outputs
  ## -------------------------------------------------------------
  
  model_results <- eventReactive(input$run_model, {
    validate(
      need(length(input$model_predictors) > 0,
           "Please select at least one predictor variable."),
      need(!is.null(input$model_outcome),
           "Please select an outcome variable.")
    )
    
    data <- model_filtered()
    outcome <- input$model_outcome
    predictors <- input$model_predictors
    
    # Keep only the variables needed for the model
    model_data <- data |>
      select(state, year, all_of(c(outcome, predictors)))
    
    # Optionally log-transform eligible continuous predictors
    log_eligible <- c("avg_wages", "avg_rent", "housing_cost")
    
    if (isTRUE(input$model_log_transform)) {
      for (v in intersect(predictors, log_eligible)) {
        new_col <- paste0("log_", v)
        model_data[[new_col]] <- log(model_data[[v]])
        predictors[predictors == v] <- new_col
      }
    }
    
    # Keep only complete cases for outcome + predictors
    complete_model_data <- model_data |>
      drop_na(all_of(c(outcome, predictors)))
    
    # User-facing checks for missing / insufficient data
    if (nrow(complete_model_data) == 0) {
      return(list(
        message = "No complete data available. Try a different year or fewer predictors.",
        data = NULL,
        formula_text = NULL,
        model = NULL,
        tidy_summary = NULL,
        glance_summary = NULL,
        outcome = outcome,
        predictors = predictors
      ))
    }
    
    if (nrow(complete_model_data) < 5) {
      return(list(
        message = "There are too few complete observations to fit the model for the selected year and variables.",
        data = complete_model_data,
        formula_text = NULL,
        model = NULL,
        tidy_summary = NULL,
        glance_summary = NULL,
        outcome = outcome,
        predictors = predictors
      ))
    }
    
    formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    model <- lm(formula_obj, data = complete_model_data)
    
    list(
      message = NULL,
      formula_text = formula_str,
      model = model,
      tidy_summary = broom::tidy(model, conf.int = TRUE),
      glance_summary = broom::glance(model),
      data = complete_model_data,
      outcome = outcome,
      predictors = predictors
    )
  })
  
  
  output$model_formula_text <- renderPrint({
    req(model_results())
    res <- model_results()
    
    if (!is.null(res$message)) {
      cat(res$message, "\n")
      invisible(NULL)
    } else {
      g <- res$glance_summary
      cat("Model formula:\n  ", res$formula_text, "\n\n")
      cat(sprintf(
        "R²: %.3f  |  Adj. R²: %.3f  |  F-statistic: %.2f  |  p-value: %.4f  |  n = %d\n",
        g$r.squared, g$adj.r.squared, g$statistic, g$p.value, g$nobs
      ))
    }
  })
  
  
  output$regression_summary_table <- renderTable({
    req(model_results())
    res <- model_results()
    
    req(is.null(res$message))
    
    res$tidy_summary |>
      mutate(
        term = ifelse(
          term == "(Intercept)",
          "(Intercept)",
          pretty_variable_names[term] %||% term
        ),
        sig = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          p.value < 0.1   ~ ".",
          TRUE            ~ ""
        )
      ) |>
      select(
        Term = term,
        Estimate = estimate,
        `Std. Error` = std.error,
        `t value` = statistic,
        `p value` = p.value,
        `95% CI Low` = conf.low,
        `95% CI High` = conf.high,
        ` ` = sig
      )
  }, digits = 4)
  
  
  output$coefficient_plot <- renderPlot({
    req(model_results())
    res <- model_results()
    
    req(is.null(res$message))
    
    coef_data <- res$tidy_summary |>
      filter(term != "(Intercept)") |>
      mutate(
        term = pretty_variable_names[term] %||% term,
        term = fct_reorder(term, estimate),
        sig = p.value < 0.05
      )
    
    ggplot(coef_data, aes(x = estimate, y = term, color = sig)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        height = 0.25, linewidth = 0.8
      ) +
      geom_point(size = 4) +
      scale_color_manual(
        values = c("TRUE" = "steelblue4", "FALSE" = "grey60"),
        labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
        name = "Significance"
      ) +
      labs(
        title = "Regression Coefficients with 95% Confidence Intervals",
        subtitle = paste("Outcome:", pretty_variable_names[res$outcome] %||% res$outcome),
        x = "Coefficient Estimate",
        y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  })
  
  output$predictor_vs_outcome_plot <- renderPlot({
    req(model_results())
    res <- model_results()
    
    req(is.null(res$message))
    
    plot_data <- res$data |>
      select(state, all_of(c(res$outcome, res$predictors))) |>
      pivot_longer(
        cols = all_of(res$predictors),
        names_to = "predictor",
        values_to = "pred_value"
      ) |>
      mutate(
        predictor_label = pretty_variable_names[predictor] %||% predictor
      )
    
    outcome_label <- pretty_variable_names[res$outcome] %||% res$outcome
    
    ggplot(plot_data, aes(x = pred_value, y = .data[[res$outcome]])) +
      geom_point(alpha = 0.6, color = "steelblue4", size = 2) +
      geom_smooth(
        method = "lm", se = TRUE, color = "firebrick3",
        linewidth = 0.9, fill = "firebrick3", alpha = 0.15
      ) +
      facet_wrap(~ predictor_label, scales = "free_x") +
      labs(
        title = paste("Predictors vs.", outcome_label),
        y = outcome_label,
        x = "Predictor Value"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold")
      )
  })
  
  
  ## -------------------------------------------------------------
  ## Data Table Output
  ## -------------------------------------------------------------
  
  output$data_table <- renderDT({
    validate(
      need(length(input$table_states) > 0, "Please select at least one state."),
      need(length(input$table_variables) > 0, "Please select at least one variable.")
    )
    
    table_data <- table_filtered() |>
      select(state, year, all_of(input$table_variables)) |>
      rename_with(
        ~ pretty_variable_names[.x],
        -c(state, year)
      )
    
    numeric_cols <- names(table_data)[sapply(table_data, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, "year")
    
    datatable(
      table_data,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 20,
        autoWidth = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "copy", text = "Copy"),
          list(extend = "csv", text = "Download CSV"),
          list(extend = "excel", text = "Download Excel")
        )
      ),
      rownames = FALSE
    ) |>
      formatRound(
        columns = numeric_cols,
        digits = 2
      )
  })
  
  ###
  ### Enter Server code above this line
} # end server

### End Server Section ----------------

shinyApp(ui, server)
