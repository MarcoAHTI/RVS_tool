# app.R
# Simple Shiny dashboard for dummy data in data/dummy_data_iteration_1/all_output.xlsx
# - var choices from name column without the "gebruikt" prefix
# - matched partner name with "gebruikt" prefix (if exists), otherwise NA

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Plotly is optional, but will be used if installed; otherwise show a text prompt to install.
plotly_installed <- requireNamespace("plotly", quietly = TRUE)

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "cohort", "died", "n_totaal", "value", "name", "type", "t",
    "q05_per_persoon", "q25_per_persoon", "mediaan_per_persoon",
    "q75_per_persoon", "q95_per_persoon"
  ))
}

data_path <- "data/dummy_data_iteration_1/all_output.xlsx"
startup_errors <- character()

read_all_data <- function(path = data_path) {
  if (!file.exists(path)) {
    msg <- sprintf("[read_all_data] File not found: %s", path)
    message(msg)
    startup_errors <<- c(startup_errors, msg)
    return(tibble::tibble())
  }
  
  sheets <- readxl::excel_sheets(path)
  df <- purrr::map_dfr(sheets, ~ readxl::read_excel(path, sheet = .x, col_types = "text") %>%
                         dplyr::mutate(cohort = as.integer(cohort),
                                       value = as.numeric(value),
                                       t = as.character(t),
                                       died = as.character(died),
                                       name = as.character(name),
                                       type = as.character(type)))
  df
}

# Initialize with empty tibble, will load reactively
all_data_initial <- tryCatch(
  read_all_data(data_path),
  error = function(e) {
    msg <- sprintf("[startup] read_all_data failed: %s", e$message)
    message(msg)
    startup_errors <<- c(startup_errors, msg)
    tibble::tibble()
  }
)

base_names <- if (nrow(all_data_initial) > 0) {
  sort(unique(all_data_initial$name[!startsWith(all_data_initial$name, "gebruik")]))
} else {
  character(0)
}

find_gebruikt_name <- function(name_choice, data) {
  if (is.null(data) || nrow(data) == 0) return(NA_character_)
  candidate1 <- paste0("gebruik", name_choice)
  candidate2 <- paste0(name_choice, "_gebruikt")
  valid <- intersect(c(candidate1, candidate2), unique(data$name))
  if (length(valid) > 0) valid[[1]] else NA_character_
}

ui <- fluidPage(
  titlePanel("RVS Tool Dummy Data Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("selected_var", "Kies variabele (name):", choices = base_names, selected = base_names[1]),
      helpText("Plot 1: boxplot-achtig overzicht op basis van quantielen in type.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Costs boxplot-like", if (plotly_installed) plotlyOutput("plot_cost") else plotOutput("plot_cost")),
        tabPanel("Usage barplot", if (plotly_installed) plotlyOutput("plot_usage") else plotOutput("plot_usage"))
      ),
      verbatimTextOutput("app_log")
    )
  )
)

server <- function(input, output, session) {
  # Load data reactively with error handling
  all_data <- reactive({
    tryCatch({
      read_all_data(data_path)
    }, error = function(e) {
      msg <- sprintf("[reactive] all_data load failed: %s", e$message)
      message(msg)
      startup_errors <<- c(startup_errors, msg)
      tibble::tibble()
    })
  })
  
  selected_data <- reactive({
    data <- all_data()
    if (nrow(data) == 0) return(tibble::tibble())
    if (!is.character(input$selected_var) || input$selected_var == "") return(tibble::tibble())
    data %>% filter(name == input$selected_var)
  })

  plot_cost_data <- reactive({
    tryCatch({
      data <- selected_data()
      if (nrow(data) == 0) return(tibble::tibble())
      
      df <- data %>%
        filter(type %in% c("q05_per_persoon", "q25_per_persoon", "mediaan_per_persoon", "q75_per_persoon", "q95_per_persoon")) %>%
        group_by(cohort, died, t, type) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(
          names_from = type,
          values_from = value,
          values_fn = mean,
          values_fill = NA_real_
        )

      df %>%
        mutate(t = factor(as.numeric(t), levels = sort(unique(as.numeric(t))))) %>%
        filter(!is.na(mediaan_per_persoon))
    }, error = function(e) {
      msg <- sprintf("[plot_cost_data] failed: %s", e$message)
      message(msg)
      startup_errors <<- c(startup_errors, msg)
      tibble::tibble()
    })
  })

  plot_usage_data <- reactive({
    tryCatch({
      data <- all_data()
      if (nrow(data) == 0) return(tibble::tibble())
      if (!is.character(input$selected_var) || input$selected_var == "") return(tibble::tibble())
      
      usage_name <- find_gebruikt_name(input$selected_var, data = data)
      if (is.na(usage_name)) {
        return(tibble::tibble())
      }

      data %>%
        filter(name == usage_name, type == "gemiddelde_per_persoon") %>%
        group_by(cohort, died, t) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(t = factor(as.numeric(t), levels = sort(unique(as.numeric(t)))))
    }, error = function(e) {
      msg <- sprintf("[plot_usage_data] failed: %s", e$message)
      message(msg)
      startup_errors <<- c(startup_errors, msg)
      tibble::tibble()
    })
  })

  output$app_log <- renderText({
    if (length(startup_errors) > 0) {
      paste(startup_errors, collapse = "\n")
    } else {
      "No startup errors detected."
    }
  })

  if (plotly_installed) {
    output$plot_cost <- plotly::renderPlotly({
      tryCatch({
        df <- plot_cost_data()
        if (nrow(df) == 0) {
          p <- ggplot() +
            geom_text(aes(0, 0, label = "Geen kosten-data beschikbaar."), size = 5) +
            xlab(NULL) + ylab(NULL) + theme_void()
          return(plotly::ggplotly(p))
        }

        p <- ggplot(df, aes(x = factor(t), group = died, color = died, fill = died)) +
          geom_errorbar(aes(ymin = q05_per_persoon, ymax = q95_per_persoon),
                        position = position_dodge(width = 0.8), width = 0.2) +
          geom_crossbar(aes(y = mediaan_per_persoon, ymin = q25_per_persoon, ymax = q75_per_persoon),
                        position = position_dodge(width = 0.8), width = 0.35, alpha = 0.35) +
          geom_point(aes(y = mediaan_per_persoon), position = position_dodge(width = 0.8), size = 2) +
          facet_wrap(~cohort, nrow = 1) +
          labs(
            title = paste("Kostenboxplot voor", input$selected_var),
            x = "t", y = "Kosten (per persoon)",
            color = "Status", fill = "Status"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

        plotly::ggplotly(p)
      }, error = function(e) {
        msg <- sprintf("[plot_cost render] %s", e$message)
        message(msg)
        startup_errors <<- c(startup_errors, msg)
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Plot failed to render."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        plotly::ggplotly(p)
      })
    })
  } else {
    output$plot_cost <- renderPlot({
      tryCatch({
        df <- plot_cost_data()
        if (nrow(df) == 0) {
          plot.new(); text(0.5, 0.5, "Geen kosten-data beschikbaar.", cex = 1.2)
          return()
        }

        ggplot(df, aes(x = factor(t), group = died, color = died, fill = died)) +
          geom_errorbar(aes(ymin = q05_per_persoon, ymax = q95_per_persoon),
                        position = position_dodge(width = 0.8), width = 0.2) +
          geom_crossbar(aes(y = mediaan_per_persoon, ymin = q25_per_persoon, ymax = q75_per_persoon),
                        position = position_dodge(width = 0.8), width = 0.35, alpha = 0.35) +
          geom_point(aes(y = mediaan_per_persoon), position = position_dodge(width = 0.8), size = 2) +
          facet_wrap(~cohort, nrow = 1) +
          labs(
            title = paste("Kostenboxplot voor", input$selected_var),
            x = "t", y = "Kosten (per persoon)",
            color = "Status", fill = "Status"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }, error = function(e) {
        msg <- sprintf("[plot_cost static] %s", e$message)
        message(msg)
        startup_errors <<- c(startup_errors, msg)
        plot.new(); text(0.5, 0.5, "Plot render error.", cex = 1.2)
      })
    })
  }

  if (plotly_installed) {
    output$plot_usage <- plotly::renderPlotly({
      tryCatch({
        df <- plot_usage_data()
        if (nrow(df) == 0) {
          p <- ggplot() +
            geom_text(aes(0, 0, label = "Geen gebruiksdata beschikbaar."), size = 5) +
            xlab(NULL) + ylab(NULL) + theme_void()
          return(plotly::ggplotly(p))
        }

        p <- ggplot(df, aes(x = factor(t), y = value, fill = died)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.7) +
          facet_wrap(~cohort, nrow = 1) +
          labs(
            title = paste("Gebruikt gemiddeld per persoon voor", input$selected_var),
            x = "t", y = "gemiddelde", fill = "Status"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

        plotly::ggplotly(p)
      }, error = function(e) {
        msg <- sprintf("[plot_usage plotly] %s", e$message)
        message(msg)
        startup_errors <<- c(startup_errors, msg)
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Plot failed to render."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        plotly::ggplotly(p)
      })
    })
  } else {
    output$plot_usage <- renderPlot({
      tryCatch({
        df <- plot_usage_data()
        if (nrow(df) == 0) {
          plot.new(); text(0.5, 0.5, "Geen gebruiksdata beschikbaar.", cex = 1.2)
          return()
        }

        ggplot(df, aes(x = factor(t), y = value, fill = died)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.7) +
          facet_wrap(~cohort, nrow = 1) +
          labs(
            title = paste("Gebruikt gemiddeld per persoon voor", input$selected_var),
            x = "t", y = "gemiddelde", fill = "Status"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }, error = function(e) {
        msg <- sprintf("[plot_usage static] %s", e$message)
        message(msg)
        startup_errors <<- c(startup_errors, msg)
        plot.new(); text(0.5, 0.5, "Plot render error.", cex = 1.2)
      })
    })
  }
}

# Run the app
options(shiny.error = function() {
  err <- geterrmessage()
  message(sprintf("[shiny.error] %s", err))
  writeLines(sprintf("[shiny.error] %s", err), con = "shiny_error.log")
})

# Safe app wrapper with proper auth support
if (exists("secure_app", mode = "function") && exists("secure_server", mode = "function")) {
  message("[startup] secure_app and secure_server found; running with authentication")
  shinyApp(
    ui = secure_app(ui),
    server = function(input, output, session) {
      res_auth <- tryCatch({
        secure_server(
          check_credentials = check_credentials(
            data.frame(
              user = c("ahti"),
              password = c("user123"),
              stringsAsFactors = FALSE
            )
          )
        )
      }, error = function(e) {
        msg <- sprintf("[secure_server] auth failed: %s", e$message)
        message(msg)
        startup_errors <<- c(startup_errors, msg)
        NULL
      })
      
      if (!is.null(res_auth)) {
        server(input, output, session)
      } else {
        output$app_log <- renderText("Authentication error - see logs.")
      }
    }
  )
} else {
  message("[startup] secure_app/secure_server not found or not loaded; running without authentication")
  shinyApp(ui, server)
}
