cat("\n========== RVS TOOL APP STARTUP ==========\n")
cat(paste0("Time: ", Sys.time(), "\n"))
cat(paste0("Working directory: ", getwd(), "\n"))
cat("=============================================\n\n")
flush.console()

# ===== PACKAGE MANAGEMENT: Install & Load =====
cat("[STARTUP] Installing and loading required packages...\n")
flush.console()

packages <- c(
  "shiny",
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "purrr",
  "plotly",
  "tibble"
)

# Identify packages that are not yet installed
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

# Install missing packages if any
if (length(new_packages) > 0) {
  cat(sprintf("[STARTUP] Installing missing packages: %s\n", paste(new_packages, collapse = ", ")))
  flush.console()
  install.packages(
    new_packages,
    lib = Sys.getenv("R_LIBS_USER"),
    repos = "https://cran.r-project.org"
  )
  cat("[STARTUP] Package installation complete.\n")
  flush.console()
}

# Load all required packages
cat("[STARTUP] Loading packages...\n")
flush.console()
lapply(packages, library, character.only = TRUE)
cat("[STARTUP] All packages loaded successfully.\n")
flush.console()

# app.R
# Simple Shiny dashboard for dummy data in data/dummy_data_iteration_1/all_output.xlsx
# - var choices from name column without the "gebruikt" prefix
# - matched partner name with "gebruikt" prefix (if exists), otherwise NA

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "cohort", "died", "n_totaal", "value", "name", "type", "t",
    "q05_per_persoon", "q25_per_persoon", "mediaan_per_persoon",
    "q75_per_persoon", "q95_per_persoon"
  ))
}

data_path <- "data/dummy_data_iteration_1/all_output.xlsx"

# Create log file for all messages
log_file <- "shiny_console.log"
unlink(log_file)  # Clear any old log file

log_msg <- function(msg) {
  cat(paste0("[", Sys.time(), "] ", msg, "\n"), file = log_file, append = TRUE)
  cat(paste0("[", Sys.time(), "] ", msg, "\n"))  # Also print to console
}

read_all_data <- function(path = data_path) {
  if (!file.exists(path)) {
    msg <- sprintf("[read_all_data] File not found: %s", path)
    log_msg(msg)
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
  log_msg(sprintf("[read_all_data] Loaded %d rows from %d sheets", nrow(df), length(sheets)))
  df
}

# Initialize with empty tibble, will load reactively
log_msg("[startup] Starting Shiny app initialization")
all_data_initial <- tryCatch(
  read_all_data(data_path),
  error = function(e) {
    msg <- sprintf("[startup] read_all_data failed: %s", e$message)
    log_msg(msg)
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
        tabPanel("Costs boxplot-like", plotlyOutput("plot_cost")),
        tabPanel("Usage barplot", plotlyOutput("plot_usage"))
      ),
      verbatimTextOutput("app_log")
    )
  )
)

server <- function(input, output, session) {
  # Use reactiveVal for error collection (properly scoped to session)
  error_log <- reactiveVal(character())
  
  add_error <- function(msg) {
    log_msg(msg)
    error_log(c(error_log(), msg))
  }
  
  # Load data reactively with error handling
  all_data <- reactive({
    log_msg("[reactive] Loading all_data...")
    tryCatch({
      df <- read_all_data(data_path)
      log_msg(sprintf("[reactive] all_data loaded: %d rows", nrow(df)))
      df
    }, error = function(e) {
      msg <- sprintf("[reactive] all_data load failed: %s", e$message)
      add_error(msg)
      tibble::tibble()
    })
  })
  
  selected_data <- reactive({
    log_msg(sprintf("[reactive] Filtering for selected_var: %s", input$selected_var))
    data <- all_data()
    if (nrow(data) == 0) {
      log_msg("[reactive] selected_data: parent data is empty")
      return(tibble::tibble())
    }
    if (!is.character(input$selected_var) || input$selected_var == "") {
      log_msg("[reactive] selected_data: invalid input$selected_var")
      return(tibble::tibble())
    }
    result <- data %>% filter(name == input$selected_var)
    log_msg(sprintf("[reactive] selected_data result: %d rows", nrow(result)))
    result
  })

  plot_cost_data <- reactive({
    log_msg("[reactive] Computing plot_cost_data...")
    tryCatch({
      data <- selected_data()
      if (nrow(data) == 0) {
        log_msg("[reactive] plot_cost_data: selected_data is empty")
        return(tibble::tibble())
      }
      
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

      df <- df %>%
        mutate(t = factor(as.numeric(t), levels = sort(unique(as.numeric(t))))) %>%
        filter(!is.na(mediaan_per_persoon))
      log_msg(sprintf("[reactive] plot_cost_data result: %d rows", nrow(df)))
      df
    }, error = function(e) {
      msg <- sprintf("[plot_cost_data] failed: %s", e$message)
      add_error(msg)
      tibble::tibble()
    })
  })

  plot_usage_data <- reactive({
    log_msg("[reactive] Computing plot_usage_data...")
    tryCatch({
      data <- all_data()
      if (nrow(data) == 0) {
        log_msg("[reactive] plot_usage_data: all_data is empty")
        return(tibble::tibble())
      }
      if (!is.character(input$selected_var) || input$selected_var == "") {
        log_msg("[reactive] plot_usage_data: invalid input$selected_var")
        return(tibble::tibble())
      }
      
      usage_name <- find_gebruikt_name(input$selected_var, data = data)
      log_msg(sprintf("[reactive] plot_usage_data: found usage_name='%s' for '%s'", 
                      if (is.na(usage_name)) "NA" else usage_name, input$selected_var))
      if (is.na(usage_name)) {
        log_msg(sprintf("[reactive] plot_usage_data: no usage variant found for '%s'", input$selected_var))
        return(tibble::tibble())
      }

      result <- data %>%
        filter(name == usage_name, type == "gemiddelde_per_persoon") %>%
        group_by(cohort, died, t) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(t = factor(as.numeric(t), levels = sort(unique(as.numeric(t)))))
      log_msg(sprintf("[reactive] plot_usage_data result: %d rows", nrow(result)))
      result
    }, error = function(e) {
      msg <- sprintf("[plot_usage_data] failed: %s", e$message)
      add_error(msg)
      tibble::tibble()
    })
  })

  output$app_log <- renderText({
    errors <- error_log()
    if (length(errors) > 0) {
      paste("=== ERROR LOG ===\n", paste(errors, collapse = "\n"))
    } else {
      "=== NO ERRORS ===\nApp is running normally. Check shiny_console.log for startup messages."
    }
  })

  output$plot_cost <- plotly::renderPlotly({
    log_msg("[render] Rendering plot_cost with plotly...")
    tryCatch({
      df <- plot_cost_data()
      if (nrow(df) == 0) {
        log_msg("[render] plot_cost: no data available")
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen kosten-data beschikbaar."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }

      log_msg(sprintf("[render] plot_cost: rendering %d rows", nrow(df)))
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
      add_error(msg)
      p <- ggplot() +
        geom_text(aes(0, 0, label = "Plot failed to render."), size = 5) +
        xlab(NULL) + ylab(NULL) + theme_void()
      plotly::ggplotly(p)
    })
  })

  output$plot_usage <- plotly::renderPlotly({
    log_msg("[render] Rendering plot_usage with plotly...")
    tryCatch({
      df <- plot_usage_data()
      if (nrow(df) == 0) {
        log_msg("[render] plot_usage: no data available")
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen gebruiksdata beschikbaar."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }

      log_msg(sprintf("[render] plot_usage: rendering %d rows", nrow(df)))
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
      msg <- sprintf("[plot_usage render] %s", e$message)
      add_error(msg)
      p <- ggplot() +
        geom_text(aes(0, 0, label = "Plot failed to render."), size = 5) +
        xlab(NULL) + ylab(NULL) + theme_void()
      plotly::ggplotly(p)
    })
  })
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
