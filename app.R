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
cat("[STARTUP] All packages loaded successfully.\n\n")
flush.console()

# ===== VARIABLE DECLARATIONS & UTILITIES =====

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "cohort", "died", "n_totaal", "value", "name", "type", "t",
    "q05_per_persoon", "q25_per_persoon", "mediaan_per_persoon",
    "q75_per_persoon", "q95_per_persoon", "bin_size", "doodsoorzaak",
    "t_numeric", "value_butterfly", "group", "interventie", "interventie_category"
  ))
}

data_path <- "data/data_iteration_1/all_output.xlsx"
log_file <- "shiny_console.log"
unlink(log_file)

log_msg <- function(msg) {
  cat(paste0("[", Sys.time(), "] ", msg, "\n"), file = log_file, append = TRUE)
  cat(paste0("[", Sys.time(), "] ", msg, "\n"))
}

read_all_data <- function(path = data_path) {
  if (!file.exists(path)) {
    msg <- sprintf("[read_all_data] File not found: %s", path)
    log_msg(msg)
    return(tibble::tibble())
  }
  
  sheets <- readxl::excel_sheets(path)
  df <- purrr::map_dfr(sheets, ~ readxl::read_excel(path, sheet = .x, col_types = "text") %>%
                         dplyr::mutate(across(c("cohort", "t", "died", "name", "type", "doodsoorzaak", "bin_size"), as.character),
                                       value = as.numeric(value),
                                       n_totaal = as.numeric(n_totaal)))
  log_msg(sprintf("[read_all_data] Loaded %d rows from %d sheets", nrow(df), length(sheets)))
  df
}

# Initialize data
log_msg("[startup] Initializing data...")
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

doodsoorzaken <- if (nrow(all_data_initial) > 0) {
  c("all", sort(unique(all_data_initial$doodsoorzaak[all_data_initial$doodsoorzaak != "all"])))
} else {
  "all"
}

log_msg(sprintf("[startup] Initialization complete: %d base names, %d doodsoorzaken", 
                length(base_names), length(doodsoorzaken)))

# ===== HELPER FUNCTIONS =====
find_gebruikt_name <- function(name_choice, data) {
  if (is.null(data) || nrow(data) == 0) return(NA_character_)
  candidate1 <- paste0("gebruik", name_choice)
  candidate2 <- paste0(name_choice, "_gebruikt")
  valid <- intersect(c(candidate1, candidate2), unique(data$name))
  if (length(valid) > 0) valid[[1]] else NA_character_
}

# Helper function: Map interventions to their constituent names
get_interventie_categories <- function(interventie_choice) {
  mapping <- list(
    "AAA" = c("AAA_kosten", "AAA_gebruikers"),
    "Heup" = c("Heup_kosten", "Heup_gebruikers"),
    "IC" = c("IC_kosten", "IC_gebruikers"),
    "Diagnostiek" = c("Diagnostiek_kosten", "Diagnostiek_gebruikers"),
    "Oncologie" = c("Oncologie_kosten", "Oncologie_gebruikers"),
    "Polyfarmacie" = c("Polyfarmacie_kosten", "Polyfarmacie_gebruikers")
  )
  return(mapping[[interventie_choice]] %||% character(0))
}

# Helper function: Get maatstaf options
get_maatstaf_options <- function() {
  c(
    "Totale kosten" = "sum_totaal_groep",
    "Kosten per persoon" = "gemiddelde_per_persoon",
    "Aantal gebruikers" = "n_totaal_gebruikers",
    "Kosten per gebruiker" = "gemiddelde_per_gebruiker",
    "Prevalentie per 100" = "prevalentie_per_100"
  )
}

# Helper function: Process measurements for standardized filtering
process_measurements <- function(data, maatstaf, handle_prevalentie = TRUE) {
  if (is.null(data) || nrow(data) == 0) return(tibble::tibble())

  # If maatstaf is prevalentie_per_100, handle specially
  if (handle_prevalentie && maatstaf == "prevalentie_per_100") {
    # Filter for data containing prevalentie_per_100 measurements
    result <- data %>% filter(type == "prevalentie_per_100")
  } else {
    # Standard filtering
    result <- data %>% filter(type == maatstaf)
  }

  return(result)
}


# ===== UI DEFINITION =====
ui <- navbarPage(
  title = "blabala",
  id = "main_nav",
  
  # --- TAB 1: Basispopulatie ---
  tabPanel("Basispopulatie",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               selectInput("pop_jaar", "Jarenselectie:", choices = c("2019", "2023", "2019 + 2023"), selected = "2019 + 2023"),
               selectInput("pop_split", "Kies populaties:", choices = c("Enkel totale populatie", "Totaal + subgroepen doodsoorzaak"), selected = "Totaal + subgroepen doodsoorzaak"),
               hr(),
               downloadButton("dl_basis", "Download Data voor Think-cell")
             ),
             mainPanel(
               plotlyOutput("plot_basispopulatie", height = "600px")
             )
           )
  ),
  
  # --- TAB 2: Zorg Totaal (1000 dagen) ---
  tabPanel("Zorg Totaal (1000 dgn)",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               selectInput("tot_pop", "Populatie:", choices = doodsoorzaken, selected = "all"),
               selectInput("tot_jaar", "Jaar:", choices = c("2019", "2023", "Beide"), selected = "2023"),
               selectInput("tot_maatstaf", "Maatstaf:",
                           choices = c("Totale kosten" = "sum_totaal_groep",
                                       "Kosten per persoon" = "gemiddelde_per_persoon",
                                       "Aantal gebruikers" = "n_totaal_gebruikers",
                                       "Kosten per gebruiker" = "gemiddelde_per_gebruiker",
                                       "Prevalentie per 100" = "prevalentie_per_100"),
                           selected = "gemiddelde_per_persoon"),
               selectInput("tot_vgl", "Kies vergelijking:", 
                           choices = c("Geen vergelijking", "Overleden vs. In leven (Controle)"), 
                           selected = "Overleden vs. In leven (Controle)"),
               hr(),
               downloadButton("dl_totaal", "Download Data voor Think-cell")
             ),
             mainPanel(
               plotlyOutput("plot_zorg_totaal", height = "600px")
             )
           )
  ),
  
  # --- TAB 3: Zorg Maandelijks ---
  tabPanel("Zorg Maandelijks",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               selectInput("mnd_domein", "Zorgdomein (Variabele):", choices = base_names, selected = base_names[1]),
               selectInput("mnd_maatstaf", "Maatstaf:",
                           choices = c("Totale kosten" = "sum_totaal_groep",
                                       "Kosten per persoon" = "gemiddelde_per_persoon",
                                       "Aantal gebruikers" = "n_totaal_gebruikers",
                                       "Kosten per gebruiker" = "gemiddelde_per_gebruiker",
                                       "Prevalentie per 100" = "prevalentie_per_100"),
                           selected = "gemiddelde_per_persoon"),
               selectInput("mnd_jaar", "Jaar:", choices = c("2019", "2023", "Beide"), selected = "2023"),
               selectInput("mnd_pop", "Populatie (Doodsoorzaak):", choices = doodsoorzaken, selected = "all"),
               selectInput("mnd_vgl", "Kies vergelijking:", 
                           choices = c("Geen vergelijking", "Overleden vs. In leven (Controle)"), 
                           selected = "Overleden vs. In leven (Controle)"),
               selectInput("mnd_grafiek", "Grafiektype:",
                           choices = c("Staafgrafiek", "Lijngrafiek"),
                           selected = "Staafgrafiek"),
               selectInput("mnd_lijnmodus", "Lijngrafiek modus:",
                           choices = c("Status (met/zonder controle)" = "status",
                                       "Alle doodsoorzaken in 1 grafiek" = "doodsoorzaak",
                                       "Totale populatie 2019 vs 2023" = "cohort"),
                           selected = "status"),
               selectizeInput("mnd_zichtbare_lijnen", "Zichtbare lijnen:",
                              choices = NULL, selected = NULL, multiple = TRUE),
               helpText("Tip: in lijngrafiekmodus kun je lijnen aan/uit zetten via 'Zichtbare lijnen'"),
               hr(),
               downloadButton("dl_maandelijks", "Download Data voor Think-cell")
             ),
             mainPanel(
               plotlyOutput("plot_zorg_maandelijks", height = "600px")
             )
           )
  ),
  
  # --- TAB 4: Costs Boxplot-like (Quantiles) ---
  tabPanel("Kosten Boxplot",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               selectInput("cost_var", "Kies variabele (name):", choices = base_names, selected = base_names[1]),
               helpText("Boxplot-achtig overzicht op basis van quantielen per cohort en status.")
             ),
             mainPanel(
               plotlyOutput("plot_cost", height = "600px")
             )
           )
  ),
  
  # --- TAB 5: Usage Barplot ---
  tabPanel("Gebruik Barplot",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               selectInput("usage_var", "Kies variabele (name):", choices = base_names, selected = base_names[1]),
               helpText("Gemiddeld gebruik per persoon over tijd per cohort en status.")
             ),
             mainPanel(
               plotlyOutput("plot_usage", height = "600px")
             )
           )
  ),
  
  # --- TAB 6: Zorg per Domein Butterfly ---
  tabPanel("Zorg per Domein (Butterfly)",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               selectInput("butterfly_domein", "Zorgdomein:", choices = base_names, selected = base_names[1]),
               selectInput("butterfly_maatstaf", "Maatstaf:",
                           choices = c("Aantal gebruikers" = "n_totaal_gebruikers",
                                       "Kosten per gebruiker" = "gemiddelde_per_persoon",
                                       "Totale kosten" = "sum_totaal_groep",
                                       "Kosten per gebruiker (alt)" = "gemiddelde_per_gebruiker",
                                       "Prevalentie per 100" = "prevalentie_per_100"),
                           selected = "gemiddelde_per_persoon"),
               selectInput("butterfly_vgl", "Vergelijking (Links vs Rechts):",
                           choices = c("Geobserveerd 2023 vs. Controle 2023" = "obs_2023_vs_ctrl_2023",
                                       "Geobserveerd 2019 vs. Geobserveerd 2023" = "obs_2019_vs_obs_2023",
                                       "Geobserveerd 2019 vs. Controle 2019" = "obs_2019_vs_ctrl_2019"),
                           selected = "obs_2023_vs_ctrl_2023"),
               hr(),
               downloadButton("dl_butterfly", "Download Data voor Think-cell")
             ),
             mainPanel(
               plotlyOutput("plot_butterfly", height = "700px")
             )
           )
  ),

  # --- TAB 7: Interventies Analysis ---
  tabPanel("Interventies",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               selectInput("int_interventie", "Selecteer interventie:",
                           choices = c("AAA", "Heup", "IC", "Diagnostiek", "Oncologie", "Polyfarmacie"),
                           selected = "AAA"),
               selectInput("int_maatstaf", "Maatstaf:",
                           choices = get_maatstaf_options(),
                           selected = "gemiddelde_per_persoon"),
               selectInput("int_jaar", "Jaar:",
                           choices = c("2019", "2023", "Beide"),
                           selected = "2023"),
               selectInput("int_vgl", "Vergelijking:",
                           choices = c("Geen vergelijking", "Overleden vs. In leven (Controle)"),
                           selected = "Overleden vs. In leven (Controle)"),
               hr(),
               downloadButton("dl_interventies", "Download Data voor Think-cell")
             ),
             mainPanel(
               plotlyOutput("plot_interventies", height = "600px")
             )
           )
  ),

  # --- TAB 8: App Logs ---
  tabPanel("Systeem Logs",
           verbatimTextOutput("app_log")
  )
)

# ===== SERVER DEFINITION =====
server <- function(input, output, session) {
  
  error_log <- reactiveVal(character())
  add_error <- function(msg) {
    log_msg(msg)
    error_log(c(error_log(), msg))
  }
  
  # 1. Load Core Data Reactively
  all_data <- reactive({
    tryCatch({
      df <- read_all_data(data_path)
      df
    }, error = function(e) {
      add_error(sprintf("[reactive] all_data load failed: %s", e$message))
      tibble::tibble()
    })
  })
  
  # ==========================================
  # SERVER LOGIC: TAB 1 - Basispopulatie
  # ==========================================
  data_basis <- reactive({
    req(nrow(all_data()) > 0)
    df <- all_data() %>% 
      filter(bin_size == "1000days", type == "n_totaal_gebruikers") %>%
      # Use an arbitrary domain just to get the distinct populations
      filter(name == base_names[1])
    
    if(input$pop_jaar != "2019 + 2023") {
      df <- df %>% filter(cohort == as.numeric(input$pop_jaar))
    }
    
    if(input$pop_split == "Enkel totale populatie") {
      df <- df %>% filter(doodsoorzaak == "all")
    }
    
    # Aggregate n_totaal
    df %>%
      group_by(cohort, doodsoorzaak, died) %>%
      summarise(n_mensen = mean(n_totaal, na.rm=TRUE), .groups = "drop")
  })
  
  output$plot_basispopulatie <- plotly::renderPlotly({
    df <- data_basis()
    p <- ggplot(df, aes(x = doodsoorzaak, y = n_mensen, fill = died)) +
      geom_col(position = position_dodge()) +
      facet_wrap(~cohort) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Basispopulatie", x = "Populatie / Doodsoorzaak", y = "Aantal")
    plotly::ggplotly(p)
  })
  
  output$dl_basis <- downloadHandler(
    filename = function() { paste("basispopulatie-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv2(data_basis(), file, row.names = FALSE) }
  )
  
  # ==========================================
  # SERVER LOGIC: TAB 2 - Zorg Totaal 1000 dgn
  # ==========================================
  data_totaal <- reactive({
    req(nrow(all_data()) > 0)
    df <- process_measurements(all_data(), input$tot_maatstaf) %>%
      filter(bin_size == "1000days",
             doodsoorzaak == input$tot_pop)

    if(input$tot_jaar != "Beide") {
      df <- df %>% filter(cohort == as.numeric(input$tot_jaar))
    }
    if(input$tot_vgl == "Geen vergelijking") {
      df <- df %>% filter(died == "Overleden")
    }

    df %>%
      group_by(name, cohort, died) %>%
      summarise(waarde = mean(value, na.rm=TRUE), .groups = "drop")
  })
  
  output$plot_zorg_totaal <- plotly::renderPlotly({
    df <- data_totaal()
    p <- ggplot(df, aes(x = reorder(name, waarde), y = waarde, fill = died)) +
      geom_col(position = position_dodge()) +
      facet_wrap(~cohort) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Zorg Totaal (Laatste 1000 dagen)", x = "Zorgdomein", y = "Waarde")
    plotly::ggplotly(p)
  })
  
  output$dl_totaal <- downloadHandler(
    filename = function() { paste("zorg_totaal-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv2(data_totaal(), file, row.names = FALSE) }
  )
  
  # ==========================================
  # SERVER LOGIC: TAB 3 - Zorg Maandelijks
  # ==========================================
  data_maandelijks <- reactive({
    req(nrow(all_data()) > 0)
    df <- process_measurements(all_data(), input$mnd_maatstaf) %>%
      filter(bin_size == "monthly",
             name == input$mnd_domein)

    if(input$mnd_jaar != "Beide") {
      df <- df %>% filter(cohort == as.numeric(input$mnd_jaar))
    }

    if(input$mnd_pop == "all") {
      df <- df %>% filter(doodsoorzaak == "all")
    } else {
      df <- df %>% filter(doodsoorzaak == input$mnd_pop)
    }

    if(input$mnd_vgl == "Geen vergelijking") {
      df <- df %>% filter(died == "Overleden")
    }

    df %>%
      mutate(t_numeric = as.numeric(t)) %>%
      arrange(desc(t_numeric))
  })

  data_maandelijks_lijn <- reactive({
    req(nrow(all_data()) > 0)
    df <- process_measurements(all_data(), input$mnd_maatstaf) %>%
      filter(bin_size == "monthly",
             name == input$mnd_domein)

    log_msg(sprintf("[data_maandelijks_lijn] Base: %d rows (domein=%s, maatstaf=%s)",
                    nrow(df), input$mnd_domein, input$mnd_maatstaf))

    has_all_pop <- any(df$doodsoorzaak == "all", na.rm = TRUE)

    if (input$mnd_lijnmodus == "doodsoorzaak") {
      if(input$mnd_jaar != "Beide") {
        df <- df %>% filter(cohort == as.numeric(input$mnd_jaar))
      }
      df <- df %>% filter(doodsoorzaak != "all", died == "Overleden")
    } else if (input$mnd_lijnmodus == "cohort") {
      if (has_all_pop) {
        df <- df %>% filter(doodsoorzaak == "all")
      }
      if(input$mnd_jaar != "Beide") {
        df <- df %>% filter(cohort == as.numeric(input$mnd_jaar))
      }
      if(input$mnd_vgl == "Geen vergelijking") {
        df <- df %>% filter(died == "Overleden")
      }
    } else {
      if(input$mnd_jaar != "Beide") {
        df <- df %>% filter(cohort == as.numeric(input$mnd_jaar))
      }
      if(input$mnd_pop == "all") {
        if (has_all_pop) {
          df <- df %>% filter(doodsoorzaak == "all")
        }
      } else {
        df <- df %>% filter(doodsoorzaak == input$mnd_pop)
      }
      if(input$mnd_vgl == "Geen vergelijking") {
        df <- df %>% filter(died == "Overleden")
      }
    }

    df <- df %>%
      mutate(t_numeric = as.numeric(t)) %>%
      filter(!is.na(t_numeric), !is.na(value)) %>%
      arrange(t_numeric)

    log_msg(sprintf("[data_maandelijks_lijn] Post-filter: %d rows", nrow(df)))
    df
  })

  lijn_data_maandelijks <- reactive({
    df <- data_maandelijks_lijn()
    if (nrow(df) == 0) return(tibble::tibble())

    if (input$mnd_lijnmodus == "doodsoorzaak") {
      df <- df %>% mutate(lijn = doodsoorzaak)
    } else if (input$mnd_lijnmodus == "cohort") {
      df <- df %>% mutate(lijn = paste0("Cohort ", cohort, " - ", died))
    } else {
      df <- df %>% mutate(lijn = died)
    }

    df <- df %>%
      mutate(lijn = trimws(as.character(lijn))) %>%
      group_by(t_numeric, lijn) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    # DEBUG: Print summarized data
    log_msg(sprintf("[lijn_data_maandelijks] Summary: %d rows, lijnen: %s", 
                    nrow(df), paste(unique(df$lijn), collapse=", ")))
    df
  })

  lijn_choices_maandelijks <- reactive({
    df <- data_maandelijks_lijn()
    if (nrow(df) == 0) return(character(0))

    if (input$mnd_lijnmodus == "doodsoorzaak") {
      sort(unique(df$doodsoorzaak))
    } else if (input$mnd_lijnmodus == "cohort") {
      sort(unique(paste0("Cohort ", df$cohort, " - ", df$died)))
    } else {
      sort(unique(df$died))
    }
  })

  observeEvent(list(lijn_choices_maandelijks(), input$mnd_grafiek), {
    lijn_choices <- lijn_choices_maandelijks()

    if (input$mnd_grafiek != "Lijngrafiek" || length(lijn_choices) == 0) {
      freezeReactiveValue(input, "mnd_zichtbare_lijnen")
      updateSelectizeInput(
        session,
        "mnd_zichtbare_lijnen",
        choices = lijn_choices,
        selected = character(0),
        server = TRUE
      )
      return()
    }

    selected_lijnen <- isolate(input$mnd_zichtbare_lijnen)
    if (is.null(selected_lijnen)) selected_lijnen <- character(0)

    selected_lijnen <- intersect(selected_lijnen, lijn_choices)
    if (length(selected_lijnen) == 0) {
      selected_lijnen <- lijn_choices
    }

    freezeReactiveValue(input, "mnd_zichtbare_lijnen")
    updateSelectizeInput(
      session,
      "mnd_zichtbare_lijnen",
      choices = lijn_choices,
      selected = selected_lijnen,
      server = TRUE
    )
  }, ignoreInit = FALSE)
  
  output$plot_zorg_maandelijks <- plotly::renderPlotly({
    if (input$mnd_grafiek == "Lijngrafiek") {
      df <- lijn_data_maandelijks()
      
      input_selection <- input$mnd_zichtbare_lijnen
      available_lijnen <- unique(df$lijn)
      
      # Determine effective selection robustly
      if (is.null(input_selection) || length(input_selection) == 0) {
        selected_lijnen <- available_lijnen
      } else {
        # Check intersection with available lines to handle stale inputs
        input_clean <- trimws(as.character(input_selection))
        valid_selection <- intersect(input_clean, available_lijnen)
        
        if (length(valid_selection) > 0) {
          selected_lijnen <- valid_selection
        } else {
          # If input selects nothing valid (stale), fallback to showing all
          selected_lijnen <- available_lijnen
        }
      }
      
      log_msg(sprintf("[renderPlotly] Input: %s. Available: %s. Effective: %s", 
              paste(input_selection, collapse=","), 
              paste(available_lijnen, collapse=","),
              paste(selected_lijnen, collapse=",")))

      df <- df %>% filter(lijn %in% selected_lijnen)

      if (nrow(df) == 0) {
        log_msg("[renderPlotly] Empty DF after line filter")
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen lijn-data beschikbaar voor de gekozen filters."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }

      p <- ggplot(df, aes(x = t_numeric, y = value, color = lijn, group = lijn)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        theme_minimal() +
        labs(
          title = paste("Maandelijkse Zorg (Lijn):", input$mnd_domein),
          x = "Maanden voor overlijden (t)",
          y = "Waarde",
          color = "Lijn"
        )
    } else {
      df <- data_maandelijks()
      p <- ggplot(df, aes(x = factor(t_numeric, levels = sort(unique(t_numeric))), y = value, fill = died)) +
        geom_col(position = position_dodge()) +
        theme_minimal() +
        labs(title = paste("Maandelijkse Zorg:", input$mnd_domein), 
             x = "Maanden voor overlijden (t)", y = "Waarde")
      if (input$mnd_jaar == "Beide") {
        p <- p + facet_wrap(~cohort, nrow = 1)
      }
    }

    plotly::ggplotly(p)
  })
  
  output$dl_maandelijks <- downloadHandler(
    filename = function() { paste("zorg_maandelijks-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv2(data_maandelijks(), file, row.names = FALSE) }
  )
  
  # ==========================================
  # SERVER LOGIC: TAB 4 - Kosten Boxplot
  # ==========================================
  selected_data <- reactive({
    log_msg(sprintf("[reactive] Filtering for cost_var: %s", input$cost_var))
    data <- all_data()
    if (nrow(data) == 0) {
      log_msg("[reactive] selected_data: parent data is empty")
      return(tibble::tibble())
    }
    if (!is.character(input$cost_var) || input$cost_var == "") {
      log_msg("[reactive] selected_data: invalid input$cost_var")
      return(tibble::tibble())
    }
    result <- data %>% filter(name == input$cost_var)
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
          title = paste("Kostenboxplot voor", input$cost_var),
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

  # ==========================================
  # SERVER LOGIC: TAB 5 - Gebruik Barplot
  # ==========================================
  plot_usage_data <- reactive({
    log_msg("[reactive] Computing plot_usage_data...")
    tryCatch({
      data <- all_data()
      if (nrow(data) == 0) {
        log_msg("[reactive] plot_usage_data: all_data is empty")
        return(tibble::tibble())
      }
      if (!is.character(input$usage_var) || input$usage_var == "") {
        log_msg("[reactive] plot_usage_data: invalid input$usage_var")
        return(tibble::tibble())
      }
      
      # First try to find usage variant
      usage_name <- find_gebruikt_name(input$usage_var, data = data)
      
      # If no usage variant, check if the variable itself has gemiddelde_per_persoon type
      if (is.na(usage_name)) {
        log_msg(sprintf("[reactive] plot_usage_data: no usage variant for '%s', checking if variable has gemiddelde_per_persoon", input$usage_var))
        check_direct <- data %>%
          filter(name == input$usage_var, type == "gemiddelde_per_persoon")
        
        if (nrow(check_direct) > 0) {
          log_msg(sprintf("[reactive] plot_usage_data: using '%s' directly (has gemiddelde_per_persoon)", input$usage_var))
          usage_name <- input$usage_var
        } else {
          log_msg(sprintf("[reactive] plot_usage_data: no usage data available for '%s'", input$usage_var))
          return(tibble::tibble())
        }
      } else {
        log_msg(sprintf("[reactive] plot_usage_data: found usage_name='%s' for '%s'", usage_name, input$usage_var))
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
          title = paste("Gebruikt gemiddeld per persoon voor", input$usage_var),
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
  
  # ==========================================
  # SERVER LOGIC: TAB 6 - Zorg per Domein Butterfly
  # ==========================================
  data_butterfly <- reactive({
    log_msg("[reactive] Computing butterfly data...")
    tryCatch({
      data <- all_data()
      if (nrow(data) == 0) {
        log_msg("[reactive] butterfly data: all_data is empty")
        return(tibble::tibble())
      }

      # Filter for 1000 days, selected domain and measure using process_measurements
      df <- process_measurements(data, input$butterfly_maatstaf) %>%
        filter(bin_size == "1000days",
               name == input$butterfly_domein)

      if (nrow(df) == 0) {
        log_msg("[reactive] butterfly: no data for selected filters")
        return(tibble::tibble())
      }

      # Parse comparison choice and create left/right groups
      if (input$butterfly_vgl == "obs_2023_vs_ctrl_2023") {
        # Left: Observed 2023, Right: Control 2023
        left_filter <- df %>% filter(cohort == "2023", died == "Overleden")
        right_filter <- df %>% filter(cohort == "2023", died == "In leven")
        left_label <- "Observed 2023"
        right_label <- "Control 2023"
      } else if (input$butterfly_vgl == "obs_2019_vs_obs_2023") {
        # Left: Observed 2019, Right: Observed 2023
        left_filter <- df %>% filter(cohort == "2019", died == "Overleden")
        right_filter <- df %>% filter(cohort == "2023", died == "Overleden")
        left_label <- "Observed 2019"
        right_label <- "Observed 2023"
      } else if (input$butterfly_vgl == "obs_2019_vs_ctrl_2019") {
        # Left: Observed 2019, Right: Control 2019
        left_filter <- df %>% filter(cohort == "2019", died == "Overleden")
        right_filter <- df %>% filter(cohort == "2019", died == "In leven")
        left_label <- "Observed 2019"
        right_label <- "Control 2019"
      }

      # Aggregate by doodsoorzaak
      left_agg <- left_filter %>%
        group_by(doodsoorzaak) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(group = left_label, value_butterfly = -value)

      right_agg <- right_filter %>%
        group_by(doodsoorzaak) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(group = right_label, value_butterfly = value)

      result <- bind_rows(left_agg, right_agg) %>%
        arrange(doodsoorzaak)

      log_msg(sprintf("[reactive] butterfly data computed: %d rows", nrow(result)))
      result
    }, error = function(e) {
      msg <- sprintf("[butterfly data] failed: %s", e$message)
      add_error(msg)
      tibble::tibble()
    })
  })
  
  output$plot_butterfly <- plotly::renderPlotly({
    log_msg("[render] Rendering butterfly chart...")
    tryCatch({
      df <- data_butterfly()
      if (nrow(df) == 0) {
        log_msg("[render] butterfly: no data available")
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen data beschikbaar voor butterfly chart."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }
      
      # Pivot to get left and right values side by side
      pivot_df <- df %>%
        pivot_wider(
          names_from = group,
          values_from = value_butterfly,
          values_fill = 0
        ) %>%
        mutate(doodsoorzaak = factor(doodsoorzaak, levels = sort(unique(doodsoorzaak))))
      
      # Get group names dynamically
      group_cols <- setdiff(colnames(pivot_df), c("doodsoorzaak", "value"))
      
      log_msg(sprintf("[render] butterfly: rendering %d rows with groups: %s", nrow(pivot_df), paste(group_cols, collapse=", ")))
      
      if (length(group_cols) < 2) {
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Onvoldoende data voor vergelijking."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }
      
      left_col <- group_cols[1]
      right_col <- group_cols[2]
      
      # Create butterfly chart with both sides
      p <- ggplot(pivot_df) +
        geom_col(aes(x = !!sym(left_col), y = doodsoorzaak, fill = left_col), 
                 position = "identity", alpha = 0.8) +
        geom_col(aes(x = !!sym(right_col), y = doodsoorzaak, fill = right_col), 
                 position = "identity", alpha = 0.8) +
        geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
        scale_x_continuous(labels = function(x) abs(x)) +
        labs(
          title = paste("Zorg per Domein:", input$butterfly_domein),
          subtitle = paste("Maatstaf:", input$butterfly_maatstaf),
          x = "Waarde (absolute schaal)", y = "Populatie / Doodsoorzaak",
          fill = "Groep"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.y = element_text(size = 10),
          plot.title = element_text(face = "bold")
        )
      
      plotly::ggplotly(p)
    }, error = function(e) {
      msg <- sprintf("[butterfly render] %s", e$message)
      add_error(msg)
      p <- ggplot() +
        geom_text(aes(0, 0, label = "Butterfly chart render error."), size = 5) +
        xlab(NULL) + ylab(NULL) + theme_void()
      plotly::ggplotly(p)
    })
  })
  
  output$dl_butterfly <- downloadHandler(
    filename = function() { paste("zorg_butterfly-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv2(data_butterfly(), file, row.names = FALSE) }
  )
  
  # ==========================================
  # SERVER LOGIC: TAB 7 - Logs
  # ==========================================
  output$app_log <- renderText({
    errors <- error_log()
    if (length(errors) > 0) paste("=== ERROR LOG ===\n", paste(errors, collapse = "\n"))
    else "=== NO ERRORS ===\nApp is running normally."
  })
}

# Run the app (Using your existing wrapper)
options(shiny.error = function() {
  err <- geterrmessage()
  message(sprintf("[shiny.error] %s", err))
  writeLines(sprintf("[shiny.error] %s", err), con = "shiny_error.log")
})

if (exists("secure_app", mode = "function") && exists("secure_server", mode = "function")) {
  shinyApp(ui = secure_app(ui), server = server)
} else {
  shinyApp(ui, server)
}