# app.R
# Simple Shiny dashboard for dummy data in data/dummy_data_iteration_1/all_output.xlsx
# - var choices from name column without the "gebruikt" prefix
# - matched partner name with "gebruikt" prefix (if exists), otherwise NA

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

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

read_all_data <- function(path = data_path) {
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

all_data <- read_all_data(data_path)

base_names <- sort(unique(all_data$name[!startsWith(all_data$name, "gebruikt")]))

find_gebruikt_name <- function(name_choice, data = all_data) {
  candidate1 <- paste0("gebruikt", name_choice)
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
      )
    )
  )
)

server <- function(input, output, session) {
  selected_data <- reactive({
    all_data %>% filter(name == input$selected_var)
  })

  plot_cost_data <- reactive({
    df <- selected_data() %>%
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
  })

  plot_usage_data <- reactive({
    usage_name <- find_gebruikt_name(input$selected_var)
    if (is.na(usage_name)) {
      return(tibble::tibble())
    }

    all_data %>%
      filter(name == usage_name, type == "gemiddelde_per_persoon") %>%
      group_by(cohort, died, t) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(t = factor(as.numeric(t), levels = sort(unique(as.numeric(t)))))
  })

  if (plotly_installed) {
    output$plot_cost <- plotly::renderPlotly({
      df <- plot_cost_data()

      if (nrow(df) == 0) {
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen kosten-data beschikbaar voor gekozen variabele (geen quantielen)."), size = 5) +
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
          x = "t", y = "Kosten (per persoon, type quantielen)",
          color = "Died", fill = "Died"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      plotly::ggplotly(p)
    })
  } else {
    output$plot_cost <- renderPlot({
      df <- plot_cost_data()

      if (nrow(df) == 0) {
        plot.new(); text(0.5, 0.5, "Geen kosten-data beschikbaar voor gekozen variabele (geen quantielen).", cex = 1.2)
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
          x = "t", y = "Kosten (per persoon, type quantielen)",
          color = "Died", fill = "Died"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  }

  if (plotly_installed) {
    output$plot_usage <- plotly::renderPlotly({
      df <- plot_usage_data()
      if (nrow(df) == 0) {
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen gebruiksdata beschikbaar voor dit variabele."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }

      p <- ggplot(df, aes(x = factor(t), y = value, fill = died)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        facet_wrap(~cohort, nrow = 1) +
        labs(
          title = paste("Gebruikt gemiddeld per persoon voor", input$selected_var),
          x = "t", y = "gemiddelde_per_persoon", fill = "Died"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      plotly::ggplotly(p)
    })
  } else {
    output$plot_usage <- renderPlot({
      df <- plot_usage_data()
      if (nrow(df) == 0) {
        plot.new(); text(0.5, 0.5, "Geen gebruiksdata beschikbaar voor dit variabele.", cex = 1.2)
        return()
      }

      ggplot(df, aes(x = factor(t), y = value, fill = died)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        facet_wrap(~cohort, nrow = 1) +
        labs(
          title = paste("Gebruikt gemiddeld per persoon voor", input$selected_var),
          x = "t", y = "gemiddelde_per_persoon", fill = "Died"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  }
}

shinyApp(ui, server)
