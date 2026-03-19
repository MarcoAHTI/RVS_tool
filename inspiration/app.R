library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(openxlsx)
library(RColorBrewer)
library(htmlwidgets)
library(webshot)
library(shinymanager)
library(ggplot2)
library(data.table)
library(stringr)

# Load geographic data
pv_sf <- st_read("data/geo/netherlands_provinces.geojson", quiet = TRUE)
gm_sf <- st_read("data/geo/netherlands_municipalities.geojson", quiet = TRUE)
wijk_sf <- st_read("data/geo/netherlands_wijken.geojson", quiet = TRUE)
buurt_sf <- st_read("data/geo/netherlands_buurten.geojson", quiet = TRUE)

# Load gemeente to province crosswalk from CSV
load_gemeente_province_mapping <- function() {
  # Try to find and load the latest CSV file
  csv_files <- list.files("data/gebieden_in_nederland/", pattern = "*.csv", full.names = TRUE)
  if (length(csv_files) == 0) {
    cat("[WARNING] No CSV files found in data/gebieden_in_nederland/\n")
    return(NULL)
  }
  
  # Use the most recent file (usually 2022)
  csv_file <- tail(sort(csv_files), 1)
  cat("[LOG] Loading crosswalk from:", csv_file, "\n")
  
  tryCatch({
    df <- read.csv(csv_file, sep = ";", encoding = "UTF-8")
    
    # Find the gemeente code and province code/name columns
    # The columns have specific names in the structure
    code_col <- "Codes.en.namen.van.gemeenten.Code..code."
    prov_code_col <- "Lokaliseringen.van.gemeenten.Provincies.Code..code."
    prov_name_col <- "Lokaliseringen.van.gemeenten.Provincies.Naam..naam."
    
    # Map column indices (0-indexed from Python perspective, but R is 1-indexed)
    # gemeente code: column 2 (index 1 in R)
    # province code: column 27 (index 26 in R) 
    # province name: column 28 (index 27 in R)
    if (ncol(df) < 28) {
      cat("[ERROR] CSV does not have enough columns\n")
      return(NULL)
    }
    
    cat("[LOG] Found columns - using gemeente code column 2, prov code column 27, prov name column 28\n")
    
    # Extract and clean the mapping using correct column indices
    mapping <- df[, c(2, 27, 28)]
    colnames(mapping) <- c("gm_code", "pv_code", "pv_name")
    
    # Clean gemeente codes (remove "GM" prefix and leading zeros)
    mapping$gm_code <- str_trim(mapping$gm_code)
    mapping$gm_code <- str_replace(mapping$gm_code, "^GM", "")
    mapping$gm_code <- str_trim(mapping$gm_code)
    mapping$gm_code <- as.character(as.numeric(mapping$gm_code))  # Remove leading zeros
    
    # Clean province codes and names
    mapping$pv_code <- str_trim(mapping$pv_code)
    mapping$pv_name <- str_trim(mapping$pv_name)
    
    # Remove duplicates
    mapping <- unique(mapping)
    
    cat("[LOG] Loaded", nrow(mapping), "gemeente-province mappings\n")
    return(mapping)
  }, error = function(e) {
    cat("[ERROR] Failed to load crosswalk:", e$message, "\n")
    return(NULL)
  })
}

gemeente_pv_mapping <- load_gemeente_province_mapping()

# Available years
available_years <- 2018:2023

# Helper function to read and prepare data
read_kraamzorg_data <- function(year) {
  file_path <- paste0("data/combined_", year, "_subset.xlsx")
  if (!file.exists(file_path)) {
    return(NULL)
  }
  read.xlsx(file_path, sheet = 1)
}

# Helper function to get available metrics from data
get_metrics <- function(df) {
  if (is.null(df)) return(NULL)
  # Exclude the first two columns (level and code)
  colnames(df)[-c(1, 2)]
}

# Shared color scale calculation for both map and PNG export
get_color_scale <- function(data) {
  values <- data$metric_value
  valid_values <- values[!is.na(values)]
  if (length(valid_values) == 0) return(NULL)
  list(
    valid_values = valid_values,
    min_val = min(valid_values, na.rm = TRUE),
    max_val = max(valid_values, na.rm = TRUE),
    mid_val = mean(valid_values, na.rm = TRUE)
  )
}

# UI
ui <- fluidPage(
  titlePanel("Kraamzorg Map Dashboard"),
  
  sidebarLayout(
    # Sidebar with controls
    sidebarPanel(
      h3("Select Parameters"),
      
      # Year selector
      selectInput(
        "year",
        "Year:",
        choices = available_years,
        selected = 2023
      ),
      
      # Metric selector (will be updated based on year)
      selectInput(
        "metric",
        "Metric:",
        choices = NULL
      ),
      
      # Region grouping selector
      selectInput(
        "region_type",
        "Region Grouping:",
        choices = c("provincie", "gemeente", "wijk", "buurt"),
        selected = "gemeente"
      ),
      
      # Download map button
      downloadButton("download_map", "Download Map as PNG"),
      
      width = 3
    ),
    
    # Main panel with map
    mainPanel(
      leafletOutput("map", height = "700px"),
      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression for the data
  kraamzorg_data <- reactive({
    cat("[LOG] Loading data for year:", input$year, "\n")
    df <- read_kraamzorg_data(input$year)
    cat("[LOG] Loaded", nrow(df), "rows and", ncol(df), "columns\n")
    return(df)
  })
  
  # Update metric choices based on selected year
  observe({
    metrics <- get_metrics(kraamzorg_data())
    updateSelectInput(session, "metric", choices = metrics)
  })
  
  # Reactive expression for geospatial data
  geo_data <- reactive({
    cat("[LOG] geo_data() called with region_type =", input$region_type, "\n")
    region_map <- list("provincie" = pv_sf, "gemeente" = gm_sf, "wijk" = wijk_sf, "buurt" = buurt_sf)
    cat("[LOG] Available regions in map:", paste(names(region_map), collapse=", "), "\n")
    result <- region_map[[input$region_type]]
    cat("[LOG] geo_data returning:", if(is.null(result)) "NULL" else paste(nrow(result), "rows"), "\n")
    return(result)
  })
  
  # Reactive expression for merged data
  merged_data <- reactive({
    cat("\n[LOG] ===== MERGED_DATA CALLED ===== \n")
    cat("[LOG] input$year =", input$year, "\n")
    cat("[LOG] input$metric =", input$metric, "\n")
    cat("[LOG] input$region_type =", input$region_type, "\n")
    
    df <- kraamzorg_data()
    cat("[LOG] Data loaded:", if(is.null(df)) "NULL" else paste(nrow(df), "rows"), "\n")
    if (is.null(df)) return(NULL)
    
    if (is.null(input$metric) || input$metric == "") {
      cat("[LOG] ERROR: metric is NULL or empty\n")
      return(NULL)
    }
    
    # Handle province level by aggregating gemeente data using crosswalk
    if (input$region_type == "provincie") {
      cat("[LOG] Province level requested - aggregating gemeente data using crosswalk\n")
      
      if (is.null(gemeente_pv_mapping)) {
        cat("[LOG] ERROR: gemeente-province mapping not available\n")
        return(NULL)
      }
      
      # Get gemeente-level data
      df_filtered <- df[df$level == "gem", c("code", input$metric)]
      cat("[LOG] Filtered gemeente data:", nrow(df_filtered), "rows\n")
      
      if (nrow(df_filtered) == 0) {
        cat("[LOG] ERROR: No gemeente rows after filtering\n")
        return(NULL)
      }
      
      # Clean gemeente codes in data
      df_filtered$code <- as.character(as.numeric(df_filtered$code))
      
      colnames(df_filtered)[2] <- "metric_value"
      
      # Merge with crosswalk to get province codes
      cat("[LOG] Merging gemeente data with province mapping\n")
      df_with_pv <- merge(df_filtered, gemeente_pv_mapping, by.x = "code", by.y = "gm_code", all.x = TRUE)
      cat("[LOG] After mapping merge:", nrow(df_with_pv), "rows\n")
      
      if (nrow(df_with_pv) == 0 || all(is.na(df_with_pv$pv_code))) {
        cat("[LOG] ERROR: Could not map any gemeenten to provinces\n")
        return(NULL)
      }
      
      # Aggregate by province code (mean of metric values)
      cat("[LOG] Aggregating to province level\n")
      aggregated <- df_with_pv %>%
        group_by(pv_code, pv_name) %>%
        summarise(
          metric_value = mean(metric_value, na.rm = TRUE),
          .groups = 'drop'
        )
      
      cat("[LOG] Aggregated to", nrow(aggregated), "provinces\n")
      
      # Get province geometry and merge with aggregated data
      geo_pv <- pv_sf
      geo_pv_df <- as.data.frame(geo_pv)
      
      cat("[LOG] Province geo data columns:", paste(head(colnames(geo_pv_df), 10), collapse=", "), "\n")
      
      # Merge on province name (the new GeoJSON has 'name' instead of 'code')
      merged_pv <- merge(geo_pv_df, aggregated, by.x = "name", by.y = "pv_name", all.x = TRUE)
      cat("[LOG] After geo merge:", nrow(merged_pv), "rows\n")
      
      # Convert back to sf
      cat("[LOG] Converting to sf...\n")
      result_sf <- st_as_sf(merged_pv, sf_column_name = "geometry")
      cat("[LOG] Final result: ", nrow(result_sf), " rows\n")
      
      return(result_sf)
    }
    
    # For other levels (gemeente, wijk, buurt) - standard processing
    level_map <- c("gemeente" = "gem", "wijk" = "wc", "buurt" = "bc")
    level_code <- level_map[input$region_type]
    cat("[LOG] Region:", input$region_type, "-> level code:", level_code, "\n")
    
    # Filter and prepare data
    cat("[LOG] Filtering df where level ==", level_code, "and selecting code and", input$metric, "\n")
    df_filtered <- df[df$level == level_code, c("code", input$metric)]
    cat("[LOG] Filtered result:", nrow(df_filtered), "rows\n")
    
    if (nrow(df_filtered) == 0) {
      cat("[LOG] ERROR: No rows after filtering\n")
      cat("[LOG] Available levels:", paste(unique(df$level), collapse=", "), "\n")
      return(NULL)
    }
    
    colnames(df_filtered)[2] <- "metric_value"
    cat("[LOG] Columns after rename:", paste(colnames(df_filtered), collapse=", "), "\n")
    
    # Get geospatial data
    geo <- geo_data()
    cat("[LOG] Geo data:", if(is.null(geo)) "NULL" else paste(nrow(geo), "rows"), "\n")
    if (is.null(geo)) return(NULL)
    
    # Convert to data frame (preserves geometry as list column)
    geo_df <- as.data.frame(geo)
    cat("[LOG] Geo converted to data.frame:", nrow(geo_df), "rows\n")
    
    # Merge using base R merge (all.x = TRUE keeps all geometries)
    cat("[LOG] Merging by 'code'...\n")
    merged <- merge(geo_df, df_filtered, by = "code", all.x = TRUE)
    cat("[LOG] After merge:", nrow(merged), "rows\n")
    cat("[LOG] Merged columns:", paste(head(colnames(merged), 10), collapse=", "), "...\n")
    
    # Convert back to sf object, explicitly specifying geometry column
    cat("[LOG] Converting back to sf...\n")
    tryCatch({
      merged_sf <- st_as_sf(merged, sf_column_name = "geometry")
      cat("[LOG] SF conversion success! Class:", class(merged_sf), "\n")
    }, error = function(e) {
      cat("[LOG] ERROR in st_as_sf:", e$message, "\n")
      return(NULL)
    })
    
    cat("[LOG] Final merged_data:", nrow(merged_sf), "rows\n")
    cat("[LOG] ===== MERGED_DATA END =====\n\n")
    return(merged_sf)
  })
  
  # Render map
  output$map <- renderLeaflet({
    cat("\n[LOG] ===== RENDERLEAFLET CALLED ===== \n")
    data <- merged_data()
    cat("[LOG] Merged data result:", if(is.null(data)) "NULL" else paste(nrow(data), "rows"), "\n")
    
    # Default map if no data
    if (is.null(data) || nrow(data) == 0) {
      cat("[LOG] Returning empty map\n")
      return(
        leaflet() %>%
          setView(lng = 5.2, lat = 52.1, zoom = 7) %>%
          htmlwidgets::onRender("function(el, x) { el.style.backgroundColor = 'white'; }")
      )
    }
    
    # Check for metric_value column
    if (!("metric_value" %in% colnames(data))) {
      cat("[LOG] ERROR: metric_value column not found\n")
      cat("[LOG] Columns:", paste(colnames(data), collapse=", "), "\n")
      return(
        leaflet() %>%
          setView(lng = 5.2, lat = 52.1, zoom = 7) %>%
          htmlwidgets::onRender("function(el, x) { el.style.backgroundColor = 'white'; }")
      )
    }
    cat("[LOG] metric_value column found\n")
    # Get color scale info
    scale_info <- get_color_scale(data)
    if (is.null(scale_info)) {
      cat("[LOG] No valid values - returning empty map\n")
      return(
        leaflet() %>%
          setView(lng = 5.2, lat = 52.1, zoom = 7) %>%
          htmlwidgets::onRender("function(el, x) { el.style.backgroundColor = 'white'; }")
      )
    }
    valid_values <- scale_info$valid_values
    min_val <- scale_info$min_val
    max_val <- scale_info$max_val
    mid_val <- scale_info$mid_val
    cat("[LOG] Creating color palette...\n")
    pal <- colorNumeric(
      palette = c("#0571b0", "#ffffff", "#ca0020"),  # Blue-White-Red scale
      domain = valid_values,
      na.color = "#cccccc"
    )
    cat("[LOG] Creating labels...\n")
    name_col <- switch(input$region_type,
      "provincie" = "name",
      "gemeente" = "gemeentenaam",
      "wijk" = "wijknaam",
      "buurt" = "buurtnaam",
      "name" # fallback
    )
    region_names <- if (name_col %in% colnames(data)) data[[name_col]] else data$code
    labels <- paste0(
      "<b>Naam:</b> ", region_names, "<br>",
      "<b>", input$metric, ":</b> ",
      ifelse(is.na(data$metric_value), "No data", round(data$metric_value, 2))
    )
    cat("[LOG] Building leaflet map with", nrow(data), "polygons...\n")
    result <- leaflet(data = data, options = leafletOptions(zoomControl = TRUE, dragging = TRUE)) %>%
      addPolygons(
        fillColor = ~pal(metric_value),
        fillOpacity = 0.7,
        color = "#333333",
        weight = 1,
        label = lapply(labels, HTML),
        popup = lapply(labels, HTML)
      ) %>%
      addLegend(
        pal = pal,
        values = ~metric_value,
        title = input$metric,
        position = "bottomright"
      ) %>%
      setView(lng = 5.2, lat = 52.1, zoom = 7) %>%
      htmlwidgets::onRender("function(el, x) { el.style.backgroundColor = 'white'; }")
    cat("[LOG] Map created successfully\n")
    cat("[LOG] ===== RENDERLEAFLET END =====\n\n")
    result
  })
  
  # Download handler for map (as PNG)
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("kraamzorg_map_", input$year, "_", input$region_type, ".png")
    },
    content = function(file) {
      tryCatch({
        data <- merged_data()
        # Use the same color scale as the interactive map for consistency
        scale_info <- get_color_scale(data)
        if (is.null(scale_info)) {
          showNotification("No valid values to export.", type = "error", duration = 7)
          stop("No valid values")
        }
        min_val <- scale_info$min_val
        max_val <- scale_info$max_val
        mid_val <- scale_info$mid_val
        map_plot <- ggplot(data) +
          geom_sf(aes(fill = metric_value), color = "#333333", size = 0.2) +
          scale_fill_gradientn(
              colors = c("#0571b0", "#ffffff", "#ca0020"), # Matches Leaflet palette
              limits = c(min_val, max_val),
              na.value = "#cccccc",
              name = input$metric
            ) +
          labs(
            title = paste0("Kraamzorg Map - ", input$region_type, " (Year ", input$year, ")"),
            subtitle = paste("Metric:", input$metric)
          ) +
          theme_void() +
          theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            legend.position = "right",
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          )
        # Save with high resolution
        ggsave(file, plot = map_plot, device = "png", width = 12, height = 9, dpi = 300, bg = "white")
      }, error = function(e) {
        showNotification(paste("Error exporting map:", e$message), type = "error", duration = 10)
        stop(e)
      })
    }
  )
}

# Run the app with login
shinyApp(
  ui = secure_app(ui),
  server = function(input, output, session) {
    res_auth <- secure_server(
      check_credentials = check_credentials(
        data.frame(
          user = c("ahti"),
          password = c("user123"),
          stringsAsFactors = FALSE
        )
      )
    )
    server(input, output, session)
  }
)
