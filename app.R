# ==============================================================================
# IVERMECTIN MDA COVERAGE DASHBOARD
# Southern Malawi Coverage Survey
# Spatial cluster map + click popups + coverage range slider
# ==============================================================================


# ==============================================================================
# 1. LOAD PACKAGES
# ==============================================================================


library(rsconnect)
library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(stringr)
library(forcats)
library(DT)
library(bslib)
library(leaflet)
library(htmltools)


#connect to online account 


model_data <- read_csv("model_data.csv")

# IMPORTANT: shinyapps.io credentials are not stored in this script.
# Configure deployment credentials locally using rsconnect::setAccountInfo()
# or the Posit/shinyapps.io account setup workflow before deploying.

# ==============================================================================
# 2. PREPARE INDIVIDUAL-LEVEL DATA
# ==============================================================================

dashboard_data <- model_data %>%
  mutate(
    
    district = as.character(district),
    healthcenter = as.character(healthcenter),
    village = as.character(village),
    cluster_id = as.character(cluster_id),
    
    # --------------------------------------------------------------------------
    # Receipt variable
    # --------------------------------------------------------------------------
    
    received_num = case_when(
      received %in% c(
        1, "1",
        "Yes", "yes", "YES",
        TRUE
      ) ~ 1,
      
      received %in% c(
        0, "0",
        "No", "no", "NO",
        FALSE
      ) ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    # --------------------------------------------------------------------------
    # Treatment / swallowing variable
    # --------------------------------------------------------------------------
    
    treated_num = case_when(
      treated %in% c(
        1, "1",
        "Yes", "yes", "YES",
        TRUE
      ) ~ 1,
      
      treated %in% c(
        0, "0",
        "No", "no", "NO",
        FALSE
      ) ~ 0,
      
      TRUE ~ NA_real_
    )
  )


# ==============================================================================
# 3. CREATE ONE RECORD PER CLUSTER
# ==============================================================================

cluster_map_data <- dashboard_data %>%
  
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(cluster_id)
  ) %>%
  
  group_by(
    district,
    healthcenter,
    cluster_id
  ) %>%
  
  summarise(
    
    # --------------------------------------------------------------------------
    # Village names
    # --------------------------------------------------------------------------
    
    village = paste(
      sort(
        unique(
          na.omit(village)
        )
      ),
      collapse = ", "
    ),
    
    # --------------------------------------------------------------------------
    # Cluster coordinates
    # --------------------------------------------------------------------------
    
    latitude = median(
      latitude,
      na.rm = TRUE
    ),
    
    longitude = median(
      longitude,
      na.rm = TRUE
    ),
    
    # --------------------------------------------------------------------------
    # Sample size
    # --------------------------------------------------------------------------
    
    participants = n(),
    
    # --------------------------------------------------------------------------
    # Treatment counts
    # --------------------------------------------------------------------------
    
    treated_n = sum(
      treated_num == 1,
      na.rm = TRUE
    ),
    
    non_treated_n = sum(
      treated_num == 0,
      na.rm = TRUE
    ),
    
    # --------------------------------------------------------------------------
    # Receipt counts
    # --------------------------------------------------------------------------
    
    received_n = sum(
      received_num == 1,
      na.rm = TRUE
    ),
    
    nonrecipient_n = sum(
      received_num == 0,
      na.rm = TRUE
    ),
    
    # --------------------------------------------------------------------------
    # Uptake
    # --------------------------------------------------------------------------
    
    uptake = mean(
      treated_num,
      na.rm = TRUE
    ) * 100,
    
    # --------------------------------------------------------------------------
    # Receipt coverage
    # --------------------------------------------------------------------------
    
    receipt = mean(
      received_num,
      na.rm = TRUE
    ) * 100,
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    uptake = round(
      uptake,
      1
    ),
    
    receipt = round(
      receipt,
      1
    ),
    
    # --------------------------------------------------------------------------
    # Coverage category used for map colours
    # --------------------------------------------------------------------------
    
    coverage_category = case_when(
      
      uptake < 60 ~ "<60%",
      
      uptake >= 60 &
        uptake < 70 ~ "60–69.9%",
      
      uptake >= 70 &
        uptake < 75 ~ "70–74.9%",
      
      uptake >= 75 &
        uptake < 80 ~ "75–79.9%",
      
      uptake >= 80 ~ "≥80%",
      
      TRUE ~ "Missing"
    ),
    
    coverage_category = factor(
      coverage_category,
      levels = c(
        "<60%",
        "60–69.9%",
        "70–74.9%",
        "75–79.9%",
        "≥80%",
        "Missing"
      )
    )
  )


# ==============================================================================
# 4. ATTACH CLUSTER COVERAGE TO INDIVIDUAL DATA
# ==============================================================================

dashboard_data <- dashboard_data %>%
  
  left_join(
    
    cluster_map_data %>%
      
      select(
        district,
        healthcenter,
        cluster_id,
        cluster_uptake = uptake,
        coverage_category
      ),
    
    by = c(
      "district",
      "healthcenter",
      "cluster_id"
    )
  )


# ==============================================================================
# 5. USER INTERFACE
# ==============================================================================

ui <- page_sidebar(
  
  title = "Ivermectin MDA Coverage Malawi",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  # =============================================================================
  # SIDEBAR
  # =============================================================================
  
  sidebar = sidebar(
    
    width = 300,
    
    h4(
      "Filters"
    ),
    
    # --------------------------------------------------------------------------
    # District
    # --------------------------------------------------------------------------
    
    selectInput(
      "district",
      "District",
      choices = c(
        "All districts",
        sort(
          unique(
            na.omit(
              dashboard_data$district
            )
          )
        )
      ),
      selected = "All districts"
    ),
    
    # --------------------------------------------------------------------------
    # Health centre
    # --------------------------------------------------------------------------
    
    selectInput(
      "healthcenter",
      "Health centre",
      choices = "All health centres"
    ),
    
    # --------------------------------------------------------------------------
    # Cluster
    # --------------------------------------------------------------------------
    
    selectInput(
      "cluster",
      "Cluster",
      choices = "All clusters"
    ),
    
    # --------------------------------------------------------------------------
    # COVERAGE RANGE SLIDER
    # --------------------------------------------------------------------------
    
    sliderInput(
      "coverage_range",
      "Cluster uptake coverage (%)",
      min = 0,
      max = 100,
      value = c(
        0,
        100
      ),
      step = 1,
      post = "%",
      sep = ""
    ),
    
    # --------------------------------------------------------------------------
    # Village
    # --------------------------------------------------------------------------
    
    selectInput(
      "village",
      "Village",
      choices = "All villages"
    ),
    
    hr(),
    
    # --------------------------------------------------------------------------
    # Reasons displayed
    # --------------------------------------------------------------------------
    
    radioButtons(
      "reason_display",
      "Reasons to display",
      choices = c(
        "Top 10" = 10,
        "All reasons" = 100
      ),
      selected = 10
    ),
    
    hr(),
    
    actionButton(
      "reset",
      "Reset filters",
      class = "btn-secondary"
    )
  ),
  
  
  # =============================================================================
  # KPI CARDS
  # =============================================================================
  
  layout_column_wrap(
    
    width = 1 / 4,
    
    value_box(
      title = "Participants",
      value = textOutput(
        "n_participants"
      ),
      showcase = "N"
    ),
    
    value_box(
      title = "Swallowed ivermectin",
      value = textOutput(
        "uptake"
      ),
      showcase = "%"
    ),
    
    value_box(
      title = "Received ivermectin",
      value = textOutput(
        "receipt"
      ),
      showcase = "%"
    ),
    
    value_box(
      title = "Non-recipients",
      value = textOutput(
        "nonrecipients"
      ),
      showcase = "N"
    )
  ),
  
  br(),
  
  # =============================================================================
  # TABS
  # =============================================================================
  
  navset_card_tab(
    
    # ==========================================================================
    # TAB 1: MAP
    # ==========================================================================
    
    nav_panel(
      
      "Cluster map",
      
      br(),
      
      h4(
        "Spatial distribution of ivermectin uptake"
      ),
      
      p(
        paste(
          "Hover over a cluster for a quick summary.")
      ),
      
      leafletOutput(
        "cluster_map",
        height = "700px"
      )
    ),
    
    # ==========================================================================
    # TAB 2: REASONS
    # ==========================================================================
    
    nav_panel(
      
      "Reasons for non-receipt",
      
      br(),
      
      h4(
        textOutput(
          "plot_location",
          inline = TRUE
        )
      ),
      
      plotOutput(
        "reason_plot",
        height = "620px"
      )
    ),
    
    # ==========================================================================
    # TAB 3: COVERAGE CATEGORY DISTRIBUTION
    # ==========================================================================
    
    nav_panel(
      
      "Coverage categories",
      
      br(),
      
      h4(
        "Selected clusters by ivermectin uptake category"
      ),
      
      p(
        "Bars show the number of unique clusters remaining after the current filters are applied."
      ),
      
      plotOutput(
        "coverage_category_plot",
        height = "620px"
      )
    ),
    
    # ==========================================================================
    # TAB 4: CLUSTER SUMMARY
    # ==========================================================================
    
    nav_panel(
      
      "Cluster summary",
      
      br(),
      
      DTOutput(
        "cluster_table"
      )
    )
  )
)


# ==============================================================================
# 6. SERVER
# ==============================================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # =============================================================================
  # HIGHLIGHT SELECTED CLUSTER ON MAP
  # =============================================================================
  
  # Draws (or clears) a halo ring around the cluster chosen in the
  # "Cluster" dropdown, so it stands out from its neighbours.
  
  update_cluster_highlight <- function() {
    
    proxy <- leafletProxy(
      "cluster_map"
    )
    
    
    proxy %>%
      removeMarker(
        layerId = "highlighted_cluster"
      )
    
    
    if (
      is.null(
        input$cluster
      ) ||
      input$cluster ==
      "All clusters"
    ) {
      
      return(
        invisible(
          NULL
        )
      )
    }
    
    
    selected_cluster <-
      cluster_map_data %>%
      
      filter(
        cluster_id ==
          input$cluster
      )
    
    
    if (
      !is.null(
        input$district
      ) &&
      input$district !=
      "All districts"
    ) {
      
      selected_cluster <-
        selected_cluster %>%
        
        filter(
          district ==
            input$district
        )
    }
    
    
    if (
      !is.null(
        input$healthcenter
      ) &&
      input$healthcenter !=
      "All health centres"
    ) {
      
      selected_cluster <-
        selected_cluster %>%
        
        filter(
          healthcenter ==
            input$healthcenter
        )
    }
    
    
    if (
      !is.null(
        input$coverage_range
      )
    ) {
      
      selected_cluster <-
        selected_cluster %>%
        
        filter(
          uptake >=
            input$coverage_range[1],
          uptake <=
            input$coverage_range[2]
        )
    }
    
    
    if (
      nrow(
        selected_cluster
      ) == 0
    ) {
      
      return(
        invisible(
          NULL
        )
      )
    }
    
    
    selected_cluster <-
      selected_cluster %>%
      slice(1)
    
    
    proxy %>%
      
      addCircleMarkers(
        
        data = selected_cluster,
        
        lng = ~longitude,
        
        lat = ~latitude,
        
        layerId = "highlighted_cluster",
        
        radius = ~pmax(
          6,
          pmin(
            12,
            sqrt(
              participants
            )
          )
        ) + 7,
        
        color = "#0033ff",
        
        weight = 4,
        
        opacity = 1,
        
        fillColor = "#0033ff",
        
        fillOpacity = 0,
        
        options =
          pathOptions(
            interactive = FALSE
          )
      )
  }
  
  
  # =============================================================================
  # DISTRICT -> HEALTH CENTRE
  # =============================================================================
  
  observeEvent(
    input$district,
    {
      
      x <- dashboard_data
      
      if (
        !is.null(
          input$district
        ) &&
        input$district !=
        "All districts"
      ) {
        
        x <- x %>%
          filter(
            district ==
              input$district
          )
      }
      
      
      hc <- sort(
        unique(
          na.omit(
            x$healthcenter
          )
        )
      )
      
      
      current_hc <- isolate(
        input$healthcenter
      )
      
      
      selected_hc <- if (
        !is.null(current_hc) &&
        current_hc %in% hc
      ) {
        
        current_hc
        
      } else {
        
        "All health centres"
      }
      
      
      updateSelectInput(
        session,
        "healthcenter",
        choices = c(
          "All health centres",
          hc
        ),
        selected =
          selected_hc
      )
    },
    ignoreInit = FALSE
  )
  
  
  # =============================================================================
  # UPDATE CLUSTER DROPDOWN
  # =============================================================================
  
  observeEvent(
    list(
      input$district,
      input$healthcenter,
      input$coverage_range
    ),
    {
      
      x <- cluster_map_data
      
      
      # District
      if (
        !is.null(
          input$district
        ) &&
        input$district !=
        "All districts"
      ) {
        
        x <- x %>%
          filter(
            district ==
              input$district
          )
      }
      
      
      # Health centre
      if (
        !is.null(
          input$healthcenter
        ) &&
        input$healthcenter !=
        "All health centres"
      ) {
        
        x <- x %>%
          filter(
            healthcenter ==
              input$healthcenter
          )
      }
      
      
      # Coverage slider
      if (
        !is.null(
          input$coverage_range
        )
      ) {
        
        x <- x %>%
          filter(
            uptake >=
              input$coverage_range[1],
            uptake <=
              input$coverage_range[2]
          )
      }
      
      
      cl <- sort(
        unique(
          na.omit(
            x$cluster_id
          )
        )
      )
      
      
      current_cluster <- isolate(
        input$cluster
      )
      
      
      selected_cluster <- if (
        !is.null(
          current_cluster
        ) &&
        current_cluster %in%
        cl
      ) {
        
        current_cluster
        
      } else {
        
        "All clusters"
      }
      
      
      updateSelectInput(
        session,
        "cluster",
        choices = c(
          "All clusters",
          cl
        ),
        selected =
          selected_cluster
      )
    },
    ignoreInit = FALSE
  )
  
  
  # =============================================================================
  # UPDATE VILLAGE DROPDOWN
  # =============================================================================
  
  observeEvent(
    list(
      input$district,
      input$healthcenter,
      input$coverage_range,
      input$cluster
    ),
    {
      
      x <- dashboard_data
      
      
      # District
      if (
        !is.null(
          input$district
        ) &&
        input$district !=
        "All districts"
      ) {
        
        x <- x %>%
          filter(
            district ==
              input$district
          )
      }
      
      
      # Health centre
      if (
        !is.null(
          input$healthcenter
        ) &&
        input$healthcenter !=
        "All health centres"
      ) {
        
        x <- x %>%
          filter(
            healthcenter ==
              input$healthcenter
          )
      }
      
      
      # Coverage
      if (
        !is.null(
          input$coverage_range
        )
      ) {
        
        x <- x %>%
          filter(
            cluster_uptake >=
              input$coverage_range[1],
            cluster_uptake <=
              input$coverage_range[2]
          )
      }
      
      
      # Cluster
      if (
        !is.null(
          input$cluster
        ) &&
        input$cluster !=
        "All clusters"
      ) {
        
        x <- x %>%
          filter(
            cluster_id ==
              input$cluster
          )
      }
      
      
      vil <- sort(
        unique(
          na.omit(
            x$village
          )
        )
      )
      
      
      updateSelectInput(
        session,
        "village",
        choices = c(
          "All villages",
          vil
        ),
        selected =
          "All villages"
      )
    },
    ignoreInit = FALSE
  )
  
  
  # =============================================================================
  # FILTERED INDIVIDUAL DATA
  # =============================================================================
  
  filtered_data <- reactive({
    
    x <- dashboard_data
    
    
    # District
    if (
      !is.null(
        input$district
      ) &&
      input$district !=
      "All districts"
    ) {
      
      x <- x %>%
        filter(
          district ==
            input$district
        )
    }
    
    
    # Health centre
    if (
      !is.null(
        input$healthcenter
      ) &&
      input$healthcenter !=
      "All health centres"
    ) {
      
      x <- x %>%
        filter(
          healthcenter ==
            input$healthcenter
        )
    }
    
    
    # Cluster
    if (
      !is.null(
        input$cluster
      ) &&
      input$cluster !=
      "All clusters"
    ) {
      
      x <- x %>%
        filter(
          cluster_id ==
            input$cluster
        )
    }
    
    
    # Coverage range
    if (
      !is.null(
        input$coverage_range
      )
    ) {
      
      x <- x %>%
        filter(
          cluster_uptake >=
            input$coverage_range[1],
          cluster_uptake <=
            input$coverage_range[2]
        )
    }
    
    
    # Village
    if (
      !is.null(
        input$village
      ) &&
      input$village !=
      "All villages"
    ) {
      
      x <- x %>%
        filter(
          village ==
            input$village
        )
    }
    
    
    x
  })
  
  
  # =============================================================================
  # FILTERED MAP DATA
  # =============================================================================
  
  filtered_map_data <- reactive({
    
    x <- cluster_map_data
    
    
    # District
    if (
      !is.null(
        input$district
      ) &&
      input$district !=
      "All districts"
    ) {
      
      x <- x %>%
        filter(
          district ==
            input$district
        )
    }
    
    
    # Health centre
    if (
      !is.null(
        input$healthcenter
      ) &&
      input$healthcenter !=
      "All health centres"
    ) {
      
      x <- x %>%
        filter(
          healthcenter ==
            input$healthcenter
        )
    }
    
    
    # Coverage slider
    if (
      !is.null(
        input$coverage_range
      )
    ) {
      
      x <- x %>%
        filter(
          uptake >=
            input$coverage_range[1],
          uptake <=
            input$coverage_range[2]
        )
    }
    
    
    x
  })
  
  
  # =============================================================================
  # KPI: PARTICIPANTS
  # =============================================================================
  
  output$n_participants <- renderText({
    
    format(
      nrow(
        filtered_data()
      ),
      big.mark = ","
    )
  })
  
  
  # =============================================================================
  # KPI: UPTAKE
  # =============================================================================
  
  output$uptake <- renderText({
    
    x <- filtered_data()
    
    
    if (
      nrow(x) == 0 ||
      all(
        is.na(
          x$treated_num
        )
      )
    ) {
      
      return(
        "NA"
      )
    }
    
    
    percent(
      mean(
        x$treated_num,
        na.rm = TRUE
      ),
      accuracy = 0.1
    )
  })
  
  
  # =============================================================================
  # KPI: RECEIPT
  # =============================================================================
  
  output$receipt <- renderText({
    
    x <- filtered_data()
    
    
    if (
      nrow(x) == 0 ||
      all(
        is.na(
          x$received_num
        )
      )
    ) {
      
      return(
        "NA"
      )
    }
    
    
    percent(
      mean(
        x$received_num,
        na.rm = TRUE
      ),
      accuracy = 0.1
    )
  })
  
  
  # =============================================================================
  # KPI: NON-RECIPIENTS
  # =============================================================================
  
  output$nonrecipients <- renderText({
    
    x <- filtered_data()
    
    
    n <- sum(
      x$received_num == 0,
      na.rm = TRUE
    )
    
    
    format(
      n,
      big.mark = ","
    )
  })
  
  
  # =============================================================================
  # FILTER DESCRIPTION
  # =============================================================================
  
  output$plot_location <- renderText({
    
    pieces <- character()
    
    
    # District
    if (
      !is.null(
        input$district
      ) &&
      input$district !=
      "All districts"
    ) {
      
      pieces <- c(
        pieces,
        paste0(
          "District: ",
          input$district
        )
      )
    }
    
    
    # Health centre
    if (
      !is.null(
        input$healthcenter
      ) &&
      input$healthcenter !=
      "All health centres"
    ) {
      
      pieces <- c(
        pieces,
        paste0(
          "Health centre: ",
          input$healthcenter
        )
      )
    }
    
    
    # Cluster
    if (
      !is.null(
        input$cluster
      ) &&
      input$cluster !=
      "All clusters"
    ) {
      
      pieces <- c(
        pieces,
        paste0(
          "Cluster: ",
          input$cluster
        )
      )
    }
    
    
    # Coverage range
    if (
      !is.null(
        input$coverage_range
      ) &&
      (
        input$coverage_range[1] > 0 ||
        input$coverage_range[2] < 100
      )
    ) {
      
      pieces <- c(
        pieces,
        paste0(
          "Cluster uptake: ",
          input$coverage_range[1],
          "–",
          input$coverage_range[2],
          "%"
        )
      )
    }
    
    
    # Village
    if (
      !is.null(
        input$village
      ) &&
      input$village !=
      "All villages"
    ) {
      
      pieces <- c(
        pieces,
        paste0(
          "Village: ",
          input$village
        )
      )
    }
    
    
    if (
      length(
        pieces
      ) == 0
    ) {
      
      return(
        "All survey communities"
      )
    }
    
    
    paste(
      pieces,
      collapse = " | "
    )
  })
  
  
  # =============================================================================
  # REASONS FOR NON-RECEIPT
  # =============================================================================
  
  reason_data <- reactive({
    
    x <- filtered_data() %>%
      
      filter(
        received_num == 0
      ) %>%
      
      filter(
        !is.na(
          received_not_why
        ),
        received_not_why != ""
      )
    
    
    denominator <- nrow(
      x
    )
    
    
    if (
      denominator == 0
    ) {
      
      return(
        data.frame(
          reason = character(),
          n = integer(),
          pct = numeric()
        )
      )
    }
    
    
    x %>%
      
      count(
        received_not_why,
        name = "n"
      ) %>%
      
      mutate(
        
        pct =
          100 *
          n /
          denominator,
        
        reason = str_replace_all(
          received_not_why,
          "_",
          " "
        ),
        
        reason = str_squish(
          reason
        ),
        
        reason = str_to_sentence(
          reason
        )
      ) %>%
      
      arrange(
        desc(n)
      ) %>%
      
      slice_head(
        n =
          as.numeric(
            input$reason_display
          )
      )
  })
  
  
  # =============================================================================
  # REASONS BAR GRAPH
  # =============================================================================
  
  output$reason_plot <- renderPlot({
    
    dat <- reason_data()
    
    
    validate(
      need(
        nrow(dat) > 0,
        paste(
          "No non-recipients with a recorded reason",
          "were found for this selection."
        )
      )
    )
    
    
    n_nonrecipient <- sum(
      filtered_data()$received_num == 0,
      na.rm = TRUE
    )
    
    
    ggplot(
      dat,
      aes(
        x = pct,
        y = fct_reorder(
          reason,
          pct
        )
      )
    ) +
      
      geom_col(
        fill = "#1f3cff",
        width = 0.88
      ) +
      
      geom_text(
        aes(
          label =
            sprintf(
              "%.1f%%",
              pct
            )
        ),
        hjust = -0.10,
        size = 4.2
      ) +
      
      scale_x_continuous(
        labels =
          function(x) {
            paste0(
              x,
              "%"
            )
          },
        expand =
          expansion(
            mult = c(
              0,
              0.13
            )
          )
      ) +
      
      labs(
        title = paste0(
          "Leading reported reasons for not receiving ivermectin ",
          "(n = ",
          comma(
            n_nonrecipient
          ),
          ")"
        ),
        x =
          "Percentage of non-recipients",
        y = NULL
      ) +
      
      theme_minimal(
        base_size = 14
      ) +
      
      theme(
        plot.title =
          element_text(
            size = 18,
            face = "bold"
          ),
        axis.text.y =
          element_text(
            size = 12
          ),
        panel.grid.major.y =
          element_blank(),
        panel.grid.minor =
          element_blank(),
        plot.margin =
          margin(
            15,
            40,
            15,
            10
          )
      )
  })
  
  
  # =============================================================================
  # INITIAL MAP
  # =============================================================================
  
  output$cluster_map <- renderLeaflet({
    
    pal <- colorFactor(
      palette = c(
        "#d73027",
        "#fc8d59",
        "#fee08b",
        "#91cf60",
        "#1a9850",
        "#bdbdbd"
      ),
      domain =
        levels(
          cluster_map_data$coverage_category
        ),
      ordered = TRUE
    )
    
    
    leaflet(
      cluster_map_data
    ) %>%
      
      addProviderTiles(
        providers$Esri.WorldGrayCanvas,
        options = providerTileOptions(
          minZoom = 5,
          maxZoom = 16
        )
      ) %>%
      
      addCircleMarkers(
        
        lng = ~longitude,
        
        lat = ~latitude,
        
        layerId = ~cluster_id,
        
        radius = ~pmax(
          6,
          pmin(
            12,
            sqrt(
              participants
            )
          )
        ),
        
        color = "#333333",
        
        weight = 1,
        
        fillColor =
          ~pal(
            coverage_category
          ),
        
        fillOpacity = 0.90,
        
        
        # ======================================================================
        # CLICK POPUP
        # ======================================================================
        
        popup = ~paste0(
          
          "<div style='min-width:270px;
                      font-family:Arial, sans-serif;'>",
          
          "<div style='font-size:19px;
                      font-weight:bold;
                      margin-bottom:8px;'>",
          
          "Cluster ",
          cluster_id,
          
          "</div>",
          
          "<b>District:</b> ",
          district,
          "<br>",
          
          "<b>Health centre:</b> ",
          healthcenter,
          "<br>",
          
          "<b>Village:</b> ",
          village,
          "<br><br>",
          
          "<b>Participants:</b> ",
          participants,
          "<br>",
          
          "<b>Received ivermectin:</b> ",
          received_n,
          "<br>",
          
          "<b>Did not receive:</b> ",
          nonrecipient_n,
          "<br>",
          
          "<b>Swallowed ivermectin:</b> ",
          treated_n,
          "<br>",
          
          "<b>Did not swallow:</b> ",
          non_treated_n,
          "<br><br>",
          
          "<b>Receipt coverage:</b> ",
          sprintf(
            "%.1f%%",
            receipt
          ),
          "<br>",
          
          "<b>Uptake:</b> ",
          sprintf(
            "%.1f%%",
            uptake
          ),
          "<br>",
          
          "<b>Coverage category:</b> ",
          coverage_category,
          
          "</div>"
        ),
        
        
        # ======================================================================
        # HOVER LABEL
        # ======================================================================
        
        label = ~paste0(
          "Cluster ",
          cluster_id,
          " | ",
          district,
          " | Uptake: ",
          sprintf(
            "%.1f%%",
            uptake
          )
        ),
        
        labelOptions =
          labelOptions(
            noHide = FALSE,
            direction = "auto",
            opacity = 0.95,
            textsize = "13px"
          )
      ) %>%
      
      addLegend(
        position =
          "bottomright",
        colors = c(
          "#d73027",
          "#fc8d59",
          "#fee08b",
          "#91cf60",
          "#1a9850"
        ),
        labels = c(
          "<60%",
          "60–69.9%",
          "70–74.9%",
          "75–79.9%",
          "≥80%"
        ),
        title =
          "Ivermectin uptake",
        opacity = 1
      )
  })
  
  
  # =============================================================================
  # CLICK MAP CLUSTER -> UPDATE FILTERS
  # =============================================================================
  
  observeEvent(
    input$cluster_map_marker_click,
    {
      
      click <-
        input$cluster_map_marker_click
      
      
      req(
        click$id
      )
      
      
      selected_cluster <-
        cluster_map_data %>%
        
        filter(
          cluster_id ==
            as.character(
              click$id
            )
        )
      
      
      # ------------------------------------------------------------------------
      # In case cluster IDs are duplicated, use clicked coordinates
      # ------------------------------------------------------------------------
      
      if (
        nrow(
          selected_cluster
        ) > 1
      ) {
        
        selected_cluster <-
          selected_cluster %>%
          
          mutate(
            click_distance =
              abs(
                latitude -
                  click$lat
              ) +
              abs(
                longitude -
                  click$lng
              )
          ) %>%
          
          arrange(
            click_distance
          )
      }
      
      
      selected_cluster <-
        selected_cluster %>%
        slice(1)
      
      
      req(
        nrow(
          selected_cluster
        ) == 1
      )
      
      
      # ------------------------------------------------------------------------
      # Capture reactive slider value before entering session$onFlushed()
      # ------------------------------------------------------------------------
      
      coverage_now <- input$coverage_range
      
      req(
        !is.null(coverage_now),
        length(coverage_now) == 2
      )
      
      
      # ------------------------------------------------------------------------
      # Update district
      # ------------------------------------------------------------------------
      
      updateSelectInput(
        session,
        "district",
        selected =
          selected_cluster$district
      )
      
      
      session$onFlushed(
        function() {
          
          
          # --------------------------------------------------------------------
          # Health centres
          # --------------------------------------------------------------------
          
          available_hc <-
            dashboard_data %>%
            
            filter(
              district ==
                selected_cluster$district
            ) %>%
            
            pull(
              healthcenter
            ) %>%
            
            unique() %>%
            
            na.omit() %>%
            
            sort()
          
          
          updateSelectInput(
            session,
            "healthcenter",
            choices = c(
              "All health centres",
              available_hc
            ),
            selected =
              selected_cluster$healthcenter
          )
          
          
          # --------------------------------------------------------------------
          # Clusters
          # --------------------------------------------------------------------
          
          available_clusters <-
            cluster_map_data %>%
            
            filter(
              district ==
                selected_cluster$district,
              healthcenter ==
                selected_cluster$healthcenter,
              uptake >=
                coverage_now[1],
              uptake <=
                coverage_now[2]
            ) %>%
            
            pull(
              cluster_id
            ) %>%
            
            unique() %>%
            
            na.omit() %>%
            
            sort()
          
          
          updateSelectInput(
            session,
            "cluster",
            choices = c(
              "All clusters",
              available_clusters
            ),
            selected =
              selected_cluster$cluster_id
          )
          
          
          # --------------------------------------------------------------------
          # Reset village only
          # --------------------------------------------------------------------
          
          updateSelectInput(
            session,
            "village",
            selected =
              "All villages"
          )
        },
        once = TRUE
      )
    }
  )
  
  
  # =============================================================================
  # CLUSTER DROPDOWN -> ZOOM MAP
  # =============================================================================
  
  observeEvent(
    input$cluster,
    {
      
      req(
        input$cluster
      )
      
      
      if (
        input$cluster ==
        "All clusters"
      ) {
        
        update_cluster_highlight()
        
        return()
      }
      
      
      selected_cluster <-
        cluster_map_data %>%
        
        filter(
          cluster_id ==
            input$cluster
        )
      
      
      if (
        !is.null(
          input$district
        ) &&
        input$district !=
        "All districts"
      ) {
        
        selected_cluster <-
          selected_cluster %>%
          
          filter(
            district ==
              input$district
          )
      }
      
      
      if (
        !is.null(
          input$healthcenter
        ) &&
        input$healthcenter !=
        "All health centres"
      ) {
        
        selected_cluster <-
          selected_cluster %>%
          
          filter(
            healthcenter ==
              input$healthcenter
          )
      }
      
      
      selected_cluster <-
        selected_cluster %>%
        
        filter(
          uptake >=
            input$coverage_range[1],
          uptake <=
            input$coverage_range[2]
        )
      
      
      if (
        nrow(
          selected_cluster
        ) == 0
      ) {
        
        update_cluster_highlight()
        
        return()
      }
      
      
      selected_cluster <-
        selected_cluster %>%
        slice(1)
      
      
      leafletProxy(
        "cluster_map"
      ) %>%
        
        setView(
          lng =
            selected_cluster$longitude,
          lat =
            selected_cluster$latitude,
          zoom = 12
        )
      
      
      update_cluster_highlight()
    }
  )
  
  
  # =============================================================================
  # UPDATE MAP WHEN FILTERS CHANGE
  # =============================================================================
  
  observeEvent(
    list(
      input$district,
      input$healthcenter,
      input$coverage_range
    ),
    {
      
      map_dat <-
        filtered_map_data()
      
      
      pal <- colorFactor(
        palette = c(
          "#d73027",
          "#fc8d59",
          "#fee08b",
          "#91cf60",
          "#1a9850",
          "#bdbdbd"
        ),
        domain =
          levels(
            cluster_map_data$coverage_category
          ),
        ordered = TRUE
      )
      
      
      proxy <-
        leafletProxy(
          "cluster_map"
        ) %>%
        
        clearMarkers()
      
      
      if (
        nrow(
          map_dat
        ) > 0
      ) {
        
        proxy %>%
          
          addCircleMarkers(
            
            data = map_dat,
            
            lng = ~longitude,
            
            lat = ~latitude,
            
            layerId = ~cluster_id,
            
            radius = ~pmax(
              6,
              pmin(
                12,
                sqrt(
                  participants
                )
              )
            ),
            
            color = "#333333",
            
            weight = 1,
            
            fillColor =
              ~pal(
                coverage_category
              ),
            
            fillOpacity = 0.90,
            
            
            # ==================================================================
            # CLICK POPUP
            # ==================================================================
            
            popup = ~paste0(
              
              "<div style='min-width:270px;
                          font-family:Arial, sans-serif;'>",
              
              "<div style='font-size:19px;
                          font-weight:bold;
                          margin-bottom:8px;'>",
              
              "Cluster ",
              cluster_id,
              
              "</div>",
              
              "<b>District:</b> ",
              district,
              "<br>",
              
              "<b>Health centre:</b> ",
              healthcenter,
              "<br>",
              
              "<b>Village:</b> ",
              village,
              "<br><br>",
              
              "<b>Participants:</b> ",
              participants,
              "<br>",
              
              "<b>Received ivermectin:</b> ",
              received_n,
              "<br>",
              
              "<b>Did not receive:</b> ",
              nonrecipient_n,
              "<br>",
              
              "<b>Swallowed ivermectin:</b> ",
              treated_n,
              "<br>",
              
              "<b>Did not swallow:</b> ",
              non_treated_n,
              "<br><br>",
              
              "<b>Receipt coverage:</b> ",
              sprintf(
                "%.1f%%",
                receipt
              ),
              "<br>",
              
              "<b>Uptake:</b> ",
              sprintf(
                "%.1f%%",
                uptake
              ),
              "<br>",
              
              "<b>Coverage category:</b> ",
              coverage_category,
              
              "</div>"
            ),
            
            
            # ==================================================================
            # HOVER LABEL
            # ==================================================================
            
            label = ~paste0(
              "Cluster ",
              cluster_id,
              " | ",
              district,
              " | Uptake: ",
              sprintf(
                "%.1f%%",
                uptake
              )
            ),
            
            labelOptions =
              labelOptions(
                noHide = FALSE,
                direction = "auto",
                opacity = 0.95,
                textsize = "13px"
              )
          )
        
        
        # ----------------------------------------------------------------------
        # Zoom behavior
        # ----------------------------------------------------------------------
        
        if (
          nrow(
            map_dat
          ) == 1
        ) {
          
          leafletProxy(
            "cluster_map"
          ) %>%
            
            setView(
              lng =
                map_dat$longitude[1],
              lat =
                map_dat$latitude[1],
              zoom = 12
            )
          
        } else {
          
          leafletProxy(
            "cluster_map"
          ) %>%
            
            fitBounds(
              lng1 =
                min(
                  map_dat$longitude,
                  na.rm = TRUE
                ),
              lat1 =
                min(
                  map_dat$latitude,
                  na.rm = TRUE
                ),
              lng2 =
                max(
                  map_dat$longitude,
                  na.rm = TRUE
                ),
              lat2 =
                max(
                  map_dat$latitude,
                  na.rm = TRUE
                )
            )
        }
      }
      
      
      update_cluster_highlight()
    },
    ignoreInit = TRUE
  )
  
  
  # =============================================================================
  # CLUSTER SUMMARY TABLE
  # =============================================================================
  
  output$cluster_table <- renderDT({
    
    cluster_summary <-
      filtered_data() %>%
      
      group_by(
        district,
        healthcenter,
        cluster_id,
        village,
        coverage_category
      ) %>%
      
      summarise(
        
        participants = n(),
        
        received = sum(
          received_num == 1,
          na.rm = TRUE
        ),
        
        swallowed = sum(
          treated_num == 1,
          na.rm = TRUE
        ),
        
        non_recipients = sum(
          received_num == 0,
          na.rm = TRUE
        ),
        
        receipt_coverage =
          mean(
            received_num,
            na.rm = TRUE
          ) * 100,
        
        uptake =
          mean(
            treated_num,
            na.rm = TRUE
          ) * 100,
        
        .groups = "drop"
      ) %>%
      
      mutate(
        
        receipt_coverage =
          round(
            receipt_coverage,
            1
          ),
        
        uptake =
          round(
            uptake,
            1
          )
      )
    
    
    datatable(
      cluster_summary,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      colnames = c(
        "District",
        "Health centre",
        "Cluster",
        "Village",
        "Coverage category",
        "Participants",
        "Received",
        "Swallowed",
        "Non-recipients",
        "Receipt coverage (%)",
        "Uptake (%)"
      )
    )
  })
  
  
  # =============================================================================
  # COVERAGE CATEGORY COUNTS
  # =============================================================================
  
  coverage_category_data <- reactive({
    
    category_levels <- c(
      "<60%",
      "60–69.9%",
      "70–74.9%",
      "75–79.9%",
      "≥80%"
    )
    
    
    filtered_data() %>%
      
      filter(
        !is.na(cluster_id),
        !is.na(coverage_category),
        coverage_category != "Missing"
      ) %>%
      
      distinct(
        district,
        healthcenter,
        cluster_id,
        coverage_category
      ) %>%
      
      count(
        coverage_category,
        name = "n_clusters"
      ) %>%
      
      tidyr::complete(
        coverage_category = factor(
          category_levels,
          levels = category_levels
        ),
        fill = list(
          n_clusters = 0
        )
      ) %>%
      
      mutate(
        coverage_category = factor(
          as.character(coverage_category),
          levels = category_levels
        )
      ) %>%
      
      arrange(
        coverage_category
      )
  })
  
  
  # =============================================================================
  # COVERAGE CATEGORY BAR CHART
  # =============================================================================
  
  output$coverage_category_plot <- renderPlot({
    
    dat <- coverage_category_data()
    
    
    validate(
      need(
        sum(dat$n_clusters) > 0,
        "No clusters were found for the current selection."
      )
    )
    
    
    total_clusters <- sum(
      dat$n_clusters
    )
    
    
    ggplot(
      dat,
      aes(
        x = coverage_category,
        y = n_clusters,
        fill = coverage_category
      )
    ) +
      
      geom_col(
        width = 0.72
      ) +
      
      geom_text(
        aes(
          label = n_clusters
        ),
        vjust = -0.45,
        size = 5,
        fontface = "bold"
      ) +
      
      scale_fill_manual(
        values = c(
          "<60%" = "#d73027",
          "60–69.9%" = "#fc8d59",
          "70–74.9%" = "#fee08b",
          "75–79.9%" = "#91cf60",
          "≥80%" = "#1a9850"
        ),
        drop = FALSE
      ) +
      
      scale_y_continuous(
        breaks = scales::pretty_breaks(),
        expand = expansion(
          mult = c(
            0,
            0.12
          )
        )
      ) +
      
      labs(
        title = paste0(
          "Distribution of selected clusters by uptake category (n = ",
          comma(total_clusters),
          ")"
        ),
        x = "Ivermectin uptake category",
        y = "Number of clusters"
      ) +
      
      guides(
        fill = "none"
      ) +
      
      theme_minimal(
        base_size = 14
      ) +
      
      theme(
        plot.title = element_text(
          size = 18,
          face = "bold"
        ),
        axis.title = element_text(
          face = "bold"
        ),
        axis.text.x = element_text(
          size = 12
        ),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(
          15,
          30,
          15,
          15
        )
      )
  })
  
  
  # =============================================================================
  # RESET FILTERS
  # =============================================================================
  
  observeEvent(
    input$reset,
    {
      
      updateSelectInput(
        session,
        "district",
        selected =
          "All districts"
      )
      
      
      updateSelectInput(
        session,
        "healthcenter",
        selected =
          "All health centres"
      )
      
      
      updateSelectInput(
        session,
        "cluster",
        selected =
          "All clusters"
      )
      
      
      updateSliderInput(
        session,
        "coverage_range",
        value = c(
          0,
          100
        )
      )
      
      
      updateSelectInput(
        session,
        "village",
        selected =
          "All villages"
      )
      
      
      leafletProxy(
        "cluster_map"
      ) %>%
        
        removeMarker(
          layerId = "highlighted_cluster"
        )
      
      
      leafletProxy(
        "cluster_map"
      ) %>%
        
        fitBounds(
          lng1 =
            min(
              cluster_map_data$longitude,
              na.rm = TRUE
            ),
          lat1 =
            min(
              cluster_map_data$latitude,
              na.rm = TRUE
            ),
          lng2 =
            max(
              cluster_map_data$longitude,
              na.rm = TRUE
            ),
          lat2 =
            max(
              cluster_map_data$latitude,
              na.rm = TRUE
            )
        )
    }
  )
}


# ==============================================================================
# 7. RUN APP
# ==============================================================================

shinyApp(
  ui = ui,
  server = server
)