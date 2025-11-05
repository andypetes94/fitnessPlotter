library(shiny)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(patchwork)
library(ggplot2)
library(dplyr)
library(fontawesome)
library(fresh)
library(tmap)
library(sf)
library(shinydashboard)

# Source plotting helpers
source("R/Plot_Runs.R")

tmap_mode("view")  # set globally, once

# --- Define custom theme ---
mytheme <- create_theme(
  adminlte_color(
    # Header Colour
    light_blue = "#1D3557"
  ),
  adminlte_sidebar(
    width = "250px",
    # Sidebar Background
    dark_bg = "#457B9D",
    # Hover Background
    dark_hover_bg = "#A8DADC",
    # Text Background
    dark_color = "#F1FAEE"
  ),
  adminlte_global(
    # Whole Background
    content_bg = "#FDFFFC", #white
    # Box Background
    box_bg = "#EBFAFA", 
    # No idea what this does
    info_box_bg = "#E63946"
  )
)

# --- UI ---
ui <- dashboardPage(
  skin = "blue",   # optional theme colour
  
  # --- Header ---
  dashboardHeader(
    title = "fitnessPlotter",
    
    dropdownMenu(
      type = "messages", badgeStatus = "success",
      messageItem("Welcome!", "Explore your training data")
    )
  ),
  
  # --- Sidebar ---
  dashboardSidebar(
    sidebarMenu(
      menuItem("Run Activities", tabName = "runs", icon = icon("running")),
      menuItem("HIIT Workouts", tabName = "hiit", icon = icon("fire"))
    )
  ),
  
  # --- Body ---
  dashboardBody(
    use_theme(mytheme),
    includeCSS("www/custom.css"),
    
    tabItems(
      
      # ------------------ RUN ACTIVITIES TAB ------------------
      tabItem(tabName = "runs",
              
              # --- Header ---
              fluidRow(
                column(12,
                       tags$h2(
                         HTML("<i class='fa-solid fa-person-running title-icon'></i> Run Visualiser"),
                         class = "text-center mb-4 fw-bold",
                         style = "margin-top: 20px; color: navy;"  # <-- adds space above
                       ),
                       tags$p(
                         "Upload a '.tcx' file to visualize your run performance.",
                         class = "text-center text-muted mb-4"
                       )
                )
              ),
              
              # --- Upload Section ---
              fluidRow(
                column(
                  width = 8, offset = 2,
                  div(
                    style = "text-align: center;",
                    div(
                      style = "display: inline-block; margin-bottom: 5px;",  # reduce bottom spacing
                      fileInput("tcx_file", "Upload Run TCX File", accept = ".tcx", buttonLabel = "Browse", placeholder = "Choose a Garmin activity file...")
                    )
                  ),
                  div(style = "margin-top: 0px;", uiOutput("activity_info"))
                ),
                style = "margin-bottom: 10px;"),
              
              tabsetPanel(
                type = "pills",
                tabPanel("🏃 Pace Splits",
                         div(class = "plot-card",
                             withSpinner(plotOutput("pace_plot", height = "450px"), type = 6),
                             downloadButton("download_pace", "Download PNG", class = "download-btn mt-3")
                         )
                ),
                tabPanel("❤️ Heart Rate (Avg per KM)",
                         div(class = "plot-card",
                             withSpinner(plotOutput("hr_line_plot", height = "450px"), type = 6),
                             downloadButton("download_hr_line", "Download PNG", class = "download-btn mt-3")
                         )
                ),
                tabPanel("🔥 Heart Rate Zones",
                         div(class = "plot-card",
                             withSpinner(plotOutput("hr_stacked_plot", height = "450px"), type = 6),
                             downloadButton("download_hr_stacked", "Download PNG", class = "download-btn mt-3")
                         )
                ),
                tabPanel("📊 Combined Summary",
                         div(class = "plot-card",
                             withSpinner(plotOutput("combined_plot", height = "1000px"), type = 6),
                             downloadButton("download_combined", "Download PNG", class = "download-btn mt-3")
                         )
                ),
                tabPanel("📍 Run Map",
                         div(class = "plot-card",
                             withSpinner(tmapOutput("runMap", height = "450px"), type = 6)
                         )
                )
              )
      ),
      
      # ------------------ HIIT WORKOUT TAB ------------------
      tabItem(tabName = "hiit",
              # --- Header ---
              fluidRow(
                column(12,
                       tags$h2(
                         HTML("<i class='fa-solid fa-fire-flame-curved title-icon'></i> HIIT Workouts"),
                         class = "text-center mb-4 fw-bold",
                         style = "margin-top: 20px; color: navy;"  # <-- adds space above
                       ),
                       tags$p(
                         "Upload a '.tcx' file to visualise your run performance.",
                         class = "text-center text-muted mb-4"
                       )
                )
              ),
              
              # --- Upload Section ---
              fluidRow(
                column(
                  width = 8, offset = 2,
                  div(
                    style = "text-align: center;",
                    div(
                      style = "display: inline-block; margin-bottom: 5px;",  # reduce bottom spacing
                      fileInput("tcx_file", "Upload a HIIT TCX File", accept = ".tcx", buttonLabel = "Browse", placeholder = "Choose a Garmin activity file...")
                    )
                  ),
                ),
                style = "margin-bottom: 10px;"),
              fluidRow(
                column(
                  width = 12,
                  box(title = "HIIT Data Coming Soon!", width = 12, status = "warning",
                      solidHeader = TRUE,
                      p("Once interval parsing is defined, plots and summaries will appear here.")
                  )
                )
              )
      )
    )
  )
)


# --- Server ---
server <- function(input, output, session) {
  
  # Define the reactive value to store the current file path
  current_file <- reactiveVal()
  
  # Auto-load default file on app startup
  observe({
    default_path <- "./activities/sample_5k.tcx"
    if (file.exists(default_path) && is.null(current_file())) {
      current_file(default_path)
    }
  })
  
  # Handle file upload
  observe({
    req(input$tcx_file)
    current_file(input$tcx_file$datapath)
  })
  
  # Reactive to read TCX file
  run_data <- reactive({
    req(current_file())
    read_tcx_data(current_file())
  })
  
  # Reactive: summarize data for plots
  run_summaries <- reactive({
    rd <- run_data()
    max_km <- max(ceiling(rd$distance / 1000), na.rm = TRUE)
    max_km_longer <- round(tail(rd$distance / 1000, 1), 2)
    sizes <- get_plot_sizes(max_km)
    total_seconds <- max(rd$time_elapsed, na.rm = TRUE)
    total_time_formatted <- format_total_time(total_seconds)
    start_date <- format(min(rd$time, na.rm = TRUE), "%d/%m/%Y")
    summaries <- summarize_run_data(rd)
    list(rd = rd, sizes = sizes, total_time_formatted = total_time_formatted,
         start_date = start_date, max_km = max_km,
         max_km_longer = max_km_longer,
         km_splits = summaries$km_splits,
         hr_df_avg = summaries$hr_df_avg,
         hr_stacked_bar = summaries$hr_stacked_bar,
         map_runs_df = summaries$map_runs_df)
  })
  
  # Display activity info
  output$activity_info <- renderUI({
    req(run_summaries())
    rs <- run_summaries()
    dist_km <- round(max(rs$rd$distance, na.rm = TRUE) / 1000, 2)
    tags$div(style = "margin-top: 0px; margin-bottom: 0px;",
             class = "text-center text-success fw-bold",
             paste("✅ Loaded", dist_km, "km activity — Total Time:", rs$total_time_formatted)
    )
  })
  
  # Generate plots
  output$pace_plot <- renderPlot({
    req(run_summaries())
    rs <- run_summaries()
    plot_km_splits(rs$km_splits, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km)
  })
  
  output$hr_line_plot <- renderPlot({
    req(run_summaries())
    rs <- run_summaries()
    plot_hr_line(rs$hr_df_avg, rs$total_time_formatted, rs$start_date, rs$max_km, rs$sizes, rs$max_km_longer)
  })
  
  output$hr_stacked_plot <- renderPlot({
    req(run_summaries())
    rs <- run_summaries()
    plot_hr_stacked(rs$hr_stacked_bar, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km)
  })
  
  output$combined_plot <- renderPlot({
    req(run_summaries())
    rs <- run_summaries()
    p_pace <- plot_km_splits(rs$km_splits, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km)
    p_hr_line <- plot_hr_line(rs$hr_df_avg, rs$total_time_formatted, rs$start_date, rs$max_km, rs$sizes, rs$max_km_longer)
    p_hr_stacked <- plot_hr_stacked(rs$hr_stacked_bar, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km)
    combine_run_plots(p_pace, p_hr_line, p_hr_stacked)
  })
  
  output$runMap <- renderTmap({
    req(run_data())  # ensure data is available
    rs <- run_summaries()
    plot_run_map(rs$map_runs_df)  # pass the reactive itself
  })
  
  # --- Download Handlers ---
  output$download_pace <- downloadHandler(
    filename = function() "pace_plot.png",
    content = function(file) {
      rs <- run_summaries()
      ggsave(file, plot = plot_km_splits(rs$km_splits, rs$total_time_formatted, rs$start_date, rs$sizes),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_hr_line <- downloadHandler(
    filename = function() "hr_line_plot.png",
    content = function(file) {
      rs <- run_summaries()
      ggsave(file, plot = plot_hr_line(rs$hr_df_avg, rs$total_time_formatted, rs$start_date, rs$max_km, rs$sizes),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_hr_stacked <- downloadHandler(
    filename = function() "hr_stacked_plot.png",
    content = function(file) {
      rs <- run_summaries()
      ggsave(file, plot = plot_hr_stacked(rs$hr_stacked_bar, rs$total_time_formatted, rs$start_date, rs$sizes),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_combined <- downloadHandler(
    filename = function() "combined_plot.png",
    content = function(file) {
      rs <- run_summaries()
      p_pace <- plot_km_splits(rs$km_splits, rs$total_time_formatted, rs$start_date, rs$sizes)
      p_hr_line <- plot_hr_line(rs$hr_df_avg, rs$total_time_formatted, rs$start_date, rs$max_km, rs$sizes)
      p_hr_stacked <- plot_hr_stacked(rs$hr_stacked_bar, rs$total_time_formatted, rs$start_date, rs$sizes)
      ggsave(file, plot = combine_run_plots(p_pace, p_hr_line, p_hr_stacked),
             width = 10, height = 12, dpi = 300)
    }
  )
}

shinyApp(ui, server)