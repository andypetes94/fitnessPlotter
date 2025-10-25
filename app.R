library(shiny)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(patchwork)
library(ggplot2)
library(dplyr)
library(fontawesome)

# Source plotting helpers
source("R/Plot_Runs.R")

# --- Define custom theme ---
theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = font_google("Lato"),
  heading_font = font_google("Montserrat"),
  bg = "#f9fafb",
  fg = "#222222",
  primary = "#000080",
  secondary = "#0d6efd"
)

# --- UI ---
ui <- fluidPage(
  theme = theme,
  tags$head(
    tags$style(HTML("
      .plot-card {
        background: white;
        border-radius: 15px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.08);
        padding: 20px;
        margin-bottom: 25px;
      }
      .title-icon {
        font-family: 'Font Awesome 7 Free';
        font-weight: 900;
        color: #000080;
        margin-right: 10px;
      }
      .download-btn {
        background-color: #000080 !important;
        border: none !important;
        color: white !important;
        font-weight: 600;
      }
    "))
  ),
  
  # --- Header ---
  fluidRow(
    column(12,
           tags$h2(
             HTML("<i class='fa-solid fa-person-running title-icon'></i> Garmin Run Visualiser"),
             class = "text-center mb-4 fw-bold",
             style = "margin-top: 20px; color: black;"  # <-- adds space above
           ),
           tags$p(
             "Upload a Garmin .tcx file to visualize your run performance.",
             class = "text-center text-muted mb-4"
           )
    )
  ),
  
  # --- Upload Section ---
  fluidRow(
    column(
      width = 8, offset = 2,
      fileInput("tcx_file", "Upload TCX File", accept = ".tcx",
                buttonLabel = "Browse", placeholder = "Choose a Garmin activity file..."),
      uiOutput("activity_info")
    )
  ),
  
  # --- Tabs for plots ---
  fluidRow(
    column(
      width = 12,
      tabsetPanel(
        type = "pills",
        tabPanel(
          "🏃 Pace Splits",
          div(class = "plot-card",
              withSpinner(plotOutput("pace_plot", height = "450px"), type = 6),
              downloadButton("download_pace", "Download PNG", class = "download-btn mt-3")
          )
        ),
        tabPanel(
          "❤️ Heart Rate (Avg per KM)",
          div(class = "plot-card",
              withSpinner(plotOutput("hr_line_plot", height = "450px"), type = 6),
              downloadButton("download_hr_line", "Download PNG", class = "download-btn mt-3")
          )
        ),
        tabPanel(
          "🔥 Heart Rate Zones",
          div(class = "plot-card",
              withSpinner(plotOutput("hr_stacked_plot", height = "450px"), type = 6),
              downloadButton("download_hr_stacked", "Download PNG", class = "download-btn mt-3")
          )
        ),
        tabPanel(
          "📊 Combined Summary",
          div(class = "plot-card",
              withSpinner(plotOutput("combined_plot", height = "1000px"), type = 6),
              downloadButton("download_combined", "Download PNG", class = "download-btn mt-3")
          )
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Reactive to read TCX file
  run_data <- reactive({
    req(input$tcx_file)
    read_tcx_data(input$tcx_file$datapath)
  })
  
  # Reactive: summarize data for plots
  run_summaries <- reactive({
    rd <- run_data()
    max_km <- max(ceiling(rd$distance / 1000), na.rm = TRUE)
    sizes <- get_plot_sizes(max_km)
    total_seconds <- max(rd$time_elapsed, na.rm = TRUE)
    total_time_formatted <- format_total_time(total_seconds)
    start_date <- format(min(rd$time, na.rm = TRUE), "%d/%m/%Y")
    summaries <- summarize_run_data(rd)
    list(rd = rd, sizes = sizes, total_time_formatted = total_time_formatted,
         start_date = start_date, max_km = max_km,
         km_splits = summaries$km_splits,
         hr_df_avg = summaries$hr_df_avg,
         hr_stacked_bar = summaries$hr_stacked_bar)
  })
  
  # Display activity info
  output$activity_info <- renderUI({
    req(run_summaries())
    rs <- run_summaries()
    dist_km <- round(max(rs$rd$distance, na.rm = TRUE) / 1000, 2)
    tags$div(class = "text-center text-success fw-bold mb-4",
             paste("✅ Loaded", dist_km, "km activity — Total Time:", rs$total_time_formatted)
    )
  })
  
  # Generate plots
  output$pace_plot <- renderPlot({
    req(run_summaries())
    rs <- run_summaries()
    plot_km_splits(rs$km_splits, rs$total_time_formatted, rs$start_date, rs$sizes)
  })
  
  output$hr_line_plot <- renderPlot({
    req(run_summaries())
    rs <- run_summaries()
    plot_hr_line(rs$hr_df_avg, rs$total_time_formatted, rs$start_date, rs$max_km, rs$sizes)
  })
  
  output$hr_stacked_plot <- renderPlot({
    req(run_summaries())
    rs <- run_summaries()
    plot_hr_stacked(rs$hr_stacked_bar, rs$total_time_formatted, rs$start_date, rs$sizes)
  })
  
  output$combined_plot <- renderPlot({
    req(run_summaries())
    rs <- run_summaries()
    p_pace <- plot_km_splits(rs$km_splits, rs$total_time_formatted, rs$start_date, rs$sizes)
    p_hr_line <- plot_hr_line(rs$hr_df_avg, rs$total_time_formatted, rs$start_date, rs$max_km, rs$sizes)
    p_hr_stacked <- plot_hr_stacked(rs$hr_stacked_bar, rs$total_time_formatted, rs$start_date, rs$sizes)
    combine_run_plots(p_pace, p_hr_line, p_hr_stacked)
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