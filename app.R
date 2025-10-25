# ---- app.R ----
# Garmin Run Visualiser (Shiny App)

library(shiny)
library(xml2)
library(dplyr)
library(ggplot2)
library(patchwork)
library(shinycssloaders)

# Load helper functions from your modular code
source("R/Plot_Runs.R")

ui <- fluidPage(
  titlePanel("🏃 Garmin Run Visualiser"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("tcx_file", "Upload a Garmin .tcx File",
                accept = c(".tcx")),
      hr(),
      downloadButton("download_pace", "⬇️ Download Pace Chart"),
      downloadButton("download_hr_line", "⬇️ Download HR Line Chart"),
      downloadButton("download_hr_stacked", "⬇️ Download HR Zones Chart"),
      downloadButton("download_combined", "⬇️ Download Combined Chart")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pace Splits", withSpinner(plotOutput("pace_plot", height = "600px"))),
        tabPanel("Heart Rate", withSpinner(plotOutput("hr_plot", height = "600px"))),
        tabPanel("Heart Rate Zones", withSpinner(plotOutput("hr_zones_plot", height = "600px"))),
        tabPanel("Combined", withSpinner(plotOutput("combined_plot", height = "1000px")))
      )
    )
  )
)

server <- function(input, output, session) {
  
  run_data_reactive <- reactive({
    req(input$tcx_file)
    read_tcx_data(input$tcx_file$datapath)
  })
  
  plots_reactive <- reactive({
    run_data <- run_data_reactive()
    start_time <- min(run_data$time, na.rm = TRUE)
    start_date <- format(start_time, "%d/%m/%Y")
    total_seconds <- max(run_data$time_elapsed, na.rm = TRUE)
    total_time_formatted <- format_total_time(total_seconds)
    max_km <- max(ceiling(run_data$distance / 1000), na.rm = TRUE)
    sizes <- get_plot_sizes(max_km)
    summaries <- summarize_run_data(run_data)
    
    list(
      pace = plot_km_splits(summaries$km_splits, total_time_formatted, start_date, sizes),
      hr_line = plot_hr_line(summaries$hr_df_avg, total_time_formatted, start_date, max_km, sizes),
      hr_stacked = plot_hr_stacked(summaries$hr_stacked_bar, total_time_formatted, start_date, sizes)
    )
  })
  
  output$pace_plot <- renderPlot({ plots_reactive()$pace })
  output$hr_plot <- renderPlot({ plots_reactive()$hr_line })
  output$hr_zones_plot <- renderPlot({ plots_reactive()$hr_stacked })
  output$combined_plot <- renderPlot({
    plots <- plots_reactive()
    combine_run_plots(plots$pace, plots$hr_line, plots$hr_stacked)
  })
  
  # ---- Downloads ----
  output$download_pace <- downloadHandler(
    filename = function() "pace_chart.png",
    content = function(file) ggsave(file, plots_reactive()$pace, width = 8, height = 6, dpi = 900)
  )
  
  output$download_hr_line <- downloadHandler(
    filename = function() "heart_rate_chart.png",
    content = function(file) ggsave(file, plots_reactive()$hr_line, width = 8, height = 6, dpi = 900)
  )
  
  output$download_hr_stacked <- downloadHandler(
    filename = function() "heart_rate_zones.png",
    content = function(file) ggsave(file, plots_reactive()$hr_stacked, width = 8, height = 6, dpi = 900)
  )
  
  output$download_combined <- downloadHandler(
    filename = function() "combined_chart.png",
    content = function(file) {
      plots <- plots_reactive()
      combined <- combine_run_plots(plots$pace, plots$hr_line, plots$hr_stacked)
      ggsave(file, combined, width = 10, height = 12, dpi = 900)
    }
  )
}

shinyApp(ui, server)

