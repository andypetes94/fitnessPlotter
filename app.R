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
library(fuzzyjoin)
library(DT)


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
  # --- Header ---
  dashboardHeader(
    #title = "fitnessPlotter",
    title = tags$div(tags$img(src = "logo_white.png", height = "30px")),
    
    dropdownMenu(
      type = "messages", badgeStatus = "success",
      messageItem("Welcome!", "Explore your training data")
    )
  ),
  
  # --- Sidebar ---
  dashboardSidebar(
    sidebarMenu(
      menuItem("Run Activities", tabName = "runs", icon = icon("running")),
      menuItem("Cardio / HYROX", tabName = "hiit", icon = icon("fire"))
      #menuItem("HYROX Workouts", tabName = "hyrox", icon = icon("dumbbell"))
    )
  ),
  
  # --- Body ---
  dashboardBody(
    # Send screen width to Shiny
    tags$script(HTML("
    $(document).on('shiny:connected', function() {
      Shiny.setInputValue('screen_width', window.innerWidth);
    });
    $(window).resize(function(){
      Shiny.setInputValue('screen_width', window.innerWidth);
    });
  ")),
    tags$style(HTML("
  table.dataTable tbody tr.selected,
  table.dataTable tbody tr.selected td {
    background-color: #f2f2f2 !important;
    color: #000000 !important;
  }
")),
    use_theme(mytheme),
    includeCSS("www/custom.css"),
    tabItems(
      
      # ------------------ RUN ACTIVITIES TAB ------------------
      tabItem(tabName = "runs",
              
              # --- Header ---
              fluidRow(
                column(12,
                       tags$img(src = "logo_black.png", height = "100px",
                                style = "display:block; margin-left:auto; margin-right:auto;"),
                       tags$h2(
                         HTML("<i class='fa-solid fa-person-running title-icon'></i> Run Visualiser"),
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
      
      # ------------------ Cardio WORKOUT TAB ------------------
      tabItem(tabName = "hiit",
              fluidRow(
                column(12, align = "center",
                       tags$img(src = "logo_black.png", height = "100px",
                                style = "display:block; margin-left:auto; margin-right:auto;"),
                       switchInput(
                         inputId = "workout_mode",
                         label = NULL,
                         onLabel = "HYROX",
                         offLabel = "Cardio",
                         value = FALSE,          # FALSE = Cardio default
                         size = "large",
                         onStatus = "primary",
                         offStatus = "info"
                       )
                )
              ),
              br(),
              # --- Header ---
              
              conditionalPanel(
                condition = "!input.workout_mode", 
              fluidRow(
                column(12,
                       tags$h2(
                         HTML("<i class='fa-solid fa-fire-flame-curved title-icon'></i> Cardio Workouts"),
                         class = "text-center mb-4 fw-bold",
                         style = "margin-top: 20px; color: navy;"  # <-- adds space above
                       ),
                       tags$p(
                         "Upload a Cardio '.tcx' file to visualise your performance.",
                         class = "text-center text-muted mb-4"
                       )
                )
              ),
              # --- New Tab ---
              fluidRow(
                column(
                  width = 12,
                  box(title = "Cardio Visualiser Just Launced! 🚀", width = 12, status = "success",
                      solidHeader = TRUE,
                      p("We now Plot Cardio Interval Data!")
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
                      fileInput("tcx_file_hiit", "Upload a Cardio TCX File", accept = ".tcx", buttonLabel = "Browse", placeholder = "Choose a Garmin activity file...")
                    )
                  ),
                  div(style = "margin-top: 0px;", uiOutput("activity_info_hiit"),
                      prettyCheckbox(
                        inputId = "remove_warmup",
                        label = "Remove Warm-Up (First) Interval",
                        value = FALSE,
                        status = "info",        # built-in color scheme, but we will override below
                        outline = TRUE,
                        fill = TRUE,
                        icon = icon("check")
                      )),
                ),
                style = "margin-bottom: 10px;"),
              
              tabsetPanel(
                type = "pills",
                tabPanel("🏃 Circuit Splits",
                         div(class = "plot-card",
                             withSpinner(plotOutput("circuit_time_plot", height = "450px"), type = 6),
                             downloadButton("download_circuit", "Download PNG", class = "download-btn mt-3")
                         )
                ),
                tabPanel("❤️ Heart Rate (Avg per Circuit)",
                         div(class = "plot-card",
                             withSpinner(plotOutput("hr_line_plot_hiit", height = "450px"), type = 6),
                             downloadButton("download_hr_line_hiit", "Download PNG", class = "download-btn mt-3")
                         )
                ),
                tabPanel("🔥 Heart Rate Zones",
                         div(class = "plot-card",
                             withSpinner(plotOutput("hr_stacked_plot_hiit", height = "450px"), type = 6),
                             downloadButton("download_hr_stacked_hiit", "Download PNG", class = "download-btn mt-3")
                         )
                ),
                tabPanel("🗑️ Heart Rate Bins",
                         div(class = "plot-card",
                             withSpinner(plotOutput("hr_plot_bins", height = "450px"), type = 6),
                             downloadButton("download_hr_bins_hiit", "Download PNG", class = "download-btn mt-3")
                         )
                ),
                tabPanel("📊 Combined Summary",
                         div(class = "plot-card",
                             withSpinner(plotOutput("combined_plot_hiit", height = "1000px"), type = 6),
                             downloadButton("download_combined_hiit", "Download PNG", class = "download-btn mt-3")
                         )
                ),
              )),
              conditionalPanel(
                condition = "input.workout_mode", 
                fluidRow(
                  column(12,
                         tags$h2(
                           HTML("<i class='fa-solid fa-dumbbell'></i> HYROX Workouts"),
                           class = "text-center mb-4 fw-bold",
                           style = "margin-top: 20px; color: navy;"  # <-- adds space above
                         ),
                         tags$p(
                           "Upload a HYROX '.tcx' file to visualise your performance.",
                           class = "text-center text-muted mb-4"
                         )
                  )
                ),
                # --- New Tab ---
                fluidRow(
                  column(
                    width = 12,
                    box(title = "HYROX Visualiser Just Launced! 🚀", width = 12, status = "success",
                        solidHeader = TRUE,
                        p("We now Plot HYROX Interval Data!")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 8, offset = 2,
                    div(
                      style = "text-align: center;",
                      div(
                        style = "display: inline-block; margin-bottom: 5px;",  # reduce bottom spacing
                        fileInput("tcx_file_hyrox", "Upload a HYROX (Cardio) TCX File", accept = ".tcx", buttonLabel = "Browse", placeholder = "Choose a Garmin activity file...")
                      )
                    ),
                  ),
                  style = "margin-bottom: 10px;"),
                
                tabsetPanel(
                  type = "pills",
                  tabPanel("🏃 Circuit Splits",
                           div(class = "plot-card",
                               withSpinner(plotOutput("circuit_time_hyrox", height = "450px"), type = 6),
                               downloadButton("download_circuit_hyrox", "Download PNG", class = "download-btn mt-3")
                           )
                  ),
                  tabPanel("❤️ Heart Rate (Avg per Split)",
                           div(class = "plot-card",
                               withSpinner(plotOutput("hr_line_plot_hyrox", height = "450px"), type = 6),
                               downloadButton("download_hr_line_hyrox", "Download PNG", class = "download-btn mt-3")
                           )
                  ),
                  tabPanel("🔥 Heart Rate Zones",
                           div(class = "plot-card",
                               withSpinner(plotOutput("hr_stacked_plot_hyrox", height = "450px"), type = 6),
                               downloadButton("download_hr_stacked_hyrox", "Download PNG", class = "download-btn mt-3")
                           )
                  ),
                  tabPanel("📈 Comparison to Average",
                           fluidRow(
                             div(style = "margin-top: 20px; display:flex; justify-content:center; align-items:center; gap:40px;",
                                 radioButtons(inputId = "hyrox_input_mode",label = "Choose Data Input Mode:",choices = c("Use Uploaded Garmin/TCX File" = "tcx", "Enter Times Manually" = "manual"),selected = "tcx",inline = TRUE),
                           )),
                           conditionalPanel(
                             condition = "input.hyrox_input_mode == 'tcx'",
                           div(class = "plot-card",
                               shinyWidgets::noUiSliderInput(inputId = "label_nudge",label = "Label Offset",min = 0,max = 50,value = 5,step = 1,color = "#3498db",format = wNumbFormat(decimals = 0)),
                               withSpinner(plotOutput("avg_plot_hyrox", height = "450px"), type = 6),
                               downloadButton("download_avg_hyrox", "Download PNG", class = "download-btn mt-3")
                           )
                  ),
                          conditionalPanel(
                  condition = "input.hyrox_input_mode == 'manual'",
                  fluidRow(
                      column(
                        width = 10, offset = 1,
                        h4("Enter Your HYROX Splits (MM:SS):"),
                        DTOutput("hyrox_manual_table"),
                        br(),
                        shinyWidgets::noUiSliderInput(inputId = "label_nudge_manual",label = "Label Offset",min = 0,max = 50,value = 5,step = 1,color = "#3498db",format = wNumbFormat(decimals = 0)),
                        withSpinner(plotOutput("avg_plot_hyrox_manual", height = "450px"), type = 6),
                        downloadButton("download_avg_manual_hyrox", "Download PNG", class = "download-btn mt-3")
                      )
                    ),
                  
                  
                  )),
                  tabPanel("📊 Combined Summary",
                           div(class = "plot-card",
                               withSpinner(plotOutput("combined_plot_hyrox", height = "1000px"), type = 6),
                               downloadButton("download_combined_hyrox", "Download PNG", class = "download-btn mt-3")
                           )
                  )
                  ),
                ),
      )
    )
  )
)


# --- Server ---
server <- function(input, output, session) {
  
  observeEvent(input$screen_width, {
    
    # Threshold:
    # < 992 px = phones + tablets in portrait mode
    if (!is.null(input$screen_width) && input$screen_width < 992) {
      sendSweetAlert(
        session,
        title = NULL,
        text = tags$div(
          tags$img(src = "logo_black.png", height = "40px",
                   style="display:block;margin-left:auto;margin-right:auto;margin-bottom:12px;"),
          tags$strong("Optimized for Desktop 💻"),
          tags$br(),
          "The full dashboard experience works best on a computer.",
          style="text-align:center;"
        ),
        html = TRUE,
        type = "info",
        btn_labels = "Continue"
      )
    }
  }, ignoreInit = FALSE)
  
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
  
  #-------- HIT Data ----------#
  
  # Define the reactive value to store the current file path
  current_file_hiit <- reactiveVal()
  
  # Auto-load default file on app startup
  observe({
    default_path <- "./activities/Sample_HIIT.tcx"
    if (file.exists(default_path) && is.null(current_file_hiit())) {
      current_file_hiit(default_path)
    }
  })
  
  # Handle file upload
  observe({
    req(input$tcx_file_hiit)
    current_file_hiit(input$tcx_file_hiit$datapath)
  })
  
  
  # Reactive to read TCX file
  hiit_data <- reactive({
    req(current_file_hiit())
    read_tcx_data_hiit(current_file_hiit())
  })
  
  #-------- HYROX Data ----------#
  
  # Define the reactive value to store the current file path
  current_file_hyrox <- reactiveVal()
  
  # Auto-load default file on app startup
  observe({
    default_path <- "./activities/Sample_HYROX.tcx"
    if (file.exists(default_path) && is.null(current_file_hyrox())) {
      current_file_hyrox(default_path)
    }
  })
  
  # Handle file upload
  observe({
    req(input$tcx_file_hyrox)
    current_file_hyrox(input$tcx_file_hyrox$datapath)
  })
  
  
  # Reactive to read TCX file
  hyrox_data <- reactive({
    req(current_file_hyrox())
    read_tcx_data_hyrox(current_file_hyrox())
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
  
  
  # Reactive: summarize data for plots
  hiit_summaries <- reactive({
    hd <- hiit_data()
    rounds <- length(unique(hd$phase))
    avg_total_hr <- mean(hd$heart_rate, na.rm = TRUE)
    total_hr <- max(hd$heart_rate, na.rm = TRUE)
    sizes <- get_plot_sizes(rounds)
    total_seconds <- max(hd$seconds_elapsed, na.rm = TRUE)
    total_time_formatted <- format_total_time(total_seconds)
    start_date <- format(min(hd$time, na.rm = TRUE), "%d/%m/%Y")
    summaries <- summarize_hiit_data(hd)
    list(hd = hd, sizes = sizes, total_time_formatted = total_time_formatted,
         start_date = start_date, rounds = rounds,
         avg_total_hr = avg_total_hr,
         total_hr = total_hr,
         circuit_splits = summaries$circuit_splits,
         hr_df_avg = summaries$hr_df_avg,
         hr_stacked_bar = summaries$hr_stacked_bar,
         hiit_binned = summaries$hiit_binned)
  })
  
  hyrox_summaries <- reactive({
    
    hd <- hyrox_data()
    rounds <- length(unique(hd$phase))
    avg_total_hr <- mean(hd$heart_rate, na.rm = TRUE)
    total_hr <- max(hd$heart_rate, na.rm = TRUE)
    sizes <- get_plot_sizes(rounds)
    total_seconds <- max(hd$seconds_elapsed, na.rm = TRUE)
    total_time_formatted <- format_total_time(total_seconds)
    start_date <- format(min(hd$time, na.rm = TRUE), "%d/%m/%Y")
    summaries <- summarize_hiit_data(hd)
    list(hd = hd, sizes = sizes, total_time_formatted = total_time_formatted,
         start_date = start_date, rounds = rounds,
         avg_total_hr = avg_total_hr,
         total_hr = total_hr,
         circuit_splits = summaries$circuit_splits,
         hr_df_avg = summaries$hr_df_avg,
         hr_stacked_bar = summaries$hr_stacked_bar)
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
  
  # Display activity info
  output$activity_info_hiit <- renderUI({
    req(hiit_summaries())
    hs <- hiit_summaries()
    circuits <- hs$rounds
    tags$div(style = "margin-top: 0px; margin-bottom: 0px;",
             class = "text-center text-success fw-bold",
             paste("✅ Loaded", circuits, " Circuits - Total Time:", hs$total_time_formatted)
    )
  })
  
  hyrox_template <- reactiveVal(
    data.frame(
      station = c("Run 1","SkiErg","Run 2","Sled Push","Run 3","Sled Pull","Run 4","Burpees","Run 5","Row","Run 6","Carry","Run 7","Lunges","Run 8","Wall Balls"),
      time = rep("04:30", 16),
      avg_time = c("5:02","4:35","4:44","3:27","5:08","5:05", "3:43","5:06","5:17","4:50","5:09","2:18","5:24","5:20","5:48","7:28"),
      stringsAsFactors = FALSE
    )
  )
  
  output$hyrox_manual_table <- renderDT({
    datatable(
      hyrox_template(),
      editable = list(target = "cell", disable = list(columns = c(0))), # only 'time' editable
      rownames = FALSE,
      options = list(dom = 't', pageLength = 16) # show all rows
    )
  })
  
  observeEvent(input$hyrox_manual_table_cell_edit, {
    info <- input$hyrox_manual_table_cell_edit
    df <- hyrox_template()
    df[info$row, info$col + 1] <- info$value   # DT uses 0-index for col
    hyrox_template(df)
  })
  
  manual_hyrox_data <- reactive({
    req(input$hyrox_input_mode == "manual")
    
    df <- hyrox_template()
    
    df <- df %>%
      mutate(
        seconds_elapsed = as.numeric(str_extract(time, "^[0-9]+")) * 60 +
          as.numeric(str_extract(time, "(?<=:)\\d+(\\.\\d+)?")),
        avg_seconds_elapsed = as.numeric(str_extract(avg_time, "^[0-9]+")) * 60 +
          as.numeric(str_extract(avg_time, "(?<=:)\\d+(\\.\\d+)?"))
      ) %>%
      rename(phase = station) %>%
      mutate(
        duration_mins = seconds_elapsed / 60,
        duration_formatted = sprintf("%02d:%02d", seconds_elapsed %/% 60, round(seconds_elapsed %% 60))
      ) %>%
      mutate(seconds_difference = avg_seconds_elapsed - seconds_elapsed,
             diff_sec_formatted = format_mm_ss(seconds_difference),
             faster = seconds_difference >= 0,
             cum_avg_seconds = cumsum(seconds_difference))
    
    df$phase <- factor(df$phase, levels = c("Run 1","SkiErg","Run 2","Sled Push","Run 3","Sled Pull","Run 4","Burpees","Run 5","Row","Run 6","Carry","Run 7","Lunges","Run 8","Wall Balls"))
    
    df
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
    
    p_pace <- plot_km_splits(rs$km_splits, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km) +
      labs(caption = NULL)
    
    p_hr_line <- plot_hr_line(rs$hr_df_avg, rs$total_time_formatted, rs$start_date, rs$max_km, rs$sizes, rs$max_km_longer) +
      labs(caption = NULL)
    
    p_hr_stacked <- plot_hr_stacked(rs$hr_stacked_bar, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km)
    
    combine_run_plots(p_pace, p_hr_line, p_hr_stacked)
  })
  
  
  output$runMap <- renderTmap({
    req(run_data())  # ensure data is available
    rs <- run_summaries()
    plot_run_map(rs$map_runs_df)  # pass the reactive itself
  })
  
  
  #-------- HIT Data ----------#
  
  output$circuit_time_plot <- renderPlot({
    req(hiit_summaries())
    hs <- hiit_summaries()
    
    circuit_splits_filtered <- hs$circuit_splits
    
    if (isTRUE(input$remove_warmup)) {
      circuit_splits_filtered <- hs$circuit_splits %>%
        filter(!grepl("warm", phase, ignore.case = TRUE))
    }
    
    rounds <- length(unique(circuit_splits_filtered$phase))
    
    plot_circuit_splits(circuit_splits_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds)
  })
  
  output$hr_line_plot_hiit <- renderPlot({
    req(hiit_summaries())
    hs <- hiit_summaries()
    
    hr_df_avg_filtered  <- hs$hr_df_avg
    
    if (isTRUE(input$remove_warmup)) {
      hr_df_avg_filtered <- hs$hr_df_avg %>%
        filter(!grepl("warm", phase, ignore.case = TRUE))
    }
    
    rounds <- length(unique(hr_df_avg_filtered$phase))
    
    plot_hr_line_hiit(hr_df_avg_filtered, hs$total_time_formatted, hs$start_date, rounds, hs$sizes, hs$max_km_longer)
  })
  
  output$hr_stacked_plot_hiit <- renderPlot({
    req(hiit_summaries())
    hs <- hiit_summaries()
    
    plot_hr_stacked_filtered <- hs$hr_stacked_bar
    
    if (isTRUE(input$remove_warmup)) {
      plot_hr_stacked_filtered <- hs$hr_stacked_bar %>%
        filter(!grepl("warm", phase, ignore.case = TRUE))
    }
    
    rounds <- length(unique(plot_hr_stacked_filtered$phase))
    
    plot_hr_stacked_hiit(plot_hr_stacked_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds)
  })
  
  output$hr_plot_bins <- renderPlot({
    req(hiit_summaries())
    hs <- hiit_summaries()
    
    plot_hiit_binned_filtered <- hs$hiit_binned

    if (isTRUE(input$remove_warmup)) {
      plot_hiit_binned_filtered <- hs$hiit_binned %>%
        filter(!grepl("warm", phase, ignore.case = TRUE))
    }

    rounds <- length(unique(plot_hiit_binned_filtered$phase))
    
    plot_hr_binned(plot_hiit_binned_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds, hs$avg_total_hr, hs$total_hr)
  })
  
  
  output$combined_plot_hiit <- renderPlot({
    req(hiit_summaries())
    hs <- hiit_summaries()

    hr_df_avg_filtered <- hs$hr_df_avg
    plot_hr_stacked_filtered <- hs$hr_stacked_bar
    circuit_splits_filtered <- hs$circuit_splits
    plot_hiit_binned_filtered <- hs$hiit_binned

    if (isTRUE(input$remove_warmup)) {

      hr_df_avg_filtered <- hs$hr_df_avg %>%
        filter(!grepl("warm", phase, ignore.case = TRUE))

      plot_hr_stacked_filtered <- hs$hr_stacked_bar %>%
        filter(!grepl("warm", phase, ignore.case = TRUE))

      circuit_splits_filtered <- hs$circuit_splits %>%
        filter(!grepl("warm", phase, ignore.case = TRUE))

       plot_hiit_binned_filtered <- hs$hiit_binned %>%
         filter(!grepl("warm", phase, ignore.case = TRUE))
    }

    rounds <- length(unique(circuit_splits_filtered$phase))

    p_circuit_hiit <- plot_circuit_splits(circuit_splits_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds) + labs(caption = NULL)
    p_hr_line_hiit <- plot_hr_line_hiit(hr_df_avg_filtered, hs$total_time_formatted, hs$start_date, rounds, hs$sizes, hs$max_km_longer) + labs(caption = NULL)
    p_hr_stacked_hiit <- plot_hr_stacked_hiit(plot_hr_stacked_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds) + labs(caption = NULL)
    p_hr_bins_hiit <- plot_hr_binned(plot_hiit_binned_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds, hs$avg_total_hr, hs$total_hr)

    combine_hyrox_plots(p_circuit_hiit, p_hr_line_hiit, p_hr_stacked_hiit, p_hr_bins_hiit)
  })
  
  # --- HYROX ---
  

  output$circuit_time_hyrox <- renderPlot({
    req(hyrox_summaries())
    hs <- hyrox_summaries()
    
    
    plot_circuit_splits(hs$circuit_splits, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds)
  })
  
  output$hr_line_plot_hyrox <- renderPlot({
    req(hyrox_summaries())
    hs <- hyrox_summaries()
    
    
    plot_hr_line_hiit(hs$hr_df_avg, hs$total_time_formatted, hs$start_date, hs$rounds, hs$sizes, hs$max_km_longer)
  })
  
  output$hr_stacked_plot_hyrox <- renderPlot({
    req(hyrox_summaries())
    hs <- hyrox_summaries()

    plot_hr_stacked_hiit(hs$hr_stacked_bar, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds)
  })
  
  
  output$avg_plot_hyrox <- renderPlot({
    req(hyrox_summaries())
    hs <- hyrox_summaries()
    
    plot_hyrox_average(hs$circuit_splits, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds, nudge_text = input$label_nudge)
  })
  
  #### Manual #####
  
  output$avg_plot_hyrox_manual <- renderPlot({
    req(manual_hyrox_data())
    
    plot_hyrox_average_manual(manual_hyrox_data(), nudge_text = input$label_nudge_manual)
  })
  
  output$combined_plot_hyrox <- renderPlot({
    req(hyrox_summaries())
    hs <- hyrox_summaries()

    p_circuit_hyrox <- plot_circuit_splits(hs$circuit_splits, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds) + labs(caption = NULL)
    if (input$hyrox_input_mode != "manual") {
    p_average_hyrox <- plot_hyrox_average(hs$circuit_splits, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds, nudge_text = input$label_nudge) + labs(caption = NULL)
    } else {
      p_average_hyrox <- plot_hyrox_average_manual(manual_hyrox_data(), nudge_text = input$label_nudge_manual) + labs(caption = NULL)
    }
    p_hr_line_hyrox <- plot_hr_line_hiit(hs$hr_df_avg, hs$total_time_formatted, hs$start_date, hs$rounds, hs$sizes, hs$max_km_longer) + labs(caption = NULL)
    p_hr_stacked_hyrox <- plot_hr_stacked_hiit(hs$hr_stacked_bar, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds)

    combine_hyrox_plots(p_circuit_hyrox, p_average_hyrox, p_hr_line_hyrox, p_hr_stacked_hyrox)
  })
  
  
  # --- Download Handlers ---
  output$download_pace <- downloadHandler(
    filename = function() "pace_plot.png",
    content = function(file) {
      rs <- run_summaries()
      ggsave(file, plot = plot_km_splits(rs$km_splits, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_hr_line <- downloadHandler(
    filename = function() "hr_line_plot.png",
    content = function(file) {
      rs <- run_summaries()
      ggsave(file, plot = plot_hr_line(rs$hr_df_avg, rs$total_time_formatted, rs$start_date, rs$max_km, rs$sizes, rs$max_km_longer),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_hr_stacked <- downloadHandler(
    filename = function() "hr_stacked_plot.png",
    content = function(file) {
      rs <- run_summaries()
      ggsave(file, plot = plot_hr_stacked(rs$hr_stacked_bar, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_combined <- downloadHandler(
    filename = function() "combined_plot.png",
    content = function(file) {
      rs <- run_summaries()
      p_pace <- plot_km_splits(rs$km_splits, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km) + labs(caption = NULL)
      p_hr_line <- plot_hr_line(rs$hr_df_avg, rs$total_time_formatted, rs$start_date, rs$max_km, rs$sizes, rs$max_km_longer) + labs(caption = NULL)
      p_hr_stacked <- plot_hr_stacked(rs$hr_stacked_bar, rs$total_time_formatted, rs$start_date, rs$sizes, rs$max_km_longer, rs$max_km) + labs(caption = NULL)
      ggsave(file, plot = combine_run_plots(p_pace, p_hr_line, p_hr_stacked),
             width = 10, height = 12, dpi = 300)
    }
  )
  
  #-------- HIT Data ----------#
  
  output$download_circuit <- downloadHandler(
    filename = function() "circuit_plot.png",
    content = function(file) {
      
      hs <- hiit_summaries()
      
      circuit_splits_filtered <- hs$circuit_splits
      
      if (isTRUE(input$remove_warmup)) {
        circuit_splits_filtered <- hs$circuit_splits %>%
          filter(!grepl("warm", phase, ignore.case = TRUE))
      }
      
      rounds <- length(unique(circuit_splits_filtered$phase))
      
      ggsave(file, plot = plot_circuit_splits(circuit_splits_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_hr_line_hiit <- downloadHandler(
    filename = function() "hr_line_plot_hiit.png",
    content = function(file) {
      
      hs <- hiit_summaries()
      
      hr_df_avg_filtered  <- hs$hr_df_avg
      
      if (isTRUE(input$remove_warmup)) {
        hr_df_avg_filtered <- hs$hr_df_avg %>%
          filter(!grepl("warm", phase, ignore.case = TRUE))
      }
      
      rounds <- length(unique(hr_df_avg_filtered$phase))
      
      ggsave(file, plot = plot_hr_line_hiit(hr_df_avg_filtered, hs$total_time_formatted, hs$start_date, rounds, hs$sizes, hs$max_km_longer),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_hr_stacked_hiit <- downloadHandler(
    filename = function() "hr_stacked_plot_hiit.png",
    content = function(file) {
      
      hs <- hiit_summaries()
      
      plot_hr_stacked_filtered <- hs$hr_stacked_bar
      
      if (isTRUE(input$remove_warmup)) {
        plot_hr_stacked_filtered <- hs$hr_stacked_bar %>%
          filter(!grepl("warm", phase, ignore.case = TRUE))
      }
      
      rounds <- length(unique(plot_hr_stacked_filtered$phase))
      
      ggsave(file, plot = plot_hr_stacked_hiit(plot_hr_stacked_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_combined_hiit <- downloadHandler(
    filename = function() "combined_plot_hiit.png",
    content = function(file) {
      hs <- hiit_summaries()
      
      hr_df_avg_filtered <- hs$hr_df_avg
      plot_hr_stacked_filtered <- hs$hr_stacked_bar
      circuit_splits_filtered <- hs$circuit_splits
      plot_hiit_binned_filtered <- hs$hiit_binned
      
      if (isTRUE(input$remove_warmup)) {
        
        hr_df_avg_filtered <- hs$hr_df_avg %>%
          filter(!grepl("warm", phase, ignore.case = TRUE))
        
        plot_hr_stacked_filtered <- hs$hr_stacked_bar %>%
          filter(!grepl("warm", phase, ignore.case = TRUE))
        
        circuit_splits_filtered <- hs$circuit_splits %>%
          filter(!grepl("warm", phase, ignore.case = TRUE))
        
        plot_hiit_binned_filtered <- hs$hiit_binned %>%
          filter(!grepl("warm", phase, ignore.case = TRUE))
      }
      
      rounds <- length(unique(circuit_splits_filtered$phase))
      
      p_circuit_hiit <- plot_circuit_splits(circuit_splits_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds) + labs(caption = NULL)
      p_hr_line_hiit <- plot_hr_line_hiit(hr_df_avg_filtered, hs$total_time_formatted, hs$start_date, rounds, hs$sizes, hs$max_km_longer) + labs(caption = NULL)
      p_hr_stacked_hiit <- plot_hr_stacked_hiit(plot_hr_stacked_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds) + labs(caption = NULL)
      p_hr_bins_hiit <- plot_hr_binned(plot_hiit_binned_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds, hs$avg_total_hr, hs$total_hr)
      
      ggsave(file, plot = combine_hyrox_plots(p_circuit_hiit, p_hr_line_hiit, p_hr_stacked_hiit, p_hr_bins_hiit),
             width = 10, height = 12, dpi = 300)
    }
  )
  
  output$download_hr_bins_hiit <- downloadHandler(
    filename = function() "hr_bins_hiit.png",
    content = function(file) {
      
      hs <- hiit_summaries()
      
      plot_hiit_binned_filtered <- hs$hiit_binned
      
      if (isTRUE(input$remove_warmup)) {
        plot_hiit_binned_filtered <- hs$hiit_binned %>%
          filter(!grepl("warm", phase, ignore.case = TRUE))
      }
      
      rounds <- length(unique(plot_hiit_binned_filtered$phase))
      
      ggsave(file, plot = plot_hr_binned(plot_hiit_binned_filtered, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, rounds, hs$avg_total_hr, hs$total_hr),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_circuit_hyrox <- downloadHandler(
    filename = function() "hr_bins_hyrox.png",
    content = function(file) {
    
      hs <- hyrox_summaries()
      
      
      ggsave(file, plot = plot_circuit_splits(hs$circuit_splits, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_hr_line_hyrox <- downloadHandler(
    filename = function() "hr_line_hyrox.png",
    content = function(file) {
      
      hs <- hyrox_summaries()
      
      
      ggsave(file, plot = plot_hr_line_hiit(hs$hr_df_avg, hs$total_time_formatted, hs$start_date, hs$rounds, hs$sizes, hs$max_km_longer),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_hr_stacked_hyrox <- downloadHandler(
    filename = function() "hr_stacked_hyrox.png",
    content = function(file) {
      
      hs <- hyrox_summaries()
      
      
      ggsave(file, plot = plot_hr_stacked_hiit(hs$hr_stacked_bar, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds),
             width = 8, height = 6, dpi = 300)
    }
  )
  
  
  output$download_avg_hyrox <- downloadHandler(
    filename = function() "avg_hyrox.png",
    content = function(file) {
      
      hs <- hyrox_summaries()
      
      ggsave(file, plot = plot_hyrox_average(hs$circuit_splits, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds, nudge_text = input$label_nudge),
             width = 8, height = 6, dpi = 300)
      
    }
  )
  
  output$download_avg_manual_hyrox <- downloadHandler(
    filename = function() "avg_manual_hyrox.png",
    content = function(file) {
      
      ggsave(file, plot = plot_hyrox_average_manual(manual_hyrox_data(), nudge_text = input$label_nudge_manual),
             width = 8, height = 6, dpi = 300)
      
    }
  )
  
  output$download_combined_hyrox <- downloadHandler(
    filename = function() "combined_hyrox.png",
    content = function(file) {
      
      hs <- hyrox_summaries()
      
      p_circuit_hyrox <- plot_circuit_splits(hs$circuit_splits, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds) + labs(caption = NULL)
      if (input$hyrox_input_mode != "manual") {
        p_average_hyrox <- plot_hyrox_average(hs$circuit_splits, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds, nudge_text = input$label_nudge) + labs(caption = NULL)
      } else {
        p_average_hyrox <- plot_hyrox_average_manual(manual_hyrox_data(), nudge_text = input$label_nudge_manual) + labs(caption = NULL)
      }
      p_hr_line_hyrox <- plot_hr_line_hiit(hs$hr_df_avg, hs$total_time_formatted, hs$start_date, hs$rounds, hs$sizes, hs$max_km_longer) + labs(caption = NULL)
      p_hr_stacked_hyrox <- plot_hr_stacked_hiit(hs$hr_stacked_bar, hs$total_time_formatted, hs$start_date, hs$sizes, hs$max_km_longer, hs$rounds)
      
      ggsave(file, plot = combine_hyrox_plots(p_circuit_hyrox, p_average_hyrox, p_hr_line_hyrox, p_hr_stacked_hyrox),
             width = 10, height = 12, dpi = 300)
    }
  )
  
}

shinyApp(ui, server)