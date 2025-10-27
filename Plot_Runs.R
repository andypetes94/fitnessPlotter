#!/usr/bin/env Rscript
# ---- Garmin Run Plot Generator ----
# Usage: Rscript Plot_Runs.R <tcx_files_or_dirs> [output_root]

# ---- Handle Command-Line Arguments ----
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0 || args[1] %in% c("-h", "--help")) {
  cat("
Usage: Rscript Plot_Runs.R <tcx_files_or_dirs> [output_root]

Arguments:
  <tcx_files_or_dirs>  One or more .tcx files or directories containing .tcx files
  [output_root]        (Optional) Directory where charts will be saved. Defaults to ./Charts

Example:
  Rscript Plot_Runs.R ./activities/*.tcx ./Charts
  Rscript Plot_Runs.R ./activities/ ./Charts
  Rscript Plot_Runs.R ./activities/*.tcx
\n")
  quit(save = "no")
}

# ---- Argument Parsing ----
if (length(args) == 1) {
  # Single input argument → can be file or wildcard expansion
  input_paths <- args
  output_root <- "./Charts"
  cat("ℹ️ No output directory specified — using default: ./Charts\n")
} else {
  # Last argument is only considered output root if it is a directory
  potential_output <- args[length(args)]
  if (dir.exists(potential_output)) {
    output_root <- potential_output
    input_paths <- args[-length(args)]
  } else if (!grepl("\\.tcx$", potential_output)) {
    # Treat as new output directory (will create it)
    output_root <- potential_output
    input_paths <- args[-length(args)]
  } else {
    # Last argument looks like a .tcx file → treat all args as input, use default Charts
    input_paths <- args
    output_root <- "./Charts"
    cat("ℹ️ Last argument looks like a .tcx file — using default output root ./Charts\n")
  }
}

if (!dir.exists(output_root)) dir.create(output_root, recursive = TRUE)


# ---- Expand directories into .tcx files ----
input_files <- c()
for (path in input_paths) {
  if (dir.exists(path)) {
    tcx_files <- list.files(path, pattern = "\\.tcx$", full.names = TRUE)
    if (length(tcx_files) == 0) {
      warning("⚠️ No .tcx files found in directory:", path)
    }
    input_files <- c(input_files, tcx_files)
  } else if (file.exists(path)) {
    input_files <- c(input_files, path)
  } else {
    warning("⚠️ Path does not exist:", path)
  }
}

if (length(input_files) == 0) {
  stop("❌ No valid .tcx files to process.")
}

# ---- Load Dependencies ----
suppressPackageStartupMessages({
  library(xml2)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(tools)
})

# ---- Load helper functions ----
source("R/Plot_Runs.R")

# ---- Main Processing Loop ----
for (file in input_files) {
  activity_name <- file_path_sans_ext(basename(file))
  tcx_xml <- read_xml(file)
  ns <- xml_ns(tcx_xml)
  sport_node <- xml_find_first(tcx_xml, ".//d1:Activity", ns)
  sport_type <- xml_attr(sport_node, "Sport")
  
  if (is.na(sport_type)) {
    cat("⚠️  Skipping", activity_name, "- no sport type found.\n")
    next
  }
  
  if (tolower(sport_type) != "running") {
    cat("⚠️  Skipping", activity_name, "- not a running activity (found:", sport_type, ")\n")
    next
  }
  
  cat("🏃 Processing:", activity_name, "-", sport_type, "\n")
  
  # Create output directory
  output_dir <- file.path(output_root, activity_name)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # ---- Parse TCX Data ----
  run_data <- read_tcx_data(file)
  start_time <- min(run_data$time, na.rm = TRUE)
  start_date <- format(start_time, "%d/%m/%Y")
  total_seconds <- max(run_data$time_elapsed, na.rm = TRUE)
  total_time_formatted <- format_total_time(total_seconds)
  
  max_km <- max(ceiling(run_data$distance / 1000), na.rm = TRUE)
  max_km_longer <- round(tail(run_data$distance / 1000, 1), 2)
  sizes <- get_plot_sizes(max_km)
  summaries <- summarize_run_data(run_data)
  
  # ---- Generate Plots ----
  p_pace <- plot_km_splits(summaries$km_splits, total_time_formatted, start_date, sizes, max_km_longer)
  p_hr_line <- plot_hr_line(summaries$hr_df_avg, total_time_formatted, start_date, max_km, sizes, max_km_longer)
  p_hr_stacked <- plot_hr_stacked(summaries$hr_stacked_bar, total_time_formatted, start_date, sizes, max_km_longer)
  combined_run <- combine_run_plots(p_pace, p_hr_line, p_hr_stacked)
  
  # ---- Save Charts ----
  ggsave(file.path(output_dir, paste0(activity_name, "_pace.png")), plot = p_pace, dpi = 900, width = 8, height = 6)
  ggsave(file.path(output_dir, paste0(activity_name, "_hr_line.png")), plot = p_hr_line, dpi = 900, width = 8, height = 6)
  ggsave(file.path(output_dir, paste0(activity_name, "_hr_stacked.png")), plot = p_hr_stacked, dpi = 900, width = 8, height = 6)
  ggsave(file.path(output_dir, paste0(activity_name, "_combined.png")), plot = combined_run, dpi = 900, width = 10, height = 12)
  
  cat("✅ Saved charts to:", output_dir, "\n\n")
}

cat("🎉 All activities processed successfully!\n")