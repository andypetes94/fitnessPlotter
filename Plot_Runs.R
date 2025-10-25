#!/usr/bin/env Rscript

# ---- Parse command-line arguments ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript Plot_Runs.R <tcx_files> <output_root>")

# Print help if no args or --help is provided
if (length(args) == 0 || args[1] %in% c("-h", "--help")) {
  cat("
Usage: Rscript Plot_Runs.R <tcx_files> <output_root>

Arguments:
  <tcx_files>   One or more .tcx files (or wildcard like ./activities/*.tcx)
  <output_root> Directory where charts will be saved

Example:
  Rscript Plot_Runs.R ./activities/*.tcx ./Charts
\n")
  quit(save = "no")
}

input_files <- args[-length(args)]
output_root <- args[length(args)]

if (!dir.exists(output_root)) dir.create(output_root, recursive = TRUE)


# Script
library(xml2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(showtext)
library(shadowtext)
library(ggborderline)
library(ggtext)
library(fontawesome)
library(rvest)
library(patchwork)
library(tools)

fa_font <- "Font Awesome 7 Free"

for (file in input_files) {
  
  activity_name <- file_path_sans_ext(basename(file))
  tcx_xml <- read_xml(file)
  
  # ✅ Detect sport type
  ns <- xml_ns(tcx_xml)
  sport_node <- xml_find_first(tcx_xml, ".//d1:Activity", ns)
  sport_type <- xml_attr(sport_node, "Sport")
  
  
  if (is.na(sport_type)) {
    cat("Skipping", activity_name, "- no sport type found.\n")
    next
  }
  
  if (tolower(sport_type) != "running") {
    cat("Skipping", activity_name, "- not a running activity (found:", sport_type, ")\n")
    next
  }
  
  cat("Processing:", activity_name, "-", sport_type, "\n")
  
  output_dir <- file.path(output_root, activity_name)
  if (!dir.exists(output_dir)) dir.create(output_dir)
# Load TCX and strip namespaces

tcx_xml <- read_xml(file)
xml_ns_strip(tcx_xml)

# Get all trackpoints
trackpoints <- xml_find_all(tcx_xml, ".//Trackpoint")

# Extract data per trackpoint
run_data <- tibble(
  time = sapply(trackpoints, function(x) xml_text(xml_find_first(x, ".//Time"))),
  latitude = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//LatitudeDegrees")))),
  longitude = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//LongitudeDegrees")))),
  altitude = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//AltitudeMeters")))),
  distance = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//DistanceMeters")))),
  heart_rate = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//HeartRateBpm/Value")))),
  #cadence = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//Cadence"))))
)

# Convert time to POSIXct
run_data <- run_data %>%
  mutate(time = as.POSIXct(time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")) %>%
  # Compute elapsed time in seconds since the first trackpoint
  mutate(time_elapsed = as.numeric(difftime(time, min(time), units = "secs"))) %>%
  mutate(Zone_Label = case_when(heart_rate > 171 ~ "Zone 5",
                                heart_rate > 153 & heart_rate <= 171 ~ "Zone 4",
                                heart_rate> 134 & heart_rate <= 153 ~ "Zone 3",
                                heart_rate> 115 & heart_rate <= 134 ~ "Zone 2",
                                T ~ "Zone 1",
  )) %>%
  mutate(kilometre = paste0("KM ", ceiling(distance / 1000))) %>%
  filter(kilometre != "KM 0" & kilometre != max(kilometre))

run_data$kilometre <- factor(run_data$kilometre, levels = unique(run_data$kilometre))

start_time <- min(run_data$time, na.rm = TRUE)

start_date <- format(start_time, "%d/%m/%Y")             # "2025-10-24"
start_clock <- format(start_time, "%H:%M")  # "08:15:30"


# Total run time in seconds
total_seconds <- max(run_data$time_elapsed, na.rm = TRUE)

# Convert to minutes and seconds
hours <- total_seconds %/% 3600
minutes <- total_seconds %/% 60        # integer division → minutes
seconds <- round(total_seconds %% 60) # remainder → seconds


# Conditional formatting
if (hours > 0) {
  total_time_formatted <- sprintf("%d:%02d:%02d", hours, minutes, seconds)
} else {
  total_time_formatted <- sprintf("%02d:%02d", minutes, seconds)
}

total_time_formatted

max_km <- max(ceiling(run_data$distance / 1000), na.rm = TRUE)
max_km

# Adjust sizes based on distances
if (max_km < 10){
  stacked_text <- 6
  stacked_axis_text <- 12
  bar_text <- 6
  geom_point_size <- 12
  geom_point_stroke <- 1.5
  point_text_size <- 4
  geom_line_width <- 6
} else if (max_km > 10 & max_km < 20) {
  stacked_text <- 5
  stacked_axis_text <- 11
  bar_text <- 4
  geom_point_size <- 12
  geom_point_stroke <- 1.5
  point_text_size <- 4
  geom_line_width <- 5
} else if (max_km >= 20 & max_km < 25) {
  stacked_text <- 4 
  stacked_axis_text <- 10
  bar_text <- 3
  geom_point_size <- 10
  geom_point_stroke <- 1
  point_text_size <- 3.5
  geom_line_width <- 4
} else if (max_km >= 25 & max_km < 30) {
  stacked_text <- 3
  stacked_axis_text <- 8
  bar_text <- 2
  geom_point_size <- 8
  geom_point_stroke <- 1
  point_text_size <- 3
  geom_line_width <- 4
} else if (max_km >= 30) {
  stacked_text <- 0
  stacked_axis_text <- 6
  bar_text <- 0
  geom_point_size <- 6
  geom_point_stroke <- 0.5
  point_text_size <- 2.5
  geom_line_width <- 6
} else {
  stacked_text <- 0
  stacked_axis_text <- 0
  bar_text <- 0
  geom_point_size <- 0
  geom_point_stroke <- 0
  point_text_size <- 0
  geom_line_width <- 0
}

hr_stacked_bar <- run_data %>%
  mutate(Colour_Code = case_when(!Zone_Label %in% c("Zone 5","Zone 4","Zone 3") ~ "Other",
                                 TRUE ~ Zone_Label)) %>%
  mutate(Colour_Code = factor(Colour_Code, levels = c("Zone 5","Zone 4","Zone 3","Other"))) %>%
  group_by(kilometre, Colour_Code) %>%
  summarise(avg_hr = mean(heart_rate),
            avg_count = n(),
            .groups = "drop") %>%
  #na.omit() %>%
  ungroup() %>%
  group_by(kilometre) %>%
  mutate(total_count = sum(avg_count),
         prop = (avg_count / sum(avg_count))*100) %>%
  ungroup()


hr_df_avg <- run_data %>%
  group_by(kilometre) %>%
  summarise(avg_hr = mean(heart_rate),
            .groups = "drop") %>%
  mutate(Zone_Label = case_when(avg_hr > 171 ~ "Zone 5",
                                avg_hr > 153 & avg_hr <= 171 ~ "Zone 4",
                                avg_hr > 134 & avg_hr <= 153 ~ "Zone 3",
                                avg_hr > 115 & avg_hr <= 134 ~ "Zone 2",
                                T ~ "Zone 1",
  ))

km_splits <- run_data %>%
  group_by(kilometre) %>%
  summarise(
    duration_secs = max(time_elapsed, na.rm = TRUE) - min(time_elapsed, na.rm = TRUE),
    avg_hr = mean(heart_rate, na.rm = TRUE)
  ) %>%
  mutate(duration_mins = duration_secs / 60) %>%
  mutate(duration_formatted = sprintf("%02d:%02d", duration_secs %/% 60, round(duration_secs %% 60)))




p4 <- ggplot(hr_stacked_bar, aes(x = kilometre, y = avg_count, fill = Colour_Code)) +
  geom_vline(xintercept = seq(0.5, 20, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
  geom_bar(position="fill", stat="identity", color = NA) +
  geom_text(aes(label = paste0(round(prop, digits = 0), "%")), position = position_fill(vjust = 0.5), color = "white", size = stacked_text, family = "Lato", fontface = "bold",) +
  scale_fill_manual(breaks = c("Other","Zone 3","Zone 4","Zone 5"),
                    values = c("gray","#FFB200","#EB5B00","#D91656")) +
  labs(subtitle = paste0("\uf7e4","<span style='font-family: Lato'> Proportion of Time in HR Zones | Time: ", total_time_formatted, " | ", start_date, " <span/>")
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_markdown(family = "Lato", size = 20, face = "bold"),
    plot.subtitle = element_markdown(family = fa_font, size = 22, color = "black"),
    axis.title = element_text(family = "Lato", size = 16, face = "bold"),
    axis.text = element_text(family = "Lato", size = 12),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(family = "Lato", size = stacked_axis_text, face = "bold"),
    axis.title.x = element_blank(),
    panel.background = element_rect(color = "white", fill = "white"),
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    legend.text = element_text(family = "Lato", size = 14, color = "black"),
    legend.title = element_blank(),
    #legend.key.size = unit(0.5, "cm")
  )




p4

ggsave(file.path(output_dir, paste0(activity_name, "_hr_stacked.png")), plot = p4, dpi = 900, width = 8, height = 6)


p5 <- ggplot(hr_df_avg, aes(x = kilometre, y = avg_hr, color = Zone_Label, fill = Zone_Label)) +
  #geom_bar(stat = "identity", fill = "green", color = NA) +
  geom_vline(xintercept = seq(0.5, 100, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
  geom_hline(yintercept =  c(134,153,171), color = "gray60") +
  # Zone 3 Colour
  geom_rect(data = hr_df_avg, aes(xmin = 0.5, xmax = max_km + 0.5, ymin = 134, ymax = 153),  fill = "#FFB200", color = NA, alpha = 0.05, show.legend = F) +
  geom_rect(data = hr_df_avg, aes(xmin = 0.5, xmax = max_km + 0.5, ymin = 153, ymax = 171),  fill = "#EB5B00", color = NA, alpha = 0.05, show.legend = F) +
  geom_rect(data = hr_df_avg, aes(xmin = 0.5, xmax = max_km + 0.5, ymin = 171, ymax = 190),  fill = "#D91656", color = NA, alpha = 0.05, show.legend = F) +
  
  # Add Line Shadow / Glow
  geom_line(group = 1, color = 'black', linewidth = geom_line_width, alpha = 0.2) +
  # Add Line
  geom_line(group = 1, color = 'white',  linewidth = (geom_line_width*(2/3))) +
  geom_point(size = geom_point_size + (geom_point_size*0.5),  alpha = 0.2, show.legend = F) +
  
  geom_point(shape = 21, size = geom_point_size, color = "white", stroke = geom_point_stroke, show.legend = F) +
  geom_text(aes(label = round(avg_hr, digits = 0)), size = point_text_size, show.legend = F, color = "white", family = "Lato", fontface = "bold") +
  
  # Scale Axis
  scale_y_continuous(limits = c(134,190)) +
  #scale_color_gradient(low = "#FB9E3A", high = "#EA2F14") +
  scale_fill_manual(breaks = c("Zone 1","Zone 3","Zone 4","Zone 5"),
                    values = c("blue","#FFB200","#EB5B00","#D91656")) +
  scale_color_manual(breaks = c("Zone 1","Zone 3","Zone 4","Zone 5"),
                     values = c("blue","#FFB200","#EB5B00","#D91656")) +
  labs(subtitle = paste0("\uf21e","<span style='font-family: Lato'> Average Heart Rate Data | Time: ", total_time_formatted, " | ", start_date, " <span/>")
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_markdown(family = "Lato", size = 20, face = "bold"),
    plot.subtitle = element_markdown(family = fa_font, size = 22, color = "black"),
    axis.title = element_text(family = "Lato", size = 16, face = "bold"),
    axis.text = element_text(family = "Lato", size = 12),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_rect(color = "white", fill = "white"),
    plot.background = element_rect(color = "white", fill = "white"),
  )



p5

ggsave(file.path(output_dir, paste0(activity_name, "_hr_line.png")), plot = p5, dpi = 900, width = 8, height = 6)



library(ggplot2)

p <- ggplot(km_splits, aes(x = kilometre, y = duration_mins, fill = duration_mins)) +
  geom_col(show.legend = F) +
  geom_vline(xintercept = seq(0.5, 100, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
  geom_hline(yintercept =  4, color = "gray20", linewidth = 1.5, linetype = "dashed") +
  geom_text(aes(label = duration_formatted, y = 0.4), size = bar_text, show.legend = F, color = "white", family = "Lato", fontface = "bold") +
  scale_fill_gradient2(high = '#8cc5e3', mid = '#3594cc', midpoint = 4, low = '#2066a8') +
  labs(subtitle = paste0("\uf2f2","<span style='font-family: Lato'> Kilometre Splits | Time: ", total_time_formatted, " | ", start_date, " <span/>")) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_markdown(family = "Lato", size = 20, face = "bold"),
    plot.subtitle = element_markdown(family = fa_font, size = 22, color = "black"),
    axis.title = element_text(family = "Lato", size = 16, face = "bold"),
    axis.text = element_text(family = "Lato", size = 12),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_rect(color = "white", fill = "white"),
    plot.background = element_rect(color = "white", fill = "white"),
  )

p

ggsave(file.path(output_dir, paste0(activity_name, "_pace.png")), plot = p, dpi = 900, width = 8, height = 6)


combined_run <- p / p5 / p4

combined_run

ggsave(file.path(output_dir, paste0(activity_name, "_combined.png")), plot = combined_run, dpi = 900, width = 10, height = 12)

cat("Saved charts to:", output_dir, "\n\n")

}

cat("✅ All activities processed successfully!\n")