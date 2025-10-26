# ---- Load required libraries ----
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

# ---- Font setup ----
fa_font <- "Font Awesome 7 Free"

# ---- 1. Read and parse TCX file ----
read_tcx_data <- function(file) {
  tcx_xml <- read_xml(file)
  xml_ns_strip(tcx_xml)
  
  trackpoints <- xml_find_all(tcx_xml, ".//Trackpoint")
  
  run_data <- tibble(
    time = sapply(trackpoints, function(x) xml_text(xml_find_first(x, ".//Time"))),
    latitude = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//LatitudeDegrees")))),
    longitude = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//LongitudeDegrees")))),
    altitude = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//AltitudeMeters")))),
    distance = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//DistanceMeters")))),
    heart_rate = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//HeartRateBpm/Value"))))
  )
  
  run_data <- run_data %>%
    mutate(
      time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      Zone_Label = case_when(
        heart_rate > 171 ~ "Zone 5",
        heart_rate > 153 ~ "Zone 4",
        heart_rate > 134 ~ "Zone 3",
        heart_rate > 115 ~ "Zone 2",
        TRUE ~ "Zone 1"
      ),
      kilometre = paste0("KM ", ceiling(distance / 1000))
    ) %>%
    {
      # --- Conditional filtering for last kilometre ---
      max_dist <- max(.$distance, na.rm = TRUE)
      remainder <- max_dist %% 1000
      
      if (remainder < 50) {
        # Drop KM 0 and the last incomplete kilometre
        filter(., kilometre != "KM 0" & kilometre != paste0("KM ", ceiling(max_dist / 1000)))
      } else {
        # Drop only KM 0
        filter(., kilometre != "KM 0")
      }
    } %>%
    mutate(time_elapsed = as.numeric(difftime(time, min(time), units = "secs")))
  
  run_data$kilometre <- factor(run_data$kilometre, levels = unique(run_data$kilometre))
  return(run_data)
}

# ---- 2. Format total run time ----
format_total_time <- function(total_seconds) {
  hours <- total_seconds %/% 3600
  minutes <- (total_seconds %% 3600) %/% 60
  seconds <- round(total_seconds %% 60)
  
  if (hours > 0) {
    return(sprintf("%d:%02d:%02d", hours, minutes, seconds))
  } else {
    return(sprintf("%02d:%02d", minutes, seconds))
  }
}

# ---- 3. Adjust plot sizes dynamically ----
get_plot_sizes <- function(max_km) {
  if (max_km < 10) {
    return(list(stacked_text = 6, stacked_axis_text = 12, bar_text = 6,
                geom_point_size = 12, geom_point_stroke = 1.5,
                point_text_size = 4, geom_line_width = 6))
  } else if (max_km > 10 & max_km < 20) {
    return(list(stacked_text = 5, stacked_axis_text = 11, bar_text = 4,
                geom_point_size = 12, geom_point_stroke = 1.5,
                point_text_size = 4, geom_line_width = 5))
  } else if (max_km >= 20 & max_km < 25) {
    return(list(stacked_text = 4, stacked_axis_text = 10, bar_text = 3,
                geom_point_size = 10, geom_point_stroke = 1,
                point_text_size = 3.5, geom_line_width = 4))
  } else if (max_km >= 25 & max_km < 30) {
    return(list(stacked_text = 3, stacked_axis_text = 8, bar_text = 2,
                geom_point_size = 8, geom_point_stroke = 1,
                point_text_size = 3, geom_line_width = 4))
  } else if (max_km >= 30) {
    return(list(stacked_text = 0, stacked_axis_text = 6, bar_text = 0,
                geom_point_size = 6, geom_point_stroke = 0.5,
                point_text_size = 2.5, geom_line_width = 6))
  } else {
    return(list(stacked_text = 0, stacked_axis_text = 0, bar_text = 0,
                geom_point_size = 0, geom_point_stroke = 0,
                point_text_size = 0, geom_line_width = 0))
  }
}

# ---- 4. Summarize data for plots ----
summarize_run_data <- function(run_data) {
  hr_stacked_bar <- run_data %>%
    mutate(Colour_Code = case_when(
      !Zone_Label %in% c("Zone 5", "Zone 4", "Zone 3") ~ "Other",
      TRUE ~ Zone_Label
    )) %>%
    mutate(Colour_Code = factor(Colour_Code, levels = c("Zone 5", "Zone 4", "Zone 3", "Other"))) %>%
    group_by(kilometre, Colour_Code) %>%
    summarise(avg_hr = mean(heart_rate), avg_count = n(), .groups = "drop") %>%
    group_by(kilometre) %>%
    mutate(total_count = sum(avg_count), prop = (avg_count / total_count) * 100) %>%
    ungroup()
  
  hr_df_avg <- run_data %>%
    group_by(kilometre) %>%
    summarise(avg_hr = mean(heart_rate), .groups = "drop") %>%
    mutate(Zone_Label = case_when(
      avg_hr > 171 ~ "Zone 5",
      avg_hr > 153 ~ "Zone 4",
      avg_hr > 134 ~ "Zone 3",
      avg_hr > 115 ~ "Zone 2",
      TRUE ~ "Zone 1"
    ))
  
  km_splits <- run_data %>%
    group_by(kilometre) %>%
    summarise(
      duration_secs = max(time_elapsed, na.rm = TRUE) - min(time_elapsed, na.rm = TRUE),
      avg_hr = mean(heart_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      duration_mins = duration_secs / 60,
      duration_formatted = sprintf("%02d:%02d", duration_secs %/% 60, round(duration_secs %% 60))
    )
  
  return(list(hr_stacked_bar = hr_stacked_bar, hr_df_avg = hr_df_avg, km_splits = km_splits))
}

# ---- 5. Plot functions ----
plot_hr_stacked <- function(hr_stacked_bar, total_time_formatted, start_date, sizes, max_km_longer) {
  ggplot(hr_stacked_bar, aes(x = kilometre, y = avg_count, fill = Colour_Code)) +
    geom_vline(xintercept = seq(0.5, 20, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    geom_bar(position = "fill", stat = "identity", color = NA) +
    geom_text(aes(label = paste0(round(prop), "%")),
              position = position_fill(vjust = 0.5),
              color = "white", size = sizes$stacked_text,
              family = "Lato", fontface = "bold") +
    scale_fill_manual(breaks = c("Other", "Zone 3", "Zone 4", "Zone 5"),
                      values = c("gray", "#FFB200", "#EB5B00", "#D91656")) +
    labs(subtitle = paste0("<img src='www/icons/fire-flame-curved-solid-full.png' width='30' height='30'> ",
                           "Heart Rate Zones | Time: ",
                           total_time_formatted, " | ", start_date, " | ",  max_km_longer, " KM</span>")) +
    theme_minimal(base_family = "Lato") +
    theme(
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      plot.subtitle = element_markdown(size = 20),
      axis.text.x = element_text(size = sizes$stacked_axis_text, face = "bold"),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
}

plot_hr_line <- function(hr_df_avg, total_time_formatted, start_date, max_km, sizes, max_km_longer) {
  ggplot(hr_df_avg, aes(x = kilometre, y = avg_hr, color = Zone_Label, fill = Zone_Label)) +
    geom_vline(xintercept = seq(0.5, 100, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    geom_rect(aes(xmin = 0.5, xmax = max_km + 0.5, ymin = 134, ymax = 153), fill = "#FFB200", color = "#FFB200", alpha = 0.05, show.legend = F) +
    geom_rect(aes(xmin = 0.5, xmax = max_km + 0.5, ymin = 153, ymax = 171), fill = "#EB5B00", color = "#EB5B00", alpha = 0.05, show.legend = F) +
    geom_rect(aes(xmin = 0.5, xmax = max_km + 0.5, ymin = 171, ymax = 190), fill = "#D91656", color = "#D91656", alpha = 0.05, show.legend = F) +
    geom_line(group = 1, color = 'black', linewidth = sizes$geom_line_width, alpha = 0.2) +
    geom_line(group = 1, color = 'white', linewidth = (sizes$geom_line_width * (2/3))) +
    geom_point(shape = 21, size = sizes$geom_point_size, color = "white", stroke = sizes$geom_point_stroke, show.legend = F) +
    geom_text(aes(label = round(avg_hr)), size = sizes$point_text_size,
              color = "white", family = "Lato", fontface = "bold") +
    scale_y_continuous(limits = c(134, 190)) +
    scale_fill_manual(breaks = c("Other", "Zone 3", "Zone 4", "Zone 5"),,
                      values = c("gray", "#FFB200", "#EB5B00", "#D91656")) +
    scale_color_manual(breaks = c("Other", "Zone 3", "Zone 4", "Zone 5"),,
                       values = c("gray", "#FFB200", "#EB5B00", "#D91656")) +
    labs(subtitle = paste0("<img src='www/icons/heart-pulse-solid-full.png' width='30' height='30'> ",
                           "Average Heart Rate | Time: ",
                           total_time_formatted, " | ", start_date, " | ",  max_km_longer, " KM</span>")) +
    theme_minimal(base_family = "Lato") +
    theme(
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      plot.subtitle = element_markdown(size = 20),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()
    )
}

plot_km_splits <- function(km_splits, total_time_formatted, start_date, sizes, max_km_longer) {
  ggplot(km_splits, aes(x = kilometre, y = duration_mins, fill = duration_mins)) +
    geom_col(show.legend = FALSE) +
    geom_vline(xintercept = seq(0.5, 100, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 4, color = "gray20", linewidth = 1.5, linetype = "dashed") +
    geom_text(aes(label = duration_formatted, y = 0.4),
              size = sizes$bar_text, color = "white", family = "Lato", fontface = "bold") +
    scale_fill_gradient2(high = '#8cc5e3', mid = '#3594cc', midpoint = 4, low = '#2066a8') +
    labs(subtitle = paste0("<img src='www/icons/stopwatch-solid-full.png' width='30' height='30'> ",
                           "Kilometre Splits | Time: ",
                           total_time_formatted, " | ", start_date, " | ",  max_km_longer, " KM</span>")) +
    theme_minimal(base_family = "Lato") +
    theme(
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      plot.subtitle = element_markdown(size = 20),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()
    )
}

# ---- 6. Combine plots ----
combine_run_plots <- function(p_pace, p_hr_line, p_hr_stacked) {
  p_pace / p_hr_line / p_hr_stacked
}