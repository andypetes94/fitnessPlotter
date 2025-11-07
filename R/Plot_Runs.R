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
  
  # ---- Check activity type ----
  activity_node <- xml_find_first(tcx_xml, ".//Activity")
  sport_type <- xml_attr(activity_node, "Sport")
  
  if (is.null(sport_type)) {
    stop("❌ Could not detect an activity type in this TCX file.")
  }
  
  if (tolower(sport_type) != "running") {
    stop(paste0("❌ This is a '", sport_type, 
                "' file — please upload a *Running* TCX file."))
  }
  
  # ---- Continue if valid running file ----
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
        filter(., kilometre != "KM 0" & kilometre != paste0("KM ", ceiling(max_dist / 1000)))
      } else {
        filter(., kilometre != "KM 0")
      }
    } %>%
    mutate(time_elapsed = as.numeric(difftime(time, min(time), units = "secs"))) %>%
    drop_na()
  
  run_data$kilometre <- factor(run_data$kilometre, levels = unique(run_data$kilometre))
  
  return(run_data)
}


# ---- 1. Read and parse TCX file ----
read_tcx_data_hiit <- function(file) {

  tcx_xml <- read_xml(file)
  xml_ns_strip(tcx_xml)
  
  # ---- Check activity type ----
  activity_node <- xml_find_first(tcx_xml, ".//Activity")
  sport_type <- xml_attr(activity_node, "Sport")
  
  #}
  
  if (is.null(sport_type)) {
    stop("❌ Could not detect an activity type in this TCX file.")
  }
  
  if (!(tolower(sport_type) %in% c("other", "hiit"))) {
    stop(paste0("❌ This is a '", sport_type, 
                "' file — please upload a correct TCX file."))
  }
  
  hiit_trackpoints <- xml_find_all(activity_node, ".//Lap")
  trackpoints <- xml_find_all(hiit_trackpoints, ".//Trackpoint")
  
  hiit_data <- tibble(
    time = sapply(trackpoints, function(x) xml_text(xml_find_first(x, ".//Time"))),
    heart_rate = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//HeartRateBpm/Value"))))
  ) %>%
    mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
           seconds_elapsed = as.numeric(time - first(time)))
  
  hiit_metadata <- tibble(
    circuit_duration = sapply(hiit_trackpoints, function(x) xml_text(xml_find_first(x, ".//TotalTimeSeconds"))),
    calories = sapply(hiit_trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//Calories")))),
    avg_heartrate = sapply(hiit_trackpoints, function(x) xml_text(xml_find_first(x, ".//AverageHeartRateBpm"))),
    max_heartrate = sapply(hiit_trackpoints, function(x) xml_text(xml_find_first(x, ".//MaximumHeartRateBpm"))),
  ) %>%
    mutate(phase = if_else(row_number() == 1,
                           "Warm",
                           paste0(row_number() - 1))) %>%
    mutate(start_sec = lag(cumsum(circuit_duration), default = 0),
           end_sec = cumsum(circuit_duration))
  
  
  
  hiit_joined <- fuzzy_left_join(
    hiit_data,
    hiit_metadata,
    by = c("seconds_elapsed" = "start_sec", "seconds_elapsed" = "end_sec"),
    match_fun = list(`>=`, `<=`)
  ) %>% 
    drop_na() %>%
    mutate(Zone_Label = case_when(
    heart_rate > 171 ~ "Zone 5",
    heart_rate > 153 ~ "Zone 4",
    heart_rate > 134 ~ "Zone 3",
    heart_rate > 115 ~ "Zone 2",
    TRUE ~ "Zone 1"
  )) %>%
    mutate(time_bin = floor(seconds_elapsed / 10)) 
  
  hiit_joined$phase <- factor(hiit_joined$phase, levels = c("Warm", seq(1,nrow(hiit_metadata) -1,1)))
  
  return(hiit_joined)
  
}

# ---- 1. Read and parse TCX file ----
read_tcx_data_hyrox <- function(file) {
  
  tcx_xml <- read_xml(file)
  xml_ns_strip(tcx_xml)
  
  # ---- Check activity type ----
  activity_node <- xml_find_first(tcx_xml, ".//Activity")
  sport_type <- xml_attr(activity_node, "Sport")
  
  #}
  
  if (is.null(sport_type)) {
    stop("❌ Could not detect an activity type in this TCX file.")
  }
  
  if (!(tolower(sport_type) %in% c("other", "hiit"))) {
    stop(paste0("❌ This is a '", sport_type, 
                "' file — please upload a correct TCX file."))
  }
  
  hiit_trackpoints <- xml_find_all(activity_node, ".//Lap")
  trackpoints <- xml_find_all(hiit_trackpoints, ".//Trackpoint")
  
  hyrox_labels <- c("Run 1","SkiErg","Run 2","Sled Push","Run 3","Sled Pull","Run 4","Burpees","Run 5","Row","Run 6","Farmers Carry","Run 7","Lunges","Run 8","Wall Balls")
  
  hiit_data <- tibble(
    time = sapply(trackpoints, function(x) xml_text(xml_find_first(x, ".//Time"))),
    heart_rate = sapply(trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//HeartRateBpm/Value"))))
  ) %>%
    mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
           seconds_elapsed = as.numeric(time - first(time)))
  
  hiit_metadata <- tibble(
    circuit_duration = sapply(hiit_trackpoints, function(x) xml_text(xml_find_first(x, ".//TotalTimeSeconds"))),
    calories = sapply(hiit_trackpoints, function(x) as.numeric(xml_text(xml_find_first(x, ".//Calories")))),
    avg_heartrate = sapply(hiit_trackpoints, function(x) xml_text(xml_find_first(x, ".//AverageHeartRateBpm"))),
    max_heartrate = sapply(hiit_trackpoints, function(x) xml_text(xml_find_first(x, ".//MaximumHeartRateBpm"))),
  ) %>%
    mutate(phase = hyrox_labels[row_number()]) %>%
    mutate(start_sec = lag(cumsum(circuit_duration), default = 0),
           end_sec = cumsum(circuit_duration))
  
  
  
  hiit_joined <- fuzzy_left_join(
    hiit_data,
    hiit_metadata,
    by = c("seconds_elapsed" = "start_sec", "seconds_elapsed" = "end_sec"),
    match_fun = list(`>=`, `<=`)
  ) %>% 
    drop_na() %>%
    mutate(Zone_Label = case_when(
      heart_rate > 171 ~ "Zone 5",
      heart_rate > 153 ~ "Zone 4",
      heart_rate > 134 ~ "Zone 3",
      heart_rate > 115 ~ "Zone 2",
      TRUE ~ "Zone 1"
    )) %>%
    mutate(time_bin = floor(seconds_elapsed / 10)) 
  
  hiit_joined$phase <- factor(hiit_joined$phase, levels = unique(hiit_joined$phase))
  
  return(hiit_joined)
  
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
    mutate(Zone_Label = factor(Zone_Label, levels = c("Zone 5", "Zone 4", "Zone 3", "Zone 2", "Zone 1"))) %>%
    group_by(kilometre, Zone_Label) %>%
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
  
  map_runs_df <- run_data %>%
    mutate(time_POSIXct = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%OSZ",tz = "UTC")) %>%
    mutate(time_sec = as.numeric(time_POSIXct - min(time_POSIXct, na.rm = TRUE)))
  
  return(list(hr_stacked_bar = hr_stacked_bar, hr_df_avg = hr_df_avg, km_splits = km_splits, map_runs_df = map_runs_df))
}

summarize_hiit_data <- function(hiit_data) {
  
  hr_stacked_bar <- hiit_data %>%
    mutate(Zone_Label = factor(Zone_Label, levels = c("Zone 5", "Zone 4", "Zone 3", "Zone 2", "Zone 1"))) %>%
    group_by(phase, Zone_Label) %>%
    summarise(avg_hr = mean(heart_rate), avg_count = n(), .groups = "drop") %>%
    group_by(phase) %>%
    mutate(total_count = sum(avg_count), prop = (avg_count / total_count) * 100) %>%
    ungroup()
  
  hr_df_avg <- hiit_data %>%
    group_by(phase) %>%
    summarise(avg_hr = mean(heart_rate), .groups = "drop") %>%
    mutate(Zone_Label = case_when(
      avg_hr > 171 ~ "Zone 5",
      avg_hr > 153 ~ "Zone 4",
      avg_hr > 134 ~ "Zone 3",
      avg_hr > 115 ~ "Zone 2",
      TRUE ~ "Zone 1"
    ))
  
  
  circuit_splits <- hiit_data %>%
    group_by(phase) %>%
    summarise(
      duration_secs = max(seconds_elapsed, na.rm = TRUE) - min(seconds_elapsed, na.rm = TRUE),
      avg_hr = mean(heart_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      duration_mins = duration_secs / 60,
      duration_formatted = sprintf("%02d:%02d", duration_secs %/% 60, round(duration_secs %% 60))
    )
  
  hiit_binned <- hiit_data %>%
    group_by(time_bin) %>%
    reframe(heart_rate = mean(heart_rate),
            Zone_Label = dplyr::last(Zone_Label),
            phase = dplyr::last(phase))
  
  return(list(hr_stacked_bar = hr_stacked_bar, hr_df_avg = hr_df_avg, circuit_splits = circuit_splits, hiit_binned = hiit_binned))
}

# ---- 5. Plot functions ----
plot_hr_stacked <- function(hr_stacked_bar, total_time_formatted, start_date, sizes, max_km_longer, max_km) {
  ggplot(hr_stacked_bar, aes(x = kilometre, y = avg_count, fill = Zone_Label)) +
    geom_vline(xintercept = seq(0.5, max_km + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    geom_bar(position = "fill", stat = "identity", color = NA) +
    geom_text(aes(label = paste0(round(prop), "%"), color = Zone_Label),
              position = position_fill(vjust = 0.5),
              size = sizes$stacked_text,
              family = "Lato", fontface = "bold") +
    scale_fill_manual(breaks = c("Zone 1","Zone 2","Zone 3", "Zone 4", "Zone 5"),
                      values = c("gray", "#fdfa72","#FFB200", "#EB5B00", "#D91656")) +
    scale_color_manual(values = c("Zone 1" = "white", "Zone 2" = "black", "Zone 3" = "white", "Zone 4" = "white", "Zone 5" = "white"),
                       guide = "none") +
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
    geom_vline(xintercept = seq(0.5, max_km + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
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
      #axis.text = element_blank(),
      axis.text.x = element_text(size = sizes$stacked_axis_text, face = "bold"),
      panel.grid = element_blank()
    )
}

plot_km_splits <- function(km_splits, total_time_formatted, start_date, sizes, max_km_longer, max_km) {
  ggplot(km_splits, aes(x = kilometre, y = duration_mins, fill = duration_mins)) +
    geom_col(show.legend = FALSE) +
    geom_vline(xintercept = seq(0.5, max_km + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
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
      #axis.text = element_blank(),
      axis.text.x = element_text(size = sizes$stacked_axis_text, face = "bold"),
      panel.grid = element_blank()
    )
}


# ---- 5. Plot functions ----
plot_hr_stacked_hiit <- function(hr_stacked_bar, total_time_formatted, start_date, sizes, max_km_longer, rounds) {
  ggplot(hr_stacked_bar, aes(x = phase, y = avg_count, fill = Zone_Label)) +
    geom_vline(xintercept = seq(0.5, rounds + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    geom_bar(position = "fill", stat = "identity", color = NA) +
    geom_text(aes(label = paste0(round(prop), "%"), color = Zone_Label),
              position = position_fill(vjust = 0.5),
              size = sizes$stacked_text,
              family = "Lato", fontface = "bold") +
    scale_fill_manual(breaks = c("Zone 1","Zone 2","Zone 3", "Zone 4", "Zone 5"),
                      values = c("gray", "#fdfa72","#FFB200", "#EB5B00", "#D91656")) +
    scale_color_manual(values = c("Zone 1" = "white", "Zone 2" = "black", "Zone 3" = "white", "Zone 4" = "white", "Zone 5" = "white"),
                       guide = "none") +
    labs(subtitle = paste0("<img src='www/icons/fire-flame-curved-solid-full.png' width='30' height='30'> ",
                           "Heart Rate Zones | Time: ",
                           total_time_formatted, " | ", start_date, " </span>")) +
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

plot_hr_line_hiit <- function(hr_df_avg, total_time_formatted, start_date, rounds, sizes, max_km_longer) {
  ggplot(hr_df_avg, aes(x = phase, y = avg_hr, color = Zone_Label, fill = Zone_Label)) +
    geom_vline(xintercept = seq(0.5, rounds + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    geom_rect(aes(xmin = 0.5, xmax = rounds + 0.5, ymin = 134, ymax = 153), fill = "#FFB200", color = "#FFB200", alpha = 0.05, show.legend = F) +
    geom_rect(aes(xmin = 0.5, xmax = rounds+ 0.5, ymin = 153, ymax = 171), fill = "#EB5B00", color = "#EB5B00", alpha = 0.05, show.legend = F) +
    geom_rect(aes(xmin = 0.5, xmax = rounds + 0.5, ymin = 171, ymax = 190), fill = "#D91656", color = "#D91656", alpha = 0.05, show.legend = F) +
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
                           total_time_formatted, " | ", start_date, " </span>")) +
    theme_minimal(base_family = "Lato") +
    theme(
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      plot.subtitle = element_markdown(size = 20),
      axis.title = element_blank(),
      #axis.text = element_blank(),
      axis.text.x = element_text(size = sizes$stacked_axis_text, face = "bold"),
      panel.grid = element_blank()
    )
}


plot_circuit_splits <- function(circuit_splits, total_time_formatted, start_date, sizes, max_km_longer, rounds) {
  
  ggplot(circuit_splits, aes(x = phase, y = duration_mins, fill = duration_mins)) +
    geom_col(show.legend = FALSE) +
    geom_vline(xintercept = seq(0.5, rounds + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    #geom_hline(yintercept = 4, color = "gray20", linewidth = 1.5, linetype = "dashed") +
    geom_text(aes(label = duration_formatted),
              size = sizes$bar_text, color = "black", nudge_y = 0.2, family = "Lato", fontface = "bold") +
    scale_fill_gradient2(high = '#8cc5e3', mid = '#3594cc', low = '#2066a8') +
    labs(subtitle = paste0("<img src='www/icons/stopwatch-solid-full.png' width='30' height='30'> ",
                           "Circuit Split Times | Time: ",
                           total_time_formatted, " | ", start_date, " </span>")) +
    theme_minimal(base_family = "Lato") +
    theme(
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      plot.subtitle = element_markdown(size = 20),
      axis.title = element_blank(),
      #axis.text = element_blank(),
      axis.text.x = element_text(size = sizes$stacked_axis_text, face = "bold"),
      panel.grid = element_blank()
    )
}

plot_hyrox_split <- function(circuit_splits, total_time_formatted, start_date, rounds, sizes, max_km_longer) {
  ggplot(hr_df_avg, aes(x = phase, y = avg_hr, color = Zone_Label, fill = Zone_Label)) +
    geom_vline(xintercept = seq(0.5, rounds + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    geom_rect(aes(xmin = 0.5, xmax = rounds + 0.5, ymin = 134, ymax = 153), fill = "#FFB200", color = "#FFB200", alpha = 0.05, show.legend = F) +
    geom_rect(aes(xmin = 0.5, xmax = rounds+ 0.5, ymin = 153, ymax = 171), fill = "#EB5B00", color = "#EB5B00", alpha = 0.05, show.legend = F) +
    geom_rect(aes(xmin = 0.5, xmax = rounds + 0.5, ymin = 171, ymax = 190), fill = "#D91656", color = "#D91656", alpha = 0.05, show.legend = F) +
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
                           total_time_formatted, " | ", start_date, " </span>")) +
    theme_minimal(base_family = "Lato") +
    theme(
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      plot.subtitle = element_markdown(size = 20),
      axis.title = element_blank(),
      #axis.text = element_blank(),
      axis.text.x = element_text(size = sizes$stacked_axis_text, face = "bold"),
      panel.grid = element_blank()
    )
}


plot_hyrox_average <- function(circuit_splits, total_time_formatted, start_date, sizes, max_km_longer, rounds, nudge_text) {
  
  
  hyrox_labels <- c("Run 1","SkiErg","Run 2","Sled Push","Run 3","Sled Pull","Run 4","Burpees","Run 5","Row","Run 6","Farmers Carry","Run 7","Lunges","Run 8","Wall Balls")
  
  avg_hyrox <- data.frame(phase = c("Run 1","SkiErg","Run 2","Sled Push","Run 3","Sled Pull","Run 4","Burpees","Run 5","Row","Run 6","Farmers Carry","Run 7","Lunges","Run 8","Wall Balls"),
                          avg_time = c("5:02","4:35","4:44","3:27","5:08","5:05", "3:43","5:06","5:17","4:50","5:09","2:18","5:24","5:20","5:48","7:28")) %>%
    mutate(avg_seconds = as.numeric(ms(avg_time)))
  
  format_mm_ss <- function(x) {
    sign <- ifelse(x < 0, "-", "")
    x_abs <- abs(x)
    sprintf("%s%02d:%02d", sign, x_abs %/% 60, x_abs %% 60)
  }
  
  circuit_splits_updated <- circuit_splits %>%
    left_join(avg_hyrox, by = c('phase' = 'phase')) %>%
    mutate(seconds_difference = avg_seconds - duration_secs,
           diff_sec_formatted = format_mm_ss(seconds_difference),
           faster = seconds_difference >= 0,
           cum_avg_seconds = cumsum(seconds_difference))
  
  circuit_splits_updated$phase <- factor(circuit_splits_updated$phase, levels = hyrox_labels)
  
  ggplot(data = circuit_splits_updated, aes(x = phase, y = seconds_difference, fill = faster)) +
    geom_col(width = 0.9, color = "white", show.legend = F) +
    geom_borderline(data = circuit_splits_updated, aes(x = phase, y = cum_avg_seconds), group = 1, color = 'gray60', bordercolour = "white", linewidth = 2.5, borderwidth = 1, alpha = 0.5) +
    geom_line(data = circuit_splits_updated, aes(x = phase, y = cum_avg_seconds), group = 1, color = 'gray60', linewidth = 3, alpha = 0.3) +
    geom_point(data = circuit_splits_updated, aes(x = phase, y = cum_avg_seconds), shape = 21, color = 'white', fill = 'gray60', size = 5, stroke = 2 ) +
    
    geom_col(width = 0.8, show.legend = F) +
    geom_vline(xintercept = seq(0.5, rounds + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    
    #Add Text
    geom_shadowtext(data = circuit_splits_updated %>% filter(faster == T), aes(label = diff_sec_formatted), size = 5, family = "Lato", color = '#379A8B', fontface = "bold", bg.colour = "white", bg.r = 0.03, nudge_y = nudge_text) +
    geom_shadowtext(data = circuit_splits_updated %>% filter(!faster == T), aes(label = diff_sec_formatted), size = 5, family = "Lato", color = '#DB444B', fontface = "bold", bg.colour = "white", bg.r = 0.03, nudge_y = -1*nudge_text) +
    geom_hline(aes(yintercept =  0), color = "gray60") +
    scale_fill_manual(values = c("TRUE" = '#379A8B', "FALSE" = '#DB444B')) +
    labs(subtitle = paste0("<img src='www/icons/dumbbell-solid-full.png' width='30' height='30'> ",
                           "Comparison to Average | Time: ",
                           total_time_formatted, " | ", start_date, " </span>")) +
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

plot_hyrox_average_manual <- function(manual_hyrox_data, nudge_text) {
  
  
  ggplot(data = manual_hyrox_data, aes(x = phase, y = seconds_difference, fill = faster)) +
    geom_col(width = 0.9, color = "white", show.legend = F) +
    geom_borderline(data = manual_hyrox_data, aes(x = phase, y = cum_avg_seconds), group = 1, color = 'gray60', bordercolour = "white", linewidth = 2.5, borderwidth = 1, alpha = 0.5) +
    geom_line(data = manual_hyrox_data, aes(x = phase, y = cum_avg_seconds), group = 1, color = 'gray60', linewidth = 3, alpha = 0.3) +
    geom_point(data = manual_hyrox_data, aes(x = phase, y = cum_avg_seconds), shape = 21, color = 'white', fill = 'gray60', size = 5, stroke = 2 ) +
    
    geom_col(width = 0.8, show.legend = F) +
    geom_vline(xintercept = seq(0.5, 16 + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    
    #Add Text
    geom_shadowtext(data = manual_hyrox_data %>% filter(faster == T), aes(label = diff_sec_formatted), size = 5, family = "Lato", color = '#379A8B', fontface = "bold", bg.colour = "white", bg.r = 0.03, nudge_y = nudge_text) +
    geom_shadowtext(data = manual_hyrox_data %>% filter(!faster == T), aes(label = diff_sec_formatted), size = 5, family = "Lato", color = '#DB444B', fontface = "bold", bg.colour = "white", bg.r = 0.03, nudge_y = -1*nudge_text) +
    geom_hline(aes(yintercept =  0), color = "gray60") +
    scale_fill_manual(values = c("TRUE" = '#379A8B', "FALSE" = '#DB444B')) +
    labs(subtitle = paste0("<img src='www/icons/dumbbell-solid-full.png' width='30' height='30'> ",
                           "Comparison to Average </span>")) +
    theme_minimal(base_family = "Lato") +
    theme(
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      plot.subtitle = element_markdown(size = 20),
      axis.text.x = element_text(size = 8, face = "bold"),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
  
}

# ---- 5. Plot functions ----
plot_hr_stacked_hyrox <- function(hr_stacked_bar, hr_df_avg, total_time_formatted, start_date, sizes, max_km_longer, rounds) {
  ggplot(hr_stacked_bar, aes(x = phase, y = avg_count, fill = Zone_Label)) +
    geom_vline(xintercept = seq(0.5, rounds + 0.5, 1), color = "gray90", linewidth = 0.5, linetype = "dashed") +
    geom_point(data = hr_df_avg, aes(x = phase, y = 1.1), shape = 22, size = sizes$geom_point_size + 1.5, color = "white", stroke = sizes$geom_point_stroke, show.legend = F) +
    geom_text(data = hr_df_avg, aes(x = phase, y = 1.1, label = round(avg_hr)), size = sizes$point_text_size + 0.5,
              color = "white", family = "Lato", fontface = "bold") +
    geom_bar(position = "fill", stat = "identity", color = NA) +
    geom_text(aes(label = paste0(round(prop), "%"), color = Zone_Label),
              position = position_fill(vjust = 0.5),
              size = sizes$stacked_text,
              family = "Lato", fontface = "bold") +
    scale_fill_manual(breaks = c("Zone 1","Zone 2","Zone 3", "Zone 4", "Zone 5"),
                      values = c("gray", "#fdfa72","#FFB200", "#EB5B00", "#D91656")) +
    scale_color_manual(values = c("Zone 1" = "white", "Zone 2" = "black", "Zone 3" = "white", "Zone 4" = "white", "Zone 5" = "white"),
                       guide = "none") +
    labs(subtitle = paste0("<img src='www/icons/fire-flame-curved-solid-full.png' width='30' height='30'> ",
                           "Heart Rate Zones | Time: ",
                           total_time_formatted, " | ", start_date, " </span>")) +
    coord_cartesian(clip = "off") +  
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


plot_hr_binned <- function(hiit_binned, total_time_formatted, start_date, sizes, max_km_longer, rounds, avg_total_hr, total_hr) {
  
  
  # compute left x position and a small bin offset 
  x_left <- min(hiit_binned$time_bin, na.rm = TRUE)
  x_offset <- (max(hiit_binned$time_bin, na.rm = TRUE) - x_left) * 0.01
  
  ggplot(hiit_binned, aes(x = time_bin, y = heart_rate, fill = Zone_Label)) +
    geom_col(color = NA, show.legend = F) +
    
    # HR Reference Lines
    geom_hline(yintercept = total_hr, linetype = 'dashed', color = "#D91656") +
    geom_hline(yintercept = avg_total_hr, linetype = "dashed", color = "gray80") +
    
    # HR Reference Text
    geom_shadowtext(data = data.frame(x = x_left + x_offset, y = avg_total_hr, Zone_Label = NA), aes(x = x, y = y), label = paste0("Avg HR: ", round(avg_total_hr, digits = 0), " bpm"), hjust = 0,vjust = -0.3, size = 4, family = "Lato",color = "gray", fontface = "bold", bg.colour = "white", bg.r = 0.03) +
    geom_shadowtext(data = data.frame(x = x_left + x_offset, y = total_hr, Zone_Label = NA), aes(x = x, y = y), label = paste0("Max HR: ", total_hr, " bpm"), hjust = 0, vjust = 1.3, size = 4, family = "Lato", color = "#D91656", fontface = "bold", bg.colour = "white", bg.r = 0.03) +
    
    scale_fill_manual(breaks = c("Zone 1","Zone 2","Zone 3", "Zone 4", "Zone 5"),
                      values = c("gray90", "#fdfa72","#FFB200", "#EB5B00", "#D91656")) +
    scale_x_continuous(breaks = seq(0, max(hiit_binned$time_bin), by = 30), # 3 bins * 10 sec = 30 sec
                       labels = function(x) sprintf("%02d:%02d", (x*10) %/% 60, (x*10) %% 60),
                       expand = expansion(mult = c(0, 0))) +
    labs(subtitle = paste0("<img src='www/icons/heart-solid-full.png' width='30' height='30'> ",
                           "Heart Rate Bins | Max HR: ",
                           total_hr, " BPM |  Avg HR: ", round(avg_total_hr, digits = 0), " BPM</span>")) +
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


# ---- 6. Combine plots ----
combine_run_plots <- function(p_pace, p_hr_line, p_hr_stacked) {
  p_pace / p_hr_line / p_hr_stacked
}

combine_hyrox_plots <- function(p_pace, p_average, p_hr_line, p_hr_stacked) {
  p_pace / p_average / p_hr_line / p_hr_stacked
}

# ---- 7. Interactive map ----
plot_run_map <- function(map_runs_df) {
  
  # 1. Get the actual data from the reactive
  
  # --------------------------
  # 2. 1 km markers
  km_markers <- map_runs_df %>%
    filter(!is.na(distance)) %>%
    mutate(km = floor(distance / 1000)) %>%
    group_by(km) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(km) %>%  # ensure sorted by km
    mutate(
      time_sec_split = time_sec - lag(time_sec),
      time_sec_split = case_when(
        km == 0 ~ 0,            # start point
        km == 1 ~ time_sec,     # first km
        TRUE ~ time_sec_split
      ),
      time_elapsed_fmt = sprintf("%02d:%02d", as.integer(time_sec %/% 60), as.integer(time_sec %% 60)),
      km_split = sprintf("%02d:%02d", as.integer(time_sec_split %/% 60), as.integer(time_sec_split %% 60))
    )
  
  km_sf <- st_as_sf(km_markers, coords = c("longitude", "latitude"), crs = 4326)
  
  # --------------------------
  # 3. Line segments for heart rate zones
  segments <- lapply(1:(nrow(map_runs_df)-1), function(i) {
    st_linestring(as.matrix(map_runs_df[i:(i+1), c("longitude", "latitude")]))
  })
  
  zone_labels <- map_runs_df$Zone_Label[1:(nrow(map_runs_df)-1)]
  valid_idx <- !is.na(zone_labels)
  
  line_sf <- st_sf(
    Zone_Label = zone_labels[valid_idx],
    Heart_Rate = map_runs_df$heart_rate[1:(nrow(map_runs_df)-1)][valid_idx],
    Time = map_runs_df$time[1:(nrow(map_runs_df)-1)][valid_idx],
    geometry = st_sfc(segments[valid_idx]),
    crs = 4326
  )
  
  # --------------------------
  # 4. Start/finish points
  points_sf <- st_as_sf(map_runs_df[c(1, nrow(map_runs_df)), ], coords = c("longitude", "latitude"), crs = 4326) %>%
    mutate(type = factor(c("Start", "Finish"), levels = c("Start", "Finish")))
  
  # --------------------------
  # 5. Colors for zones
  zone_colors <- c(
    "Zone 1" = "gray",
    "Zone 2" = "#fdfa72",
    "Zone 3" = "#FFB200",
    "Zone 4" = "#EB5B00",
    "Zone 5" = "#D91656"
  )
  
  # --------------------------
  # 6. Plot with tmap
  #tmap_mode("view") # Not required
  
  tm_shape(line_sf) +
    tm_lines(
      col = "Zone_Label",
      lwd = 3,
      col.scale = tm_scale(values = zone_colors),
      col.legend = tm_legend(title = "Heart Rate Zone"),
      popup.vars = c("Heart Rate" = "Heart_Rate",
                     "Zone" = "Zone_Label",
                     "Time" = "Time")
    ) +
    tm_shape(km_sf) +
    tm_symbols(
      col = "white",
      size = 0.5,
      border.col = "black",
      popup.vars = c("KM" = "km",
                     "Time Elapsed" = "time_elapsed_fmt",
                     "Split" = "km_split")
    )
}