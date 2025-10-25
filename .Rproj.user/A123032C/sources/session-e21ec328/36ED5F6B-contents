init <- function() {
  packages <- c(
    "xml2", "dplyr", "ggplot2", "tidyverse", "showtext",
    "shadowtext", "ggborderline", "ggtext", "fontawesome",
    "rvest", "patchwork", "tools"
  )
  
  installed <- packages %in% rownames(installed.packages())
  if (any(!installed)) {
    install.packages(packages[!installed])
  }
  
  # Optional: load them all
  lapply(packages, library, character.only = TRUE)
  
  cat("✅ All packages installed and loaded!\n")
}