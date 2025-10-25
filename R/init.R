init <- function() {
  packages <- c(
    "xml2", "dplyr", "ggplot2", "tidyverse", "showtext",
    "shadowtext", "ggborderline", "ggtext", "fontawesome",
    "rvest", "patchwork", "tools", "shiny", "shinythemes", "shinyjs"
  )
  
  # Check which packages are not installed
  installed <- packages %in% rownames(installed.packages())
  if (any(!installed)) {
    install.packages(packages[!installed])
  }
  
  # Load all packages
  lapply(packages, library, character.only = TRUE)
  
  cat("✅ All packages installed and loaded!\n")
}