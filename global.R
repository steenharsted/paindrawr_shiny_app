library(shiny)
library(shinyWidgets)
library(shinyvalidate)
library(bslib)
library(here)
library(jsonlite)
library(base64enc)
library(zip)
library(jsonvalidate)
library(rlang)
library(glue)

# Load configuration first
source("config.R", local = TRUE)
config <- get_app_config()

# Safer environment loading
safely_load_env <- function() {
  tryCatch({
    if (file.exists(".Renviron")) readRenviron(".Renviron")
    message("PSK_SAVE_DIR=", config$save_dir)
  }, error = function(e) {
    warning("Failed to load .Renviron: ", e$message)
  })
}
safely_load_env()

options(shiny.autoreload = FALSE)

# Use config values instead of hardcoded ones
appname <- paste0(config$app_name, "_", config$version)
options(shiny.maxRequestSize = config$max_request_size)
SAVE_DIR <- config$save_dir

if (!dir.exists(SAVE_DIR)) {
  stop("Storage dir missing: ", SAVE_DIR,
       ". Create it on the server and chown to user 'shiny'.")
}

# Source helper functions
source("helpers.R", local = TRUE)