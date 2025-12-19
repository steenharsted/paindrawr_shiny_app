# Configuration management
get_app_config <- function() {
  list(
    app_name = "paindrawr",
    version = "0.2.0",
    max_request_size = 10 * 1024^2,
    canvas_width = 380L,
    canvas_height = 606L,
    save_dir = Sys.getenv("PSK_SAVE_DIR", unset = "storage")
  )
}