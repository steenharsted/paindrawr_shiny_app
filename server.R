# Add this at the very top of server.R, before any other code
options(
  sass.cache = FALSE,
  sass.cache_dir = tempdir(),
  shiny.error = function() {
    cat("Shiny error occurred at:", Sys.time(), "\n")
    traceback()
  }
)

# Add logging
cat("Server starting at:", Sys.time(), "\n")
cat("Working directory:", getwd(), "\n")
cat("Temp directory:", tempdir(), "\n")

# Your existing server code continues here...

# ---- server ----
server <- function(input, output, session) {

  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("id", shinyvalidate::sv_required("ID er påkrævet"))
  iv$add_rule("id", ~ if (!grepl("^[A-Za-z0-9_-]+$", .))
    "Kun A–Z, a–z, 0–9, _ eller -")
  iv$add_rule("id", ~ if (nchar(.) > 20) "Maks 20 tegn")
  iv$enable()         # gives realtime messages



  rv <- reactiveValues(
    coords = data.frame(x = numeric(), y = numeric(), strokeId = integer()),
    settings = data.frame(strokeId = integer(), tool = character(),
                          lineWidth = numeric(), sprayDensity = numeric(),
                          sprayRadius = numeric(), lineColor = character(),
                          alpha = numeric(), canvasWidth = integer(),
                          canvasHeight = integer()),
    last_json = NULL, 
    last_png = NULL, 
    last_base = NULL
  )
  
  observe({
    session$sendCustomMessage("updateSettings", list(
      tool = input$tool,
      lineWidth = input$lineWidth,
      lineColor = input$lineColor,
      sprayDensity = input$sprayDensity %||% 1,
      sprayRadius = input$sprayRadius %||% 10,
      alpha = 1-(input$alpha/100)
    ))
  })
  
  observeEvent(input$reset, {
    session$sendCustomMessage("resetCanvas", list())
    rv$coords   <- rv$coords[0, ]
    rv$settings <- rv$settings[0, ]
  })
  
  observeEvent(input$stroke_coordinates, {
    cj <- input$stroke_coordinates
    if (!is.null(cj) && nzchar(cj)) {
      nl <- jsonlite::fromJSON(cj)
      if (length(nl)) rv$coords <- rbind(rv$coords, as.data.frame(nl))
    }
  })
  
  observeEvent(input$stroke_settings, {
    sj <- input$stroke_settings
    if (!is.null(sj) && nzchar(sj)) {
      nl <- jsonlite::fromJSON(sj)
      if (length(nl)) {
        df <- as.data.frame(nl)
        df$alpha <- as.numeric(df$alpha)
        rv$settings <- rbind(rv$settings, df)
      }
    }
  })
  
  observeEvent(input$save_trigger, {
      req(iv$is_valid())
      id   <- input$id
      base <- stamp_id(id)          # compute per click
      out_json <- file.path(SAVE_DIR, paste0(base, ".json"))
      png_path <- file.path(SAVE_DIR, paste0(base, ".png"))
    
    if (nrow(rv$settings)) {
      w <- unique(na.omit(rv$settings$canvasWidth))
      h <- unique(na.omit(rv$settings$canvasHeight))
      if (length(w) != 1L || length(h) != 1L) { w <- 380L; h <- 606L }
    } else { w <- 380L; h <- 606L }
    
    payload <- tryCatch(
      psk_build_payload(id, rv$coords, rv$settings, w, h),
      error = function(e) { showNotification(paste("Build error:", e$message), type="error"); NULL }
    )
    if (!is.null(payload)) {
      json_str <- jsonlite::toJSON(payload, auto_unbox = TRUE)
      valid <- TRUE; vmsg <- NULL
      if (file.exists(here("schema","painsketch_v1.schema.json")) ||
          file.exists(here("schema","painsketch_v1_1.schema.json"))) {
        valid <- tryCatch({ psk_validate_if_schema(json_str); TRUE },
                          error = function(e) { vmsg <<- e$message; FALSE })
      }
      if (!valid) showNotification(paste("Schema validation failed:", vmsg), type = "error")
      
      # JSON
      atomic_write_lines(json_str, out_json)
      showNotification(paste("JSON saved:", basename(out_json)), type = "message")
    }
    
    dataURL <- input$canvas_image
    if (!is.null(dataURL) && nzchar(dataURL)) {
      base64_string <- sub('^data:image/png;base64,', '', dataURL)
      img_data <- base64enc::base64decode(base64_string)
      # PNG
      atomic_write_bin(img_data, png_path)
      showNotification(paste("PNG saved:", basename(png_path)), type = "message")
    } else {
      showNotification("No image data received.", type = "warning")
    }
      
    rv$last_json <- out_json
    rv$last_png  <- png_path
    rv$last_base <- base
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      base <- if (!is.null(rv$last_base)) rv$last_base else input$id
      paste0("drawing_data_", base, ".zip")
    },
    content = function(file) {
      req(!is.null(rv$last_json), !is.null(rv$last_png))
      req(file.exists(rv$last_json), file.exists(rv$last_png))

      tmpdir <- file.path(tempdir(), "psk_zip")
      dir.create(tmpdir, showWarnings = FALSE)
      files <- c(rv$last_json, rv$last_png)
      ok <- file.copy(files, tmpdir, overwrite = TRUE)
      req(all(ok))

      # Use zipr with root
      zip::zipr(zipfile = file,
                files   = basename(files),
                root    = tmpdir)
    },
    contentType = "application/zip"
  )
}