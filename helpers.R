# ---- helpers ----
psk_build_payload <- function(id, coords_df, settings_df, w = config$canvas_width, h = config$canvas_height) {
  settings_df <- settings_df[order(match(settings_df$strokeId, unique(settings_df$strokeId))), ]
  strokes <- lapply(seq_len(nrow(settings_df)), function(k) {
    st  <- settings_df[k, ]
    pts <- coords_df[coords_df$strokeId == st$strokeId, c("x","y"), drop = FALSE]
    p_px <- Map(function(x,y) c(as.integer(round(x)), as.integer(round(y))), pts$x, pts$y)
    
    s <- list(
      i  = as.integer(st$strokeId),
      q  = as.integer(k),
      t  = as.character(st$tool),
      bw = as.numeric(st$lineWidth),
      c  = as.character(st$lineColor),
      a  = as.integer(round(pmin(pmax(st$alpha,0),1) * 255)),
      p  = p_px
    )
    if (st$tool == "spray") {
      s$pr <- as.integer(st$sprayRadius)
      s$pd <- as.numeric(st$sprayDensity)
    }
    s
  })
  
  # Keep JSON structure - only w and h come from config defaults
  list(v=2L, id=id, w=as.integer(w), h=as.integer(h),
       coord="px", ts=format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
       app=appname, s=strokes)
}

# Keep validation function exactly the same - no config changes needed
psk_validate_if_schema <- function(json_str) {
  paths <- c(here("schema","painsketch_v1.schema.json"),
             here("schema","painsketch_v1_1.schema.json"))
  sp <- paths[file.exists(paths)][1]
  if (!is.na(sp) && nzchar(sp)) {
    v <- jsonvalidate::json_validator(sp, engine = "ajv")
    ok <- v(json_str, verbose = TRUE)
    if (!isTRUE(ok)) stop("JSON failed schema validation.")
  }
}

# atomic writers
atomic_write_lines <- function(txt, final_path) {
  tmp <- tempfile(tmpdir = SAVE_DIR, fileext = ".tmp")
  writeLines(txt, tmp, useBytes = TRUE)
  if (!file.rename(tmp, final_path)) stop("Atomic rename failed: ", final_path)
}

atomic_write_bin <- function(bytes, final_path) {
  tmp <- tempfile(tmpdir = SAVE_DIR, fileext = ".tmp")
  writeBin(bytes, tmp)
  if (!file.rename(tmp, final_path)) stop("Atomic rename failed: ", final_path)
}

# Make sure ids are unique (adds time to id)
stamp_id <- function(id) {
  timestamp <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
  paste0(id, "_", timestamp)
}

# Read schema files
schema_file <- here("schema", "painsketch_v1.schema.json")
if (file.exists(schema_file)) {
  psk_schema <- fromJSON(schema_file, simplifyVector = FALSE)
} else {
  stop("Schema file not found: ", schema_file)
}