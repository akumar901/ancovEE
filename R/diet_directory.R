#' Get path to the persistent diet directory file
#' @keywords internal
.diet_dir_path <- function() {
  dir <- file.path(path.expand("~"), ".ancovEE")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, "diet_directory.json")
}

#' Load diet directory from disk
#' @keywords internal
.load_diet_dir <- function() {
  path <- .diet_dir_path()
  
  # Default built-in diets
  defaults <- list(
    HFD   = "HFD",
    HFLAA = "HFLAA",
    HFLP  = "HFLP",
    CHOW  = "CHOW",
    CHO   = "CHOW",
    CHW   = "CHOW",
    CWO   = "CHOW"
  )
  
  if (!file.exists(path)) {
    # First time — save defaults
    .save_diet_dir(defaults)
    return(defaults)
  }
  
  # Read existing file
  lines  <- readLines(path, warn = FALSE)
  parsed <- tryCatch(
    jsonlite_simple_parse(lines),
    error = function(e) defaults
  )
  
  # Merge with defaults so built-ins are always present
  for (nm in names(defaults)) {
    if (is.null(parsed[[nm]])) parsed[[nm]] <- defaults[[nm]]
  }
  
  return(parsed)
}

#' Save diet directory to disk (simple JSON, no dependency)
#' @keywords internal
.save_diet_dir <- function(dir_list) {
  path  <- .diet_dir_path()
  lines <- c("{")
  nms   <- names(dir_list)
  for (i in seq_along(nms)) {
    comma <- if (i < length(nms)) "," else ""
    lines <- c(lines, sprintf('  "%s": "%s"%s', nms[i], dir_list[[nms[i]]], comma))
  }
  lines <- c(lines, "}")
  writeLines(lines, path)
}

#' Simple JSON parser (no external dependency)
#' @keywords internal
jsonlite_simple_parse <- function(lines) {
  text <- paste(lines, collapse = "\n")
  # Remove braces
  text <- gsub("^\\s*\\{|\\}\\s*$", "", text)
  # Split by comma+newline
  pairs <- strsplit(text, ",\\s*\n")[[1]]
  result <- list()
  for (p in pairs) {
    p <- trimws(p)
    if (nchar(p) == 0) next
    # Extract key and value
    m <- regmatches(p, regexpr('"([^"]+)"\\s*:\\s*"([^"]+)"', p))
    if (length(m) == 0) next
    kv <- regmatches(m, gregexpr('"([^"]+)"', m))[[1]]
    kv <- gsub('"', '', kv)
    if (length(kv) >= 2) result[[kv[1]]] <- kv[2]
  }
  result
}

#' View the current diet directory
#'
#' @description
#' Prints all known diet prefixes and their normalized group names
#' stored in \code{~/.ancovEE/diet_directory.json}.
#'
#' @return A named character vector of diet mappings (invisibly).
#'
#' @examples
#' \dontrun{
#' view_diet_directory()
#' }
#'
#' @export
view_diet_directory <- function() {
  dir <- .load_diet_dir()
  message("=================================================")
  message("  ancovEE Diet Directory")
  message("=================================================")
  message(sprintf("  Location: %s\n", .diet_dir_path()))
  message(sprintf("  %-12s → %s", "Prefix", "Group Name"))
  message(sprintf("  %s", paste(rep("-", 30), collapse = "")))
  for (nm in names(dir)) {
    message(sprintf("  %-12s → %s", nm, dir[[nm]]))
  }
  message(sprintf("\n  %d entries total", length(dir)))
  return(invisible(unlist(dir)))
}

#' Reset the diet directory to factory defaults
#'
#' @description
#' Resets \code{~/.ancovEE/diet_directory.json} to the built-in defaults:
#' HFD, HFLAA, HFLP, CHOW, CHO, CHW, CWO.
#'
#' @return NULL invisibly.
#'
#' @examples
#' \dontrun{
#' reset_diet_directory()
#' }
#'
#' @export
reset_diet_directory <- function() {
  defaults <- list(
    HFD   = "HFD",
    HFLAA = "HFLAA",
    HFLP  = "HFLP",
    CHOW  = "CHOW",
    CHO   = "CHOW",
    CHW   = "CHOW",
    CWO   = "CHOW"
  )
  .save_diet_dir(defaults)
  message("[ancovEE] Diet directory reset to defaults.")
  view_diet_directory()
  return(invisible(NULL))
}
