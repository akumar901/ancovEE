#' Parse Promethion Indirect Calorimetry Data (Step 2)
#'
#' @description
#' **Step 2 of the ancovEE workflow.**
#'
#' Reads a Promethion/Sable Systems indirect calorimetry Excel file,
#' prompts the user to select a datetime range from the Macro13 sheet,
#' and extracts 4 parameters (VO2, VCO2, RER, kcal_hr) for all chambers
#' into a single Excel file with one sheet per parameter.
#'
#' @param input_file Character. Path to the Promethion \code{.xlsx} file.
#' @param output_file Character. Path for the output \code{.xlsx} file.
#'   Defaults to \code{"Promethion_Output.xlsx"}.
#' @param start_datetime Character. Start of datetime range in format
#'   \code{"YYYY/MM/DD HH:MM:SS"}. If \code{NULL} (default), the user
#'   is prompted interactively.
#' @param end_datetime Character. End of datetime range in format
#'   \code{"YYYY/MM/DD HH:MM:SS"}. If \code{NULL} (default), the user
#'   is prompted interactively.
#' @param verbose Logical. Print progress to console? Default \code{TRUE}.
#'
#' @return A named list of 4 data.frames: \code{VO2}, \code{VCO2},
#'   \code{RER}, \code{kcal_hr}. Also writes to \code{output_file}.
#'
#' @examples
#' \dontrun{
#' # Interactive — will prompt for datetime range
#' result <- parse_promethion("Promethion.xlsx")
#'
#' # Non-interactive — supply datetime range directly
#' result <- parse_promethion(
#'   input_file     = "Promethion.xlsx",
#'   output_file    = "Promethion_Output.xlsx",
#'   start_datetime = "2025/10/31 12:25:26",
#'   end_datetime   = "2025/11/03 14:25:26"
#' )
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom openxlsx createWorkbook addWorksheet createStyle writeData
#'   setColWidths saveWorkbook
#'
#' @export
parse_promethion <- function(input_file,
                             output_file    = "Promethion_Output.xlsx",
                             start_datetime = NULL,
                             end_datetime   = NULL,
                             verbose        = TRUE) {

  # --- input checks -----------------------------------------------------------
  if (!file.exists(input_file)) {
    stop(sprintf("Input file not found: %s", input_file))
  }

  # ---------------------------------------------------------------------------
  # 1. Read Macro13 sheet
  # ---------------------------------------------------------------------------
  if (verbose) message("[Step 2] Reading Macro13 sheet...")

  raw <- readxl::read_excel(
    input_file,
    sheet        = "Macro13",
    col_names    = TRUE,
    .name_repair = "minimal"
  )

  # ---------------------------------------------------------------------------
  # 2. Parse DateTime column
  # ---------------------------------------------------------------------------
  # Show available range to user
  dt_raw      <- raw[[1]]
  dt_parsed   <- as.POSIXct(as.character(dt_raw), format = "%Y/%m/%d %H:%M:%S", tz = "UTC")

  # Handle cases where Excel stores datetime as POSIXct already
  if (all(is.na(dt_parsed))) {
    dt_parsed <- as.POSIXct(dt_raw, tz = "UTC")
  }

  dt_min <- min(dt_parsed, na.rm = TRUE)
  dt_max <- max(dt_parsed, na.rm = TRUE)

  if (verbose) {
    message(sprintf(
      "[Step 2] Available datetime range in Macro13:\n         From: %s\n         To  : %s",
      format(dt_min, "%Y/%m/%d %H:%M:%S"),
      format(dt_max, "%Y/%m/%d %H:%M:%S")
    ))
  }

  # ---------------------------------------------------------------------------
  # 3. Get datetime range from user (interactive or supplied)
  # ---------------------------------------------------------------------------
  if (is.null(start_datetime)) {
    message("\nEnter START datetime (format: YYYY/MM/DD HH:MM:SS)")
    message(sprintf("  Earliest available: %s", format(dt_min, "%Y/%m/%d %H:%M:%S")))
    start_datetime <- trimws(readline(prompt = "  Start: "))
  }

  if (is.null(end_datetime)) {
    message("Enter END datetime (format: YYYY/MM/DD HH:MM:SS)")
    message(sprintf("  Latest available:   %s", format(dt_max, "%Y/%m/%d %H:%M:%S")))
    end_datetime <- trimws(readline(prompt = "  End  : "))
  }

  # Parse user input
  dt_start <- as.POSIXct(start_datetime, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
  dt_end   <- as.POSIXct(end_datetime,   format = "%Y/%m/%d %H:%M:%S", tz = "UTC")

  if (is.na(dt_start)) stop(sprintf("Could not parse start datetime: '%s'\nUse format: YYYY/MM/DD HH:MM:SS", start_datetime))
  if (is.na(dt_end))   stop(sprintf("Could not parse end datetime: '%s'\nUse format: YYYY/MM/DD HH:MM:SS", end_datetime))
  if (dt_start >= dt_end) stop("Start datetime must be before end datetime.")
  if (dt_start < dt_min)  warning("Start datetime is before the earliest available timepoint.")
  if (dt_end   > dt_max)  warning("End datetime is after the latest available timepoint.")

  # ---------------------------------------------------------------------------
  # 4. Filter rows to selected datetime range
  # ---------------------------------------------------------------------------
  keep <- !is.na(dt_parsed) & dt_parsed >= dt_start & dt_parsed <= dt_end
  raw_filtered <- raw[keep, ]

  if (nrow(raw_filtered) == 0) {
    stop("No data rows found in the specified datetime range. Please check your input.")
  }

  if (verbose) {
    message(sprintf(
      "[Step 2] Selected %d time points (%s to %s)",
      nrow(raw_filtered),
      start_datetime,
      end_datetime
    ))
  }

  # DateTime column (formatted as character for clean Excel output)
  dt_col <- data.frame(
    DateTime = format(dt_parsed[keep], "%Y/%m/%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )

  # ---------------------------------------------------------------------------
  # 5. Extract the 4 parameter blocks
  #    VO2     : columns matching VO2_M_2  to VO2_M_16
  #    VCO2    : columns matching VCO2_M_2 to VCO2_M_16
  #    RER     : columns matching RER_M_2  to RER_M_16
  #    kcal_hr : columns matching kcal_hr_M_2 to kcal_hr_M_16
  # ---------------------------------------------------------------------------
  all_cols <- names(raw_filtered)

  extract_param <- function(pattern) {
    cols <- grep(pattern, all_cols, value = TRUE)
    if (length(cols) == 0) stop(sprintf("No columns found matching pattern: %s", pattern))
    cbind(dt_col, raw_filtered[, cols, drop = FALSE])
  }

  params <- list(
    VO2     = extract_param("^VO2_M_"),
    VCO2    = extract_param("^VCO2_M_"),
    RER     = extract_param("^RER_M_"),
    kcal_hr = extract_param("^kcal_hr_M_")
  )

  if (verbose) {
    for (nm in names(params)) {
      message(sprintf(
        "[Step 2] %-8s — %d rows x %d chambers",
        nm,
        nrow(params[[nm]]),
        ncol(params[[nm]]) - 1   # subtract DateTime col
      ))
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Write output Excel — one sheet per parameter
  # ---------------------------------------------------------------------------
  wb <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(
    fontColour     = "#FFFFFF",
    fgFill         = "#4472C4",
    halign         = "CENTER",
    textDecoration = "Bold",
    border         = "Bottom"
  )

  for (nm in names(params)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(
      wb,
      sheet       = nm,
      x           = params[[nm]],
      headerStyle = header_style
    )
    openxlsx::setColWidths(
      wb,
      sheet  = nm,
      cols   = 1:ncol(params[[nm]]),
      widths = "auto"
    )
  }

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

  if (verbose) {
    message(sprintf("\n[Step 2] Output saved to: %s", output_file))
    message("[Step 2] Sheets: VO2 | VCO2 | RER | kcal_hr")
    message("[Step 2] Complete.")
  }

  return(invisible(params))
}
