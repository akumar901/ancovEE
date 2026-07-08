#' Parse NMR Body Composition Data (Step 1)
#'
#' @description
#' **Step 1 of the ancovEE workflow.**
#'
#' Reads a Bruker minispec NMR Excel export file, dynamically locates the
#' "Lean" compound block regardless of column position, and extracts a clean
#' 4-column table of lean body mass values per animal.
#'
#' The function handles the typical Bruker minispec format where:
#' \itemize{
#'   \item Rows 1-5 contain metadata (Sample Batch, Calibration, etc.)
#'   \item Row 6 contains column headers ("Sample Name", "Compound", "Mass", "Unit", ...)
#'   \item Row 7 is a blank or sub-header row
#'   \item Rows 8+ contain one animal per row
#'   \item Multiple compound blocks exist (Fat, Lean, Free Body Fluid),
#'         each with their own Compound / Mass / Unit triplet
#' }
#'
#' @param input_file Character. Path to the Bruker minispec NMR \code{.xlsx} file.
#' @param output_file Character. Path for the output \code{.xlsx} file.
#'   Defaults to \code{"NMR_Lean_Output.xlsx"} in the current working directory.
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages
#'   to the console.
#'
#' @return
#' A \code{data.frame} (invisibly) with the following 4 columns:
#' \describe{
#'   \item{Sample_Name}{Animal identifier as recorded in the NMR file (e.g. "hfd1")}
#'   \item{Compound}{Always "Lean"}
#'   \item{Mass}{Lean body mass in grams}
#'   \item{Unit}{Always "g"}
#' }
#' The same table is written to \code{output_file} as a formatted Excel file.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- parse_nmr_lean(
#'   input_file  = "NMR_INPUT.xlsx",
#'   output_file = "NMR_Lean_Output.xlsx"
#' )
#'
#' # Suppress console messages
#' result <- parse_nmr_lean(
#'   input_file  = "NMR_INPUT.xlsx",
#'   output_file = "NMR_Lean_Output.xlsx",
#'   verbose     = FALSE
#' )
#'
#' # Inspect result
#' head(result)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom openxlsx createWorkbook addWorksheet createStyle writeData
#'   setColWidths saveWorkbook
#'
#' @export
parse_nmr_lean <- function(input_file,
                           output_file = "NMR_Lean_Output.xlsx",
                           verbose     = TRUE) {

  # --- input checks -----------------------------------------------------------
  if (!file.exists(input_file)) {
    stop(sprintf("Input file not found: %s", input_file))
  }
  if (!grepl("\\.xlsx?$", input_file, ignore.case = TRUE)) {
    stop("Input file must be an Excel file (.xlsx or .xls)")
  }

  # ---------------------------------------------------------------------------
  # 1. Read the raw sheet without any header assumptions
  # ---------------------------------------------------------------------------
  raw <- readxl::read_excel(
    input_file,
    sheet        = 1,
    col_names    = FALSE,
    .name_repair = "minimal"
  )

  # ---------------------------------------------------------------------------
  # 2. Find the header row — the row containing "Sample Name" in column 1
  # ---------------------------------------------------------------------------
  header_row <- NULL
  for (i in seq_len(nrow(raw))) {
    val <- as.character(raw[[i, 1]])
    if (!is.na(val) && trimws(val) == "Sample Name") {
      header_row <- i
      break
    }
  }

  if (is.null(header_row)) {
    stop(paste(
      "Could not find a row with 'Sample Name' in column 1.",
      "Please check that the file is a valid Bruker minispec NMR export."
    ))
  }

  if (verbose) message(sprintf("[Step 1] Header row found at row %d", header_row))

  # ---------------------------------------------------------------------------
  # 3. Extract header row values
  # ---------------------------------------------------------------------------
  header_vals <- as.character(unlist(raw[header_row, ]))

  # ---------------------------------------------------------------------------
  # 4. Locate all "Compound" columns; find the one containing "Lean"
  # ---------------------------------------------------------------------------
  compound_cols <- which(header_vals == "Compound")

  if (length(compound_cols) == 0) {
    stop("No column named 'Compound' found in the header row.")
  }

  if (verbose) {
    message(sprintf(
      "[Step 1] Found %d 'Compound' column(s) at position(s): %s",
      length(compound_cols),
      paste(compound_cols, collapse = ", ")
    ))
  }

  # Determine data start row (skip blank sub-header row if present)
  next_val       <- as.character(raw[[header_row + 1, 1]])
  data_start_row <- if (is.na(next_val) || trimws(next_val) == "") {
    header_row + 2
  } else {
    header_row + 1
  }

  # Find which Compound column has "Lean" in data rows
  lean_compound_col <- NULL
  for (cc in compound_cols) {
    vals <- trimws(as.character(unlist(raw[data_start_row:nrow(raw), cc])))
    vals <- vals[!is.na(vals) & vals != "NA"]
    if (length(vals) > 0 && any(vals == "Lean")) {
      lean_compound_col <- cc
      break
    }
  }

  if (is.null(lean_compound_col)) {
    stop(paste(
      "Could not find 'Lean' values under any 'Compound' column.",
      "Please check that the NMR file includes Lean body mass measurements."
    ))
  }

  if (verbose) {
    message(sprintf(
      "[Step 1] Lean compound block: Compound=col %d | Mass=col %d | Unit=col %d",
      lean_compound_col,
      lean_compound_col + 1,
      lean_compound_col + 2
    ))
  }

  # ---------------------------------------------------------------------------
  # 5. Identify Mass and Unit columns (always immediately after Compound)
  # ---------------------------------------------------------------------------
  lean_mass_col <- lean_compound_col + 1
  lean_unit_col <- lean_compound_col + 2

  # Warn if headers don't match expected names
  mass_header <- trimws(as.character(header_vals[lean_mass_col]))
  unit_header <- trimws(as.character(header_vals[lean_unit_col]))

  if (mass_header != "Mass") {
    warning(sprintf(
      "Expected 'Mass' at column %d but found '%s'", lean_mass_col, mass_header
    ))
  }
  if (unit_header != "Unit") {
    warning(sprintf(
      "Expected 'Unit' at column %d but found '%s'", lean_unit_col, unit_header
    ))
  }

  # ---------------------------------------------------------------------------
  # 6. Extract data rows
  # ---------------------------------------------------------------------------
  data_rows <- raw[data_start_row:nrow(raw), ]

  sample_name <- trimws(as.character(unlist(data_rows[, 1])))
  compound    <- trimws(as.character(unlist(data_rows[, lean_compound_col])))
  mass        <- suppressWarnings(
                   as.numeric(unlist(data_rows[, lean_mass_col]))
                 )
  unit        <- trimws(as.character(unlist(data_rows[, lean_unit_col])))

  # Keep only Lean rows with a valid sample name
  keep <- !is.na(sample_name) & nchar(sample_name) > 0 &
          !is.na(compound)    & compound == "Lean"

  sample_name <- sample_name[keep]
  compound    <- compound[keep]
  mass        <- mass[keep]
  unit        <- unit[keep]

  if (length(sample_name) == 0) {
    stop("No valid Lean data rows were found after filtering.")
  }

  # ---------------------------------------------------------------------------
  # 7. Build output data.frame — 4 columns only
  # ---------------------------------------------------------------------------
  result <- data.frame(
    Sample_Name = sample_name,
    Compound    = compound,
    Mass        = mass,
    Unit        = unit,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message(sprintf("\n[Step 1] Extracted %d animals", nrow(result)))
    print(result)
  }

  # ---------------------------------------------------------------------------
  # 8. Write output Excel file with formatted header
  # ---------------------------------------------------------------------------
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Lean_Data")

  header_style <- openxlsx::createStyle(
    fontColour     = "#FFFFFF",
    fgFill         = "#4472C4",
    halign         = "CENTER",
    textDecoration = "Bold",
    border         = "Bottom"
  )

  openxlsx::writeData(
    wb,
    sheet       = "Lean_Data",
    x           = result,
    headerStyle = header_style
  )

  openxlsx::setColWidths(
    wb,
    sheet  = "Lean_Data",
    cols   = 1:ncol(result),
    widths = "auto"
  )

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

  if (verbose) {
    message(sprintf("[Step 1] Output saved to: %s", output_file))
  }

  return(invisible(result))
}
