#' Normalize EE, VO2 and VCO2 by Lean Body Mass for PRISM (Step 6)
#'
#' @description
#' **Step 6 of the ancovEE workflow.**
#'
#' Reads VCO2, VO2 and kcal_hr sheets from the Promethion output file,
#' divides every individual timepoint value for each cage by the
#' corresponding animal's lean body mass (LBM) from the Cage_Assignment
#' sheet in EE_Summary.xlsx. Columns are renamed using Mouse_ID and
#' ordered by group (e.g. all HFD animals first, then CHOW) for easy
#' copy-paste into PRISM. RER is copied as-is (no normalization needed).
#'
#' Output Excel file has 4 sheets:
#' \itemize{
#'   \item \strong{VCO2_norm} — VCO2 per LBM, columns as VCO2_MouseID_per_LBM
#'   \item \strong{VO2_norm} — VO2 per LBM, columns as VO2_MouseID_per_LBM
#'   \item \strong{kcal_hr_norm} — kcal/hr per LBM
#'   \item \strong{RER} — copied as-is, no normalization
#' }
#'
#' @param promethion_file Character. Path to the Promethion output \code{.xlsx}
#'   from \code{run_step2()}.
#' @param ee_summary_file Character. Path to \code{EE_Summary.xlsx} from
#'   \code{run_step3()} containing \code{Cage_Assignment} sheet.
#' @param ancova_input_file Character. Path to \code{ANCOVA_Input.csv} or
#'   \code{EE_Summary.xlsx} containing \code{ANCOVA_Input} sheet for group
#'   assignments.
#' @param output_xlsx Character. Path for the normalized output \code{.xlsx}.
#'   Defaults to \code{"Normalized_Output.xlsx"}.
#' @param verbose Logical. Print progress? Default \code{TRUE}.
#'
#' @return A named list of 4 data.frames.
#'
#' @examples
#' \dontrun{
#' norm <- run_step6(
#'   promethion_file  = "Promethion_Output.xlsx",
#'   ee_summary_file  = "EE_Summary.xlsx",
#'   ancova_input_file = "ANCOVA_Input.csv",
#'   output_xlsx      = "Normalized_Output.xlsx"
#' )
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   setColWidths saveWorkbook addStyle
#' @export
normalize_prism <- function(promethion_file,
                            ee_summary_file,
                            ancova_input_file,
                            output_xlsx = "Normalized_Output.xlsx",
                            verbose     = TRUE) {

  if (!file.exists(promethion_file))  stop(sprintf("Promethion file not found: %s", promethion_file))
  if (!file.exists(ee_summary_file))  stop(sprintf("EE Summary file not found: %s", ee_summary_file))
  if (!file.exists(ancova_input_file)) stop(sprintf("ANCOVA input file not found: %s", ancova_input_file))

  # ---------------------------------------------------------------------------
  # 1. Read Cage_Assignment — Cage, Mouse_ID, LBM
  # ---------------------------------------------------------------------------
  if (verbose) message("[Step 6] Reading Cage_Assignment...")

  cage_df <- readxl::read_excel(
    ee_summary_file,
    sheet        = "Cage_Assignment",
    col_names    = TRUE,
    .name_repair = "minimal"
  )

  # Build lookups: cage -> mouse_id, cage -> LBM
  cage_to_mouse <- setNames(as.character(cage_df$Mouse_ID), as.character(cage_df$Cage))
  cage_to_lbm   <- setNames(as.numeric(cage_df$Lean_Mass_g), as.character(cage_df$Cage))

  if (verbose) {
    message(sprintf("[Step 6] Found %d animals in Cage_Assignment", nrow(cage_df)))
  }

  # ---------------------------------------------------------------------------
  # 2. Read ANCOVA_Input — get Group per Mouse_ID
  # ---------------------------------------------------------------------------
  if (verbose) message("[Step 6] Reading group assignments from ANCOVA_Input...")

  if (grepl("\\.csv$", ancova_input_file, ignore.case = TRUE)) {
    ancova_df <- read.csv(ancova_input_file, stringsAsFactors = FALSE)
    # CSV has Group and LBM — need to match back to Mouse_ID via LBM
    # Join via LBM matching with cage_df
    mouse_to_group <- c()
    for (i in seq_len(nrow(cage_df))) {
      mouse <- cage_df$Mouse_ID[i]
      lbm   <- cage_df$Lean_Mass_g[i]
      # Find matching row in ancova_df by LBM
      match_row <- which(abs(ancova_df$LBM - lbm) < 0.0001)
      if (length(match_row) > 0) {
        mouse_to_group[mouse] <- ancova_df$Group[match_row[1]]
      }
    }
  } else {
    # Read from ANCOVA_Input sheet in EE_Summary.xlsx
    ancova_df <- readxl::read_excel(
      ancova_input_file,
      sheet        = "ANCOVA_Input",
      col_names    = TRUE,
      .name_repair = "minimal"
    )
    mouse_to_group <- c()
    for (i in seq_len(nrow(cage_df))) {
      mouse <- cage_df$Mouse_ID[i]
      lbm   <- cage_df$Lean_Mass_g[i]
      match_row <- which(abs(ancova_df$LBM - lbm) < 0.0001)
      if (length(match_row) > 0) {
        mouse_to_group[mouse] <- ancova_df$Group[match_row[1]]
      }
    }
  }

  if (verbose) {
    message("[Step 6] Group assignments:")
    for (mouse in names(mouse_to_group)) {
      message(sprintf("  %-12s -> %s", mouse, mouse_to_group[mouse]))
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Determine group order — sort animals by group
  #    All animals of group 1 first, then group 2
  # ---------------------------------------------------------------------------
  unique_groups  <- unique(mouse_to_group)
  ordered_mice   <- c()
  ordered_groups <- c()

  for (grp in unique_groups) {
    mice_in_grp   <- names(mouse_to_group)[mouse_to_group == grp]
    ordered_mice  <- c(ordered_mice, mice_in_grp)
    ordered_groups <- c(ordered_groups, rep(grp, length(mice_in_grp)))
  }

  # Map back to ordered cages
  mouse_to_cage  <- setNames(as.character(cage_df$Cage), as.character(cage_df$Mouse_ID))
  ordered_cages  <- mouse_to_cage[ordered_mice]

  if (verbose) {
    message("\n[Step 6] Column order for normalized sheets:")
    for (i in seq_along(ordered_mice)) {
      message(sprintf("  %d. %-8s | %-12s | LBM=%.5f",
                      i, ordered_cages[i], ordered_mice[i],
                      cage_to_lbm[ordered_cages[i]]))
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Helper: normalize one sheet with group-ordered, mouse-named columns
  # ---------------------------------------------------------------------------
  normalize_sheet <- function(sheet_name, param_prefix) {

    if (verbose) message(sprintf("\n[Step 6] Normalizing %s...", sheet_name))

    raw <- readxl::read_excel(
      promethion_file,
      sheet        = sheet_name,
      col_names    = TRUE,
      .name_repair = "minimal"
    )

    dt_col <- data.frame(DateTime = raw[[1]], stringsAsFactors = FALSE)

    # Process in group order
    norm_list <- lapply(seq_along(ordered_cages), function(i) {
      cage      <- ordered_cages[i]
      mouse     <- ordered_mice[i]
      lbm       <- cage_to_lbm[cage]

      # Find column in raw data matching this cage
      # e.g. cage="M_1" -> look for "VCO2_M_1"
      col_name  <- paste0(param_prefix, "_", cage)

      if (!col_name %in% names(raw)) {
        if (verbose) warning(sprintf("  Column '%s' not found in sheet '%s'", col_name, sheet_name))
        return(NULL)
      }

      values    <- as.numeric(raw[[col_name]])
      norm      <- values / lbm
      new_name  <- paste0(param_prefix, "_", mouse, "_per_LBM")

      if (verbose) {
        message(sprintf("  %-20s / %.5f g -> %-30s [mean=%.6f]",
                        col_name, lbm, new_name, mean(norm, na.rm = TRUE)))
      }

      df <- data.frame(x = norm, stringsAsFactors = FALSE)
      names(df) <- new_name
      df
    })

    norm_list <- Filter(Negate(is.null), norm_list)
    result    <- cbind(dt_col, do.call(cbind, norm_list))
    return(result)
  }

  # ---------------------------------------------------------------------------
  # 5. Normalize VCO2, VO2, kcal_hr
  # ---------------------------------------------------------------------------
  vco2_norm   <- normalize_sheet("VCO2",    "VCO2")
  vo2_norm    <- normalize_sheet("VO2",     "VO2")
  kcalhr_norm <- normalize_sheet("kcal_hr", "kcal_hr")

  # ---------------------------------------------------------------------------
  # 6. Read RER as-is but reorder columns by group with mouse ID names
  # ---------------------------------------------------------------------------
  if (verbose) message("\n[Step 6] Copying RER (no normalization - already a ratio)...")

  rer_raw  <- readxl::read_excel(
    promethion_file,
    sheet        = "RER",
    col_names    = TRUE,
    .name_repair = "minimal"
  )

  dt_col   <- data.frame(DateTime = rer_raw[[1]], stringsAsFactors = FALSE)

  rer_list <- lapply(seq_along(ordered_cages), function(i) {
    cage     <- ordered_cages[i]
    mouse    <- ordered_mice[i]
    col_name <- paste0("RER_", cage)
    if (!col_name %in% names(rer_raw)) return(NULL)
    df <- data.frame(x = as.numeric(rer_raw[[col_name]]), stringsAsFactors = FALSE)
    names(df) <- paste0("RER_", mouse)
    df
  })

  rer_list <- Filter(Negate(is.null), rer_list)
  rer      <- cbind(dt_col, do.call(cbind, rer_list))

  if (verbose) {
    message(sprintf("  RER: %d timepoints x %d cages reordered by group",
                    nrow(rer), ncol(rer) - 1))
  }

  # ---------------------------------------------------------------------------
  # 7. Write Excel — 4 sheets, group-ordered columns
  # ---------------------------------------------------------------------------
  wb <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(
    fontColour     = "#FFFFFF",
    fgFill         = "#4472C4",
    halign         = "CENTER",
    textDecoration = "Bold",
    border         = "Bottom"
  )

  num_style <- openxlsx::createStyle(numFmt = "0.000000")

  sheets <- list(
    VCO2_norm    = vco2_norm,
    VO2_norm     = vo2_norm,
    kcal_hr_norm = kcalhr_norm,
    RER          = rer
  )

  for (sheet_name in names(sheets)) {
    df <- sheets[[sheet_name]]
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, df, headerStyle = header_style)
    if (ncol(df) > 1) {
      openxlsx::addStyle(wb, sheet_name, style = num_style,
                         rows = 2:(nrow(df) + 1), cols = 2:ncol(df),
                         gridExpand = TRUE)
    }
    openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df),
                           widths = c(22, rep(22, ncol(df) - 1)))
  }

  openxlsx::saveWorkbook(wb, output_xlsx, overwrite = TRUE)

  if (verbose) {
    message(sprintf("\n[Step 6] Output saved to: %s", output_xlsx))
    message("[Step 6] Sheets: VCO2_norm | VO2_norm | kcal_hr_norm | RER")
    message(sprintf("[Step 6] Groups in order: %s",
                    paste(unique_groups, collapse = " -> ")))
    message(sprintf("[Step 6] Each sheet: %d timepoints x %d animals",
                    nrow(vco2_norm), ncol(vco2_norm) - 1))
  }

  return(invisible(list(
    VCO2_norm    = vco2_norm,
    VO2_norm     = vo2_norm,
    kcal_hr_norm = kcalhr_norm,
    RER          = rer
  )))
}

#' Run Step 6: Normalize for PRISM
#'
#' @description
#' User-friendly wrapper for \code{\link{normalize_prism}}.
#' **Step 6 of the ancovEE workflow.**
#'
#' @param promethion_file Character. Path to Promethion output \code{.xlsx}.
#' @param ee_summary_file Character. Path to \code{EE_Summary.xlsx}.
#' @param ancova_input_file Character. Path to \code{ANCOVA_Input.csv} or
#'   \code{EE_Summary.xlsx} for group assignments.
#' @param output_xlsx Character. Output file path.
#' @param verbose Logical. Print progress? Default \code{TRUE}.
#'
#' @return Named list of 4 data.frames.
#'
#' @examples
#' \dontrun{
#' norm <- run_step6(
#'   promethion_file   = "Promethion_Output.xlsx",
#'   ee_summary_file   = "EE_Summary.xlsx",
#'   ancova_input_file = "ANCOVA_Input.csv",
#'   output_xlsx       = "Normalized_Output.xlsx"
#' )
#' }
#' @export
run_step6 <- function(promethion_file,
                      ee_summary_file,
                      ancova_input_file,
                      output_xlsx = "Normalized_Output.xlsx",
                      verbose     = TRUE) {

  if (verbose) {
    message("=================================================")
    message("  ancovEE - Step 6: Normalize for PRISM         ")
    message("=================================================")
    message(sprintf("Promethion file   : %s", promethion_file))
    message(sprintf("EE Summary file   : %s", ee_summary_file))
    message(sprintf("ANCOVA input file : %s", ancova_input_file))
    message(sprintf("Output            : %s\n", output_xlsx))
  }

  result <- normalize_prism(
    promethion_file   = promethion_file,
    ee_summary_file   = ee_summary_file,
    ancova_input_file = ancova_input_file,
    output_xlsx       = output_xlsx,
    verbose           = verbose
  )

  if (verbose) message("\n[Step 6] Complete. File ready for PRISM.")
  return(invisible(result))
}
