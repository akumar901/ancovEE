#' Calculate Energy Expenditure Summary (Step 3)
#'
#' @description
#' **Step 3 of the ancovEE workflow.**
#'
#' Reads the \code{kcal_hr} sheet from the Promethion output file (Step 2)
#' and the NMR lean mass output file (Step 1), calculates per-mouse averages,
#' prompts the user to map each cage position to an animal ID, shows a
#' confirmation table, and saves a single Excel file with two sheets:
#' \itemize{
#'   \item \strong{EE_Summary} — Avg_kcal_hr and Avg_kcal_day per cage position
#'   \item \strong{Cage_Assignment} — merged table with Mouse_ID, Cage,
#'     Avg_kcal_hr, Avg_kcal_day and Lean_Mass_g
#' }
#'
#' @param promethion_file Character. Path to the Promethion output \code{.xlsx}
#'   file from \code{run_step2()}.
#' @param nmr_file Character. Path to the NMR lean mass \code{.xlsx}
#'   file from \code{run_step1()}.
#' @param output_xlsx Character. Path for the output Excel file.
#'   Defaults to \code{"EE_Summary.xlsx"}.
#' @param verbose Logical. Print progress to console? Default \code{TRUE}.
#'
#' @return A named list with two data.frames: \code{EE_Summary} and
#'   \code{Cage_Assignment}.
#'
#' @examples
#' \dontrun{
#' EE <- calc_ee_summary(
#'   promethion_file = "Promethion_Output.xlsx",
#'   nmr_file        = "NMR_Lean_Output.xlsx",
#'   output_xlsx     = "EE_Summary.xlsx"
#' )
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom openxlsx createWorkbook addWorksheet createStyle writeData
#'   addStyle setColWidths saveWorkbook
#'
#' @export
calc_ee_summary <- function(promethion_file,
                            nmr_file,
                            output_xlsx = "EE_Summary.xlsx",
                            verbose     = TRUE) {

  # --- input checks -----------------------------------------------------------
  if (!file.exists(promethion_file)) stop(sprintf("Promethion file not found: %s", promethion_file))
  if (!file.exists(nmr_file))        stop(sprintf("NMR file not found: %s", nmr_file))

  # ---------------------------------------------------------------------------
  # 1. Read kcal_hr sheet from Promethion output
  # ---------------------------------------------------------------------------
  if (verbose) message("[Step 3] Reading kcal_hr sheet...")

  kcal_data <- readxl::read_excel(
    promethion_file,
    sheet        = "kcal_hr",
    col_names    = TRUE,
    .name_repair = "minimal"
  )

  mouse_cols <- names(kcal_data)[names(kcal_data) != "DateTime"]

  if (verbose) {
    message(sprintf(
      "[Step 3] Found %d time points x %d cage positions",
      nrow(kcal_data),
      length(mouse_cols)
    ))
  }

  # ---------------------------------------------------------------------------
  # 2. Calculate Avg_kcal_hr and Avg_kcal_day per cage
  # ---------------------------------------------------------------------------
  ee_summary <- data.frame(
    Cage         = mouse_cols,
    Avg_kcal_hr  = sapply(mouse_cols, function(m) mean(as.numeric(kcal_data[[m]]), na.rm = TRUE)),
    Avg_kcal_day = sapply(mouse_cols, function(m) mean(as.numeric(kcal_data[[m]]), na.rm = TRUE) * 24),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (verbose) {
    message("\n[Step 3] EE Summary per cage:")
    message(sprintf("  %-15s %12s %13s", "Cage", "Avg_kcal_hr", "Avg_kcal_day"))
    message(sprintf("  %s", paste(rep("-", 42), collapse = "")))
    for (i in seq_len(nrow(ee_summary))) {
      message(sprintf(
        "  %-15s %12.6f %13.6f",
        ee_summary$Cage[i],
        ee_summary$Avg_kcal_hr[i],
        ee_summary$Avg_kcal_day[i]
      ))
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Read NMR lean mass output
  # ---------------------------------------------------------------------------
  if (verbose) message("\n[Step 3] Reading NMR lean mass file...")

  nmr_data <- readxl::read_excel(
    nmr_file,
    sheet        = "Lean_Data",
    col_names    = TRUE,
    .name_repair = "minimal"
  )

  valid_ids <- nmr_data$Sample_Name
  if (verbose) {
    message(sprintf("[Step 3] Valid animal IDs from NMR: %s",
                    paste(valid_ids, collapse = ", ")))
  }

  # ---------------------------------------------------------------------------
  # 4. Interactive cage-to-animal mapping with confirmation + correction loop
  # ---------------------------------------------------------------------------
  cage_labels <- gsub("kcal_hr_", "", mouse_cols)

  # Outer loop — keeps repeating until user confirms the full table
  repeat {

    assignment <- data.frame(
      Mouse_ID     = character(length(mouse_cols)),
      Cage         = cage_labels,
      Avg_kcal_hr  = ee_summary$Avg_kcal_hr,
      Avg_kcal_day = ee_summary$Avg_kcal_day,
      Lean_Mass_g  = NA_real_,
      stringsAsFactors = FALSE
    )

    assigned_ids <- c()

    message("\n=================================================")
    message("  Cage Assignment")
    message("=================================================")
    message("For each cage position, enter the animal ID.")
    message("Press Enter or type 'blank' to skip empty cages.\n")

    # Inner loop — go through each cage one by one
    for (i in seq_along(cage_labels)) {
      repeat {
        input <- trimws(readline(
          prompt = sprintf("  Cage %-5s → Animal ID: ", cage_labels[i])
        ))

        # Empty or blank = skip
        if (input == "" || tolower(input) == "blank") {
          assignment$Mouse_ID[i] <- NA
          message(sprintf("  Cage %s → [empty]", cage_labels[i]))
          break
        }

        # Validate against NMR file
        if (!input %in% valid_ids) {
          message(sprintf(
            "  ⚠ '%s' not found in NMR file. Valid IDs are: %s",
            input, paste(valid_ids, collapse = ", ")
          ))
          message("  Please re-enter.")
          next
        }

        # Warn if already assigned
        if (input %in% assigned_ids) {
          message(sprintf(
            "  ⚠ '%s' has already been assigned to another cage. Please re-enter.",
            input
          ))
          next
        }

        # Valid — assign
        assignment$Mouse_ID[i] <- input
        assigned_ids           <- c(assigned_ids, input)

        # Look up lean mass
        lean_val               <- nmr_data$Mass[nmr_data$Sample_Name == input]
        assignment$Lean_Mass_g[i] <- if (length(lean_val) > 0) lean_val[1] else NA

        message(sprintf(
          "  Cage %-5s → %-8s (Lean mass: %.5f g)",
          cage_labels[i], input, assignment$Lean_Mass_g[i]
        ))
        break
      }
    }

    # -------------------------------------------------------------------------
    # 5. Confirmation table — show full assignment and ask if correct
    # -------------------------------------------------------------------------
    message("\n=================================================")
    message("  Please review the full cage assignment below:")
    message("=================================================")
    message(sprintf(
      "  %-10s %-8s %12s %13s %12s",
      "Mouse_ID", "Cage", "Avg_kcal_hr", "Avg_kcal_day", "Lean_Mass_g"
    ))
    message(sprintf("  %s", paste(rep("-", 60), collapse = "")))

    for (i in seq_len(nrow(assignment))) {
      if (is.na(assignment$Mouse_ID[i])) {
        message(sprintf(
          "  %-10s %-8s %12.6f %13.6f %12s",
          "[empty]",
          assignment$Cage[i],
          assignment$Avg_kcal_hr[i],
          assignment$Avg_kcal_day[i],
          "N/A"
        ))
      } else {
        message(sprintf(
          "  %-10s %-8s %12.6f %13.6f %12.5f",
          assignment$Mouse_ID[i],
          assignment$Cage[i],
          assignment$Avg_kcal_hr[i],
          assignment$Avg_kcal_day[i],
          assignment$Lean_Mass_g[i]
        ))
      }
    }

    message(sprintf("  %s", paste(rep("-", 60), collapse = "")))
    message(sprintf(
      "  %d mice assigned, %d cage(s) empty\n",
      sum(!is.na(assignment$Mouse_ID)),
      sum(is.na(assignment$Mouse_ID))
    ))

    # Ask user to confirm
    confirm <- trimws(tolower(readline(
      prompt = "  Does this look correct? (yes / no): "
    )))

    if (confirm == "yes" || confirm == "y") {
      message("\n[Step 3] Assignment confirmed!")
      break  # exit outer repeat loop

    } else {

      # -----------------------------------------------------------------------
      # 6. Correction — ask which cage to fix, keep asking until done
      # -----------------------------------------------------------------------
      message("\n  Which cage(s) would you like to reassign?")
      message("  Type a cage label (e.g. M_2) to fix it, or type 'done' when finished.\n")

      repeat {
        fix_cage <- trimws(toupper(readline(prompt = "  Fix cage (or 'done'): ")))

        if (tolower(fix_cage) == "done") {
          break
        }

        # Find the cage in assignment
        cage_idx <- which(assignment$Cage == fix_cage)

        if (length(cage_idx) == 0) {
          message(sprintf(
            "  ⚠ Cage '%s' not found. Available cages: %s",
            fix_cage, paste(cage_labels, collapse = ", ")
          ))
          next
        }

        # Remove current assignment from assigned_ids so it can be reused
        old_id <- assignment$Mouse_ID[cage_idx]
        if (!is.na(old_id)) {
          assigned_ids <- assigned_ids[assigned_ids != old_id]
        }

        # Get new animal ID
        repeat {
          new_input <- trimws(readline(
            prompt = sprintf("  Cage %-5s → New Animal ID (or blank to empty): ", fix_cage)
          ))

          if (new_input == "" || tolower(new_input) == "blank") {
            assignment$Mouse_ID[cage_idx]    <- NA
            assignment$Lean_Mass_g[cage_idx] <- NA
            message(sprintf("  Cage %s → [empty]", fix_cage))
            break
          }

          if (!new_input %in% valid_ids) {
            message(sprintf(
              "  ⚠ '%s' not found in NMR file. Valid IDs: %s",
              new_input, paste(valid_ids, collapse = ", ")
            ))
            next
          }

          if (new_input %in% assigned_ids) {
            message(sprintf(
              "  ⚠ '%s' is already assigned to another cage.",
              new_input
            ))
            next
          }

          # Valid correction
          assignment$Mouse_ID[cage_idx]    <- new_input
          assigned_ids                     <- c(assigned_ids, new_input)
          lean_val                         <- nmr_data$Mass[nmr_data$Sample_Name == new_input]
          assignment$Lean_Mass_g[cage_idx] <- if (length(lean_val) > 0) lean_val[1] else NA

          message(sprintf(
            "  Cage %-5s → %-8s (Lean mass: %.5f g)",
            fix_cage, new_input, assignment$Lean_Mass_g[cage_idx]
          ))
          break
        }

        message("  Type another cage to fix, or 'done' to re-review the full table.")
      }
      # After corrections, loop back to show the full table again for re-confirmation
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Build final Cage_Assignment (remove empty cages)
  # ---------------------------------------------------------------------------
  cage_assignment <- assignment[!is.na(assignment$Mouse_ID), ]
  rownames(cage_assignment) <- NULL

  # ---------------------------------------------------------------------------
  # 8. Write Excel output — 2 sheets
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

  # Sheet 1: EE_Summary
  openxlsx::addWorksheet(wb, "EE_Summary")
  openxlsx::writeData(wb, sheet = "EE_Summary", x = ee_summary, headerStyle = header_style)
  openxlsx::addStyle(wb, sheet = "EE_Summary", style = num_style,
                     rows = 2:(nrow(ee_summary) + 1), cols = 2:3, gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet = "EE_Summary", cols = 1:3, widths = c(20, 15, 15))

  # Sheet 2: Cage_Assignment
  openxlsx::addWorksheet(wb, "Cage_Assignment")
  openxlsx::writeData(wb, sheet = "Cage_Assignment", x = cage_assignment, headerStyle = header_style)
  openxlsx::addStyle(wb, sheet = "Cage_Assignment", style = num_style,
                     rows = 2:(nrow(cage_assignment) + 1), cols = 3:5, gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet = "Cage_Assignment", cols = 1:5,
                         widths = c(15, 12, 15, 15, 14))

  openxlsx::saveWorkbook(wb, output_xlsx, overwrite = TRUE)

  if (verbose) {
    message(sprintf("\n[Step 3] Excel saved to: %s", output_xlsx))
    message("[Step 3] Sheets: EE_Summary | Cage_Assignment")
  }

  return(invisible(list(
    EE_Summary      = ee_summary,
    Cage_Assignment = cage_assignment
  )))
}
