#' Prepare ANCOVA Input Sheet (Step 4)
#'
#' @description
#' **Step 4 of the ancovEE workflow.**
#'
#' Reads the \code{Cage_Assignment} sheet from \code{EE_Summary.xlsx},
#' isolates \code{Mouse_ID}, \code{Avg_kcal_day} and \code{Lean_Mass_g},
#' renames them to \code{Group}, \code{EE} and \code{LBM}, intelligently
#' parses group names from animal IDs using a persistent diet directory,
#' and saves the result as a new sheet \code{ANCOVA_Input} in the same
#' Excel file.
#'
#' @param ee_file Character. Path to \code{EE_Summary.xlsx} from
#'   \code{run_step3()}.
#' @param verbose Logical. Print progress? Default \code{TRUE}.
#'
#' @return A \code{data.frame} with columns \code{Group}, \code{EE},
#'   \code{LBM}. Also adds \code{ANCOVA_Input} sheet to \code{ee_file}.
#'
#' @examples
#' \dontrun{
#' ancova_data <- prepare_ancova_input("EE_Summary.xlsx")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom openxlsx loadWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths saveWorkbook
#'
#' @export
prepare_ancova_input <- function(ee_file,
                                 output_csv = NULL,
                                 verbose    = TRUE) {

  if (!file.exists(ee_file)) stop(sprintf("File not found: %s", ee_file))

  # ---------------------------------------------------------------------------
  # 1. Read Cage_Assignment sheet
  # ---------------------------------------------------------------------------
  if (verbose) message("[Step 4] Reading Cage_Assignment sheet...")

  cage_data <- readxl::read_excel(
    ee_file,
    sheet        = "Cage_Assignment",
    col_names    = TRUE,
    .name_repair = "minimal"
  )

  # Check required columns exist
  required <- c("Mouse_ID", "Avg_kcal_day", "Lean_Mass_g")
  missing  <- required[!required %in% names(cage_data)]
  if (length(missing) > 0) {
    stop(sprintf("Missing columns in Cage_Assignment: %s", paste(missing, collapse = ", ")))
  }

  # Isolate the 3 columns
  working <- data.frame(
    Group = cage_data$Mouse_ID,
    EE    = cage_data$Avg_kcal_day,
    LBM   = cage_data$Lean_Mass_g,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message(sprintf("[Step 4] Found %d animals", nrow(working)))
    message(sprintf("[Step 4] Animal IDs: %s",
                    paste(working$Group, collapse = ", ")))
  }

  # ---------------------------------------------------------------------------
  # 2. Load persistent diet directory
  # ---------------------------------------------------------------------------
  diet_dir <- .load_diet_dir()

  if (verbose) {
    message(sprintf(
      "[Step 4] Loaded diet directory (%d entries)",
      length(diet_dir)
    ))
  }

  # ---------------------------------------------------------------------------
  # 3. Extract alphabetic prefix from each Mouse_ID
  #    e.g. "HFD1577" -> "HFD", "CW1595R" -> "CW" (partial, needs review)
  # ---------------------------------------------------------------------------
  extract_prefix <- function(id) {
    # Extract leading alphabetic characters
    m <- regmatches(id, regexpr("^[A-Za-z]+", id))
    if (length(m) == 0 || nchar(m) == 0) return(NA)
    toupper(m)
  }

  working$prefix <- sapply(working$Group, extract_prefix)

  # ---------------------------------------------------------------------------
  # 4. Group animals by prefix and resolve each group interactively
  # ---------------------------------------------------------------------------
  unique_prefixes <- unique(working$prefix[!is.na(working$prefix)])

  message("\n=================================================")
  message("  Step 4: Group Name Assignment")
  message("=================================================")

  # Map prefix -> final group name
  prefix_map <- list()

  for (pfx in unique_prefixes) {
    animals_with_pfx <- working$Group[working$prefix == pfx]
    n                <- length(animals_with_pfx)

    message(sprintf(
      "\n  Found prefix '%s' in %d animal(s): %s",
      pfx, n, paste(animals_with_pfx, collapse = ", ")
    ))

    # Check if prefix is already in diet directory
    if (!is.null(diet_dir[[pfx]])) {
      known_name <- diet_dir[[pfx]]
      message(sprintf(
        "  '%s' is in your diet directory → Group name: '%s'",
        pfx, known_name
      ))

      confirm <- trimws(tolower(readline(
        prompt = sprintf("  Confirm '%s' as group name for these animals? (yes/no): ", known_name)
      )))

      if (confirm %in% c("yes", "y")) {
        prefix_map[[pfx]] <- known_name
        message(sprintf("  ✓ '%s' → '%s'", pfx, known_name))
        next
      }
    }

    # Not in directory or user said no — ask for group name
    repeat {
      group_name <- trimws(toupper(readline(
        prompt = sprintf("  Enter group name for prefix '%s': ", pfx)
      )))

      if (nchar(group_name) == 0) {
        message("  ⚠ Group name cannot be empty. Please enter a name.")
        next
      }

      # Confirm
      confirm <- trimws(tolower(readline(
        prompt = sprintf("  Use '%s' as group name for %d animal(s)? (yes/no): ",
                         group_name, n)
      )))

      if (confirm %in% c("yes", "y")) {
        prefix_map[[pfx]] <- group_name

        # Ask to save to diet directory
        save_q <- trimws(tolower(readline(
          prompt = sprintf(
            "  Add '%s' → '%s' to your diet directory for future use? (yes/no): ",
            pfx, group_name
          )
        )))

        if (save_q %in% c("yes", "y")) {
          diet_dir[[pfx]] <- group_name
          .save_diet_dir(diet_dir)
          message(sprintf("  ✓ Saved '%s' → '%s' to diet directory", pfx, group_name))
        }

        message(sprintf("  ✓ '%s' → '%s'", pfx, group_name))
        break
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 5. Handle any leftover animal IDs that didn't match a clean prefix
  #    e.g. "CW1595R" where prefix = "CW" but that's ambiguous
  # ---------------------------------------------------------------------------
  # Check for prefixes not resolved
  unresolved <- working$Group[!working$prefix %in% names(prefix_map)]

  if (length(unresolved) > 0) {
    message(sprintf(
      "\n  ⚠ %d animal(s) could not be auto-assigned: %s",
      length(unresolved), paste(unresolved, collapse = ", ")
    ))

    # Get available group names already assigned
    available_groups <- unique(unlist(prefix_map))

    for (animal_id in unresolved) {
      message(sprintf("\n  Animal ID: '%s'", animal_id))
      message(sprintf(
        "  Available groups: %s",
        paste(available_groups, collapse = ", ")
      ))

      repeat {
        group_name <- trimws(toupper(readline(
          prompt = sprintf("  Which group does '%s' belong to? ", animal_id)
        )))

        if (nchar(group_name) == 0) {
          message("  ⚠ Cannot be empty.")
          next
        }

        confirm <- trimws(tolower(readline(
          prompt = sprintf("  Assign '%s' to group '%s'? (yes/no): ",
                           animal_id, group_name)
        )))

        if (confirm %in% c("yes", "y")) {
          # Store individual override
          prefix_map[[animal_id]] <- group_name
          message(sprintf("  ✓ '%s' → '%s'", animal_id, group_name))

          # Offer to save to directory
          save_q <- trimws(tolower(readline(
            prompt = "  Add this to diet directory for future use? (yes/no): "
          )))
          if (save_q %in% c("yes", "y")) {
            pfx_of_animal    <- extract_prefix(animal_id)
            diet_dir[[pfx_of_animal]] <- group_name
            .save_diet_dir(diet_dir)
            message(sprintf("  ✓ Saved to diet directory"))
          }
          break
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Apply group names to working data frame
  # ---------------------------------------------------------------------------
  working$Group <- sapply(seq_len(nrow(working)), function(i) {
    id  <- working$Group[i]
    pfx <- working$prefix[i]

    # Check individual override first (for leftovers like CW1595R)
    if (!is.null(prefix_map[[id]]))  return(prefix_map[[id]])
    # Then check prefix map
    if (!is.null(prefix_map[[pfx]])) return(prefix_map[[pfx]])
    # Fallback
    return(id)
  })

  # Drop helper prefix column
  working$prefix <- NULL

  # ---------------------------------------------------------------------------
  # 7. Show final table and confirm
  # ---------------------------------------------------------------------------
  message("\n=================================================")
  message("  ANCOVA Input Preview:")
  message("=================================================")
  message(sprintf("  %-10s %12s %12s", "Group", "EE", "LBM"))
  message(sprintf("  %s", paste(rep("-", 38), collapse = "")))
  for (i in seq_len(nrow(working))) {
    message(sprintf(
      "  %-10s %12.6f %12.5f",
      working$Group[i], working$EE[i], working$LBM[i]
    ))
  }
  message(sprintf("\n  Groups: %s",
                  paste(unique(working$Group), collapse = ", ")))

  confirm_final <- trimws(tolower(readline(
    prompt = "\n  Does this look correct? (yes/no): "
  )))

  if (!confirm_final %in% c("yes", "y")) {
    message("  Please re-run prepare_ancova_input() to reassign groups.")
    return(invisible(NULL))
  }

  # ---------------------------------------------------------------------------
  # 8. Add ANCOVA_Input sheet to existing EE_Summary.xlsx
  # ---------------------------------------------------------------------------
  wb <- openxlsx::loadWorkbook(ee_file)

  # Remove sheet if it already exists (re-run scenario)
  if ("ANCOVA_Input" %in% names(wb)) {
    openxlsx::removeWorksheet(wb, "ANCOVA_Input")
  }

  openxlsx::addWorksheet(wb, "ANCOVA_Input")

  header_style <- openxlsx::createStyle(
    fontColour     = "#FFFFFF",
    fgFill         = "#4472C4",
    halign         = "CENTER",
    textDecoration = "Bold",
    border         = "Bottom"
  )

  num_style <- openxlsx::createStyle(numFmt = "0.000000")

  openxlsx::writeData(
    wb,
    sheet       = "ANCOVA_Input",
    x           = working,
    headerStyle = header_style
  )

  openxlsx::addStyle(
    wb,
    sheet      = "ANCOVA_Input",
    style      = num_style,
    rows       = 2:(nrow(working) + 1),
    cols       = 2:3,
    gridExpand = TRUE
  )

  openxlsx::setColWidths(
    wb,
    sheet  = "ANCOVA_Input",
    cols   = 1:3,
    widths = c(12, 14, 14)
  )

  openxlsx::saveWorkbook(wb, ee_file, overwrite = TRUE)

  if (verbose) {
    message(sprintf("\n[Step 4] ANCOVA_Input sheet added to: %s", ee_file))
    message("[Step 4] Columns: Group | EE | LBM")
  }

  # ---------------------------------------------------------------------------
  # 9. Save CSV — same folder as ee_file unless output_csv specified
  # ---------------------------------------------------------------------------
  if (is.null(output_csv)) {
    output_csv <- file.path(
      dirname(ee_file),
      "ANCOVA_Input.csv"
    )
  }

  write.csv(working, output_csv, row.names = FALSE)

  if (verbose) {
    message(sprintf("[Step 4] CSV saved to        : %s", output_csv))
    message("[Step 4] Complete.")
  }

  return(invisible(working))
}
