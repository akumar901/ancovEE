#' Run Step 4: Prepare ANCOVA Input
#'
#' @description
#' User-friendly wrapper for \code{\link{prepare_ancova_input}}.
#' **Step 4 of the ancovEE workflow.**
#'
#' Reads \code{Cage_Assignment} from \code{EE_Summary.xlsx}, renames columns,
#' intelligently parses group names from animal IDs using a persistent diet
#' directory, and saves a new \code{ANCOVA_Input} sheet to the same file.
#'
#' ## ancovEE Workflow
#' \enumerate{
#'   \item run_step1() — Parse NMR file → lean body mass
#'   \item run_step2() — Parse Promethion file → VO2, VCO2, RER, kcal_hr
#'   \item run_step3() — EE averages + cage-to-animal mapping
#'   \item \strong{run_step4()} — Prepare ANCOVA input sheet
#'   \item run_step5() — Run ANCOVA EE analysis \emph{(coming soon)}
#' }
#'
#' @param ee_file Character. Path to \code{EE_Summary.xlsx} from
#'   \code{run_step3()}.
#' @param verbose Logical. Print progress? Default \code{TRUE}.
#'
#' @return A \code{data.frame} with columns \code{Group}, \code{EE}, \code{LBM}.
#'
#' @examples
#' \dontrun{
#' ancova_data <- run_step4("EE_Summary.xlsx")
#' }
#'
#' @seealso \code{\link{prepare_ancova_input}}, \code{\link{view_diet_directory}},
#'   \code{\link{reset_diet_directory}}
#' @export
run_step4 <- function(ee_file,
                      output_csv = NULL,
                      verbose    = TRUE) {

  if (verbose) {
    message("=================================================")
    message("  ancovEE — Step 4: Preparing ANCOVA Input      ")
    message("=================================================")
    message(sprintf("File: %s\n", ee_file))
  }

  result <- prepare_ancova_input(
    ee_file    = ee_file,
    output_csv = output_csv,
    verbose    = verbose
  )

  if (verbose && !is.null(result)) {
    message("\n[Step 4] Complete. Run run_step5() for ANCOVA analysis.")
  }

  return(invisible(result))
}
