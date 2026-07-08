#' Run Step 3: Energy Expenditure Summary and Cage Assignment
#'
#' @description
#' User-friendly wrapper for \code{\link{calc_ee_summary}}.
#' **Step 3 of the ancovEE workflow.**
#'
#' Calculates Avg_kcal_hr and Avg_kcal_day per cage from the Promethion
#' output, then interactively maps each cage position to an animal ID
#' from the NMR file. Outputs a single Excel file with two sheets:
#' \code{EE_Summary} and \code{Cage_Assignment}.
#'
#' ## ancovEE Workflow
#' \enumerate{
#'   \item run_step1() — Parse NMR file → lean body mass
#'   \item run_step2() — Parse Promethion file → VO2, VCO2, RER, kcal_hr
#'   \item \strong{run_step3()} — EE averages + cage-to-animal mapping
#'   \item run_step4() — Run ANCOVA EE analysis \emph{(coming soon)}
#' }
#'
#' @param promethion_file Character. Path to the Promethion output \code{.xlsx}
#'   from \code{run_step2()}.
#' @param nmr_file Character. Path to the NMR lean mass \code{.xlsx}
#'   from \code{run_step1()}.
#' @param output_xlsx Character. Path for the output Excel file.
#'   Defaults to \code{"EE_Summary.xlsx"}.
#' @param verbose Logical. Print progress? Default \code{TRUE}.
#'
#' @return A named list with \code{EE_Summary} and \code{Cage_Assignment}
#'   data.frames.
#'
#' @examples
#' \dontrun{
#' result <- run_step3(
#'   promethion_file = "Promethion_Output.xlsx",
#'   nmr_file        = "NMR_Lean_Output.xlsx",
#'   output_xlsx     = "EE_Summary.xlsx"
#' )
#' }
#'
#' @seealso \code{\link{calc_ee_summary}}
#' @export
run_step3 <- function(promethion_file,
                      nmr_file,
                      output_xlsx = "EE_Summary.xlsx",
                      verbose     = TRUE) {

  if (verbose) {
    message("=================================================")
    message("  ancovEE — Step 3: EE Summary & Cage Assignment")
    message("=================================================")
    message(sprintf("Promethion file : %s", promethion_file))
    message(sprintf("NMR file        : %s", nmr_file))
    message(sprintf("Output          : %s\n", output_xlsx))
  }

  result <- calc_ee_summary(
    promethion_file = promethion_file,
    nmr_file        = nmr_file,
    output_xlsx     = output_xlsx,
    verbose         = verbose
  )

  if (verbose) {
    message("\n[Step 3] Complete. Pass EE_Summary.xlsx to run_step4() when ready.")
  }

  return(invisible(result))
}
