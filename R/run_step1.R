#' Run Step 1: Parse NMR Body Composition File
#'
#' @description
#' User-friendly wrapper for \code{\link{parse_nmr_lean}}.
#' This is the **entry point** for the ancovEE workflow.
#'
#' Provide your Bruker minispec NMR \code{.xlsx} file and an output path.
#' The function extracts lean body mass per animal and saves a clean
#' 4-column Excel file ready for the next step.
#'
#' ## ancovEE Workflow
#' \enumerate{
#'   \item \strong{run_step1()} — Parse NMR file → Sample_Name, Compound, Mass, Unit
#'   \item run_step2() — Merge with energy expenditure data \emph{(coming soon)}
#'   \item run_step3() — Run ANCOVA EE analysis \emph{(coming soon)}
#' }
#'
#' @param input_file Character. Path to your NMR \code{.xlsx} file from the
#'   Bruker minispec system.
#' @param output_file Character. Where to save the output \code{.xlsx}.
#'   Defaults to \code{"NMR_Lean_Output.xlsx"} in the current working directory.
#' @param verbose Logical. Print progress to console? Default \code{TRUE}.
#'
#' @return A \code{data.frame} with 4 columns:
#'   \code{Sample_Name}, \code{Compound}, \code{Mass}, \code{Unit}.
#'   Also writes the result to \code{output_file}.
#'
#' @examples
#' \dontrun{
#' lean_data <- run_step1(
#'   input_file  = "NMR_INPUT.xlsx",
#'   output_file = "NMR_Lean_Output.xlsx"
#' )
#'
#' head(lean_data)
#' }
#'
#' @seealso \code{\link{parse_nmr_lean}} for the full function with all details.
#'
#' @export
run_step1 <- function(input_file,
                      output_file = "NMR_Lean_Output.xlsx",
                      verbose     = TRUE) {

  if (verbose) {
    message("=================================================")
    message("  ancovEE — Step 1: Parsing NMR Body Composition")
    message("=================================================")
    message(sprintf("Input : %s", input_file))
    message(sprintf("Output: %s\n", output_file))
  }

  result <- parse_nmr_lean(
    input_file  = input_file,
    output_file = output_file,
    verbose     = verbose
  )

  if (verbose) {
    message("\n[Step 1] Complete. Pass the output to run_step2() when ready.")
  }

  return(invisible(result))
}
