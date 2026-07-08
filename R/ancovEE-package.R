#' ancovEE: ANCOVA-Based Energy Expenditure Analysis
#'
#' @description
#' A toolkit for mouse metabolic phenotyping energy expenditure (EE) analysis
#' following the NIDDK Mouse Metabolic Phenotyping Centers (MMPC) framework.
#'
#' ## Workflow
#' \enumerate{
#'   \item \strong{\code{\link{run_step1}}} — Parse NMR body composition Excel
#'         file (Bruker minispec) to extract lean body mass per animal with
#'         automatic group detection.
#'   \item \strong{run_step2} — Merge lean mass with energy expenditure data
#'         \emph{(coming soon)}
#'   \item \strong{run_step3} — Run ANCOVA EE analysis with interaction testing
#'         and Johnson-Neyman cutoffs \emph{(coming soon)}
#' }
#'
#' ## Quick Start
#' \preformatted{
#' library(ancovEE)
#'
#' # Step 1: Parse your NMR file
#' lean_data <- run_step1(
#'   input_file  = "NMR_INPUT.xlsx",
#'   output_file = "NMR_Lean_Output.xlsx"
#' )
#' }
#'
#' @keywords internal
"_PACKAGE"
