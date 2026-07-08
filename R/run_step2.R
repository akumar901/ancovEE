#' Run Step 2: Parse Promethion Indirect Calorimetry File
#'
#' @description
#' User-friendly wrapper for \code{\link{parse_promethion}}.
#' **Step 2 of the ancovEE workflow.**
#'
#' Reads the Macro13 sheet from a Promethion Sable Systems Excel export,
#' shows the available datetime range, prompts for start/end datetimes,
#' and extracts VO2, VCO2, RER and kcal_hr for all chambers into a
#' single Excel file with one sheet per parameter.
#'
#' ## ancovEE Workflow
#' \enumerate{
#'   \item run_step1() — Parse NMR file → lean body mass
#'   \item \strong{run_step2()} — Parse Promethion file → VO2, VCO2, RER, kcal_hr
#'   \item run_step3() — Run ANCOVA EE analysis \emph{(coming soon)}
#' }
#'
#' @param input_file Character. Path to the Promethion \code{.xlsx} file.
#' @param output_file Character. Path for the output \code{.xlsx}.
#'   Defaults to \code{"Promethion_Output.xlsx"}.
#' @param start_datetime Character. Start datetime as \code{"YYYY/MM/DD HH:MM:SS"}.
#'   If \code{NULL} (default), user is prompted interactively.
#' @param end_datetime Character. End datetime as \code{"YYYY/MM/DD HH:MM:SS"}.
#'   If \code{NULL} (default), user is prompted interactively.
#' @param verbose Logical. Print progress? Default \code{TRUE}.
#'
#' @return A named list of 4 data.frames: \code{VO2}, \code{VCO2},
#'   \code{RER}, \code{kcal_hr}.
#'
#' @examples
#' \dontrun{
#' # Interactive — prompts for datetime range
#' result <- run_step2("Promethion.xlsx")
#'
#' # Non-interactive — supply datetimes directly
#' result <- run_step2(
#'   input_file     = "Promethion.xlsx",
#'   output_file    = "Promethion_Output.xlsx",
#'   start_datetime = "2025/10/31 12:25:26",
#'   end_datetime   = "2025/11/03 14:25:26"
#' )
#' }
#'
#' @seealso \code{\link{parse_promethion}}
#' @export
run_step2 <- function(input_file,
                      output_file    = "Promethion_Output.xlsx",
                      start_datetime = NULL,
                      end_datetime   = NULL,
                      verbose        = TRUE) {

  if (verbose) {
    message("=================================================")
    message("  ancovEE — Step 2: Parsing Promethion IC Data  ")
    message("=================================================")
    message(sprintf("Input : %s", input_file))
    message(sprintf("Output: %s\n", output_file))
  }

  result <- parse_promethion(
    input_file     = input_file,
    output_file    = output_file,
    start_datetime = start_datetime,
    end_datetime   = end_datetime,
    verbose        = verbose
  )

  if (verbose) {
    message("\n[Step 2] Complete. Pass the output to run_step3() when ready.")
  }

  return(invisible(result))
}
