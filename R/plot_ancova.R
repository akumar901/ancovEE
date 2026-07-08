#' Generate ANCOVA Regression and Diagnostic Plots
#'
#' @description
#' Generates three plots matching the MMPC regression analysis style:
#' \enumerate{
#'   \item \strong{Regression Plot} — top, full width
#'   \item \strong{dfbeta LBM} — bottom left
#'   \item \strong{dfbeta Group} — bottom right
#' }
#'
#' @param ancova_results A named list returned by \code{\link{run_step5}}.
#' @param output_pdf Character. Path for the output PDF.
#' @param group_colors Character vector of length 2. Colors for the two groups.
#' @param verbose Logical. Print progress? Default \code{TRUE}.
#'
#' @return Invisibly returns the file path of the saved PDF.
#'
#' @examples
#' \dontrun{
#' results <- run_step5("ANCOVA_Input.csv")
#' plot_ancova(results, output_pdf = "ANCOVA_Plots.pdf")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline
#'   scale_color_manual labs theme_bw theme element_text margin
#' @importFrom gridExtra grid.arrange arrangeGrob
#'
#' @export
plot_ancova <- function(ancova_results,
                        output_pdf   = "ANCOVA_Plots.pdf",
                        group_colors = c("#2196F3", "#4CAF50"),
                        verbose      = TRUE) {

  if (!requireNamespace("ggplot2",   quietly = TRUE)) stop("Package 'ggplot2' required.")
  if (!requireNamespace("gridExtra", quietly = TRUE)) stop("Package 'gridExtra' required.")

  # ---------------------------------------------------------------------------
  # Extract components
  # ---------------------------------------------------------------------------
  data       <- ancova_results$data
  model      <- ancova_results$model
  model_type <- ancova_results$model_type
  dfb        <- ancova_results$dfbetas

  group_col <- names(data)[sapply(data, is.factor)][1]
  num_cols  <- names(data)[sapply(data, is.numeric)]
  ee_col    <- num_cols[1]
  cov_col   <- num_cols[2]
  groups    <- levels(data[[group_col]])

  names(group_colors) <- groups

  # Common theme for all plots
  base_theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 10,
                                               hjust = 0.5),
      plot.caption     = ggplot2::element_text(
                           size       = 8,
                           hjust      = 0,
                           vjust      = 1,
                           color      = "gray20",
                           lineheight = 1.3,
                           margin     = ggplot2::margin(t = 8)
                         ),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(8, 8, 40, 8)
    )

  # ---------------------------------------------------------------------------
  # PLOT 1: Regression — top full width
  # ---------------------------------------------------------------------------
  caption1 <- paste(
    "Figure 1. Regression Plot.",
    "Each point represents one animal.",
    "The x-axis shows lean body mass (LBM, g) and the y-axis shows energy expenditure (EE, kcal/day).",
    "Colored lines are the ANCOVA-fitted regression lines for each group.",
    if (model_type == "standard") {
      "Lines are parallel because slopes did NOT differ significantly between groups\n(interaction p > 0.05) - standard ANCOVA is appropriate."
    } else {
      "Lines are NOT parallel - slopes differ significantly between groups\n(interaction p < 0.05). Separate slopes are fitted per group."
    },
    sep = "\n"
  )

  caption2 <- paste(
    "Figure 2. dfbeta Plot for LBM.",
    "Each point shows how much the LBM slope estimate would change if that animal were removed.",
    "Points far from zero indicate influential animals that have a large impact on the LBM slope.",
    "A robust result shows no single animal dominating the estimate.",
    "If one point is unusually large, investigate that data point carefully.",
    sep = "\n"
  )

  caption3 <- paste(
    "Figure 3. dfbeta Plot for Group.",
    "Each point shows how much the group difference estimate would change if that animal were removed.",
    "Points far from zero indicate animals that strongly influence the group comparison result.",
    "A robust group difference should remain consistent even when any single animal is excluded.",
    sep = "\n"
  )

  p1 <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x     = .data[[cov_col]],
      y     = .data[[ee_col]],
      color = .data[[group_col]]
    )
  ) +
    ggplot2::geom_point(size = 3, shape = 15) +
    ggplot2::geom_smooth(method = "lm", se = FALSE,
                         linewidth = 1, formula = y ~ x) +
    ggplot2::scale_color_manual(values = group_colors) +
    ggplot2::labs(
      title   = "MMPC Regression Analysis Data Plots.",
      x       = paste0('"', cov_col, '"'),
      y       = paste0('"', ee_col, '"'),
      color   = "Group",
      caption = caption1
    ) +
    base_theme +
    ggplot2::theme(legend.position = "top")

  # ---------------------------------------------------------------------------
  # PLOT 2: dfbeta — Covariate (LBM) — bottom left
  # ---------------------------------------------------------------------------
  cov_dfb_col <- grep(cov_col, colnames(dfb), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(cov_dfb_col)) cov_dfb_col <- colnames(dfb)[2]

  dfb_cov <- data.frame(
    Index  = seq_len(nrow(dfb)),
    dfbeta = dfb[, cov_dfb_col]
  )

  p2 <- ggplot2::ggplot(dfb_cov, ggplot2::aes(x = Index, y = dfbeta)) +
    ggplot2::geom_point(color = "#2196F3", size = 2, shape = 15) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::labs(
      title   = paste0('dfbeta Plot - "', cov_col, '"'),
      x       = "Index",
      y       = paste0('"', cov_col, '"'),
      caption = caption2
    ) +
    base_theme +
    ggplot2::scale_x_continuous(breaks = seq(0, nrow(dfb) + 1, by = 2))

  # ---------------------------------------------------------------------------
  # PLOT 3: dfbeta — Group — bottom right
  # ---------------------------------------------------------------------------
  grp_dfb_col <- grep(group_col, colnames(dfb), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(grp_dfb_col)) grp_dfb_col <- colnames(dfb)[3]

  dfb_grp <- data.frame(
    Index  = seq_len(nrow(dfb)),
    dfbeta = dfb[, grp_dfb_col]
  )

  p3 <- ggplot2::ggplot(dfb_grp, ggplot2::aes(x = Index, y = dfbeta)) +
    ggplot2::geom_point(color = "#2196F3", size = 2, shape = 15) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::labs(
      title   = paste0('dfbeta Plot - "', group_col, '"'),
      x       = "Index",
      y       = paste0('"', group_col, '"'),
      caption = caption3
    ) +
    base_theme +
    ggplot2::scale_x_continuous(breaks = seq(0, nrow(dfb) + 1, by = 2))

  # ---------------------------------------------------------------------------
  # Save all 3 plots to PDF — wide enough for full captions
  # ---------------------------------------------------------------------------
  pdf(output_pdf, width = 14, height = 22)

  gridExtra::grid.arrange(
    p1,
    gridExtra::arrangeGrob(p2, p3, ncol = 2),
    nrow    = 2,
    heights = c(1.4, 1.6)
  )

  dev.off()

  if (verbose) {
    message(sprintf("[Step 5] Plots saved to: %s", output_pdf))
    message("[Step 5] Layout: Regression (top) | dfbeta LBM + dfbeta Group (bottom side by side)")
  }

  return(invisible(output_pdf))
}
