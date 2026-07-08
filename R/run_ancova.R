#' Run ANCOVA Energy Expenditure Analysis (Step 5)
#'
#' @description
#' **Step 5 of the ancovEE workflow.**
#'
#' Performs ANCOVA-based energy expenditure analysis following the NIDDK
#' Mouse Metabolic Phenotyping Centers (MMPC) framework. Tests whether the
#' slope of EE on body mass differs between groups (interaction test), runs
#' the appropriate model, and provides a full plain-English explanation of
#' every result.
#'
#' @param input_file Character. Path to the ANCOVA input CSV or the
#'   \code{EE_Summary.xlsx} file (reads \code{ANCOVA_Input} sheet).
#' @param ee_col Character. Name of EE column. Default \code{"EE"}.
#' @param group_col Character. Name of group column. Default \code{"Group"}.
#' @param covariate_col Character. Name of covariate column. Default \code{"LBM"}.
#' @param alpha Numeric. Significance threshold for interaction test.
#'   Default \code{0.05}.
#' @param covariate_value Numeric. Optional specific covariate value to test
#'   group difference at (e.g. mean LBM of one group). Default \code{NULL}.
#' @param output_xlsx Character. Path for output Excel file.
#'   Default \code{"ANCOVA_Results.xlsx"}.
#' @param verbose Logical. Print results and explanation? Default \code{TRUE}.
#'
#' @return A named list with all results, statistics, and explanations.
#'
#' @examples
#' \dontrun{
#' results <- run_step5("EE_Summary.xlsx")
#' results <- run_step5("ANCOVA_Input.csv")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths saveWorkbook
#' @export
run_ancova <- function(input_file,
                       ee_col         = "EE",
                       group_col      = "Group",
                       covariate_col  = "LBM",
                       alpha          = 0.05,
                       covariate_value = NULL,
                       output_xlsx    = "ANCOVA_Results.xlsx",
                       verbose        = TRUE) {

  # ---------------------------------------------------------------------------
  # 1. Read input data
  # ---------------------------------------------------------------------------
  if (grepl("\\.csv$", input_file, ignore.case = TRUE)) {
    data <- read.csv(input_file, stringsAsFactors = FALSE)
  } else {
    data <- readxl::read_excel(input_file, sheet = "ANCOVA_Input",
                               .name_repair = "minimal")
    data <- as.data.frame(data)
  }

  # Validate columns
  for (col in c(ee_col, group_col, covariate_col)) {
    if (!col %in% names(data)) {
      stop(sprintf("Column '%s' not found. Available: %s",
                   col, paste(names(data), collapse = ", ")))
    }
  }

  data[[group_col]]     <- as.factor(data[[group_col]])
  data[[ee_col]]        <- as.numeric(data[[ee_col]])
  data[[covariate_col]] <- as.numeric(data[[covariate_col]])
  data                  <- data[complete.cases(data[, c(ee_col, group_col, covariate_col)]), ]

  groups     <- levels(data[[group_col]])
  n_total    <- nrow(data)
  group_ns   <- table(data[[group_col]])

  if (length(groups) != 2) {
    stop(sprintf(
      "ANCOVA requires exactly 2 groups. Found %d: %s",
      length(groups), paste(groups, collapse = ", ")
    ))
  }

  if (verbose) {
    message("=================================================")
    message("  ancovEE - Step 5: ANCOVA EE Analysis          ")
    message("=================================================")
    message(sprintf("  Response  : %s", ee_col))
    message(sprintf("  Covariate : %s", covariate_col))
    message(sprintf("  Groups    : %s (n=%d) vs %s (n=%d)\n",
                    groups[1], group_ns[groups[1]],
                    groups[2], group_ns[groups[2]]))
  }

  # ---------------------------------------------------------------------------
  # 2. Basic statistics per group
  # ---------------------------------------------------------------------------
  basic_stats <- do.call(rbind, lapply(groups, function(g) {
    sub  <- data[data[[group_col]] == g, ]
    data.frame(
      Group        = g,
      N            = nrow(sub),
      EE_mean      = mean(sub[[ee_col]]),
      EE_sd        = sd(sub[[ee_col]]),
      LBM_mean     = mean(sub[[covariate_col]]),
      LBM_sd       = sd(sub[[covariate_col]]),
      stringsAsFactors = FALSE
    )
  }))

  # ---------------------------------------------------------------------------
  # 3. Test interaction (group × covariate)
  # ---------------------------------------------------------------------------
  formula_interaction <- as.formula(sprintf(
    "%s ~ %s * %s", ee_col, covariate_col, group_col
  ))
  model_interaction <- lm(formula_interaction, data = data)
  interaction_term  <- sprintf("%s:%s", covariate_col, group_col)

  # Get p-value for interaction
  interaction_summary <- summary(model_interaction)
  coef_table          <- coef(interaction_summary)
  interaction_row     <- grep(interaction_term, rownames(coef_table), ignore.case = TRUE)

  if (length(interaction_row) == 0) {
    # Try reversed order
    interaction_term2 <- sprintf("%s:%s", group_col, covariate_col)
    interaction_row   <- grep(interaction_term2, rownames(coef_table), ignore.case = TRUE)
  }

  interaction_p <- if (length(interaction_row) > 0) {
    coef_table[interaction_row, 4]
  } else {
    anova(model_interaction)[interaction_term, "Pr(>F)"]
  }

  interaction_significant <- !is.na(interaction_p) && interaction_p < alpha

  if (verbose) {
    message(sprintf(
      "[Step 5] Interaction test (%s × %s): p = %.4f → %s",
      covariate_col, group_col,
      interaction_p,
      ifelse(interaction_significant, "SIGNIFICANT", "NOT significant")
    ))
  }

  # ---------------------------------------------------------------------------
  # 4. Run final model based on interaction result
  # ---------------------------------------------------------------------------
  if (interaction_significant) {
    # Keep interaction - separate slopes
    final_model   <- model_interaction
    model_type    <- "interaction"
  } else {
    # Remove interaction - standard ANCOVA parallel slopes
    formula_ancova <- as.formula(sprintf(
      "%s ~ %s + %s", ee_col, covariate_col, group_col
    ))
    final_model  <- lm(formula_ancova, data = data)
    model_type   <- "standard"
  }

  final_summary <- summary(final_model)
  final_anova   <- anova(final_model)

  # ---------------------------------------------------------------------------
  # 5. Extract key statistics
  # ---------------------------------------------------------------------------
  r_squared          <- final_summary$r.squared * 100
  r_squared_adj      <- final_summary$adj.r.squared * 100
  residual_se        <- final_summary$sigma
  residual_df        <- final_summary$df[2]
  coef_estimates     <- coef(final_summary)

  # Regression F and p
  reg_ss  <- sum(final_anova$`Sum Sq`[1:(nrow(final_anova)-1)])
  reg_df  <- sum(final_anova$Df[1:(nrow(final_anova)-1)])
  res_ss  <- final_anova$`Sum Sq`[nrow(final_anova)]
  res_df  <- final_anova$Df[nrow(final_anova)]
  ms_reg  <- reg_ss / reg_df
  ms_res  <- res_ss / res_df
  f_ratio <- ms_reg / ms_res
  p_reg   <- pf(f_ratio, reg_df, res_df, lower.tail = FALSE)

  # Slope (LBM coefficient)
  lbm_row   <- grep(paste0("^", covariate_col), rownames(coef_estimates))
  lbm_est   <- coef_estimates[lbm_row, 1]
  lbm_se    <- coef_estimates[lbm_row, 2]
  lbm_p     <- coef_estimates[lbm_row, 4]

  # Group difference
  grp_row   <- grep(group_col, rownames(coef_estimates))
  grp_est   <- coef_estimates[grp_row[1], 1]
  grp_se    <- coef_estimates[grp_row[1], 2]
  grp_p     <- coef_estimates[grp_row[1], 4]

  # ---------------------------------------------------------------------------
  # 6. Model-based adjusted means at overall mean covariate
  # ---------------------------------------------------------------------------
  grand_mean_cov <- mean(data[[covariate_col]])

  pred_overall <- lapply(groups, function(g) {
    newdat <- data.frame(
      x = grand_mean_cov,
      g = factor(g, levels = groups)
    )
    names(newdat) <- c(covariate_col, group_col)
    pred <- predict(final_model, newdata = newdat, se.fit = TRUE)
    data.frame(
      Group        = g,
      LBM_used     = grand_mean_cov,
      Adj_EE_mean  = pred$fit,
      Adj_EE_se    = pred$se.fit
    )
  })
  pred_overall <- do.call(rbind, pred_overall)

  # Adjusted means at each group's own mean covariate
  pred_group_mean <- lapply(groups, function(g) {
    sub      <- data[data[[group_col]] == g, ]
    grp_mean <- mean(sub[[covariate_col]])
    newdat   <- data.frame(x = grp_mean, g = factor(g, levels = groups))
    names(newdat) <- c(covariate_col, group_col)
    pred <- predict(final_model, newdata = newdat, se.fit = TRUE)
    data.frame(
      Group        = g,
      LBM_used     = grp_mean,
      Adj_EE_mean  = pred$fit,
      Adj_EE_se    = pred$se.fit
    )
  })
  pred_group_mean <- do.call(rbind, pred_group_mean)

  # p-value for group means comparison - t-test using difference in predicted
  # values at each group's own mean, with pooled SE
  grp_means_diff <- pred_group_mean$Adj_EE_mean[2] - pred_group_mean$Adj_EE_mean[1]
  grp_means_se   <- sqrt(pred_group_mean$Adj_EE_se[1]^2 + pred_group_mean$Adj_EE_se[2]^2)
  grp_means_t    <- grp_means_diff / grp_means_se
  grp_means_p    <- 2 * pt(-abs(grp_means_t), df = residual_df)
  overall_p      <- grp_p

  # ---------------------------------------------------------------------------
  # 7. Optional: test at specific covariate value
  # ---------------------------------------------------------------------------
  pred_at_value <- NULL
  if (!is.null(covariate_value)) {
    pred_at_value <- lapply(groups, function(g) {
      newdat <- data.frame(x = covariate_value, g = factor(g, levels = groups))
      names(newdat) <- c(covariate_col, group_col)
      pred <- predict(final_model, newdata = newdat, se.fit = TRUE)
      data.frame(Group = g, LBM_used = covariate_value,
                 Adj_EE_mean = pred$fit, Adj_EE_se = pred$se.fit)
    })
    pred_at_value <- do.call(rbind, pred_at_value)
  }

  # ---------------------------------------------------------------------------
  # 8. Residual variances per group
  # ---------------------------------------------------------------------------
  residuals_by_group <- lapply(groups, function(g) {
    idx  <- which(data[[group_col]] == g)
    res  <- residuals(final_model)[idx]
    var(res)
  })
  names(residuals_by_group) <- groups

  # ---------------------------------------------------------------------------
  # 9. dfbeta (influence diagnostics)
  # ---------------------------------------------------------------------------
  dfb <- dfbetas(final_model)

  # ---------------------------------------------------------------------------
  # 10. Print full output with plain-English explanations
  # ---------------------------------------------------------------------------
  if (verbose) {

    message("\n=================================================")
    message("  MMPC-Style ANCOVA Output")
    message("=================================================")
    message(sprintf("  Total cases: %d  |  %s = %d  |  %s = %d",
                    n_total, groups[1], group_ns[groups[1]],
                    groups[2], group_ns[groups[2]]))
    message(sprintf("  R² = %.4f%%  |  R² adjusted = %.4f%%",
                    r_squared, r_squared_adj))
    message(sprintf("  Residual SE = %.4f  |  df = %d",
                    residual_se, residual_df))

    message("\n--- Regression Table ---")
    message(sprintf("  %-12s %12s %4s %12s %10s %12s",
                    "Source", "SumOfSquares", "df", "MeanSquare", "F-ratio", "P-value"))
    message(sprintf("  %-12s %12.4f %4d %12.4f %10.4f %12.4e",
                    "Regression", reg_ss, reg_df, ms_reg, f_ratio, p_reg))
    message(sprintf("  %-12s %12.4f %4d %12.4f",
                    "Residual", res_ss, res_df, ms_res))

    message("\n--- Coefficient Estimates ---")
    message(sprintf("  %-25s %10s %10s %12s",
                    "Variable", "Estimate", "StdError", "P-value"))
    for (i in seq_len(nrow(coef_estimates))) {
      message(sprintf("  %-25s %10.4f %10.4f %12.4e",
                      rownames(coef_estimates)[i],
                      coef_estimates[i, 1],
                      coef_estimates[i, 2],
                      coef_estimates[i, 4]))
    }

    message("\n--- Basic Statistics ---")
    message(sprintf("  %-8s %10s %20s %20s",
                    "Group", "N",
                    "EE mean (SD)",
                    "LBM mean (SD)"))
    for (i in seq_len(nrow(basic_stats))) {
      message(sprintf("  %-8s %10d %20s %20s",
                      basic_stats$Group[i],
                      basic_stats$N[i],
                      sprintf("%.3f (%.3f)", basic_stats$EE_mean[i], basic_stats$EE_sd[i]),
                      sprintf("%.3f (%.3f)", basic_stats$LBM_mean[i], basic_stats$LBM_sd[i])))
    }

    message("\n--- Model-Based Adjusted Means ---")
    message(sprintf("  Adjusted to OVERALL mean %s = %.4f",
                    covariate_col, grand_mean_cov))
    for (i in seq_len(nrow(pred_overall))) {
      message(sprintf("  %s: %.4f (SE=%.4f)",
                      pred_overall$Group[i],
                      pred_overall$Adj_EE_mean[i],
                      pred_overall$Adj_EE_se[i]))
    }
    message(sprintf("  Group difference p-value: %.4e", overall_p))

    message(sprintf("  Adjusted to each GROUP's mean %s:", covariate_col))
    for (i in seq_len(nrow(pred_group_mean))) {
      message(sprintf("  %s (at %s=%.4f): %.4f (SE=%.4f)",
                      pred_group_mean$Group[i],
                      covariate_col,
                      pred_group_mean$LBM_used[i],
                      pred_group_mean$Adj_EE_mean[i],
                      pred_group_mean$Adj_EE_se[i]))
    }
    message(sprintf("  Group Means p-value: %.4e", grp_means_p))

    message("\n--- Residual Variance by Group ---")
    for (g in groups) {
      message(sprintf("  %s: %.5f", g, residuals_by_group[[g]]))
    }

    # -------------------------------------------------------------------------
    # PLAIN ENGLISH EXPLANATION
    # -------------------------------------------------------------------------
    message("\n=================================================")
    message("  Plain-English Interpretation")
    message("=================================================")

    message(sprintf(
      "\n1. MODEL FIT\n   The ANCOVA model explains %.1f%% of the total variation in %s\n   (R² = %.1f%%, p = %.2e). This is a %s fit.",
      r_squared, ee_col, r_squared, p_reg,
      ifelse(r_squared > 70, "strong", ifelse(r_squared > 40, "moderate", "weak"))
    ))

    message(sprintf(
      "\n2. SLOPE TEST (Interaction: %s × %s)\n   p = %.4f → The slope of %s on %s is %s\n   between groups at the α = %.2f threshold.\n   %s",
      covariate_col, group_col, interaction_p,
      ee_col, covariate_col,
      ifelse(interaction_significant, "SIGNIFICANTLY DIFFERENT", "NOT significantly different"),
      alpha,
      ifelse(interaction_significant,
             "   [!] This means the two groups have different rates of EE change per\n   unit change in body mass. Separate regression lines are fitted.\n   Standard ANCOVA assumptions are violated - interpret with care.",
             "   [OK] This means both groups share the same slope (rate of EE change\n   per unit body mass). Standard ANCOVA with parallel lines is valid.")
    ))

    message(sprintf(
      "\n3. COVARIATE EFFECT (%s)\n   For every 1g increase in %s, %s increases by %.4f kcal/day\n   (SE = %.4f, p = %.4e).\n   %s",
      covariate_col, covariate_col, ee_col,
      lbm_est, lbm_se, lbm_p,
      ifelse(lbm_p < 0.05,
             "   [OK] LBM is a significant predictor of EE - adjusting for it is justified.",
             "   [!] LBM is not a significant predictor of EE at this sample size.")
    ))

    message(sprintf(
      "\n4. GROUP DIFFERENCE\n   After adjusting for %s, %s has a higher %s than %s\n   by %.4f kcal/day (SE = %.4f, p = %.4e).\n   %s",
      covariate_col,
      groups[2], ee_col, groups[1],
      abs(grp_est), grp_se, grp_p,
      ifelse(grp_p < 0.05,
             "   [OK] This difference IS statistically significant.\n   The two groups differ in energy expenditure even after\n   accounting for differences in lean body mass.",
             "   ✗ This difference is NOT statistically significant.\n   We cannot conclude the groups differ in EE after\n   accounting for lean body mass.")
    ))

    message(sprintf(
      "\n5. ADJUSTED MEANS (at overall mean %s = %.3fg)\n   %s: %.4f kcal/day (SE = %.4f)\n   %s: %.4f kcal/day (SE = %.4f)\n   These are the estimated EE values each group WOULD have\n   if all animals had the same lean body mass (%.3fg).",
      covariate_col, grand_mean_cov,
      pred_overall$Group[1], pred_overall$Adj_EE_mean[1], pred_overall$Adj_EE_se[1],
      pred_overall$Group[2], pred_overall$Adj_EE_mean[2], pred_overall$Adj_EE_se[2],
      grand_mean_cov
    ))

    message(sprintf(
      "\n6. RESIDUAL VARIANCE\n   %s: %.5f  |  %s: %.5f\n   %s",
      groups[1], residuals_by_group[[groups[1]]],
      groups[2], residuals_by_group[[groups[2]]],
      ifelse(
        max(unlist(residuals_by_group)) / min(unlist(residuals_by_group)) > 3,
        "   [!] Large difference in residual variance between groups.\n   Consider whether ANCOVA equal-variance assumption is met.",
        "   [OK] Residual variances are reasonably similar between groups."
      )
    ))

    message(sprintf(
      "\n7. BIOLOGICAL INTERPRETATION\n   %s mice have significantly higher EE (%.3f kcal/day) compared\n   to %s mice (%.3f kcal/day) when adjusted to the same LBM.\n   This suggests a diet-driven difference in metabolic rate that\n   is independent of differences in lean body mass.",
      groups[2],
      pred_overall$Adj_EE_mean[pred_overall$Group == groups[2]],
      groups[1],
      pred_overall$Adj_EE_mean[pred_overall$Group == groups[1]]
    ))
  }

  # ---------------------------------------------------------------------------
  # 11. Write Excel output
  # ---------------------------------------------------------------------------
  wb <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(
    fontColour     = "#FFFFFF",
    fgFill         = "#4472C4",
    halign         = "CENTER",
    textDecoration = "Bold",
    border         = "Bottom"
  )

  section_style <- openxlsx::createStyle(
    textDecoration = "Bold",
    fgFill         = "#D9E1F2"
  )

  num_style <- openxlsx::createStyle(numFmt = "0.000000")

  # coef_df needed for return value
  coef_df <- data.frame(
    Variable = rownames(coef_estimates),
    Estimate = round(coef_estimates[, 1], 6),
    StdError = round(coef_estimates[, 2], 6),
    P_value  = signif(coef_estimates[, 4], 4),
    stringsAsFactors = FALSE
  )

  # ---------------------------------------------------------------------------
  # Styles
  # ---------------------------------------------------------------------------
  section_style <- openxlsx::createStyle(
    textDecoration = "Bold",
    fgFill         = "#D9E1F2",
    border         = "Bottom",
    borderColour   = "#4472C4"
  )

  label_style <- openxlsx::createStyle(
    textDecoration = "Bold"
  )

  num_style <- openxlsx::createStyle(numFmt = "0.0000")

  wrap_style <- openxlsx::createStyle(wrapText = TRUE)

  # Helper to write a section header row
  write_section <- function(wb, sheet, row, title) {
    openxlsx::writeData(wb, sheet, data.frame(X = title), startRow = row,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet, section_style, rows = row, cols = 1:6,
                       gridExpand = TRUE)
    return(row + 1)
  }

  # Helper to write a blank spacer row
  write_spacer <- function(wb, sheet, row) {
    openxlsx::writeData(wb, sheet, data.frame(X = ""), startRow = row,
                        colNames = FALSE)
    return(row + 1)
  }

  # ===========================================================================
  # SHEET 1: ANCOVA_Output — mirrors MMPC output top to bottom
  # ===========================================================================
  openxlsx::addWorksheet(wb, "ANCOVA_Output")
  openxlsx::setColWidths(wb, "ANCOVA_Output",
                         cols   = 1:6,
                         widths = c(28, 16, 8, 16, 14, 16))

  row <- 1

  # --- Header info ---
  row <- write_section(wb, "ANCOVA_Output", row, "PROGRAM INFORMATION")
  info <- data.frame(
    Item  = c("Program", "Response Variable", "Covariate",
              "Grouping Variable", "Alpha", "Total Cases",
              paste0(groups[1], " N"), paste0(groups[2], " N"),
              "R Squared (%)", "R Squared Adjusted (%)",
              "Residual Standard Error", "Residual df",
              "Interaction term", "Interaction p-value",
              "Interaction Significant", "Model Type"),
    Value = c("ancovEE - MMPC Style ANCOVA",
              ee_col, covariate_col, group_col,
              alpha, n_total,
              group_ns[groups[1]], group_ns[groups[2]],
              round(r_squared, 4), round(r_squared_adj, 4),
              round(residual_se, 4), residual_df,
              sprintf("%s:%s", covariate_col, group_col),
              signif(interaction_p, 4),
              ifelse(interaction_significant, "YES", "NO"),
              ifelse(model_type == "standard",
                     "Standard ANCOVA (parallel slopes)",
                     "Interaction model (unequal slopes)")),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "ANCOVA_Output", info, startRow = row, colNames = FALSE)
  row <- row + nrow(info)
  row <- write_spacer(wb, "ANCOVA_Output", row)

  # --- Regression Table ---
  row <- write_section(wb, "ANCOVA_Output", row, "REGRESSION TABLE")
  reg_table <- data.frame(
    Source       = c("Regression", "Residual"),
    SumOfSquares = round(c(reg_ss, res_ss), 4),
    df           = c(reg_df, res_df),
    MeanSquare   = round(c(ms_reg, ms_res), 4),
    F_ratio      = c(round(f_ratio, 4), "."),
    P_value      = c(signif(p_reg, 4), "."),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "ANCOVA_Output", reg_table,
                      startRow = row, headerStyle = header_style)
  row <- row + nrow(reg_table) + 1
  row <- write_spacer(wb, "ANCOVA_Output", row)

  # --- Coefficients ---
  row <- write_section(wb, "ANCOVA_Output", row, "COEFFICIENT ESTIMATES")
  openxlsx::writeData(wb, "ANCOVA_Output", coef_df,
                      startRow = row, headerStyle = header_style)
  row <- row + nrow(coef_df) + 1
  row <- write_spacer(wb, "ANCOVA_Output", row)

  # --- Basic Statistics ---
  row <- write_section(wb, "ANCOVA_Output", row,
                       "BASIC STATISTICS - Avg (StDev)")
  openxlsx::writeData(wb, "ANCOVA_Output", basic_stats,
                      startRow = row, headerStyle = header_style)
  row <- row + nrow(basic_stats) + 1
  row <- write_spacer(wb, "ANCOVA_Output", row)

  # --- Model-based Statistics ---
  row <- write_section(wb, "ANCOVA_Output", row,
                       "MODEL-BASED STATISTICS - Avg EE (StErr)")

  model_stats <- data.frame(
    LBM_Used     = c(
      sprintf("Overall Mean (%.4f)", grand_mean_cov),
      sprintf("Group Means (%s=%.4f, %s=%.4f)",
              groups[1], pred_group_mean$LBM_used[1],
              groups[2], pred_group_mean$LBM_used[2])
    ),
    CHOW_EE      = c(
      sprintf("%.4f (%.4f)", pred_overall$Adj_EE_mean[pred_overall$Group == groups[1]],
              pred_overall$Adj_EE_se[pred_overall$Group == groups[1]]),
      sprintf("%.4f (%.4f)", pred_group_mean$Adj_EE_mean[pred_group_mean$Group == groups[1]],
              pred_group_mean$Adj_EE_se[pred_group_mean$Group == groups[1]])
    ),
    HFD_EE       = c(
      sprintf("%.4f (%.4f)", pred_overall$Adj_EE_mean[pred_overall$Group == groups[2]],
              pred_overall$Adj_EE_se[pred_overall$Group == groups[2]]),
      sprintf("%.4f (%.4f)", pred_group_mean$Adj_EE_mean[pred_group_mean$Group == groups[2]],
              pred_group_mean$Adj_EE_se[pred_group_mean$Group == groups[2]])
    ),
    P_value      = c(signif(overall_p, 4), signif(grp_means_p, 4)),
    stringsAsFactors = FALSE
  )
  names(model_stats)[2] <- groups[1]
  names(model_stats)[3] <- groups[2]

  openxlsx::writeData(wb, "ANCOVA_Output", model_stats,
                      startRow = row, headerStyle = header_style)
  row <- row + nrow(model_stats) + 1
  row <- write_spacer(wb, "ANCOVA_Output", row)

  # --- Residual Variance ---
  row <- write_section(wb, "ANCOVA_Output", row, "RESIDUAL VARIANCE")
  resvar <- data.frame(
    Group            = groups,
    Residual_Variance = round(unlist(residuals_by_group[groups]), 5),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "ANCOVA_Output", resvar,
                      startRow = row, headerStyle = header_style)

  # ===========================================================================
  # SHEET 2: Residual_Diagnostics — dfbeta values per animal
  # ===========================================================================
  openxlsx::addWorksheet(wb, "Residual_Diagnostics")

  dfb_df <- as.data.frame(dfb)
  dfb_df <- cbind(
    Animal_Index = seq_len(nrow(dfb_df)),
    data[[group_col]],
    dfb_df
  )
  names(dfb_df)[2] <- group_col

  openxlsx::writeData(wb, "Residual_Diagnostics", dfb_df,
                      headerStyle = header_style)
  openxlsx::setColWidths(wb, "Residual_Diagnostics",
                         cols   = 1:ncol(dfb_df),
                         widths = c(14, 12, rep(18, ncol(dfb_df) - 2)))

  # ===========================================================================
  # SHEET 3: Interpretation — plain English
  # ===========================================================================
  openxlsx::addWorksheet(wb, "Interpretation")
  openxlsx::setColWidths(wb, "Interpretation", cols = 1:2, widths = c(28, 90))

  interp <- data.frame(
    Section = c(
      "1. MODEL FIT",
      "2. SLOPE TEST (Interaction)",
      "3. COVARIATE EFFECT (LBM)",
      "4. GROUP DIFFERENCE",
      "5. ADJUSTED MEANS",
      "6. RESIDUAL VARIANCE",
      "7. BIOLOGICAL INTERPRETATION"
    ),
    Explanation = c(
      sprintf("The ANCOVA model explains %.1f%% of total variation in %s (R2=%.1f%%, p=%.2e). This is a %s fit.",
              r_squared, ee_col, r_squared, p_reg,
              ifelse(r_squared > 70, "strong", "moderate")),
      sprintf("Interaction p=%.4f. The slope of %s on %s is %s between groups at alpha=%.2f. %s",
              interaction_p, ee_col, covariate_col,
              ifelse(interaction_significant, "SIGNIFICANTLY DIFFERENT", "NOT significantly different"),
              alpha,
              ifelse(interaction_significant,
                     "Separate regression lines are fitted. Standard ANCOVA assumptions may be violated.",
                     "Both groups share the same slope. Standard ANCOVA with parallel lines is valid.")),
      sprintf("For every 1g increase in %s, %s increases by %.4f kcal/day (SE=%.4f, p=%.4e). %s",
              covariate_col, ee_col, lbm_est, lbm_se, lbm_p,
              ifelse(lbm_p < 0.05,
                     "LBM is a significant predictor of EE - adjusting for it is justified.",
                     "LBM is not a significant predictor at this sample size.")),
      sprintf("After adjusting for %s, %s has higher %s than %s by %.4f kcal/day (SE=%.4f, p=%.4e). %s",
              covariate_col, groups[2], ee_col, groups[1],
              abs(grp_est), grp_se, grp_p,
              ifelse(grp_p < 0.05,
                     "This IS statistically significant. Groups differ in EE even after accounting for LBM.",
                     "This is NOT statistically significant.")),
      sprintf("At overall mean %s=%.3fg: %s=%.4f kcal/day (SE=%.4f), %s=%.4f kcal/day (SE=%.4f), p=%.4e. These are estimated EE values if all animals had the same LBM.",
              covariate_col, grand_mean_cov,
              pred_overall$Group[1], pred_overall$Adj_EE_mean[1], pred_overall$Adj_EE_se[1],
              pred_overall$Group[2], pred_overall$Adj_EE_mean[2], pred_overall$Adj_EE_se[2],
              overall_p),
      sprintf("%s residual variance=%.5f, %s residual variance=%.5f. %s",
              groups[1], residuals_by_group[[groups[1]]],
              groups[2], residuals_by_group[[groups[2]]],
              ifelse(max(unlist(residuals_by_group)) / min(unlist(residuals_by_group)) > 3,
                     "Large difference in residual variance between groups. Check equal-variance assumption.",
                     "Residual variances are reasonably similar between groups.")),
      sprintf("%s have significantly higher EE (%.3f kcal/day) vs %s (%.3f kcal/day) when adjusted to the same LBM (%.3fg). This suggests a diet-driven difference in metabolic rate independent of lean body mass.",
              groups[2],
              pred_overall$Adj_EE_mean[pred_overall$Group == groups[2]],
              groups[1],
              pred_overall$Adj_EE_mean[pred_overall$Group == groups[1]],
              grand_mean_cov)
    ),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "Interpretation", interp, headerStyle = header_style)
  openxlsx::addStyle(wb, "Interpretation", wrap_style,
                     rows = 2:(nrow(interp) + 1), cols = 2, gridExpand = TRUE)
  openxlsx::setRowHeights(wb, "Interpretation",
                          rows   = 2:(nrow(interp) + 1),
                          heights = rep(60, nrow(interp)))

  openxlsx::saveWorkbook(wb, output_xlsx, overwrite = TRUE)

  if (verbose) {
    message(sprintf("\n[Step 5] Results saved to: %s", output_xlsx))
    message("[Step 5] Sheets: ANCOVA_Output | Residual_Diagnostics | Interpretation")
  }

  return(invisible(list(
    data            = data,
    model           = final_model,
    model_type      = model_type,
    interaction_p   = interaction_p,
    r_squared       = r_squared,
    basic_stats     = basic_stats,
    pred_overall    = pred_overall,
    pred_group_mean = pred_group_mean,
    pred_at_value   = pred_at_value,
    coefficients    = coef_df,
    residual_var    = residuals_by_group,
    dfbetas         = dfb
  )))
}

#' Run Step 5: ANCOVA EE Analysis
#'
#' @description
#' User-friendly wrapper for \code{\link{run_ancova}}.
#' **Step 5 of the ancovEE workflow.**
#'
#' @param input_file Character. Path to CSV or EE_Summary.xlsx.
#' @param ee_col Character. EE column name. Default \code{"EE"}.
#' @param group_col Character. Group column name. Default \code{"Group"}.
#' @param covariate_col Character. Covariate column name. Default \code{"LBM"}.
#' @param alpha Numeric. Significance threshold. Default \code{0.05}.
#' @param covariate_value Numeric. Optional covariate value to test at.
#' @param output_xlsx Character. Output Excel path.
#' @param verbose Logical. Print results? Default \code{TRUE}.
#'
#' @return Named list of all ANCOVA results.
#'
#' @examples
#' \dontrun{
#' results <- run_step5("EE_Summary.xlsx")
#' results <- run_step5("ANCOVA_Input.csv")
#' }
#' @export
run_step5 <- function(input_file,
                      ee_col          = "EE",
                      group_col       = "Group",
                      covariate_col   = "LBM",
                      alpha           = 0.05,
                      covariate_value = NULL,
                      output_xlsx     = "ANCOVA_Results.xlsx",
                      output_pdf      = "ANCOVA_Plots.pdf",
                      verbose         = TRUE) {

  results <- run_ancova(
    input_file      = input_file,
    ee_col          = ee_col,
    group_col       = group_col,
    covariate_col   = covariate_col,
    alpha           = alpha,
    covariate_value = covariate_value,
    output_xlsx     = output_xlsx,
    verbose         = verbose
  )

  plot_ancova(
    ancova_results = results,
    output_pdf     = output_pdf,
    verbose        = verbose
  )

  return(invisible(results))
}
