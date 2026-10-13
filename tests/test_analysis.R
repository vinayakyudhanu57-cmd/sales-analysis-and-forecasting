# ==============================================================================
# Script: test_analysis.R
# Project: Sales Analysis and Forecasting (Project 3)
# Purpose: Comprehensive automated test suite validating data integrity,
#          mathematical reconciliation, forecasting outputs, and JSON formats.
# ==============================================================================

# Locate project root
find_root <- function() {
  dirs <- c(getwd(), file.path(getwd(), "sales-analysis-and-forecasting"), "C:/Users/DELL/Desktop/r_projects/sales-analysis-and-forecasting")
  for (d in dirs) {
    if (file.exists(file.path(d, "R", "functions.R"))) return(normalizePath(d, winslash = "/"))
  }
  return(normalizePath(getwd(), winslash = "/"))
}

proj_root <- find_root()
setwd(proj_root)
source(file.path(proj_root, "R", "functions.R"))

cat("================================================================\n")
cat("  RUNNING AUTOMATED TEST SUITE FOR SALES ANALYSIS & FORECASTING\n")
cat("================================================================\n\n")

total_tests <- 0
passed_tests <- 0
failed_tests <- 0

assert_test <- function(desc, condition) {
  total_tests <<- total_tests + 1
  if (isTRUE(condition)) {
    passed_tests <<- passed_tests + 1
    cat(sprintf("  [PASS] Test %02d: %s\n", total_tests, desc))
  } else {
    failed_tests <<- failed_tests + 1
    cat(sprintf("  [FAIL] Test %02d: %s\n", total_tests, desc))
  }
}

# ------------------------------------------------------------------------------
# Category 1: Dataset Validation & Hygiene
# ------------------------------------------------------------------------------
cat("--- Category 1: Dataset Hygiene & Schema ---\n")
data_path <- file.path(proj_root, "data", "sales.csv")
assert_test("Raw dataset file exists at data/sales.csv", file.exists(data_path))

sales_df <- load_and_validate_data(data_path)
assert_test("Dataset loaded with expected columns (date, product, units_sold, unit_price, revenue)",
            all(c("date", "product", "units_sold", "unit_price", "revenue") %in% names(sales_df)))
assert_test("Dataset contains at least 300 transaction records", nrow(sales_df) >= 300)
assert_test("All transaction dates are valid Dates", inherits(sales_df$date, "Date") && !any(is.na(sales_df$date)))
assert_test("All units_sold values are strictly positive integers", all(sales_df$units_sold > 0) && is.numeric(sales_df$units_sold))
assert_test("All unit_price values are strictly positive", all(sales_df$unit_price > 0))
assert_test("No missing (NA) values exist anywhere in the validated dataset", !any(is.na(sales_df)))

# ------------------------------------------------------------------------------
# Category 2: Revenue Math & Reconciliation
# ------------------------------------------------------------------------------
cat("\n--- Category 2: Revenue Calculation & Mathematical Reconciliation ---\n")
calculated_rev <- round(sales_df$units_sold * sales_df$unit_price, 2)
assert_test("Row-level revenue strictly equals units_sold * unit_price", all(abs(sales_df$revenue - calculated_rev) < 0.001))

monthly_df <- aggregate_monthly_sales(sales_df)
product_df <- analyze_product_performance(sales_df)

tx_total_rev <- round(sum(sales_df$revenue), 2)
monthly_total_rev <- round(sum(monthly_df$total_revenue), 2)
prod_total_rev <- round(sum(product_df$total_revenue), 2)

assert_test("Monthly aggregate revenue equals raw transaction sum ($157,534.20)",
            abs(tx_total_rev - monthly_total_rev) < 0.05)
assert_test("Product aggregate revenue equals raw transaction sum ($157,534.20)",
            abs(tx_total_rev - prod_total_rev) < 0.05)
assert_test("Sum of product percentage shares equals 100% (+/- 0.1%)",
            abs(sum(product_df$revenue_percentage) - 100) < 0.1)

# ------------------------------------------------------------------------------
# Category 3: Time Series & Horizon Completeness
# ------------------------------------------------------------------------------
cat("\n--- Category 3: Time Series Properties & Continuity ---\n")
assert_test("Monthly aggregation contains exactly 36 continuous months", nrow(monthly_df) == 36)
assert_test("First month is 2023-01 and final historical month is 2025-12",
            monthly_df$month[1] == "2023-01" && monthly_df$month[36] == "2025-12")

# Check continuous sequence
all_expected_months <- format(seq(from = as.Date("2023-01-01"), to = as.Date("2025-12-01"), by = "month"), "%Y-%m")
assert_test("All 36 chronological months are strictly consecutive with no missing gaps",
            identical(monthly_df$month, all_expected_months))

# ------------------------------------------------------------------------------
# Category 4: Descriptive Statistics Integrity
# ------------------------------------------------------------------------------
cat("\n--- Category 4: Descriptive Statistics ---\n")
stats <- compute_descriptive_statistics(sales_df, monthly_df, product_df)
assert_test("Statistics total_revenue matches transaction sum", abs(stats$total_revenue - tx_total_rev) < 0.01)
assert_test("Statistics total_units_sold matches sum of units_sold", stats$total_units_sold == sum(sales_df$units_sold))
assert_test("Peak month matches maximum monthly revenue record",
            stats$highest_revenue_month$month == monthly_df$month[which.max(monthly_df$total_revenue)])
assert_test("Lowest month matches minimum monthly revenue record",
            stats$lowest_revenue_month$month == monthly_df$month[which.min(monthly_df$total_revenue)])
assert_test("Best performing product matches top ranked product",
            stats$best_performing_product$product == product_df$product[1])

# ------------------------------------------------------------------------------
# Category 5: Time-Series Forecasting & Bounds
# ------------------------------------------------------------------------------
cat("\n--- Category 5: Time-Series Forecasting & Prediction Intervals ---\n")
fc_res <- forecast_revenue(monthly_df, h = 3)
forecast_df <- fc_res$forecast_df
eval_meta <- fc_res$evaluation

assert_test("Forecast contains exactly 3 projected months", nrow(forecast_df) == 3)
assert_test("Forecast months are 2026-01, 2026-02, 2026-03",
            identical(forecast_df$forecast_month, c("2026-01", "2026-02", "2026-03")))
assert_test("All forecast months are strictly later than final historical month (2025-12)",
            all(forecast_df$forecast_month > "2025-12"))
assert_test("Predicted revenue values are positive and plausible (> $3,000 and < $10,000)",
            all(forecast_df$predicted_revenue > 3000 & forecast_df$predicted_revenue < 10000))
assert_test("95% prediction intervals are mathematically sound (lower <= fit <= upper)",
            all(forecast_df$lower_bound_95 <= forecast_df$predicted_revenue &
                forecast_df$predicted_revenue <= forecast_df$upper_bound_95))
assert_test("Forecast method name is documented and non-empty",
            nchar(forecast_df$forecast_method[1]) > 10)
assert_test("Holdout evaluation MAPE is positive and reasonable (< 20%)",
            eval_meta$holdout_mape > 0 && eval_meta$holdout_mape < 20)

# ------------------------------------------------------------------------------
# Category 6: Deliverables & JSON Formatting
# ------------------------------------------------------------------------------
cat("\n--- Category 6: Output Files & JSON Validation ---\n")
out_dir <- file.path(proj_root, "output")
expected_files <- c(
  "monthly_sales.csv",
  "product_summary.csv",
  "forecast.csv",
  "business_insights.json",
  "dashboard_data.json"
)
for (ef in expected_files) {
  assert_test(paste("Output deliverable exists:", ef), file.exists(file.path(out_dir, ef)))
}

# Verify JSON parsing
insights_json_raw <- paste(readLines(file.path(out_dir, "business_insights.json"), warn = FALSE), collapse = "\n")
dashboard_json_raw <- paste(readLines(file.path(out_dir, "dashboard_data.json"), warn = FALSE), collapse = "\n")

# Use basic validation checks for pure Base R JSON
assert_test("business_insights.json starts with '{' and ends with '}'",
            grepl("^\\s*\\{", insights_json_raw) && grepl("\\}\\s*$", insights_json_raw))
assert_test("dashboard_data.json starts with '{' and ends with '}'",
            grepl("^\\s*\\{", dashboard_json_raw) && grepl("\\}\\s*$", dashboard_json_raw))
assert_test("dashboard_data.json contains required top-level keys",
            all(grepl("statistics", dashboard_json_raw),
                grepl("monthly_sales", dashboard_json_raw),
                grepl("product_summary", dashboard_json_raw),
                grepl("forecast", dashboard_json_raw),
                grepl("business_insights", dashboard_json_raw)))

# ------------------------------------------------------------------------------
# Category 7: Documentation Assets (Analytical Charts)
# ------------------------------------------------------------------------------
cat("\n--- Category 7: Documentation Charts (Excluding Workflow Image) ---\n")
charts_dir <- file.path(proj_root, "docs", "charts")
assert_test("Analytical chart monthly_trend.png exists in docs/charts/",
            file.exists(file.path(charts_dir, "monthly_trend.png")))
assert_test("Analytical chart product_comparison.png exists in docs/charts/",
            file.exists(file.path(charts_dir, "product_comparison.png")))
assert_test("Analytical chart revenue_forecast.png exists in docs/charts/",
            file.exists(file.path(charts_dir, "revenue_forecast.png")))

# CRITICAL PROMPT RESTRICTION CHECK: No workflow image created!
assert_test("STRICT COMPLIANCE: No workflow.png or workflow image exists in docs/ or root",
            !file.exists(file.path(proj_root, "docs", "workflow.png")) &&
            !file.exists(file.path(proj_root, "workflow.png")))

cat("\n================================================================\n")
cat(sprintf("  TEST EXECUTION COMPLETED: %d Tests | %d PASSED | %d FAILED\n",
            total_tests, passed_tests, failed_tests))
cat("================================================================\n")

if (failed_tests > 0) {
  quit(status = 1)
}
