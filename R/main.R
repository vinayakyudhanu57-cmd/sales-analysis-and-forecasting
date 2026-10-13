# ==============================================================================
# Script: main.R
# Project: Sales Analysis and Forecasting (Project 3)
# Purpose: Main execution pipeline for synthetic sales data processing,
#          statistical analysis, time-series forecasting, chart generation,
#          and CSV/JSON output generation.
# ==============================================================================

# Locate project root reliably
find_project_root <- function() {
  candidates <- c(
    getwd(),
    file.path(getwd(), "sales-analysis-and-forecasting"),
    "C:/Users/DELL/Desktop/r_projects/sales-analysis-and-forecasting"
  )
  for (dir in candidates) {
    if (file.exists(file.path(dir, "R", "functions.R"))) {
      return(normalizePath(dir, winslash = "/"))
    }
  }
  return(normalizePath(getwd(), winslash = "/"))
}

project_root <- find_project_root()
setwd(project_root)

cat("================================================================\n")
cat("  SALES ANALYSIS AND FORECASTING PIPELINE (R 4.6.1)\n")
cat("  Workspace Root:", project_root, "\n")
cat("================================================================\n\n")

# Source analytical functions
source(file.path(project_root, "R", "functions.R"))

# Step 1: Load and Validate Data
dataset_path <- file.path(project_root, "data", "sales.csv")
cat("[Step 1/6] Loading and validating sales dataset...\n")
sales_df <- load_and_validate_data(dataset_path)
cat(sprintf("  -> Verified %d transactions across %d unique products.\n",
            nrow(sales_df), length(unique(sales_df$product))))
cat(sprintf("  -> Date range: %s to %s\n",
            min(sales_df$date), max(sales_df$date)))

# Step 2: Monthly Aggregation
cat("[Step 2/6] Aggregating monthly sales performance...\n")
monthly_df <- aggregate_monthly_sales(sales_df)
cat(sprintf("  -> Processed %d continuous months (from %s to %s).\n",
            nrow(monthly_df), monthly_df$month[1], monthly_df$month[nrow(monthly_df)]))

# Step 3: Product Analysis
cat("[Step 3/6] Analyzing product performance & contribution...\n")
product_df <- analyze_product_performance(sales_df)
cat(sprintf("  -> Top Product: %s ($%s, %0.1f%% of revenue)\n",
            product_df$product[1],
            format(product_df$total_revenue[1], big.mark = ","),
            product_df$revenue_percentage[1]))

# Step 4: Descriptive Statistics
cat("[Step 4/6] Computing descriptive statistics...\n")
stats <- compute_descriptive_statistics(sales_df, monthly_df, product_df)
cat(sprintf("  -> Grand Revenue: $%s across %s total units sold.\n",
            format(stats$total_revenue, big.mark = ","),
            format(stats$total_units_sold, big.mark = ",")))
cat(sprintf("  -> Average Monthly Revenue: $%s (Peak: %s at $%s)\n",
            format(stats$average_monthly_revenue, big.mark = ","),
            stats$highest_revenue_month$month,
            format(stats$highest_revenue_month$revenue, big.mark = ",")))

# Step 5: Time-Series Forecasting
cat("[Step 5/6] Fitting time-series model & forecasting next 3 months...\n")
fc_result <- forecast_revenue(monthly_df, h = 3)
forecast_df <- fc_result$forecast_df
eval_metrics <- fc_result$evaluation

cat(sprintf("  -> Forecast Method: %s\n", eval_metrics$model_method))
cat(sprintf("  -> Holdout Validation MAPE: %0.2f%% (MAE: $%0.2f)\n",
            eval_metrics$holdout_mape, eval_metrics$holdout_mae))
for (i in 1:nrow(forecast_df)) {
  cat(sprintf("     * %s: Projected $%s (95%% PI: $%s - $%s)\n",
              forecast_df$forecast_month[i],
              format(forecast_df$predicted_revenue[i], big.mark = ","),
              format(forecast_df$lower_bound_95[i], big.mark = ","),
              format(forecast_df$upper_bound_95[i], big.mark = ",")))
}

# Step 6: Business Insights & Exports
cat("[Step 6/6] Generating business insights and exporting deliverables...\n")
insights <- generate_business_insights(stats, monthly_df, product_df, forecast_df, eval_metrics)

# Assemble dashboard data object
dashboard_payload <- list(
  statistics = stats,
  monthly_sales = monthly_df,
  product_summary = product_df,
  forecast = forecast_df,
  evaluation = eval_metrics,
  business_insights = insights,
  metadata = list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    environment = R.version.string,
    data_disclaimer = "Synthetic commercial retail dataset generated for analytical demonstration purposes. Forecasts are statistical estimates and do not guarantee future commercial performance."
  )
)

# Export all outputs
output_path <- file.path(project_root, "output")
export_outputs(monthly_df, product_df, forecast_df, insights, dashboard_payload, output_dir = output_path)

# Generate analytical charts
charts_path <- file.path(project_root, "docs", "charts")
cat("Generating visual analytical charts in docs/charts/...\n")
generate_analytical_charts(sales_df, monthly_df, product_df, forecast_df, output_dir = charts_path)

cat("\n================================================================\n")
cat("  PIPELINE EXECUTION COMPLETED SUCCESSFULLY!\n")
cat("================================================================\n")
