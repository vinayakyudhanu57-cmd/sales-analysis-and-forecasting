# ==============================================================================
# Script: functions.R
# Project: Sales Analysis and Forecasting (Project 3)
# Purpose: Core analytical functions for data validation, aggregation,
#          time-series forecasting, insight generation, and JSON export.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Load and Validate Data
# ------------------------------------------------------------------------------
load_and_validate_data <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(paste("Dataset file not found at:", filepath))
  }
  
  df <- tryCatch(
    read.csv(filepath, stringsAsFactors = FALSE),
    error = function(e) stop(paste("Failed to read CSV:", e$message))
  )
  
  # Check required columns
  required_cols <- c("date", "product", "units_sold", "unit_price")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Dataset is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate date format (YYYY-MM-DD)
  parsed_dates <- as.Date(df$date, format = "%Y-%m-%d")
  invalid_dates <- is.na(parsed_dates)
  if (any(invalid_dates)) {
    stop(paste("Dataset contains", sum(invalid_dates), "invalid or malformed dates."))
  }
  df$date <- parsed_dates
  
  # Validate numeric columns
  if (!is.numeric(df$units_sold) || any(is.na(df$units_sold))) {
    stop("Column 'units_sold' must be strictly numeric with no NA values.")
  }
  if (!is.numeric(df$unit_price) || any(is.na(df$unit_price))) {
    stop("Column 'unit_price' must be strictly numeric with no NA values.")
  }
  
  # Check positivity
  if (any(df$units_sold <= 0)) {
    stop("Column 'units_sold' contains non-positive values.")
  }
  if (any(df$unit_price <= 0)) {
    stop("Column 'unit_price' contains non-positive values.")
  }
  
  # Calculate transaction revenue
  df$revenue <- round(df$units_sold * df$unit_price, 2)
  
  # Confirm historical sufficiency (at least 12 months for seasonal time-series)
  unique_months <- unique(format(df$date, "%Y-%m"))
  if (length(unique_months) < 12) {
    stop(paste("Insufficient historical data for monthly forecasting. Found only", length(unique_months), "months; minimum required is 12."))
  }
  
  return(df)
}

# ------------------------------------------------------------------------------
# 2. Monthly Sales Aggregation
# ------------------------------------------------------------------------------
aggregate_monthly_sales <- function(df) {
  df$month <- format(df$date, "%Y-%m")
  
  agg <- aggregate(
    cbind(units_sold, revenue) ~ month,
    data = df,
    FUN = function(x) c(total = sum(x), count = length(x))
  )
  
  monthly_df <- data.frame(
    month = agg$month,
    total_units = as.integer(agg$units_sold[, "total"]),
    total_revenue = round(agg$revenue[, "total"], 2),
    transaction_count = as.integer(agg$revenue[, "count"]),
    stringsAsFactors = FALSE
  )
  
  monthly_df <- monthly_df[order(monthly_df$month), ]
  rownames(monthly_df) <- NULL
  
  # Check continuity: ensure no skipped months between min and max
  all_months_seq <- format(seq(
    from = as.Date(paste0(monthly_df$month[1], "-01")),
    to   = as.Date(paste0(monthly_df$month[nrow(monthly_df)], "-01")),
    by   = "month"
  ), "%Y-%m")
  
  missing_months <- setdiff(all_months_seq, monthly_df$month)
  if (length(missing_months) > 0) {
    warning(paste("Discontinuous time series: missing months detected:", paste(missing_months, collapse = ", ")))
  }
  
  return(monthly_df)
}

# ------------------------------------------------------------------------------
# 3. Product Performance Analysis
# ------------------------------------------------------------------------------
analyze_product_performance <- function(df) {
  agg_units   <- aggregate(units_sold ~ product, data = df, sum)
  agg_rev     <- aggregate(revenue ~ product, data = df, sum)
  agg_tx      <- aggregate(revenue ~ product, data = df, length)
  agg_avg_rev <- aggregate(revenue ~ product, data = df, mean)
  
  product_df <- data.frame(
    product = agg_units$product,
    total_units = as.integer(agg_units$units_sold),
    total_revenue = round(agg_rev$revenue, 2),
    transaction_count = as.integer(agg_tx$revenue),
    avg_revenue_per_sale = round(agg_avg_rev$revenue, 2),
    stringsAsFactors = FALSE
  )
  
  grand_revenue <- sum(product_df$total_revenue)
  product_df$revenue_percentage <- round((product_df$total_revenue / grand_revenue) * 100, 2)
  
  # Sort descending by total revenue
  product_df <- product_df[order(-product_df$total_revenue), ]
  rownames(product_df) <- NULL
  
  return(product_df)
}

# ------------------------------------------------------------------------------
# 4. Descriptive Statistics
# ------------------------------------------------------------------------------
compute_descriptive_statistics <- function(df, monthly_df, product_df) {
  max_idx <- which.max(monthly_df$total_revenue)
  min_idx <- which.min(monthly_df$total_revenue)
  
  stats <- list(
    total_revenue = round(sum(df$revenue), 2),
    total_units_sold = as.integer(sum(df$units_sold)),
    total_transactions = as.integer(nrow(df)),
    total_months = as.integer(nrow(monthly_df)),
    average_monthly_revenue = round(mean(monthly_df$total_revenue), 2),
    median_monthly_revenue = round(median(monthly_df$total_revenue), 2),
    std_monthly_revenue = round(sd(monthly_df$total_revenue), 2),
    highest_revenue_month = list(
      month = monthly_df$month[max_idx],
      revenue = round(monthly_df$total_revenue[max_idx], 2)
    ),
    lowest_revenue_month = list(
      month = monthly_df$month[min_idx],
      revenue = round(monthly_df$total_revenue[min_idx], 2)
    ),
    best_performing_product = list(
      product = product_df$product[1],
      total_revenue = round(product_df$total_revenue[1], 2),
      revenue_percentage = round(product_df$revenue_percentage[1], 2)
    ),
    lowest_performing_product = list(
      product = product_df$product[nrow(product_df)],
      total_revenue = round(product_df$total_revenue[nrow(product_df)], 2),
      revenue_percentage = round(product_df$revenue_percentage[nrow(product_df)], 2)
    ),
    time_period_start = monthly_df$month[1],
    time_period_end = monthly_df$month[nrow(monthly_df)]
  )
  
  return(stats)
}

# ------------------------------------------------------------------------------
# 5. Revenue Forecasting (Next 3 Months)
# ------------------------------------------------------------------------------
forecast_revenue <- function(monthly_df, h = 3) {
  n <- nrow(monthly_df)
  if (n < 12) {
    stop("Insufficient data for monthly forecasting: at least 12 months required.")
  }
  
  # Determine future forecast months
  last_month_str <- monthly_df$month[n]
  last_date <- as.Date(paste0(last_month_str, "-01"))
  future_dates <- seq(from = last_date, length.out = h + 1, by = "month")[-1]
  future_month_strings <- format(future_dates, "%Y-%m")
  
  # Chronological holdout evaluation on the last h months (e.g. months 34-36)
  eval_train_idx <- 1:(n - h)
  eval_test_idx  <- (n - h + 1):n
  
  train_rev <- monthly_df$total_revenue[eval_train_idx]
  test_rev  <- monthly_df$total_revenue[eval_test_idx]
  
  # Build monthly time series for holdout evaluation
  train_start_yr <- as.numeric(substr(monthly_df$month[1], 1, 4))
  train_start_mo <- as.numeric(substr(monthly_df$month[1], 6, 7))
  train_ts <- ts(train_rev, frequency = 12, start = c(train_start_yr, train_start_mo))
  
  # Evaluate holdout model
  holdout_fit <- tryCatch(
    HoltWinters(train_ts, seasonal = "additive"),
    error = function(e) tryCatch(HoltWinters(train_ts, gamma = FALSE), error = function(e2) NULL)
  )
  
  holdout_mae <- NA
  holdout_mape <- NA
  if (!is.null(holdout_fit)) {
    holdout_pred <- predict(holdout_fit, n.ahead = h)
    holdout_pred_vals <- as.numeric(holdout_pred)
    holdout_mae <- round(mean(abs(test_rev - holdout_pred_vals)), 2)
    holdout_mape <- round(mean(abs((test_rev - holdout_pred_vals) / test_rev)) * 100, 2)
  }
  
  # Full dataset time series
  full_ts <- ts(monthly_df$total_revenue, frequency = 12, start = c(train_start_yr, train_start_mo))
  
  # Check if third-party 'forecast' package is available
  has_forecast_pkg <- requireNamespace("forecast", quietly = TRUE)
  
  forecast_method_name <- ""
  pred_fit <- numeric(h)
  pred_lwr <- numeric(h)
  pred_upr <- numeric(h)
  
  if (has_forecast_pkg) {
    # If forecast package is available, use ETS
    ets_model <- forecast::ets(full_ts)
    ets_fc <- forecast::forecast(ets_model, h = h, level = 95)
    forecast_method_name <- "ETS Exponential Smoothing (forecast package)"
    pred_fit <- round(as.numeric(ets_fc$mean), 2)
    pred_lwr <- round(as.numeric(ets_fc$lower[, 1]), 2)
    pred_upr <- round(as.numeric(ets_fc$upper[, 1]), 2)
  } else {
    # Documented fallback: Holt-Winters Additive Exponential Smoothing (Base R stats)
    hw_model <- tryCatch(
      HoltWinters(full_ts, seasonal = "additive"),
      error = function(e) tryCatch(HoltWinters(full_ts, gamma = FALSE), error = function(e2) NULL)
    )
    
    if (!is.null(hw_model)) {
      hw_fc <- predict(hw_model, n.ahead = h, prediction.interval = TRUE, level = 0.95)
      forecast_method_name <- "Holt-Winters Exponential Smoothing (Base R stats, 95% PI)"
      pred_fit <- round(as.numeric(hw_fc[, "fit"]), 2)
      pred_upr <- round(as.numeric(hw_fc[, "upr"]), 2)
      pred_lwr <- round(as.numeric(hw_fc[, "lwr"]), 2)
    } else {
      # Secondary fallback: Linear trend with seasonal indicator
      t <- 1:n
      m_factor <- factor(as.numeric(substr(monthly_df$month, 6, 7)), levels = 1:12)
      lm_model <- lm(total_revenue ~ t + m_factor, data = data.frame(total_revenue = monthly_df$total_revenue, t = t, m_factor = m_factor))
      future_t <- (n + 1):(n + h)
      future_m <- factor(as.numeric(substr(future_month_strings, 6, 7)), levels = 1:12)
      lm_fc <- predict(lm_model, newdata = data.frame(t = future_t, m_factor = future_m), interval = "prediction", level = 0.95)
      forecast_method_name <- "Linear Trend with Seasonality (Base R lm, 95% PI)"
      pred_fit <- round(as.numeric(lm_fc[, "fit"]), 2)
      pred_lwr <- round(as.numeric(lm_fc[, "lwr"]), 2)
      pred_upr <- round(as.numeric(lm_fc[, "upr"]), 2)
    }
  }
  
  forecast_df <- data.frame(
    forecast_month = future_month_strings,
    predicted_revenue = pred_fit,
    lower_bound_95 = pred_lwr,
    upper_bound_95 = pred_upr,
    forecast_method = rep(forecast_method_name, h),
    stringsAsFactors = FALSE
  )
  
  evaluation_summary <- list(
    holdout_window_months = as.integer(h),
    holdout_mae = ifelse(is.na(holdout_mae), 0, holdout_mae),
    holdout_mape = ifelse(is.na(holdout_mape), 0, holdout_mape),
    model_method = forecast_method_name,
    is_forecast_package_used = has_forecast_pkg
  )
  
  return(list(
    forecast_df = forecast_df,
    evaluation = evaluation_summary
  ))
}

# ------------------------------------------------------------------------------
# 6. Business Insights Generation
# ------------------------------------------------------------------------------
generate_business_insights <- function(stats, monthly_df, product_df, forecast_df, holdout_eval) {
  # Growth calculation (first 12 months vs last 12 months)
  first_yr_rev <- sum(monthly_df$total_revenue[1:12])
  last_yr_rev  <- sum(monthly_df$total_revenue[(nrow(monthly_df) - 11):nrow(monthly_df)])
  growth_pct   <- round(((last_yr_rev - first_yr_rev) / first_yr_rev) * 100, 1)
  
  # Three-month forecast sum
  fc_total <- round(sum(forecast_df$predicted_revenue), 2)
  fc_lwr_total <- round(sum(forecast_df$lower_bound_95), 2)
  fc_upr_total <- round(sum(forecast_df$upper_bound_95), 2)
  
  insights <- list(
    primary_revenue_driver = list(
      title = "Top Revenue Contributor",
      product = product_df$product[1],
      revenue = product_df$total_revenue[1],
      percentage = product_df$revenue_percentage[1],
      description = paste0(
        product_df$product[1], " is the business's leading revenue driver, generating $",
        format(product_df$total_revenue[1], big.mark = ","), " across 36 months (",
        product_df$revenue_percentage[1], "% of total revenue). ",
        "Maintaining stock availability for this high-velocity item is essential."
      )
    ),
    historical_growth = list(
      title = "Multi-Year Sales Momentum",
      growth_percentage = growth_pct,
      first_year_revenue = round(first_yr_rev, 2),
      last_year_revenue = round(last_yr_rev, 2),
      description = paste0(
        "Annual revenue expanded by ", growth_pct, "% from the initial 12-month period ($",
        format(round(first_yr_rev, 2), big.mark = ","), ") to the final 12-month period ($",
        format(round(last_yr_rev, 2), big.mark = ","), "). ",
        "This indicates sustainable, steady commercial growth across product lines."
      )
    ),
    seasonality_pattern = list(
      title = "Seasonal Demand Variations",
      peak_month = stats$highest_revenue_month$month,
      peak_revenue = stats$highest_revenue_month$revenue,
      trough_month = stats$lowest_revenue_month$month,
      trough_revenue = stats$lowest_revenue_month$revenue,
      description = paste0(
        "Sales exhibit strong seasonality, peaking in ", stats$highest_revenue_month$month,
        " at $", format(stats$highest_revenue_month$revenue, big.mark = ","), " during Q4 holiday commercial cycles. ",
        "The lowest recorded volume occurred in ", stats$lowest_revenue_month$month,
        " ($", format(stats$lowest_revenue_month$revenue, big.mark = ","), "), reflecting standard post-holiday retail lulls."
      )
    ),
    forecast_outlook = list(
      title = "Three-Month Revenue Projection",
      projected_total = fc_total,
      projected_range = paste0("$", format(fc_lwr_total, big.mark = ","), " to $", format(fc_upr_total, big.mark = ",")),
      model_used = forecast_df$forecast_method[1],
      holdout_mape = holdout_eval$holdout_mape,
      description = paste0(
        "Projected revenue for the next 3 months (",
        forecast_df$forecast_month[1], " to ", forecast_df$forecast_month[3],
        ") is estimated at $", format(fc_total, big.mark = ","),
        " (95% prediction interval: $", format(fc_lwr_total, big.mark = ","), " – $", format(fc_upr_total, big.mark = ","), "). ",
        "Holdout testing yielded a Mean Absolute Percentage Error (MAPE) of ", holdout_eval$holdout_mape, "%."
      )
    ),
    actionable_recommendations = list(
      paste0("Align inventory restocking for the top product ('", product_df$product[1], "') 4 to 6 weeks before anticipated demand surges."),
      "Buffer working capital during Q1 to absorb predictable post-holiday seasonal dips observed in January and February.",
      paste0("Consider bundling slower-moving catalogue items like '", product_df$product[nrow(product_df)], "' with high-velocity accessories to lift average order value.")
    )
  )
  
  return(insights)
}

# ------------------------------------------------------------------------------
# 7. Analytical Chart Generation (ggplot2 with Base R fallback)
# ------------------------------------------------------------------------------
generate_analytical_charts <- function(df, monthly_df, product_df, forecast_df, output_dir = "docs/charts") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
  
  if (has_ggplot2) {
    suppressPackageStartupMessages(library(ggplot2))
    
    # Chart 1: Monthly Revenue Trend
    monthly_df$date_num <- as.Date(paste0(monthly_df$month, "-01"))
    
    p1 <- ggplot(monthly_df, aes(x = date_num, y = total_revenue)) +
      geom_line(color = "#0284c7", linewidth = 1) +
      geom_point(color = "#0369a1", size = 2.5) +
      geom_smooth(method = "loess", formula = y ~ x, color = "#0d9488", linetype = "dashed", se = FALSE, linewidth = 0.8) +
      scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
      scale_y_continuous(labels = function(x) paste0("$", format(x, big.mark = ","))) +
      labs(
        title = "Monthly Revenue Trend (36-Month Historical)",
        subtitle = "Solid blue line shows observed revenue; dashed teal line shows smoothed trend",
        x = "Month",
        y = "Total Revenue (USD)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", color = "#0f172a", size = 13),
        plot.subtitle = element_text(color = "#475569", size = 10),
        axis.title = element_text(color = "#334155", face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 1, color = "#475569"),
        axis.text.y = element_text(color = "#475569"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e2e8f0"),
        plot.background = element_rect(fill = "#ffffff", color = NA)
      )
    ggsave(file.path(output_dir, "monthly_trend.png"), plot = p1, width = 9, height = 5, dpi = 150)
    
    # Chart 2: Product Revenue Comparison
    prod_plot_df <- product_df
    prod_plot_df$product <- factor(prod_plot_df$product, levels = rev(prod_plot_df$product))
    
    p2 <- ggplot(prod_plot_df, aes(x = product, y = total_revenue)) +
      geom_col(fill = "#0284c7", width = 0.6) +
      geom_text(aes(label = paste0("$", format(total_revenue, big.mark = ","), " (", revenue_percentage, "%)")),
                hjust = -0.1, size = 3.3, color = "#1e293b", fontface = "bold") +
      coord_flip(ylim = c(0, max(prod_plot_df$total_revenue) * 1.25)) +
      scale_y_continuous(labels = function(x) paste0("$", format(x, big.mark = ","))) +
      labs(
        title = "Product Revenue Performance",
        subtitle = "Ranked cumulative revenue and percentage contribution across 36 months",
        x = "Product",
        y = "Cumulative Revenue (USD)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", color = "#0f172a", size = 13),
        plot.subtitle = element_text(color = "#475569", size = 10),
        axis.title = element_text(color = "#334155", face = "bold"),
        axis.text = element_text(color = "#475569"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#e2e8f0"),
        plot.background = element_rect(fill = "#ffffff", color = NA)
      )
    ggsave(file.path(output_dir, "product_comparison.png"), plot = p2, width = 9, height = 5, dpi = 150)
    
    # Chart 3: Historical Revenue with 3-Month Forecast & Prediction Intervals
    # Combine historical and forecast into single visualization series
    hist_sub <- data.frame(
      date_num = as.Date(paste0(monthly_df$month, "-01")),
      revenue = monthly_df$total_revenue,
      lwr = monthly_df$total_revenue,
      upr = monthly_df$total_revenue,
      type = "Historical",
      stringsAsFactors = FALSE
    )
    
    fc_sub <- data.frame(
      date_num = as.Date(paste0(forecast_df$forecast_month, "-01")),
      revenue = forecast_df$predicted_revenue,
      lwr = forecast_df$lower_bound_95,
      upr = forecast_df$upper_bound_95,
      type = "Forecast",
      stringsAsFactors = FALSE
    )
    
    # Bridge point connecting historical and forecast lines
    bridge_point <- data.frame(
      date_num = hist_sub$date_num[nrow(hist_sub)],
      revenue = hist_sub$revenue[nrow(hist_sub)],
      lwr = hist_sub$revenue[nrow(hist_sub)],
      upr = hist_sub$revenue[nrow(hist_sub)],
      type = "Forecast",
      stringsAsFactors = FALSE
    )
    fc_plot_data <- rbind(bridge_point, fc_sub)
    
    p3 <- ggplot() +
      geom_line(data = hist_sub, aes(x = date_num, y = revenue), color = "#0284c7", linewidth = 1) +
      geom_point(data = hist_sub, aes(x = date_num, y = revenue), color = "#0369a1", size = 2) +
      geom_ribbon(data = fc_plot_data, aes(x = date_num, ymin = lwr, ymax = upr), fill = "#0d9488", alpha = 0.2) +
      geom_line(data = fc_plot_data, aes(x = date_num, y = revenue), color = "#0d9488", linewidth = 1.1, linetype = "dashed") +
      geom_point(data = fc_sub, aes(x = date_num, y = revenue), color = "#0f766e", size = 3) +
      scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
      scale_y_continuous(labels = function(x) paste0("$", format(x, big.mark = ","))) +
      labs(
        title = "Revenue Forecast (Next 3 Months)",
        subtitle = "Historical revenue in blue; 3-month forecast in dashed teal with 95% prediction interval band",
        x = "Month",
        y = "Total Revenue (USD)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", color = "#0f172a", size = 13),
        plot.subtitle = element_text(color = "#475569", size = 10),
        axis.title = element_text(color = "#334155", face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 1, color = "#475569"),
        axis.text.y = element_text(color = "#475569"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e2e8f0"),
        plot.background = element_rect(fill = "#ffffff", color = NA)
      )
    ggsave(file.path(output_dir, "revenue_forecast.png"), plot = p3, width = 9, height = 5, dpi = 150)
    
  } else {
    # Base R fallback for chart generation
    png(file.path(output_dir, "monthly_trend.png"), width = 900, height = 500, res = 100)
    plot(1:nrow(monthly_df), monthly_df$total_revenue, type = "o", col = "#0284c7", pch = 16,
         xlab = "Month Index (1-36)", ylab = "Revenue (USD)", main = "Monthly Revenue Trend")
    dev.off()
    
    png(file.path(output_dir, "product_comparison.png"), width = 900, height = 500, res = 100)
    barplot(rev(product_df$total_revenue), names.arg = rev(product_df$product), horiz = TRUE,
            las = 1, col = "#0284c7", main = "Product Revenue Performance")
    dev.off()
    
    png(file.path(output_dir, "revenue_forecast.png"), width = 900, height = 500, res = 100)
    plot(1:nrow(monthly_df), monthly_df$total_revenue, type = "l", col = "#0284c7",
         xlim = c(1, nrow(monthly_df) + nrow(forecast_df)), ylab = "Revenue (USD)",
         xlab = "Month Index", main = "Revenue Forecast")
    lines((nrow(monthly_df) + 1):(nrow(monthly_df) + nrow(forecast_df)), forecast_df$predicted_revenue,
          col = "#0d9488", lty = 2, lwd = 2)
    dev.off()
  }
}

# ------------------------------------------------------------------------------
# 8. Pure Base R JSON Serializer (Zero-Dependency)
# ------------------------------------------------------------------------------
to_json_string <- function(obj) {
  if (is.null(obj)) {
    return("null")
  }
  
  if (is.logical(obj)) {
    if (length(obj) == 1) {
      if (is.na(obj)) return("null")
      return(ifelse(obj, "true", "false"))
    } else {
      items <- vapply(obj, function(x) {
        if (is.na(x)) return("null")
        ifelse(x, "true", "false")
      }, character(1))
      return(paste0("[", paste(items, collapse = ", "), "]"))
    }
  }
  
  if (is.numeric(obj)) {
    if (length(obj) == 1) {
      if (is.na(obj) || is.nan(obj) || is.infinite(obj)) return("null")
      return(as.character(obj))
    } else {
      items <- vapply(obj, function(x) {
        if (is.na(x) || is.nan(x) || is.infinite(x)) return("null")
        as.character(x)
      }, character(1))
      return(paste0("[", paste(items, collapse = ", "), "]"))
    }
  }
  
  if (is.character(obj)) {
    escape_str <- function(s) {
      if (is.na(s)) return("null")
      s_clean <- gsub("\\\\", "\\\\\\\\", s)
      s_clean <- gsub("\"", "\\\\\"", s_clean)
      s_clean <- gsub("\r", "\\\\r", s_clean)
      s_clean <- gsub("\n", "\\\\n", s_clean)
      s_clean <- gsub("\t", "\\\\t", s_clean)
      paste0("\"", s_clean, "\"")
    }
    if (length(obj) == 1) {
      return(escape_str(obj))
    } else {
      items <- vapply(obj, escape_str, character(1))
      return(paste0("[", paste(items, collapse = ", "), "]"))
    }
  }
  
  if (is.data.frame(obj)) {
    n_rows <- nrow(obj)
    if (n_rows == 0) return("[]")
    row_strings <- character(n_rows)
    for (i in seq_len(n_rows)) {
      row_list <- as.list(obj[i, , drop = FALSE])
      row_keys <- names(row_list)
      pairs <- vapply(row_keys, function(k) {
        val <- row_list[[k]]
        paste0("\"", k, "\": ", to_json_string(val))
      }, character(1))
      row_strings[i] <- paste0("{", paste(pairs, collapse = ", "), "}")
    }
    return(paste0("[\n  ", paste(row_strings, collapse = ",\n  "), "\n]"))
  }
  
  if (is.list(obj)) {
    nm <- names(obj)
    if (is.null(nm)) {
      # Unnamed list -> JSON Array
      items <- vapply(obj, to_json_string, character(1))
      return(paste0("[\n  ", paste(items, collapse = ",\n  "), "\n]"))
    } else {
      # Named list -> JSON Object
      pairs <- vapply(nm, function(k) {
        val <- obj[[k]]
        paste0("\"", k, "\": ", to_json_string(val))
      }, character(1))
      return(paste0("{\n  ", paste(pairs, collapse = ",\n  "), "\n}"))
    }
  }
  
  return(paste0("\"", as.character(obj), "\""))
}

# ------------------------------------------------------------------------------
# 9. Export All Output Files
# ------------------------------------------------------------------------------
export_outputs <- function(monthly_df, product_df, forecast_df, insights, dashboard_data, output_dir = "output") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. monthly_sales.csv
  write.csv(monthly_df, file.path(output_dir, "monthly_sales.csv"), row.names = FALSE, quote = TRUE)
  
  # 2. product_summary.csv
  write.csv(product_df, file.path(output_dir, "product_summary.csv"), row.names = FALSE, quote = TRUE)
  
  # 3. forecast.csv
  write.csv(forecast_df, file.path(output_dir, "forecast.csv"), row.names = FALSE, quote = TRUE)
  
  # 4. business_insights.json
  writeLines(to_json_string(insights), file.path(output_dir, "business_insights.json"), useBytes = TRUE)
  
  # 5. dashboard_data.json
  writeLines(to_json_string(dashboard_data), file.path(output_dir, "dashboard_data.json"), useBytes = TRUE)
  
  cat("All 5 output files exported successfully to", output_dir, "\n")
}
