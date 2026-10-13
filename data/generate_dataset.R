# ==============================================================================
# Script: generate_dataset.R
# Project: Sales Analysis and Forecasting (Project 3)
# Purpose: Generate synthetic retail sales dataset covering 36 months (2023-2025)
#          across 5 commercial office/tech accessory products.
# ==============================================================================

set.seed(42)

# Define time horizon: 36 months (January 2023 to December 2025)
start_date <- as.Date("2023-01-01")
end_date   <- as.Date("2025-12-31")

# Define product catalogue with baseline prices and volume profiles
products <- list(
  list(name = "Wireless Keyboard",       base_price = 48.00, base_vol = 18, price_sd = 2.0),
  list(name = "Ergonomic Mouse",         base_price = 32.00, base_vol = 24, price_sd = 1.5),
  list(name = "USB-C Multiport Hub",     base_price = 28.00, base_vol = 28, price_sd = 1.2),
  list(name = "Aluminum Laptop Stand",   base_price = 42.00, base_vol = 15, price_sd = 1.8),
  list(name = "Desk Mat",                base_price = 19.00, base_vol = 32, price_sd = 1.0)
)

records <- list()
record_idx <- 1

# Generate month sequence
months <- seq(from = as.Date("2023-01-01"), to = as.Date("2025-12-01"), by = "month")

for (m_idx in seq_along(months)) {
  m_date <- months[m_idx]
  year <- as.numeric(format(m_date, "%Y"))
  month_num <- as.numeric(format(m_date, "%m"))
  
  # Year-over-year business growth factor (modest upward drift)
  growth_factor <- 1.0 + (m_idx - 1) * 0.008  # ~28% overall growth over 36 months
  
  # Seasonality: Q4 peak (Nov/Dec holiday season) and Sept back-to-work bump
  seasonal_factor <- 1.0
  if (month_num == 11) seasonal_factor <- 1.30
  if (month_num == 12) seasonal_factor <- 1.45
  if (month_num == 9)  seasonal_factor <- 1.15
  if (month_num == 1)  seasonal_factor <- 0.90
  if (month_num == 2)  seasonal_factor <- 0.88
  if (month_num == 7)  seasonal_factor <- 0.92
  
  # Days in the current month
  days_in_month <- as.numeric(diff(seq(m_date, length.out = 2, by = "month")))
  
  for (prod in products) {
    # Generate 2 to 3 discrete transactions per product per month
    num_tx <- sample(2:3, 1)
    tx_days <- sort(sample(1:days_in_month, num_tx))
    
    for (d in tx_days) {
      tx_date <- sprintf("%04d-%02d-%02d", year, month_num, d)
      
      # Base units sold per order with realistic noise
      mean_units <- (prod$base_vol / num_tx) * growth_factor * seasonal_factor
      units <- round(rnorm(1, mean = mean_units, sd = max(2, mean_units * 0.15)))
      units <- max(3, units) # Ensure strictly positive integer
      
      # Unit price with small variance (promotional discounts / bulk pricing)
      price <- round(rnorm(1, mean = prod$base_price, sd = prod$price_sd), 2)
      price <- max(round(prod$base_price * 0.85, 2), price) # Never below 85% of base
      
      records[[record_idx]] <- list(
        date = tx_date,
        product = prod$name,
        units_sold = as.integer(units),
        unit_price = as.numeric(price)
      )
      record_idx <- record_idx + 1
    }
  }
}

# Convert to data frame
sales_df <- do.call(rbind, lapply(records, as.data.frame, stringsAsFactors = FALSE))
sales_df <- sales_df[order(sales_df$date, sales_df$product), ]
rownames(sales_df) <- NULL

# Ensure directory exists
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Export to CSV
write.csv(sales_df, "data/sales.csv", row.names = FALSE, quote = TRUE)

cat("Successfully generated synthetic sales dataset:\n")
cat("- Total transactions:", nrow(sales_df), "\n")
cat("- Date range:", min(sales_df$date), "to", max(sales_df$date), "\n")
cat("- Total months:", length(unique(substr(sales_df$date, 1, 7))), "\n")
cat("- Products:", length(unique(sales_df$product)), "\n")
cat("- Total units sold:", sum(sales_df$units_sold), "\n")
cat("- Estimated revenue: $", format(round(sum(sales_df$units_sold * sales_df$unit_price), 2), big.mark = ","), "\n")
