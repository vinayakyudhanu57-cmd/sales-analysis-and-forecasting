# Test Cases Specification: Sales Analysis and Forecasting

This document defines the comprehensive testing protocol for the **Sales Analysis and Forecasting** application (Project 3). It details data integrity, mathematical reconciliation, time-series forecasting, JSON structure, and frontend validation.

---

## 1. Test Execution Summary

- **Test Suite Script:** `tests/test_analysis.R`
- **Execution Engine:** Base R 4.6.1 (`C:\Program Files\R\R-4.6.1\bin\Rscript.exe`)
- **Total Automated Tests Executed:** 38
- **Tests Passed:** 38 (100%)
- **Tests Failed:** 0
- **Reconciliation Status:** Strict zero-discrepancy mathematical reconciliation between row transactions, monthly aggregation, and product summaries ($157,534.20).

---

## 2. Test Specifications by Category

### Category 1: Dataset Validation & Schema Hygiene

| Test ID | Test Case Name | Objective & Assertion Criteria | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **TC-01** | Raw File Existence | Verify `data/sales.csv` exists and is readable. | File exists | **PASS** |
| **TC-02** | Schema Column Verification | Ensure required columns exist: `date`, `product`, `units_sold`, `unit_price`, `revenue`. | All 5 columns present | **PASS** |
| **TC-03** | Observation Volume | Verify dataset contains sufficient records for time-series aggregation ($N \ge 300$). | 445 records found | **PASS** |
| **TC-04** | Date Parsing & Format | Ensure all dates conform to `YYYY-MM-DD` and parse without `NA`. | Zero malformed dates | **PASS** |
| **TC-05** | Units Sold Positivity | Verify all order quantities are strictly positive integers ($units > 0$). | All values $\ge 3$ | **PASS** |
| **TC-06** | Unit Price Positivity | Verify unit price is positive and non-zero ($price > 0$). | Min price $16.15 | **PASS** |
| **TC-07** | Missing Value Check | Check that no missing (`NA`), `NaN`, or infinite values exist in data. | 0 missing values | **PASS** |

---

### Category 2: Revenue Calculation & Mathematical Reconciliation

| Test ID | Test Case Name | Objective & Assertion Criteria | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **TC-08** | Transaction Revenue Math | `revenue = units_sold * unit_price` computed accurately for all 445 rows. | Exact match ($|diff| < 0.001$) | **PASS** |
| **TC-09** | Monthly Revenue Sum Reconciliation | $\sum \text{monthly\_revenue} == \sum \text{transaction\_revenue}$. | Exact match ($157,534.20) | **PASS** |
| **TC-10** | Product Revenue Sum Reconciliation | $\sum \text{product\_revenue} == \sum \text{transaction\_revenue}$. | Exact match ($157,534.20) | **PASS** |
| **TC-11** | Product Revenue Percentage Sum | $\sum \text{revenue\_percentage} \approx 100.0\%$. | Exact sum 100.01% | **PASS** |

---

### Category 3: Time Series Continuity & Horizon

| Test ID | Test Case Name | Objective & Assertion Criteria | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **TC-12** | Monthly Record Count | Exactly 36 continuous months aggregated from `2023-01` to `2025-12`. | 36 distinct months | **PASS** |
| **TC-13** | Time Boundary Check | Earliest month is `2023-01`; latest historical month is `2025-12`. | Exactly matches boundaries | **PASS** |
| **TC-14** | Gap Detection | Ensure no intermediate calendar month was omitted. | 0 gaps detected | **PASS** |

---

### Category 4: Descriptive Statistics

| Test ID | Test Case Name | Objective & Assertion Criteria | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **TC-15** | Summary Total Revenue | `stats$total_revenue` equals grand sum. | $157,534.20 | **PASS** |
| **TC-16** | Summary Total Units Sold | `stats$total_units_sold` equals total units. | 5,027 units | **PASS** |
| **TC-17** | Highest Revenue Month | Correctly identifies peak month and revenue. | `2025-11` ($6,469.77) | **PASS** |
| **TC-18** | Lowest Revenue Month | Correctly identifies lowest month and revenue. | `2023-02` ($3,124.31) | **PASS** |
| **TC-19** | Best Performing Product | Correctly identifies top product by total revenue. | Wireless Keyboard ($37,899.14) | **PASS** |

---

### Category 5: Time-Series Forecasting & Bounds

| Test ID | Test Case Name | Objective & Assertion Criteria | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **TC-20** | Forecast Horizon Length | Exactly $h = 3$ forecast months produced. | 3 months generated | **PASS** |
| **TC-21** | Forecast Target Dates | Projected months strictly follow historical horizon: `2026-01`, `2026-02`, `2026-03`. | Exactly matches sequence | **PASS** |
| **TC-22** | Chronological Ordering | All forecast dates are strictly later than `2025-12`. | Strictly future dates | **PASS** |
| **TC-23** | Plausibility of Forecast Revenue | Predicted revenues fall within sensible commercial range ($3k to $10k). | Range: $4,526.82 – $4,936.01 | **PASS** |
| **TC-24** | 95% Prediction Interval Soundness | Mathematical check: $\text{lower\_bound} \le \text{predicted\_revenue} \le \text{upper\_bound}$. | Satisfied for all 3 months | **PASS** |
| **TC-25** | Method Identification | `forecast_method` explicitly names Holt-Winters Exponential Smoothing. | Non-empty documentation string | **PASS** |
| **TC-26** | Chronological Holdout Evaluation | Holdout MAPE calculated on final quarter without leakage. | MAPE = 7.20% (MAE = $336.73) | **PASS** |

---

### Category 6: Deliverables & JSON Formatting

| Test ID | Test Case Name | Objective & Assertion Criteria | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **TC-27** | Monthly Sales CSV | File `output/monthly_sales.csv` exists and has 37 lines. | Valid CSV file | **PASS** |
| **TC-28** | Product Summary CSV | File `output/product_summary.csv` exists and has 6 lines. | Valid CSV file | **PASS** |
| **TC-29** | Forecast CSV | File `output/forecast.csv` exists with prediction columns. | Valid CSV file | **PASS** |
| **TC-30** | Business Insights JSON | File `output/business_insights.json` exists and parses cleanly. | Valid JSON object | **PASS** |
| **TC-31** | Dashboard Data JSON | File `output/dashboard_data.json` exists and contains all metrics. | Valid JSON object (8.4 KB) | **PASS** |
| **TC-32** | Pure Base R JSON Syntax | JSON strings start with `{` and terminate with `}`. | Valid boundary syntax | **PASS** |
| **TC-33** | Dashboard JSON Key Completeness | Contains `statistics`, `monthly_sales`, `product_summary`, `forecast`, `business_insights`. | All keys present | **PASS** |

---

### Category 7: Documentation Assets & Restrictions

| Test ID | Test Case Name | Objective & Assertion Criteria | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **TC-34** | Monthly Trend Chart | `docs/charts/monthly_trend.png` exists. | 25.1 KB PNG image | **PASS** |
| **TC-35** | Product Comparison Chart | `docs/charts/product_comparison.png` exists. | 39.2 KB PNG image | **PASS** |
| **TC-36** | Revenue Forecast Chart | `docs/charts/revenue_forecast.png` exists. | 24.1 KB PNG image | **PASS** |
| **TC-37** | Workflow Prompt Existence | `docs/workflow-diagram-prompt.txt` exists and contains 12-step instructions. | File present | **PASS** |
| **TC-38** | STRICT IMAGE RESTRICTION | Verify NO `workflow.png` or workflow image was generated in project. | No workflow image exists | **PASS** |

---

### Category 8: Frontend & Web Integration

| Test ID | Test Case Name | Test Description & Verification | Result |
| :--- | :--- | :--- | :--- |
| **TC-39** | HTTP Server Access | Verified `index.html` returns HTTP 200 via `curl.exe` on port 8086. | HTTP 200 OK |
| **TC-40** | JSON Endpoint Access | Verified `output/dashboard_data.json` returns HTTP 200 via `curl.exe`. | HTTP 200 OK |
| **TC-41** | SVG Trend Chart Rendering | Dynamically plots 36 months, shaded area, average reference line, and hover tooltip. | Verified |
| **TC-42** | Error Alert Fallback | Friendly `#error-alert` banner rendered if JSON fetch fails. | Verified |
| **TC-43** | Responsive Layout | Tested breakpoints for desktop (>1024px), tablet (768px-1024px), and mobile (<768px). | Verified |
| **TC-44** | Academic Branding Isolation | No college codes, student names, or degree labels appear in visible web UI. | Verified |
