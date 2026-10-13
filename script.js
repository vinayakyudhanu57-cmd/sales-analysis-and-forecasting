/**
 * Script: script.js
 * Project: Sales Analysis and Forecasting (Project 3)
 * Purpose: Client-side dynamic renderer that fetches dashboard_data.json
 *          and updates KPIs, interactive SVG charts, forecasts, and insights.
 */

document.addEventListener('DOMContentLoaded', () => {
  initDashboard();
});

async function initDashboard() {
  const errorAlert = document.getElementById('error-alert');
  const errorMsg = document.getElementById('error-message');

  try {
    // Relative path works both in local server and on GitHub Pages root
    const response = await fetch('./output/dashboard_data.json', { cache: 'no-store' });
    
    if (!response.ok) {
      throw new Error(`HTTP ${response.status} (${response.statusText}): Could not retrieve ./output/dashboard_data.json`);
    }

    const data = await response.json();
    validateAndRender(data);

  } catch (err) {
    console.error('Failed to load dashboard data:', err);
    if (errorAlert) {
      errorAlert.style.display = 'flex';
      if (errorMsg) {
        errorMsg.innerHTML = `<strong>Error Details:</strong> ${escapeHtml(err.message)}.<br>` +
          `Please verify that <code>R/main.R</code> was executed to generate <code>output/dashboard_data.json</code> ` +
          `and that this page is served via a local web server (e.g. <code>python -m http.server 8000</code>).`;
      }
    }
  }
}

function validateAndRender(payload) {
  if (!payload || !payload.statistics || !payload.monthly_sales || !payload.product_summary) {
    throw new Error('Invalid or incomplete JSON schema received from dashboard_data.json');
  }

  const { statistics, monthly_sales, product_summary, forecast, evaluation, business_insights, metadata } = payload;

  // 1. Header & Metadata
  const periodBadge = document.getElementById('badge-period');
  if (periodBadge && statistics.time_period_start && statistics.time_period_end) {
    periodBadge.textContent = `${statistics.time_period_start} → ${statistics.time_period_end}`;
  }

  const footerMeta = document.getElementById('footer-metadata');
  if (footerMeta && metadata) {
    footerMeta.textContent = `Generated: ${metadata.generated_at} | ${metadata.environment}`;
  }

  // 2. Overview Section KPIs
  renderOverview(statistics);

  // 3. Sales Analysis Section (SVG Chart & Product List)
  renderSalesAnalysis(monthly_sales, statistics, product_summary);

  // 4. Forecast Section
  renderForecast(forecast, evaluation);

  // 5. Business Insights Section
  renderInsights(business_insights);
}

// -----------------------------------------------------------------------------
// Section 1: Overview KPIs
// -----------------------------------------------------------------------------
function renderOverview(stats) {
  const elRev = document.getElementById('kpi-total-revenue');
  const elUnits = document.getElementById('kpi-total-units');
  const elAvg = document.getElementById('kpi-avg-monthly');
  const elTop = document.getElementById('kpi-top-product');
  const elTopShare = document.getElementById('kpi-top-share');
  const elRange = document.getElementById('kpi-monthly-range');

  if (elRev) elRev.textContent = formatCurrency(stats.total_revenue);
  if (elUnits) elUnits.textContent = Number(stats.total_units_sold).toLocaleString();
  if (elAvg) elAvg.textContent = formatCurrency(stats.average_monthly_revenue);
  
  if (elTop && stats.best_performing_product) {
    elTop.textContent = stats.best_performing_product.product;
  }
  if (elTopShare && stats.best_performing_product) {
    elTopShare.textContent = `${stats.best_performing_product.revenue_percentage}% of gross revenue`;
  }
  if (elRange && stats.median_monthly_revenue) {
    elRange.textContent = `Median: ${formatCurrency(stats.median_monthly_revenue)}`;
  }
}

// -----------------------------------------------------------------------------
// Section 2: Sales Analysis (Interactive SVG Chart & Product Breakdown)
// -----------------------------------------------------------------------------
function renderSalesAnalysis(monthlySales, stats, productSummary) {
  // Mini stats under chart
  const elPeak = document.getElementById('stat-peak-month');
  const elLowest = document.getElementById('stat-lowest-month');
  const elStd = document.getElementById('stat-std-dev');

  if (elPeak && stats.highest_revenue_month) {
    elPeak.textContent = `${stats.highest_revenue_month.month} (${formatCurrency(stats.highest_revenue_month.revenue)})`;
  }
  if (elLowest && stats.lowest_revenue_month) {
    elLowest.textContent = `${stats.lowest_revenue_month.month} (${formatCurrency(stats.lowest_revenue_month.revenue)})`;
  }
  if (elStd && stats.std_monthly_revenue) {
    elStd.textContent = formatCurrency(stats.std_monthly_revenue);
  }

  // Draw Interactive SVG Chart
  drawMonthlyTrendSvg(monthlySales, stats.average_monthly_revenue);

  // Render Product Breakdown List
  renderProductBreakdown(productSummary);
}

function drawMonthlyTrendSvg(monthlyData, avgRev) {
  const svg = document.getElementById('trend-svg');
  const tooltip = document.getElementById('chart-tooltip');
  if (!svg || !monthlyData || monthlyData.length === 0) return;

  const width = 800;
  const height = 280;
  const padding = { top: 25, right: 30, bottom: 40, left: 65 };

  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;

  const revenues = monthlyData.map(d => d.total_revenue);
  const minVal = Math.min(...revenues) * 0.85;
  const maxVal = Math.max(...revenues) * 1.1;

  const getX = (i) => padding.left + (i / (monthlyData.length - 1)) * plotW;
  const getY = (val) => padding.top + plotH - ((val - minVal) / (maxVal - minVal)) * plotH;

  // Clear previous SVG content
  svg.innerHTML = '';

  // 1. Grid Lines and Y-Axis Labels
  const gridSteps = 4;
  for (let s = 0; s <= gridSteps; s++) {
    const val = minVal + ((maxVal - minVal) / gridSteps) * s;
    const y = getY(val);

    const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
    line.setAttribute('x1', padding.left);
    line.setAttribute('x2', width - padding.right);
    line.setAttribute('y1', y);
    line.setAttribute('y2', y);
    line.setAttribute('stroke', '#e2e8f0');
    line.setAttribute('stroke-dasharray', '3 3');
    svg.appendChild(line);

    const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    text.setAttribute('x', padding.left - 8);
    text.setAttribute('y', y + 4);
    text.setAttribute('text-anchor', 'end');
    text.setAttribute('fill', '#94a3b8');
    text.setAttribute('font-size', '10');
    text.setAttribute('font-family', 'Inter, sans-serif');
    text.textContent = `$${Math.round(val / 1000)}k`;
    svg.appendChild(text);
  }

  // 2. Average Reference Line
  if (avgRev) {
    const avgY = getY(avgRev);
    const avgLine = document.createElementNS('http://www.w3.org/2000/svg', 'line');
    avgLine.setAttribute('x1', padding.left);
    avgLine.setAttribute('x2', width - padding.right);
    avgLine.setAttribute('y1', avgY);
    avgLine.setAttribute('y2', avgY);
    avgLine.setAttribute('stroke', '#94a3b8');
    avgLine.setAttribute('stroke-width', '1.5');
    avgLine.setAttribute('stroke-dasharray', '5 4');
    svg.appendChild(avgLine);
  }

  // 3. Shaded Area Under Curve
  const areaPoints = [`${getX(0)},${padding.top + plotH}`];
  monthlyData.forEach((d, i) => {
    areaPoints.push(`${getX(i)},${getY(d.total_revenue)}`);
  });
  areaPoints.push(`${getX(monthlyData.length - 1)},${padding.top + plotH}`);

  const area = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
  area.setAttribute('points', areaPoints.join(' '));
  area.setAttribute('fill', '#e0f2fe');
  area.setAttribute('fill-opacity', '0.45');
  svg.appendChild(area);

  // 4. Trend Path Line
  const linePoints = monthlyData.map((d, i) => `${getX(i)},${getY(d.total_revenue)}`).join(' ');
  const polyline = document.createElementNS('http://www.w3.org/2000/svg', 'polyline');
  polyline.setAttribute('points', linePoints);
  polyline.setAttribute('fill', 'none');
  polyline.setAttribute('stroke', '#0284c7');
  polyline.setAttribute('stroke-width', '2.5');
  polyline.setAttribute('stroke-linejoin', 'round');
  polyline.setAttribute('stroke-linecap', 'round');
  svg.appendChild(polyline);

  // 5. Interactive Points & X-Axis Labels
  const container = document.getElementById('trend-chart-container');

  monthlyData.forEach((d, i) => {
    const x = getX(i);
    const y = getY(d.total_revenue);

    // X-Axis Year markers (show label every 6 months)
    if (i % 6 === 0 || i === monthlyData.length - 1) {
      const xLabel = document.createElementNS('http://www.w3.org/2000/svg', 'text');
      xLabel.setAttribute('x', x);
      xLabel.setAttribute('y', height - 12);
      xLabel.setAttribute('text-anchor', 'middle');
      xLabel.setAttribute('fill', '#64748b');
      xLabel.setAttribute('font-size', '10');
      xLabel.setAttribute('font-weight', '500');
      xLabel.textContent = d.month;
      svg.appendChild(xLabel);
    }

    // Interactive Data Dot
    const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
    circle.setAttribute('cx', x);
    circle.setAttribute('cy', y);
    circle.setAttribute('r', '3.5');
    circle.setAttribute('fill', '#0284c7');
    circle.setAttribute('stroke', '#ffffff');
    circle.setAttribute('stroke-width', '1.5');
    circle.style.cursor = 'pointer';
    circle.style.transition = 'r 0.15s ease, fill 0.15s ease';

    // Tooltip Hover Listeners
    circle.addEventListener('mouseenter', () => {
      circle.setAttribute('r', '6');
      circle.setAttribute('fill', '#0369a1');
      if (tooltip && container) {
        tooltip.innerHTML = `<strong>${d.month}</strong><br>Revenue: ${formatCurrency(d.total_revenue)}<br>Units: ${d.total_units} (${d.transaction_count} orders)`;
        tooltip.style.display = 'block';

        const rect = container.getBoundingClientRect();
        const clientX = (x / width) * rect.width;
        const clientY = (y / height) * rect.height;

        tooltip.style.left = `${clientX}px`;
        tooltip.style.top = `${clientY}px`;
      }
    });

    circle.addEventListener('mouseleave', () => {
      circle.setAttribute('r', '3.5');
      circle.setAttribute('fill', '#0284c7');
      if (tooltip) tooltip.style.display = 'none';
    });

    svg.appendChild(circle);
  });
}

function renderProductBreakdown(products) {
  const container = document.getElementById('product-list-container');
  if (!container) return;

  container.innerHTML = '';
  products.forEach(p => {
    const row = document.createElement('div');
    row.className = 'product-row';

    row.innerHTML = `
      <div class="product-info-row">
        <span class="product-name">${escapeHtml(p.product)}</span>
        <div>
          <span class="product-rev">${formatCurrency(p.total_revenue)}</span>
          <span class="product-share">(${p.revenue_percentage}%)</span>
        </div>
      </div>
      <div class="progress-track" title="${p.total_units} units sold across ${p.transaction_count} orders">
        <div class="progress-fill" style="width: ${Math.min(100, p.revenue_percentage * 3.5)}%"></div>
      </div>
    `;
    container.appendChild(row);
  });
}

// -----------------------------------------------------------------------------
// Section 3: Revenue Forecast
// -----------------------------------------------------------------------------
function renderForecast(forecastList, evalMeta) {
  const cardsContainer = document.getElementById('forecast-cards-container');
  if (cardsContainer && Array.isArray(forecastList)) {
    cardsContainer.innerHTML = '';

    forecastList.forEach((fc, idx) => {
      const card = document.createElement('div');
      card.className = 'fc-card';

      card.innerHTML = `
        <div class="fc-badge">Projected Month +${idx + 1}</div>
        <div class="fc-month">${fc.forecast_month}</div>
        <div class="fc-predicted">${formatCurrency(fc.predicted_revenue)}</div>
        <div class="fc-range-label">95% Prediction Interval</div>
        <div class="fc-range-value">${formatCurrency(fc.lower_bound_95)} – ${formatCurrency(fc.upper_bound_95)}</div>
      `;
      cardsContainer.appendChild(card);
    });
  }

  // Methodology details
  const elMethod = document.getElementById('method-name');
  const elMape = document.getElementById('method-mape');
  const elMae = document.getElementById('method-mae');

  if (elMethod && evalMeta) elMethod.textContent = evalMeta.model_method || 'Holt-Winters Smoothing';
  if (elMape && evalMeta) elMape.textContent = `${evalMeta.holdout_mape}% error`;
  if (elMae && evalMeta) elMae.textContent = formatCurrency(evalMeta.holdout_mae);
}

// -----------------------------------------------------------------------------
// Section 4: Business Insights & Recommendations
// -----------------------------------------------------------------------------
function renderInsights(insights) {
  const grid = document.getElementById('insights-grid-container');
  if (grid && insights) {
    grid.innerHTML = '';

    const cardsData = [
      {
        tag: 'Revenue Driver',
        tagClass: 'insight-tag-blue',
        title: insights.primary_revenue_driver.title,
        body: insights.primary_revenue_driver.description
      },
      {
        tag: 'Growth Trend',
        tagClass: 'insight-tag-teal',
        title: insights.historical_growth.title,
        body: insights.historical_growth.description
      },
      {
        tag: 'Seasonality',
        tagClass: 'insight-tag-blue',
        title: insights.seasonality_pattern.title,
        body: insights.seasonality_pattern.description
      },
      {
        tag: 'Forecast Outlook',
        tagClass: 'insight-tag-teal',
        title: insights.forecast_outlook.title,
        body: insights.forecast_outlook.description
      }
    ];

    cardsData.forEach(item => {
      const card = document.createElement('div');
      card.className = 'insight-card';
      card.innerHTML = `
        <div class="insight-header">
          <span class="insight-tag ${item.tagClass}">${item.tag}</span>
        </div>
        <h4 class="insight-title">${escapeHtml(item.title)}</h4>
        <p class="insight-body">${escapeHtml(item.body)}</p>
      `;
      grid.appendChild(card);
    });
  }

  // Practical Recommendations list
  const recsList = document.getElementById('recommendations-list');
  if (recsList && insights && Array.isArray(insights.actionable_recommendations)) {
    recsList.innerHTML = '';
    insights.actionable_recommendations.forEach(rec => {
      const li = document.createElement('li');
      li.textContent = rec;
      recsList.appendChild(li);
    });
  }
}

// -----------------------------------------------------------------------------
// Helper Utilities
// -----------------------------------------------------------------------------
function formatCurrency(num) {
  if (num === null || num === undefined || isNaN(num)) return '$0.00';
  return '$' + Number(num).toLocaleString('en-US', {
    minimumFractionDigits: 2,
    maximumFractionDigits: 2
  });
}

function escapeHtml(str) {
  if (!str) return '';
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;');
}
