## ---------------------------------------------------------------------------
## gas_crude_spread.R
##
## Visualizes US retail gasoline prices (GASREGW, $/gal) against Brent crude
## (DCOILBRENTEU, $/bbl) to show how elevated gasoline is relative to crude,
## especially as crude has come off its recent highs.
##
## Data note: this is the weekly export - GASREGW is genuinely weekly here,
## and DCOILBRENTEU (a daily series) has been aligned to the same weekly
## dates by FRED. The script does not assume any particular frequency, so it
## works with either the monthly or weekly export, but the recent-window
## panel below is tuned for weekly resolution (finer x-axis breaks).
##
## Comparability note: gasoline is converted from $/gallon to $/bbl-equivalent
## (x 42 gallons/bbl) so it's on the same footing as Brent. This is NOT a true
## 3-2-1 crack spread (which uses wholesale/spot product prices, not retail
## pump prices that embed taxes, distribution, and marketing margins) - it's a
## simple, transparent way to see the two series move together in the same
## units and to flag when retail gasoline is unusually rich relative to crude.
## ---------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(scales)

## Set to TRUE if you have patchwork installed and want the two panels
## combined into one figure. If FALSE, the two plots are drawn separately.
use_patchwork <- requireNamespace("patchwork", quietly = TRUE)
if (use_patchwork) library(patchwork)

# ---- 1. Load & prep data ---------------------------------------------------

setwd("/Users/prest/Dropbox/RFF/Projects/DOGMA/data/FRED Data/2026-07-07--crude vs gasoline/")
# csv_path <- "/Users/Owner/Dropbox/RFF/Projects/DOGMA/data/FRED Data/2026-07-07--crude vs gasoline/"   # adjust path as needed (weekly export)
csv_path <- ""

dt <- fread(paste0(csv_path,"fredgraph.csv"))
setnames(dt, c("date", "gas_gal", "brent"))
dt[, date := as.Date(date)]

# Drop rows before gasoline data starts (pre-1990) and any missing obs
dt <- dt[!is.na(gas_gal) & !is.na(brent)]

# Convert gasoline to a $/bbl-equivalent so it's comparable to Brent
dt[, gas_bbl := gas_gal * 42]

# Spread and ratio: how rich is gasoline relative to crude, in each unit
dt[, spread := gas_bbl - brent]          # $/bbl-equivalent gross margin proxy
dt[, ratio  := gas_bbl / brent]          # unitless, easier to compare across price levels

# 8-week rolling mean of the spread to smooth out weekly noise for the chart
dt[, spread_smooth := frollmean(spread, n = 8, align = "right")]

# ---- 2. Historical context for the current reading -------------------------

latest <- dt[.N]
pctile_spread <- ecdf(dt$spread)(latest$spread)
pctile_ratio  <- ecdf(dt$ratio)(latest$ratio)

cat(sprintf(
  "Latest obs: %s\n  Brent: $%.2f/bbl | Gasoline: $%.3f/gal ($%.2f/bbl-equiv)\n  Spread: $%.2f/bbl-equiv (%.0fth percentile of history since %s)\n  Ratio:  %.2fx (%.0fth percentile)\n",
  format(latest$date, "%b %Y"), latest$brent, latest$gas_gal, latest$gas_bbl,
  latest$spread, pctile_spread * 100, format(min(dt$date), "%Y"),
  latest$ratio, pctile_ratio * 100
))

# ---- 3. Panel A: levels, full history --------------------------------------

levels_long <- melt(
  dt[, .(date, brent, gas_bbl)],
  id.vars = "date",
  variable.name = "series",
  value.name = "price"
)
levels_long[, series := factor(series,
                               levels = c("brent", "gas_bbl"),
                               labels = c("Brent crude ($/bbl)", "Gasoline ($/bbl-equivalent)")
)]

p_levels <- ggplot(levels_long, aes(date, price, color = series)) +
  geom_line(size = 0.9) +
  scale_color_manual(values = c("Brent crude ($/bbl)" = "#1b4965",
                                "Gasoline ($/bbl-equivalent)" = "#d1495b")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Gasoline (bbl-equivalent) vs. Brent crude",
    x = NULL, y = "$/bbl",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# ---- 4. Panel B: spread over time, recent period highlighted ---------------

recent_start <- max(dt$date) - 365  # last 12 months shaded

p_spread <- ggplot(dt, aes(date, spread)) +
  annotate("rect",
           xmin = recent_start, xmax = max(dt$date),
           ymin = -Inf, ymax = Inf,
           fill = "#f4a261", alpha = 0.15
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(aes(y = spread), color = "#264653", size = 0.4, alpha = 0.4) +
  geom_line(aes(y = spread_smooth), color = "#264653", size = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Gasoline minus Brent (bbl-equivalent), shaded = last 12 months",
    subtitle = "Thin line = weekly spread, thick line = 8-week rolling mean",
    x = NULL, y = "$/bbl-equivalent spread"
  ) +
  theme_minimal(base_size = 12)

# ---- 5. Zoomed-in recent view (last 3 years) for the current divergence ---

recent <- dt[date >= max(date) - 365 * 3]
recent_long <- melt(
  recent[, .(date, brent, gas_bbl)],
  id.vars = "date", variable.name = "series", value.name = "price"
)
recent_long[, series := factor(series,
                               levels = c("brent", "gas_bbl"),
                               labels = c("Brent crude ($/bbl)", "Gasoline ($/bbl-equivalent)")
)]

p_recent <- ggplot(recent_long, aes(date, price, color = series)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c("Brent crude ($/bbl)" = "#1b4965",
                                "Gasoline ($/bbl-equivalent)" = "#d1495b")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Last 3 years: crude has rolled over, gasoline has not followed",
    x = NULL, y = "$/bbl", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))

# ---- 6. Output --------------------------------------------------------------

if (use_patchwork) {
  combined <- (p_levels / p_spread / p_recent) +
    plot_annotation(
      title = "Gasoline vs. Brent crude: full history and recent divergence",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
  ggsave("gas_crude_spread.png", combined, width = 10, height = 13, dpi = 150)
  print(combined)
} else {
  ggsave("gas_crude_levels.png", p_levels, width = 10, height = 5, dpi = 150)
  ggsave("gas_crude_spread_panel.png", p_spread, width = 10, height = 5, dpi = 150)
  ggsave("gas_crude_recent.png", p_recent, width = 10, height = 5, dpi = 150)
  print(p_levels); print(p_spread); print(p_recent)
}

## ---------------------------------------------------------------------------
## 7. Scatter: gasoline ($/gal) vs. Brent crude ($/bbl), with regime-specific
##    best-fit lines, points colored by time, and the most recent data
##    point(s) highlighted.
##
## Note: here gasoline is left in its native $/gal units (not converted to
## $/bbl-equivalent) since the scatter is about the retail pass-through
## relationship itself, not a unit-matched spread.
## ---------------------------------------------------------------------------

window_5yr <- dt[date >= max(date) - 365 * 5]
window_1yr <- dt[date >= max(date) - 365 * 1]

fit_full <- lm(gas_gal ~ brent, data = dt)
fit_5yr  <- lm(gas_gal ~ brent, data = window_5yr)
fit_1yr  <- lm(gas_gal ~ brent, data = window_1yr)

cat(sprintf(
  paste0(
    "Pass-through regressions (gasoline $/gal on Brent $/bbl):\n",
    "  Full history (%s-%s): slope = %.4f $/gal per $/bbl, intercept = $%.2f\n",
    "  Last 5 years:              slope = %.4f $/gal per $/bbl, intercept = $%.2f\n",
    "  Last 12 months:            slope = %.4f $/gal per $/bbl, intercept = $%.2f\n"
  ),
  format(min(dt$date), "%Y"), format(max(dt$date), "%Y"),
  coef(fit_full)[2], coef(fit_full)[1],
  coef(fit_5yr)[2],  coef(fit_5yr)[1],
  coef(fit_1yr)[2],  coef(fit_1yr)[1]
))

# Highlight the last ~2 months of points, and label the single latest one
recent_pts <- dt[date >= max(date) - 60]
latest_pt  <- dt[.N]

p_scatter <- ggplot(dt, aes(x = brent, y = gas_gal)) +
  geom_point(aes(color = as.numeric(date)), size = 1.6, alpha = 0.5) +
  scale_color_viridis_c(
    name = NULL,
    breaks = as.numeric(as.Date(paste0(seq(1990, 2025, 5), "-01-01"))),
    labels = seq(1990, 2025, 5)
  ) +
  geom_smooth(data = dt, method = "lm", se = FALSE,
              color = "grey40", linetype = "dotted", size = 0.8) +
  geom_smooth(data = window_5yr, method = "lm", se = FALSE,
              color = "#e76f51", size = 1) +
  geom_smooth(data = window_1yr, method = "lm", se = FALSE,
              color = "#d1495b", size = 1.2, linetype = "dashed") +
  geom_point(data = recent_pts, color = "black", fill = "#ffe66d",
             shape = 21, size = 3, stroke = 0.8) +
  geom_text(data = latest_pt, aes(label = format(date, "%b %d, %Y")),
            vjust = -1.1, fontface = "bold", size = 3.5) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Gasoline vs. Brent crude: pass-through scatter with regime fits",
    subtitle = paste0(
      "Grey dotted = full-history fit  |  Orange solid = last 5 years\n",
      "Red dashed = last 12 months  |  Yellow points = last ~2 months"
    ),
    x = "Brent crude ($/bbl)", y = "Gasoline ($/gal)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave("gas_crude_scatter.png", p_scatter, width = 9, height = 7, dpi = 150)
print(p_scatter)

## ---------------------------------------------------------------------------
## 8. Interactive version: hover-enabled scatter + rolling-regime deviation
##    panel, stacked with subplot(). Each panel has its own independent hover
##    tooltip (date, prices, deviation) - no cross-panel linked highlighting,
##    since that requires crosstalk, which pulls in a dplyr/vctrs dependency
##    chain that throws `vec_proxy_compare()` errors on some older package
##    combinations (seen on R 4.0.2). Dropping crosstalk avoids that entirely.
##
## Regime definition here: for each week, fit gas_gal ~ brent on the trailing
## 5-year window of weekly data (right-aligned, i.e. the window ENDS at and
## INCLUDES that week - a rolling "fair value" line, not a strict one-step-
## ahead out-of-sample forecast). Deviation = actual gasoline price minus
## what that trailing 5-year relationship would predict at that week's
## crude price. The first ~5 years of the sample have no rolling regime yet
## (not enough trailing history) and are dropped from this section only.
##
## Requires: plotly, htmlwidgets (install.packages() if needed)
## ---------------------------------------------------------------------------

library(plotly)
library(htmlwidgets)

# ---- Rolling 5-year regime, on a true CALENDAR window (no extra packages) --
# Same goal as before - a genuine ~5-calendar-year trailing window rather
# than a fixed row count, so the one date gap (late 1990/early 1991, ~6
# missing weeks) doesn't distort nearby windows - but implemented with
# data.table's own rolling join instead of slider/lubridate, avoiding two
# more dependencies.
#
# Mechanics: cumulative sums of x, y, xy, x^2 give an O(1) lookup for "sum
# over rows 1..i". A data.table rolling join finds, for each row, the first
# row whose date falls within its trailing ~5-year window (n_days below).
# Subtracting cumulative sums at that boundary gives the sum WITHIN the
# window, and from there the same closed-form OLS slope/intercept as before
# - just over a calendar-exact window instead of a fixed row count.

n_days <- round(365.25 * 5)  # ~1826 days; using 365.25 nets out leap years on
# average - precise enough here, since the goal
# is avoiding the ~1-week distortion from the
# date gap, not day-level calendar precision

dt[, row_n  := .I]
dt[, cum_x  := cumsum(brent)]
dt[, cum_y  := cumsum(gas_gal)]
dt[, cum_xy := cumsum(brent * gas_gal)]
dt[, cum_xx := cumsum(brent^2)]

lookup <- data.table(date = dt$date - n_days)
dt[, start_idx := dt[lookup, on = "date", roll = -Inf, which = TRUE]]

# A window only counts as "complete" once there's a genuine 5 years of prior
# history to draw on - not just however much history happens to exist yet
dt[, complete_window := date >= (min(date) + n_days)]

prior_x  <- c(0, dt$cum_x)[dt$start_idx]
prior_y  <- c(0, dt$cum_y)[dt$start_idx]
prior_xy <- c(0, dt$cum_xy)[dt$start_idx]
prior_xx <- c(0, dt$cum_xx)[dt$start_idx]
n_obs    <- dt$row_n - dt$start_idx + 1

sum_x  <- dt$cum_x  - prior_x
sum_y  <- dt$cum_y  - prior_y
sum_xy <- dt$cum_xy - prior_xy
sum_xx <- dt$cum_xx - prior_xx

dt[, slope_roll     := ifelse(complete_window, (n_obs * sum_xy - sum_x * sum_y) / (n_obs * sum_xx - sum_x^2), NA_real_)]
dt[, intercept_roll := ifelse(complete_window, (sum_y - slope_roll * sum_x) / n_obs, NA_real_)]
dt[, gas_hat_roll   := intercept_roll + slope_roll * brent]
dt[, resid_roll     := gas_gal - gas_hat_roll]

# Rolling min/max of Brent within each point's own trailing window, so the
# hover-line can be clipped to the crude-price range that actually generated
# it. Variable-width windows (from the one date gap) rule out a simple
# fixed-width frollapply, so this loops explicitly over the ~1,780 rows with
# a complete window - trivially fast even unvectorized at this data size.
dt[, brent_roll_min := NA_real_]
dt[, brent_roll_max := NA_real_]
complete_rows <- which(dt$complete_window)
for (i in complete_rows) {
  window_vals <- dt$brent[dt$start_idx[i]:i]
  set(dt, i = i, j = "brent_roll_min", value = min(window_vals))
  set(dt, i = i, j = "brent_roll_max", value = max(window_vals))
}

dt_roll <- dt[!is.na(resid_roll)]  # drop the first ~5 years with no rolling regime yet

dt_roll[, hover_txt := sprintf(
  "%s<br>Brent: $%.2f/bbl<br>Gasoline: $%.3f/gal<br>Predicted (5-yr regime): $%.3f/gal<br>Deviation: %s$%.3f/gal",
  format(date, "%b %d, %Y"), brent, gas_gal, gas_hat_roll,
  ifelse(resid_roll >= 0, "+", "-"), abs(resid_roll)
)]

# Converting to a plain data.frame here (rather than passing the data.table
# directly) as a cheap workaround: data.table objects can interact oddly with
# dplyr's internal comparison/grouping machinery, which plotly calls under
# the hood even for a bare plot_ly(). Must happen AFTER the := calls above,
# since := is data.table-only syntax.
#
# UPDATE: confirmed root cause is a real vctrs/dplyr mismatch (dplyr 1.0.0 is
# too old for vctrs 0.6.5) - fix is install.packages("dplyr") (and possibly
# "rlang") then a FULL R restart (dplyr caches the old vctrs namespace in
# memory otherwise). Keeping the data.frame conversion below regardless,
# since it's harmless and one less variable once dplyr is updated.
dt_roll <- as.data.frame(dt_roll)

year_breaks <- seq(2000, 2025, 5)
year_ticks  <- as.numeric(as.Date(paste0(year_breaks, "-01-01")))

# ---- Best-fit lines for the interactive scatter ----------------------------
# Three fixed-window regimes, plus a dynamic line that updates on hover to
# show the trailing 5-year regression ending at whichever point the mouse is
# over. Each line - including the hover one - is drawn only across the range
# of crude prices actually observed in ITS OWN window, not the full plot's
# x-axis, so nothing visually extrapolates beyond the data that produced it.

line_y <- function(fit, xr) coef(fit)[1] + coef(fit)[2] * xr

window_5yr_recent <- dt[date >= max(date) - 365 * 5]
window_2yr_recent <- dt[date >= max(date) - 365 * 2]

fit_full_all   <- lm(gas_gal ~ brent, data = dt)
fit_5yr_recent <- lm(gas_gal ~ brent, data = window_5yr_recent)
fit_2yr_recent <- lm(gas_gal ~ brent, data = window_2yr_recent)

x_full <- range(dt$brent)
x_5yr  <- range(window_5yr_recent$brent)
x_2yr  <- range(window_2yr_recent$brent)

y_full <- line_y(fit_full_all, x_full)
y_5yr  <- line_y(fit_5yr_recent, x_5yr)
y_2yr  <- line_y(fit_2yr_recent, x_2yr)

# Iran war onset (2/23/2026-present): now ON by default and styled yellow,
# taking over the role the separate "last ~4 months" trace used to play (that
# trace is dropped - this one already covers essentially the same recent
# window and more).
iran_pts <- dt_roll[dt_roll$date >= as.Date("2026-02-23"), ]
iran_pts <- iran_pts[order(iran_pts$date), ]
iran_pts$label_text <- format(iran_pts$date, "%b %d")
# Suppress the label on the single latest date here, since that date gets its
# own always-visible annotation below (avoids two overlapping text labels at
# the same point)
iran_pts$label_text[iran_pts$date == max(dt_roll$date)] <- ""

# 2021-2022 price spike/fall (9/2021-12/2022): still OFF by default (toggle
# via legend), but now labels only the first observation of each month, plus
# 6/27/2022 specifically (the date with the single highest deviation in this
# window, worth comparing against the current 6/29/2026 reading) - labeling
# every one of the ~65 weekly points here was too cluttered.
spike_pts <- dt_roll[dt_roll$date >= as.Date("2021-09-01") & dt_roll$date <= as.Date("2022-12-31"), ]
spike_pts <- spike_pts[order(spike_pts$date), ]
spike_ym <- format(spike_pts$date, "%Y-%m")
spike_label_mask <- !duplicated(spike_ym) | (spike_pts$date == as.Date("2022-06-27"))
spike_pts$label_text <- ifelse(spike_label_mask, format(spike_pts$date, "%b %d, %Y"), "")

# The Iran war onset and the "off by default" toggle for the 2021-2022 trace
# both use plotly's native legendonly/click-to-toggle mechanism rather than a
# literal HTML checkbox - given how much JS debugging you've already been
# through this session, leaning on plotly's built-in behavior instead of more
# custom JavaScript.
#
# NOTE on labeling: plotly has no built-in collision avoidance for text
# labels (unlike ggrepel in the static plots), so labels can still overlap at
# default zoom, especially as the Iran-war window keeps growing - zoom/pan in
# the HTML to spread them out, or say the word if you'd like monthly-only
# labeling applied there too.

latest_pt <- dt_roll[nrow(dt_roll), ]  # always shown regardless of any trace's legend toggle

p1 <- plot_ly(dt_roll, x = ~brent, y = ~gas_gal, type = "scatter", mode = "markers",
              text = ~hover_txt, hoverinfo = "text", name = "Weekly observations",
              showlegend = FALSE,   # legend swatch for a continuous color scale isn't useful; the colorbar covers it
              marker = list(
                size = 6, opacity = 0.5,
                color = ~as.numeric(date), colorscale = "Viridis",
                colorbar = list(title = "Year", tickvals = year_ticks, ticktext = year_breaks, x = 1.02)
              )) %>%
  add_lines(x = x_full, y = y_full, name = "Full history (1990-present)",
            line = list(color = "grey40", dash = "dot", width = 1.5), inherit = FALSE) %>%
  add_lines(x = x_5yr, y = y_5yr, name = "Last 5 years",
            line = list(color = "#e76f51", width = 1.5), inherit = FALSE) %>%
  add_lines(x = x_2yr, y = y_2yr, name = "Last 2 years",
            line = list(color = "#d1495b", dash = "dash", width = 1.5), inherit = FALSE) %>%
  add_markers(data = iran_pts, x = ~brent, y = ~gas_gal, name = "Since Iran war onset (2/23/2026)",
              mode = "markers+text", text = ~label_text, textposition = "top center",
              hovertext = ~hover_txt, hoverinfo = "text", inherit = FALSE,
              textfont = list(size = 9),
              marker = list(size = 9, color = "#ffe66d", line = list(color = "black", width = 1))) %>%
  add_markers(data = spike_pts, x = ~brent, y = ~gas_gal, name = "2021-2022 spike/fall (optional)",
              mode = "markers+text", text = ~label_text, textposition = "top center",
              hovertext = ~hover_txt, hoverinfo = "text", inherit = FALSE, visible = "legendonly",
              textfont = list(size = 9),
              marker = list(size = 8, symbol = "square", color = "#457b9d",
                            line = list(color = "black", width = 1))) %>%
  add_annotations(data = latest_pt, x = ~brent, y = ~gas_gal,
                  text = ~format(date, "%b %d, %Y"), showarrow = TRUE, arrowhead = 2,
                  ax = 0, ay = -30, font = list(size = 11, color = "black")) %>%
  layout(
    title = "Gasoline vs. Brent: pass-through scatter, colored by year",
    xaxis = list(title = "Brent crude ($/bbl)"),
    yaxis = list(title = "Gasoline ($/gal)"),
    legend = list(orientation = "h", x = 0, y = -0.3, xanchor = "left"),
    margin = list(t = 100, b = 140),
    # Initial placeholder for the hover-regime line, drawn as a SHAPE (not a
    # data trace) - see note below on why that matters for performance.
    shapes = list(list(
      type = "line", xref = "x", yref = "y",
      x0 = x_full[1], x1 = x_full[2], y0 = y_full[1], y1 = y_full[2],
      line = list(color = "black", width = 3)
    ))
  )

# The hover-regime line is a layout "shape", updated via Plotly.relayout,
# rather than a plot trace updated via Plotly.restyle (which is what an
# earlier version did, and which caused freezing). Restyle rewrites full
# trace data - including recalculating the colorbar/marker pool - and doing
# that on every mouse move eventually bogs the browser down; worse, because
# shapes/traces are both part of what plotly.js scans for hover targets,
# restyling a data trace from inside a hover handler can re-trigger
# plotly_hover on the very next redraw, creating a feedback loop. A shape
# isn't a hover target and isn't part of the trace pool, so relayout-ing it
# is cheap and doesn't re-fire hover events. Added a lastKey guard too, to
# skip redundant updates when the mouse jitters within the same point.
#
# Trace order (curveNumber): 0=points, 1=full line, 2=5yr line, 3=2yr line,
# 4=Iran-war, 5=2021-22 spike. Only 0/4/5 are real data points, so those are
# the only ones the hover handler reacts to. (Named, not positional, JS data
# keys below - "iran"/"spike" rather than a trace-index number - so this
# doesn't need re-numbering again if traces get added/removed later.)
p1 <- htmlwidgets::onRender(p1, "
  function(el, x, data) {
    var lastKey = null;
    el.on('plotly_hover', function(evt) {
      var pt = evt.points[0];
      var idx = pt.pointNumber;
      var slope, intercept, xmin, xmax;
      if (pt.curveNumber === 0) {
        slope = data.main.slope[idx]; intercept = data.main.intercept[idx];
        xmin = data.main.xmin[idx]; xmax = data.main.xmax[idx];
      } else if (pt.curveNumber === 4) {
        slope = data.iran.slope[idx]; intercept = data.iran.intercept[idx];
        xmin = data.iran.xmin[idx]; xmax = data.iran.xmax[idx];
      } else if (pt.curveNumber === 5) {
        slope = data.spike.slope[idx]; intercept = data.spike.intercept[idx];
        xmin = data.spike.xmin[idx]; xmax = data.spike.xmax[idx];
      } else {
        return;  // ignore the fit-line traces themselves
      }
      var key = pt.curveNumber + '-' + idx;
      if (key === lastKey) return;
      lastKey = key;
      var y0 = intercept + slope * xmin;
      var y1 = intercept + slope * xmax;
      Plotly.relayout(el, {
        'shapes[0].x0': xmin, 'shapes[0].x1': xmax,
        'shapes[0].y0': y0,   'shapes[0].y1': y1
      });
    });
  }
", data = list(
  main = list(slope = dt_roll$slope_roll, intercept = dt_roll$intercept_roll,
              xmin = dt_roll$brent_roll_min, xmax = dt_roll$brent_roll_max),
  iran = list(slope = iran_pts$slope_roll, intercept = iran_pts$intercept_roll,
              xmin = iran_pts$brent_roll_min, xmax = iran_pts$brent_roll_max),
  spike = list(slope = spike_pts$slope_roll, intercept = spike_pts$intercept_roll,
               xmin = spike_pts$brent_roll_min, xmax = spike_pts$brent_roll_max)
))

p2 <- plot_ly(dt_roll, x = ~date, y = ~resid_roll, type = "scatter", mode = "lines+markers",
              text = ~hover_txt, hoverinfo = "text",
              line = list(color = "#264653", width = 1),
              marker = list(size = 3, color = "#264653")) %>%
  layout(
    title = "Deviation from rolling 5-yr regime, over time",
    xaxis = list(title = NULL),
    yaxis = list(title = "Deviation from 5-yr regime ($/gal)"),
    margin = list(t = 100),
    shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                       y0 = 0, y1 = 0, line = list(dash = "dot", color = "grey")))
  )

# ---- Source caption ---------------------------------------------------------
# After several failed attempts to inject the caption directly into
# saveWidget()'s output (a plotly annotation that wouldn't render in the
# standalone file; a readLines()/writeLines() round-trip that corrupted the
# file into a blank page; a plain append that landed the caption after the
# closing </html> tag where browsers won't reliably show it) - rather than
# keep guessing at that file's internal structure, this stops modifying it
# entirely. Instead, it writes a small SEPARATE wrapper HTML page from
# scratch (fully under our control, so there's nothing uncertain about it)
# that embeds the original, untouched widget file via an <iframe>, with the
# caption as an ordinary paragraph below it. Open the wrapper file (not the
# raw widget file) to see the plot with its caption.
wrap_with_caption <- function(widget_path, wrapper_path, page_title) {
  caption <- paste0(
    '<p style="font-family:sans-serif;font-size:11px;color:#888;',
    'margin:8px 20px;">Source: FRED series ',
    '<a href="https://fred.stlouisfed.org/series/GASREGW">GASREGW</a> and ',
    '<a href="https://fred.stlouisfed.org/series/DCOILBRENTEU">DCOILBRENTEU</a></p>'
  )
  wrapper_html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>', page_title, '</title>',
    '<style>body{margin:0;font-family:sans-serif;} ',
    'iframe{width:100%;height:85vh;border:none;display:block;}</style></head><body>',
    '<iframe src="', widget_path, '"></iframe>',
    caption,
    '</body></html>'
  )
  writeLines(wrapper_html, wrapper_path)
}

# NOTE: subplot() and htmltools::browsable(tagList(...)) were both tried to
# combine these into a single HTML file and both hit environment-specific
# errors on this machine (a vctrs/dplyr version issue for subplot(), then a
# separate htmlwidgets/htmltools namespace error for tagList()). Saving each
# panel as its own self-contained HTML file sidesteps both and is confirmed
# working - you'll just have two files to open instead of one, and no synced
# zoom/pan between them (each is otherwise fully interactive on its own:
# hover, zoom, pan).
saveWidget(p1, "gas_crude_scatter_interactive.html", selfcontained = TRUE)
saveWidget(p2, "gas_crude_deviation_interactive.html", selfcontained = TRUE)
wrap_with_caption("gas_crude_scatter_interactive.html", "gas_crude_scatter_page.html",
                  "Gasoline vs. Brent: pass-through scatter")
wrap_with_caption("gas_crude_deviation_interactive.html", "gas_crude_deviation_page.html",
                  "Deviation from rolling 5-yr regime")
# Open gas_crude_scatter_page.html and gas_crude_deviation_page.html (not the
# "..._interactive.html" files directly) to see the plots with captions.