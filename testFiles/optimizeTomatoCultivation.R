# =============================================================================
# CUMBA — Irrigation Optimisation — Figures & Table  [REVISED v3]
# =============================================================================
# Figure layout (figOPT_ABC_composite.png):
#   Row 1 (full width) — Panel A: 4 scatter facets
#                        (irr×yield | irr×brix | brix×yield | n.irr×iwue)
#                        dominated = grey, Pareto = turbo colour
#   Row 2 left  — Panel B: 4-tile heatmap, dark->light monochrome, adaptive text
#   Row 2 right — Panel C: Pareto bubble, Brix inset + DOY/WSI legends top
#
# Table (figOPT_TABLE_lmm.png + tab_lmm_coefs.csv):
#   Rows = LMM terms, Columns = Yield / Brix / Irr / IWUE
#   Cell = "estimate (SE)*"
#
# Pareto front objectives (5): high(yield) * high(brix) * low(irrigation)
#                               * high(iwue) * low(n_irrigation)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------------
library(tidyverse)
library(devtools)
library(cumba)
library(lme4)
library(lmerTest)
library(rPref)
library(broom.mixed)
library(patchwork)
library(akima)
library(cowplot)   # needed for get_legend() in Panel C

fig_dir <- "figures"
dir.create(fig_dir, showWarnings = FALSE)

# PNG save helper — base R, always available
save_png <- function(plot, filename, width_cm, height_cm, dpi = 300) {
  path <- file.path(fig_dir, filename)
  grDevices::png(path,
                 width  = width_cm, height = height_cm,
                 units  = "cm",    res    = dpi)
  print(plot)
  grDevices::dev.off()
  message("Saved: ", path)
}

# -----------------------------------------------------------------------------
# 1. Load weather
# -----------------------------------------------------------------------------
weather <- read.csv(paste0(getwd(), '//testFiles//weather_foggia.csv')) |>
  mutate(
    Rad  = as.numeric(Rg),
    ET0  = as.numeric(ET0),
    Tx   = TMAX, Tn = TMIN,
    Site = 'Foggia', P = RAIN, DATE = MDATE, Lat = 41
  ) |>
  mutate(DATE = as.Date(DATE, format = '%m/%d/%Y'), year = year(DATE))

devtools::document()
devtools::load_all()

# -----------------------------------------------------------------------------
# 2. Parameters
# -----------------------------------------------------------------------------
cumba_par <- cumba::cumbaParameters
cumba_par$CycleLength$value <- cumba_par$CycleLength$value + 150

# ── UPDATE FILENAME AFTER EACH NEW CALIBRATION ─────────────────────────────
ga_rds <- readRDS(file.path(inputDir, "ga_result_06_19_26.rds"))
# ───────────────────────────────────────────────────────────────────────────

if (is.null(ga_rds$best_params_named)) {
  stop("best_params_named not found in RDS — re-run calibration script")
}
best <- ga_rds$best_params_named

# --- Dynamically assign every calibrated parameter found in the RDS --------
# --- Dynamically assign every calibrated parameter found in the RDS --------
# No hardcoded parameter names: works for any calibration script, any subset.
unmatched <- character(0)
for (nm in names(best)) {
  if (nm %in% names(cumba_par)) {
    cumba_par[[nm]]$value <- best[[nm]]
  } else {
    unmatched <- c(unmatched, nm)
  }
}
if (length(unmatched) > 0) {
  warning(sprintf(
    "Parameter(s) in RDS not found in cumba::cumbaParameters and were skipped: %s",
    paste(unmatched, collapse = ", ")
  ))
}

cat(sprintf("\nCalibrated parameters loaded from RDS (n = %d):\n", length(best)))
for (nm in names(best)) cat(sprintf("  %-36s = %.4f\n", nm, best[[nm]]))

cat(sprintf("\nFixed parameters (not in this calibration run, n = %d):\n",
            length(setdiff(names(cumba_par), names(best)))))
for (nm in setdiff(names(cumba_par), names(best)))
  cat(sprintf("  %-36s = %s\n", nm, format(cumba_par[[nm]]$value)))
# -----------------------------------------------------------------------------
# 3. Experimental design
# -----------------------------------------------------------------------------
vegetativeWS     <- c(0.6, 0.7, 0.8, 0.9)
reproductiveWS   <- c(0.6, 0.7, 0.8, 0.9)
ripeningWS       <- c(0.6, 0.7, 0.8, 0.9)
transplantingDOY <- c(110, 115, 120, 125)

total <- length(vegetativeWS) * length(reproductiveWS) *
  length(ripeningWS)   * length(transplantingDOY)
cat(sprintf("Running %d combinations x 22 years = %d simulations\n",
            total, total * 22))

cumba_par$FieldCapacity$value <- 0.35
cumba_par$WiltingPoint$value  <- 0.15
cumba_par$FruitWaterContentMax$value<-0.905


# -----------------------------------------------------------------------------
# 4. Run simulations
# -----------------------------------------------------------------------------
results_list <- list(); i <- 1L

for (vws  in vegetativeWS) for (rws in reproductiveWS)
  for (riws in ripeningWS)   for (td  in transplantingDOY) {
    
    sim <- cumba_scenario(
      weather |> filter(year >= 2000), cumba_par,
      estimateRad = TRUE, estimateET0 = TRUE,
      irrigationStrategy = list(
        vegetative   = list(wsLevel = vws,  turnMin = 2L),
        reproductive = list(wsLevel = rws,  turnMin = 2L),
        ripening     = list(wsLevel = riws, turnMin = 2L)
      ),
      irrigationStopCycle = 90,
      transplantingDOY = td,
      fullOut = TRUE,
      irrigationEfficiency = .85
    )
    
    irr_cumul <- sim |> group_by(year) |>
      filter(cycleCompletion < 100) |>
      summarise(irr_total    = sum(irrigation,     na.rm = TRUE),
                n_irrigation = sum(irrigation > 0, na.rm = TRUE),
                .groups = "drop")
    
    harvest_vals <- sim |> group_by(year) |>
      filter(cycleCompletion >= 100) |> slice_head(n = 1) |> ungroup()
    
    results_list[[i]] <- harvest_vals |>
      left_join(irr_cumul, by = "year") |>
      mutate(yield = fruitFreshWeightAct / 100, brix = brixAct,
             irrigation = irr_total,
             iwue = yield / irrigation,     # Mg ha-1 mm-1
             vegetativeWS = vws, reproductiveWS = rws,
             ripeningWS = riws, transplantingDOY = td, harvestDOY = doy) |>
      select(year, yield, brix, irrigation, n_irrigation, iwue,
             vegetativeWS, reproductiveWS, ripeningWS, transplantingDOY, harvestDOY)
    
    i <- i + 1L
    if (i %% 50L == 0L) cat(sprintf("  %d / %d done\n", i - 1L, total))
  }

results_df <- bind_rows(results_list)
saveRDS(results_df, 'results_irrigation_optimizer_calibrated.rds')
results_df <- readRDS('results_irrigation_optimizer_calibrated.rds')

# -----------------------------------------------------------------------------
# 5. Aggregation
# -----------------------------------------------------------------------------
results_agg <- results_df |>
  group_by(vegetativeWS, reproductiveWS, ripeningWS, transplantingDOY) |>
  summarise(across(c(yield, brix, irrigation, n_irrigation, iwue),
                   list(mean = \(x) mean(x, na.rm = TRUE),
                        sd   = \(x) sd(x,   na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# -----------------------------------------------------------------------------
# 6. Pareto — 5 objectives, includes n_irrigation (low = better)
# -----------------------------------------------------------------------------
pareto_levels <- psel(
  results_agg,
  high(yield_mean) * high(brix_mean) * low(irrigation_mean) *
    high(iwue_mean) * low(n_irrigation_mean),
  top = nrow(results_agg)
)
results_agg$pareto_level <- pareto_levels$.level
pareto_df    <- filter(results_agg, pareto_level == 1L)
dominated_df <- filter(results_agg, pareto_level >  1L)
cat(sprintf("Pareto (5 objectives incl. n_irrigation): %d / %d\n",
            nrow(pareto_df), nrow(results_agg)))

# -----------------------------------------------------------------------------
# 7. LMM
# -----------------------------------------------------------------------------
results_lmer <- results_df |>
  mutate(across(c(vegetativeWS, reproductiveWS, ripeningWS,
                  transplantingDOY), factor))

mod_yield <- lmer(yield      ~ vegetativeWS + reproductiveWS + ripeningWS +
                    transplantingDOY + (1|year), data = results_lmer, REML = TRUE)
mod_brix  <- lmer(brix       ~ vegetativeWS + reproductiveWS + ripeningWS +
                    transplantingDOY + (1|year), data = results_lmer, REML = TRUE)
mod_irr   <- lmer(irrigation ~ vegetativeWS + reproductiveWS + ripeningWS +
                    transplantingDOY + (1|year), data = results_lmer, REML = TRUE)
mod_iwue  <- lmer(iwue       ~ vegetativeWS + reproductiveWS + ripeningWS +
                    transplantingDOY + (1|year), data = results_lmer, REML = TRUE)

# -----------------------------------------------------------------------------
# 8. Shared aesthetics
# -----------------------------------------------------------------------------
LBL_YIELD <- "Yield (Mg ha\u207B\u00b9)"
LBL_BRIX  <- "Brix (\u00b0)"
LBL_IRR   <- "Irrigation (mm)"
LBL_IWUE  <- "IWUE (Mg ha\u207B\u00b9 mm\u207B\u00b9)"
LBL_NIRR  <- "N\u00b0 irrigations"

COL_DOM   <- "#C8C6BC"

# Dark/light blue monochrome for Panel B
mono_fill <- function(label, dir = 1) {
  cols <- if (dir == 1) c("#EFF3FF", "#08306B") else c("#08306B", "#EFF3FF")
  scale_fill_gradient(low = cols[1], high = cols[2], name = label,
                      guide = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                             barwidth = unit(3.5, "cm"), barheight = unit(0.32, "cm")))
}

theme_paper <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "grey93", linewidth = 0.3),
      legend.key.size   = unit(0.45, "cm"),
      legend.text       = element_text(size = 11),
      legend.title      = element_text(size = 12, face = "bold"),
      axis.title.x      = element_text(size = 13, margin = margin(t = 3)),
      axis.title.y      = element_text(size = 13, margin = margin(r = 3)),
      axis.text         = element_text(size = 11),
      strip.text        = element_text(size = 13, face = "bold"),
      axis.line         = element_line(colour = "grey35", linewidth = 0.28),
      axis.ticks        = element_line(colour = "grey35", linewidth = 0.22),
      plot.tag          = element_text(face = "bold", size = 22),
      plot.margin       = margin(3, 3, 3, 3)
    )
}

# =============================================================================
# PANEL A — 4 scatter facets, legend compatta, plot massimizzato
# =============================================================================
make_scatter_long <- function(df, status) {
  bind_rows(
    df |> transmute(x = irrigation_mean, y = yield_mean,
                    col = brix_mean,      col_lab = LBL_BRIX,
                    xlab = LBL_IRR,       ylab = LBL_YIELD,
                    panel = "a) Irr vs Yield", status = status),
    df |> transmute(x = irrigation_mean, y = brix_mean,
                    col = yield_mean,     col_lab = LBL_YIELD,
                    xlab = LBL_IRR,       ylab = LBL_BRIX,
                    panel = "b) Irr vs Brix", status = status),
    df |> transmute(x = brix_mean,       y = yield_mean,
                    col = iwue_mean,      col_lab = LBL_IWUE,
                    xlab = LBL_BRIX,      ylab = LBL_YIELD,
                    panel = "c) Brix vs Yield", status = status),
    df |> transmute(x = n_irrigation_mean, y = iwue_mean,
                    col = irrigation_mean,  col_lab = LBL_IRR,
                    xlab = LBL_NIRR,        ylab = LBL_IWUE,
                    panel = "d) N.irr vs IWUE", status = status)
  )
}

scatter_dom    <- make_scatter_long(dominated_df, "dominated")
scatter_pareto <- make_scatter_long(pareto_df,    "pareto")

make_sub_scatter <- function(pan_label, col_label, col_dir = 1, tag = NULL) {
  dom <- filter(scatter_dom,    panel == pan_label)
  par <- filter(scatter_pareto, panel == pan_label)
  p <- ggplot() +
    geom_point(data = dom, aes(x = x, y = y),
               color = COL_DOM, size = 1.2, alpha = 0.3) +
    geom_point(data = par, aes(x = x, y = y, color = col),
               size = 2.2, alpha = 1) +
    scale_color_viridis_c(
      option = "turbo", direction = col_dir, name = col_label,
      guide = guide_colorbar(
        title.position = "top",
        title.hjust     = 0.5,
        barwidth        = unit(4.5, "cm"),
        barheight       = unit(0.22, "cm"),
        label.theme     = element_text(size = 10)
      )
    ) +
    labs(x = dom$xlab[1], y = dom$ylab[1]) +
    theme_paper() +
    theme(
      legend.position     = "top",
      legend.direction    = "horizontal",
      legend.title        = element_text(size = 12, face = "bold", margin = margin(b = 0)),
      legend.text         = element_text(size = 11),
      legend.margin       = margin(0, 0, 0, 0),
      legend.box.margin   = margin(0, 0, 0, 0),
      legend.box.spacing  = unit(1, "pt"),
      plot.margin         = margin(t = 1, r = 1, b = 1, l = 1)
    )
  if (!is.null(tag)) p <- p + labs(tag = tag)
  p
}

sA <- make_sub_scatter("a) Irr vs Yield",   LBL_BRIX,  1, tag = "A")
sB <- make_sub_scatter("b) Irr vs Brix",    LBL_YIELD, 1)
sC <- make_sub_scatter("c) Brix vs Yield",  LBL_IWUE,  1)
sD <- make_sub_scatter("d) N.irr vs IWUE",  LBL_IRR,  -1)

panel_A <- (sA | sB | sC | sD) +
  plot_layout(axis_titles = "collect")

# =============================================================================
# PANEL B — Heatmap 4-tile, blue dark/light monochrome
# =============================================================================
heat_data <- pareto_df |>
  pivot_longer(cols = c(vegetativeWS, reproductiveWS, ripeningWS),
               names_to = "phase", values_to = "ws_level") |>
  mutate(
    phase = recode(phase,
                   vegetativeWS   = "Vegetative",
                   reproductiveWS = "Reproductive",
                   ripeningWS     = "Ripening"),
    phase = factor(phase, levels = c("Vegetative", "Reproductive", "Ripening"))
  ) |>
  group_by(phase, ws_level) |>
  summarise(n = n(),
            yield_mean      = mean(yield_mean),
            brix_mean       = mean(brix_mean),
            irrigation_mean = mean(irrigation_mean),
            iwue_mean       = mean(iwue_mean),
            n_irrigation_mean = mean(n_irrigation_mean),
            .groups = "drop")

add_text_color <- function(df, fill_var, dir = 1) {
  rng <- range(df[[fill_var]], na.rm = TRUE)
  df |> mutate(
    fill_scaled = (.data[[fill_var]] - rng[1]) / diff(rng),
    fill_lum    = if (dir == 1) 1 - fill_scaled else fill_scaled,
    txt_color   = if_else(fill_lum > 0.55, "grey10", "white")
  )
}

make_heat_tile <- function(fill_var, fill_label, dir = 1, digits = 1, tag = NULL) {
  d <- add_text_color(heat_data, fill_var, dir)
  p <- ggplot(d, aes(x = factor(ws_level), y = phase,
                     fill = .data[[fill_var]])) +
    geom_tile(color = "white", linewidth = 1.0) +
    geom_text(aes(label = round(.data[[fill_var]], digits),
                  color = txt_color),
              size = 4.4, fontface = "bold") +
    scale_color_identity() +
    mono_fill(fill_label, dir = dir) +
    labs(x = "Water stress threshold", y = NULL, title = fill_label) +
    coord_cartesian(expand = FALSE) +
    theme_paper() +
    theme(
      legend.position = "none",
      plot.title       = element_text(size = 12, face = "bold",
                                      hjust = 0.5, margin = margin(b = 4)),
      plot.margin      = margin(t = 4, r = 3, b = 2, l = 3),
      panel.grid       = element_blank()
    )
  if (!is.null(tag)) p <- p + labs(tag = tag)
  p
}

h_yield <- make_heat_tile("yield_mean",      LBL_YIELD,  1, 1, tag = "B")
h_brix  <- make_heat_tile("brix_mean",       LBL_BRIX,   1, 2)
h_irr   <- make_heat_tile("irrigation_mean", LBL_IRR,   -1, 0)
h_iwue  <- make_heat_tile("iwue_mean",       LBL_IWUE,   1, 2)

panel_B <- (h_yield | h_brix) / (h_irr | h_iwue)

# =============================================================================
# PANEL C — Pareto bubble. Brix legend inside plot (top-left),
#           DOY + WSI legends in one row above the panel
# =============================================================================
# =============================================================================
# PANEL C — Pareto bubble. Outer ring (always circle) = reproductive water
#           stress (border color, light->dark single-hue). Inner point shape
#           = transplanting DOY, inner point color = brix. Fixed size.
#           Brix legend inside plot (top-left), DOY + WSI legends above panel.
# =============================================================================
library(ggnewscale)

# =============================================================================
# PANEL C — Pareto bubble. Outer ring (always circle) = reproductive water
#           stress (border color, grey light->dark). Inner point shape =
#           transplanting DOY, inner point color = brix (red light->dark).
#           Fixed size. Brix legend inside plot (top-left), DOY + WSI
#           legends above panel.
# =============================================================================
library(ggnewscale)
panel_C <- ggplot(pareto_df,
                  aes(x = yield_mean, y = irrigation_mean)) +
  # --- outer ring: always a circle, border color = reproductive water stress ---
  geom_point(aes(color = factor(reproductiveWS)),
             shape = 21, fill = NA, size = 6, stroke = 2) +
  scale_color_manual(
    values = c("0.6" = "lightblue", "0.7" = "lightblue4",
               "0.8" = "slateblue", "0.9" = "slateblue4"),
    name = "Reproductive Water Stress",
    guide = guide_legend(title.position = "top", nrow = 1, order = 2,
                         override.aes = list(size = 7, shape = 21,
                                             fill = NA, stroke = 1.4))
  ) +
  ggnewscale::new_scale_color() +
  # --- inner point: shape = DOY, color = brix (custom inferno, bold yellow), fixed size ---
  geom_point(aes(shape = factor(transplantingDOY), color = brix_mean),
             size = 3.5) +
  scale_color_gradientn(
    colors = c("#f7d000", "#f98c0a", "#bc3754", "#57106e", "#000004"),
    name = LBL_BRIX,
    guide = "none"
  ) +
  scale_shape_manual(
    values = c("110" = 15, "115" = 16, "120" = 17, "125" = 18),
    name   = "Transplanting Day of Year",
    guide  = guide_legend(title.position = "top", nrow = 1, order = 1,
                          override.aes = list(size = 3, color = "grey25"))
  ) +
  labs(tag = "C", x = LBL_YIELD, y = LBL_IRR,
       caption = sprintf("Non-dominated (n = %d)", nrow(pareto_df))) +
  theme_paper() +
  theme(
    legend.position   = "top",
    legend.box        = "horizontal",
    legend.box.just   = "left",
    legend.spacing.y  = unit(0.05, "cm"),
    legend.text       = element_text(size = 10),
    legend.title      = element_text(size = 11, face = "bold"),
    legend.key.size   = unit(0.38, "cm"),
    legend.margin     = margin(1, 1, 1, 1),
    plot.margin       = margin(4, 4, 4, 4)
  )

# Brix colorbar as separate grob, inset into the plot (top-left)
legend_brix_plot <- ggplot(pareto_df, aes(x = yield_mean, y = irrigation_mean, color = brix_mean)) +
  geom_point() +
  scale_color_gradientn(
    colors = c("#f7d000", "#f98c0a", "#bc3754", "#57106e", "#000004"),
    name = LBL_BRIX,
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0,
      barwidth = unit(3.2, "cm"), barheight = unit(0.28, "cm")
    )
  ) +
  theme_paper() +
  theme(
    legend.position       = "top",
    legend.justification  = "left",
    legend.title          = element_text(size = 11, face = "bold"),
    legend.text           = element_text(size = 11),
    legend.background     = element_rect(fill = alpha("white", 0.85),
                                         color = "grey80", linewidth = 0.2),
    legend.margin         = margin(2, 4, 2, 4)
  )
legend_brix <- cowplot::get_legend(legend_brix_plot)
panel_C <- panel_C +
  patchwork::inset_element(legend_brix,
                           left = 0.05, right = 0.32,
                           bottom = 0.84, top = 1.0,
                           align_to = "panel")
panel_C
# =============================================================================
# TABLE — LMM coefficients, variables in columns, terms in rows
# CSV + ggplot2 PNG
# =============================================================================
extract_coefs <- function(mod, resp) {
  tidy(mod, effects = "fixed", conf.int = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(
      response  = resp,
      phase     = case_when(
        str_detect(term, "vegetativeWS")   ~ "Vegetative",
        str_detect(term, "reproductiveWS") ~ "Reproductive",
        str_detect(term, "ripeningWS")     ~ "Ripening",
        str_detect(term, "transplanting")  ~ "Transplanting DOY"
      ),
      level_num = as.numeric(str_extract(term, "[0-9.]+$")),
      term_label = case_when(
        str_detect(term, "vegetativeWS")   ~ paste0("Veg. WS ",  str_extract(term, "[0-9.]+$")),
        str_detect(term, "reproductiveWS") ~ paste0("Rep. WS ",  str_extract(term, "[0-9.]+$")),
        str_detect(term, "ripeningWS")     ~ paste0("Rip. WS ",  str_extract(term, "[0-9.]+$")),
        str_detect(term, "transplanting")  ~ paste0("DOY ",      str_extract(term, "[0-9]+$"))
      ),
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE            ~ "ns"
      ),
      cell = sprintf("%.2f (%.2f)%s", estimate, std.error, sig)
    )
}

coefs_all <- bind_rows(
  extract_coefs(mod_yield, LBL_YIELD),
  extract_coefs(mod_brix,  LBL_BRIX),
  extract_coefs(mod_irr,   LBL_IRR),
  extract_coefs(mod_iwue,  LBL_IWUE)
)

tab_wide <- coefs_all |>
  select(phase, term_label, level_num, response, cell) |>
  pivot_wider(names_from = response, values_from = cell) |>
  arrange(phase, level_num) |>
  select(phase, term_label,
         all_of(c(LBL_YIELD, LBL_BRIX, LBL_IRR, LBL_IWUE)))

write.csv(tab_wide, file.path(fig_dir, "tab_lmm_coefs.csv"),
          row.names = FALSE)
message("Saved: ", file.path(fig_dir, "tab_lmm_coefs.csv"))

tab_plot_data <- tab_wide |>
  mutate(row_id = row_number()) |>
  pivot_longer(cols = all_of(c(LBL_YIELD, LBL_BRIX, LBL_IRR, LBL_IWUE)),
               names_to = "response", values_to = "cell") |>
  mutate(
    response = factor(response,
                      levels = c(LBL_YIELD, LBL_BRIX, LBL_IRR, LBL_IWUE)),
    col_id = as.integer(response),
    is_sig = !str_ends(cell, "ns"),
    txt_color = if_else(is_sig, "#1a1a1a", "grey55")
  )

n_rows <- max(tab_plot_data$row_id)
n_cols <- 4L
stripe  <- "#F3F2EE"

x_term <- 0.01
x_vals <- c(0.32, 0.50, 0.68, 0.86)
col_headers <- c(LBL_YIELD, LBL_BRIX, LBL_IRR, LBL_IWUE)

phase_rows <- tab_wide |>
  mutate(row_id = row_number()) |>
  group_by(phase) |>
  summarise(first_row = min(row_id), .groups = "drop")

fig_table <- ggplot() +
  geom_rect(data = filter(tab_wide, row_number() %% 2 == 0) |>
              mutate(row_id = row_number()),
            aes(xmin = 0, xmax = 1,
                ymin = row_id - 0.5, ymax = row_id + 0.5),
            fill = stripe, color = NA) +
  annotate("rect", xmin = 0, xmax = 1,
           ymin  = n_rows + 0.52, ymax = n_rows + 1.55,
           fill = "grey18", color = NA) +
  annotate("text",
           x     = c(x_term + 0.01, x_vals),
           y     = n_rows + 1.03,
           label = c("Term", col_headers),
           hjust = c(0, 0.5, 0.5, 0.5, 0.5),
           fontface = "bold", size = 3.0, color = "white") +
  geom_rect(data = phase_rows,
            aes(xmin = 0, xmax = 1,
                ymin = first_row - 0.5 - 0.48,
                ymax = first_row - 0.5),
            fill = "grey88", color = NA) +
  geom_text(data = phase_rows,
            aes(x = x_term, y = first_row - 0.5 - 0.24,
                label = phase),
            hjust = 0, fontface = "bold.italic",
            size = 2.9, color = "grey20") +
  geom_text(data = tab_wide |> mutate(row_id = row_number()),
            aes(x = x_term, y = row_id, label = term_label),
            hjust = 0, size = 2.75, color = "grey15") +
  geom_text(data = filter(tab_plot_data, col_id == 1),
            aes(x = x_vals[1], y = row_id, label = cell, color = txt_color),
            hjust = 0.5, size = 2.65) +
  geom_text(data = filter(tab_plot_data, col_id == 2),
            aes(x = x_vals[2], y = row_id, label = cell, color = txt_color),
            hjust = 0.5, size = 2.65) +
  geom_text(data = filter(tab_plot_data, col_id == 3),
            aes(x = x_vals[3], y = row_id, label = cell, color = txt_color),
            hjust = 0.5, size = 2.65) +
  geom_text(data = filter(tab_plot_data, col_id == 4),
            aes(x = x_vals[4], y = row_id, label = cell, color = txt_color),
            hjust = 0.5, size = 2.65) +
  scale_color_identity() +
  annotate("segment",
           x = c(0.29, 0.47, 0.65, 0.83), xend = c(0.29, 0.47, 0.65, 0.83),
           y = 0.5, yend = n_rows + 1.55,
           color = "grey75", linewidth = 0.3) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = n_rows + 1.55,
           fill = NA, color = "grey40", linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.45, n_rows + 1.6), expand = c(0, 0)) +
  labs(
    title    = "LMM fixed-effect coefficients",
    subtitle = "Estimate (SE)  \u2014  ns p\u202f\u2265\u202f0.05 \u00b7 * p\u202f<\u202f0.05 \u00b7 ** p\u202f<\u202f0.01 \u00b7 *** p\u202f<\u202f0.001\nReference level: lowest WSI threshold \u00b7 Random intercept: year"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(size = 12, face = "bold",
                                 margin = margin(b = 3, l = 4)),
    plot.subtitle = element_text(size = 8.5, color = "grey40",
                                 margin = margin(b = 6, l = 4)),
    plot.margin   = margin(10, 8, 8, 8)
  )

# =============================================================================
# COMPOSITE  [ Panel A — full width ]
#             [ Panel B (left) | Panel C (right) ]
# Tags A / B / C are set per-sub-plot via labs(tag=...) on sA, h_yield, panel_C
# =============================================================================
fig_composite <- (panel_A) /
  (panel_B | panel_C) +
  plot_layout(heights = c(0.55, 1)) +   # ridotto da 0.7 a 0.55
  plot_annotation(theme = theme_paper())

# =============================================================================
# Export
# =============================================================================
save_png(fig_composite, "figOPT_ABC_composite.png", 33, 28)
save_png(fig_table,     "figOPT_TABLE_lmm.png",     22, 18)

message("\nDone — figures in: ", normalizePath(fig_dir))


# -----------------------------------------------------------------------------
# 7b. DIAGNOSTICS — structured summary for results-section writing
# -----------------------------------------------------------------------------
diag_dir <- file.path(fig_dir, "diagnostics")
dir.create(diag_dir, showWarnings = FALSE)

diag_lines <- character(0)
add_line <- function(...) diag_lines <<- c(diag_lines, sprintf(...))

add_line("=============================================================")
add_line("CUMBA IRRIGATION OPTIMISATION — DIAGNOSTIC SUMMARY")
add_line("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M"))
add_line("=============================================================")

# --- 1. Design & Pareto size --------------------------------------------
add_line("")
add_line("--- 1. EXPERIMENTAL DESIGN & PARETO FRONT -------------------")
add_line("Total strategy combinations tested : %d", nrow(results_agg))
add_line("Years per combination              : %d", n_distinct(results_df$year))
add_line("Total simulation-years             : %d", nrow(results_df))
add_line("Pareto-optimal (non-dominated)      : %d  (%.1f%% of combinations)",
         nrow(pareto_df), 100 * nrow(pareto_df) / nrow(results_agg))
add_line("Objectives in Pareto selection      : high(yield), high(brix), low(irrigation), high(iwue), low(n_irrigation)")

# --- 2. Range comparison: full space vs Pareto front ---------------------
add_line("")
add_line("--- 2. RANGE: FULL DESIGN SPACE vs PARETO FRONT --------------")
range_compare <- function(var, label) {
  full_r   <- range(results_agg[[var]], na.rm = TRUE)
  pareto_r <- range(pareto_df[[var]],    na.rm = TRUE)
  add_line("%-14s | full: %7.2f to %7.2f | pareto: %7.2f to %7.2f",
           label, full_r[1], full_r[2], pareto_r[1], pareto_r[2])
}
range_compare("yield_mean",      "Yield")
range_compare("brix_mean",       "Brix")
range_compare("irrigation_mean", "Irrigation")
range_compare("iwue_mean",       "IWUE")
range_compare("n_irrigation_mean","N.irrigations")

# --- 3. Single-objective extremes (best strategy per objective) ----------
add_line("")
add_line("--- 3. BEST STRATEGY PER SINGLE OBJECTIVE (within Pareto front) ---")
describe_strategy <- function(row) {
  sprintf("vegWS=%.1f repWS=%.1f ripWS=%.1f DOY=%d -> yield=%.2f brix=%.2f irr=%.0f iwue=%.3f n_irr=%.1f",
          row$vegetativeWS, row$reproductiveWS, row$ripeningWS, row$transplantingDOY,
          row$yield_mean, row$brix_mean, row$irrigation_mean, row$iwue_mean, row$n_irrigation_mean)
}
best_yield <- pareto_df |> slice_max(yield_mean, n = 1, with_ties = FALSE)
best_brix  <- pareto_df |> slice_max(brix_mean,  n = 1, with_ties = FALSE)
best_irr   <- pareto_df |> slice_min(irrigation_mean, n = 1, with_ties = FALSE)
best_iwue  <- pareto_df |> slice_max(iwue_mean,  n = 1, with_ties = FALSE)
best_nirr  <- pareto_df |> slice_min(n_irrigation_mean, n = 1, with_ties = FALSE)

add_line("Max yield     : %s", describe_strategy(best_yield))
add_line("Max brix      : %s", describe_strategy(best_brix))
add_line("Min irrigation: %s", describe_strategy(best_irr))
add_line("Max iwue      : %s", describe_strategy(best_iwue))
add_line("Min n.irr     : %s", describe_strategy(best_nirr))

# --- 4. Compromise solution (closest to normalised ideal point) ----------
add_line("")
add_line("--- 4. COMPROMISE SOLUTION (closest to ideal point, normalised) ---")
norm01 <- function(x, dir = 1) {
  r <- range(x, na.rm = TRUE)
  if (diff(r) == 0) return(rep(0.5, length(x)))
  z <- (x - r[1]) / diff(r)
  if (dir == -1) z <- 1 - z
  z
}
pareto_scored <- pareto_df |>
  mutate(
    z_yield = norm01(yield_mean,  1),
    z_brix  = norm01(brix_mean,   1),
    z_irr   = norm01(irrigation_mean, -1),
    z_iwue  = norm01(iwue_mean,   1),
    z_nirr  = norm01(n_irrigation_mean, -1),
    dist_to_ideal = sqrt((1 - z_yield)^2 + (1 - z_brix)^2 +
                           (1 - z_irr)^2  + (1 - z_iwue)^2 + (1 - z_nirr)^2)
  ) |>
  arrange(dist_to_ideal)

compromise <- pareto_scored |> slice_head(n = 1)
add_line("Compromise (min Euclidean dist to ideal, equal weights):")
add_line("  %s", describe_strategy(compromise))
add_line("  normalised scores -> yield=%.2f brix=%.2f irr=%.2f iwue=%.2f n_irr=%.2f (1=best)",
         compromise$z_yield, compromise$z_brix, compromise$z_irr,
         compromise$z_iwue, compromise$z_nirr)

# --- 5. Pareto front composition by phase (which WSI thresholds dominate) ---
add_line("")
add_line("--- 5. PARETO FRONT COMPOSITION BY PHASE (n strategies per WSI level) ---")
for (ph in c("vegetativeWS", "reproductiveWS", "ripeningWS")) {
  tab <- table(pareto_df[[ph]])
  add_line("%-16s : %s", ph,
           paste(sprintf("WSI=%s (n=%d)", names(tab), tab), collapse = "  "))
}
tab_doy <- table(pareto_df$transplantingDOY)
add_line("%-16s : %s", "transplantingDOY",
         paste(sprintf("DOY=%s (n=%d)", names(tab_doy), tab_doy), collapse = "  "))

# --- 6. LMM fixed-effects: significant terms, sign, magnitude ------------
add_line("")
add_line("--- 6. LMM SIGNIFICANT FIXED EFFECTS (p < 0.05) ----------------")
summarise_lmm <- function(mod, label) {
  tt <- tidy(mod, effects = "fixed") |> filter(term != "(Intercept)")
  sig <- tt |> filter(p.value < 0.05) |>
    mutate(dir = if_else(estimate > 0, "+", "-"))
  add_line("[%s]", label)
  if (nrow(sig) == 0) {
    add_line("  No significant fixed effects at p<0.05")
  } else {
    for (k in seq_len(nrow(sig))) {
      add_line("  %s %s  estimate=%.3f  SE=%.3f  p=%.4f",
               sig$dir[k], sig$term[k], sig$estimate[k], sig$std.error[k], sig$p.value[k])
    }
  }
}
summarise_lmm(mod_yield, "Yield")
summarise_lmm(mod_brix,  "Brix")
summarise_lmm(mod_irr,   "Irrigation")
summarise_lmm(mod_iwue,  "IWUE")

# --- 7. Variance explained (R2m / R2c) if MuMIn available ----------------
add_line("")
add_line("--- 7. VARIANCE EXPLAINED (marginal / conditional R2) ----------")
if (requireNamespace("MuMIn", quietly = TRUE)) {
  r2_report <- function(mod, label) {
    r2 <- MuMIn::r.squaredGLMM(mod)
    add_line("%-12s : R2m = %.3f   R2c = %.3f", label, r2[1, "R2m"], r2[1, "R2c"])
  }
  r2_report(mod_yield, "Yield")
  r2_report(mod_brix,  "Brix")
  r2_report(mod_irr,   "Irrigation")
  r2_report(mod_iwue,  "IWUE")
} else {
  add_line("MuMIn not installed — skipped (install.packages('MuMIn') to enable)")
}

# --- 8. Random effect (year) variance components --------------------------
add_line("")
add_line("--- 8. RANDOM EFFECT (year) VARIANCE COMPONENTS ----------------")
report_varcomp <- function(mod, label) {
  vc <- as.data.frame(VarCorr(mod))
  yr_var  <- vc$vcov[vc$grp == "year"]
  res_var <- vc$vcov[vc$grp == "Residual"]
  icc <- yr_var / (yr_var + res_var)
  add_line("%-12s : year_var=%.4f  residual_var=%.4f  ICC=%.3f",
           label, yr_var, res_var, icc)
}
report_varcomp(mod_yield, "Yield")
report_varcomp(mod_brix,  "Brix")
report_varcomp(mod_irr,   "Irrigation")
report_varcomp(mod_iwue,  "IWUE")

add_line("")
add_line("=============================================================")
add_line("END OF DIAGNOSTIC SUMMARY")
add_line("=============================================================")

cat(paste(diag_lines, collapse = "\n"), "\n")
writeLines(diag_lines, file.path(diag_dir, "diagnostic_summary.txt"))
message("Saved: ", file.path(diag_dir, "diagnostic_summary.txt"))

diag_strategies <- bind_rows(
  best_yield |> mutate(criterion = "max_yield"),
  best_brix  |> mutate(criterion = "max_brix"),
  best_irr   |> mutate(criterion = "min_irrigation"),
  best_iwue  |> mutate(criterion = "max_iwue"),
  best_nirr  |> mutate(criterion = "min_n_irrigation"),
  compromise |> select(-starts_with("z_"), -dist_to_ideal) |> mutate(criterion = "compromise")
)
write.csv(diag_strategies, file.path(diag_dir, "diag_key_strategies.csv"), row.names = FALSE)
message("Saved: ", file.path(diag_dir, "diag_key_strategies.csv"))