# Fig 6A - LFI
pas = 62

LFI_current = read.csv(here("data-mpa-CC-paper/model_outputs/LFI_with_sd_among_reps_NoCC_GSA-prop_11-07-2023.csv")) 

LFI_future = read.csv(here("data-mpa-CC-paper/model_outputs/LFI_with_sd_among_reps_CC_2100_GSA-prop_11-07-2023.csv"))

LFI_current_lit = LFI_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
  group_by(mpa_coverage, mpa, scenario) %>%
  summarise(mean_LFI_reps = mean(mean_LFI_60_years))

LFI_baseline_nompa = unique(LFI_current_lit$mean_LFI_reps[LFI_current_lit$mpa_coverage == 0])


LFI_future_Med = LFI_future %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  filter(mpa_coverage %in% c(0,10)) %>%
  rename(mpa_scenario = scenario) %>%
  group_by(mpa_coverage, mpa_scenario) %>%
  summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
  mutate(LFI_baseline = LFI_baseline_nompa) %>%
  mutate(LFI_change = (mean_LFI_reps - LFI_baseline)/LFI_baseline * 100) %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "S3-Random EEZ",
                                        grepl("Random", mpa_scenario) ~ "S2-Random basin",
                                        mpa_scenario %in% c("Existing network") ~ "S1-Current",
                                        mpa_scenario %in% c("Micheli") ~ "S4-Micheli",
                                        mpa_scenario %in% c("Mazor scenario 8") ~ "S5-Mazor GFCM",
                                        mpa_scenario %in% c("Mazor scenario 9") ~ "S6-Mazor SAUP",
                                        TRUE ~ mpa_scenario)) %>%

  group_by(mpa_coverage, mpa_scenario_group) %>%
  summarise(LFI_change_mean = mean(LFI_change, na.rm = T),
            LFI_change_sd = sd(LFI_change, na.rm = T)) %>%
  ungroup() %>%
  mutate(mpa_coverage = case_when(mpa_coverage == 0 ~ "0%",
                                  TRUE ~ "10%")) %>%
  mutate(mpa_scenario_group = case_when(mpa_coverage == "0%" ~ NA,
                                        TRUE ~ mpa_scenario_group)) %>%
  distinct(LFI_change_mean, .keep_all = T) 

# PLOT barplot
mpa_colors <- c("S1-Current" = "gold", 
                "S2-Random basin" = "gray60", 
                "S3-Random EEZ" = "black",
                "S4-Micheli" = "darkorange", 
                "S5-Mazor GFCM" = "darkslateblue", 
                "S6-Mazor SAUP" = "darkorchid")

LFI_future_Med <- LFI_future_Med %>%
  mutate(xpos = if_else(mpa_coverage == "0%", 1, 1.6))

# ---- Settings shared by BOTH plots (run once, before both plots) ----
angle   <- 35      # visual angle in degrees
gap_fix <- 0.038   # vertical distance between stripes in data units (same for ALL bars)
bar_w   <- 0.25

panel_w <- 740
panel_h <- 805
x_span  <- 1.17
y_span  <- 1.26

make_hatch <- function(bar0, value_col = "TLcatch_change_mean") {
  x0 <- bar0$xpos - bar_w / 2
  x1 <- bar0$xpos + bar_w / 2
  ymin_bar <- min(0, bar0[[value_col]])
  ymax_bar <- max(0, bar0[[value_col]])
  gap <- gap_fix                                   # fixed, not dependent on bar height
  
  # slope from the visual angle, snapped to a whole number of gaps across the bar width
  slope_target <- tan(angle * pi / 180) * (y_span / x_span) * (panel_w / panel_h)
  m     <- max(1, round(slope_target * (x1 - x0) / gap))
  slope <- m * gap / (x1 - x0)
  
  # stripes anchored at the top edge, running down past the bottom-left corner
  n_down <- ceiling((ymax_bar - ymin_bar) / gap) + m
  cc_seq <- ymax_bar - gap * (0:n_down)
  
  eps <- 1e-9
  bind_rows(lapply(cc_seq, function(cc) {
    xa <- max(x0, x0 + (ymin_bar - cc) / slope)
    xb <- min(x1, x0 + (ymax_bar - cc) / slope)
    if (xb - xa < eps) return(NULL)
    data.frame(x = xa, xend = xb,
               y    = cc + slope * (xa - x0),
               yend = cc + slope * (xb - x0))
  })) %>%
    filter(is.finite(x), is.finite(xend), is.finite(y), is.finite(yend),
           (xend - x) > 1e-6) %>%
    mutate(y    = pmin(pmax(y,    ymin_bar), ymax_bar),
           yend = pmin(pmax(yend, ymin_bar), ymax_bar))
}
# ---- Legend key ----
draw_key_hatch <- function(data, params, size) {
  rise <- tan(angle * pi / 180)
  offs <- seq(-1, 1, by = 0.2)
  grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "grey95", col = "grey30")),
    grid::gTree(
      children = grid::gList(
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
          y0 = grid::unit(offs, "npc"), y1 = grid::unit(offs + rise, "npc"),
          gp = grid::gpar(col = "grey30", lwd = 1)
        )
      ),
      vp = grid::viewport(clip = "on")
    )
  )
}
# ---- Same hatch as Fig 8C, rescaled to any y-axis range ----
# Fig 8C reference: gap = 0.038 on a y-range of 1.15 (+5% expansion each side)
gap_frac <- 0.038 / (1.15 * 1.1)   # stripe spacing as a fraction of the visible y-axis (~0.030)

make_hatch_ref <- function(bar0, value_col, ylim_range) {
  y_span_new <- diff(ylim_range) * 1.1          # visible axis range incl. default 5% expansion
  gap        <- gap_frac * y_span_new           # same visual spacing as Fig 8C
  
  x0 <- bar0$xpos - bar_w / 2
  x1 <- bar0$xpos + bar_w / 2
  ymin_bar <- min(0, bar0[[value_col]])
  ymax_bar <- max(0, bar0[[value_col]])
  
  # same visual angle as Fig 8C, converted with the new y-span
  slope_target <- tan(angle * pi / 180) * (y_span_new / x_span) * (panel_w / panel_h)
  m     <- max(1, round(slope_target * (x1 - x0) / gap))
  slope <- m * gap / (x1 - x0)
  
  n_down <- ceiling((ymax_bar - ymin_bar) / gap) + m
  cc_seq <- ymax_bar - gap * (0:n_down)
  
  bind_rows(lapply(cc_seq, function(cc) {
    xa <- max(x0, x0 + (ymin_bar - cc) / slope)
    xb <- min(x1, x0 + (ymax_bar - cc) / slope)
    if (xb - xa < 1e-9) return(NULL)
    data.frame(x = xa, xend = xb,
               y    = cc + slope * (xa - x0),
               yend = cc + slope * (xb - x0))
  })) %>%
    filter(is.finite(x), is.finite(xend), is.finite(y), is.finite(yend),
           (xend - x) > 1e-6) %>%
    mutate(y    = pmin(pmax(y,    ymin_bar), ymax_bar),
           yend = pmin(pmax(yend, ymin_bar), ymax_bar))
}
# Make plot -----------------------------
bar0  <- filter(LFI_future_Med, mpa_coverage == "0%")
hatch <- make_hatch_ref(bar0, value_col = "LFI_change_mean", ylim_range = c(-7, 0))


LFI_Med <- ggplot() +
  # 0% bar: light fill + outline, own legend entry via linetype
  geom_col(
    data = bar0,
    aes(x = xpos, y = LFI_change_mean, linetype = "No MPA, RCP8.5"),
    fill = "grey95",
    colour = "grey30",
    linewidth = 0.4,
    width = bar_w,
    key_glyph = draw_key_hatch
  ) +
  # stripes on top of the 0% bar
  geom_segment(
    data = hatch,
    aes(x = x, xend = xend, y = y, yend = yend),
    colour = "grey30",
    linewidth = 0.4,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  # 10% bars: unchanged
  geom_col(
    data = filter(LFI_future_Med, mpa_coverage == "10%"),
    aes(x = xpos, y = LFI_change_mean, fill = mpa_scenario_group),
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_errorbar(
    data = LFI_future_Med %>%
      filter(mpa_coverage == "10%") %>%
      mutate(
        ymin = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                       LFI_change_mean - LFI_change_sd, NA_real_),
        ymax = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                       LFI_change_mean + LFI_change_sd, NA_real_)
      ),
    aes(x = xpos, ymin = ymin, ymax = ymax, group = mpa_scenario_group),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  scale_x_continuous(breaks = c(1, 1.6), labels = c("0%", "10%")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = mpa_colors, name = "MPA scenarios") +
  scale_linetype_manual(values = c("No MPA, RCP8.5" = "solid"), name = NULL) +
  guides(linetype = guide_legend(order = 1),   # No MPA on top
         fill     = guide_legend(order = 2)) + # your scenario legend below
  theme_bw() +
  theme(legend.box = "vertical",
        legend.box.just = "left") +
  labs(y = "Change in LFI (%)",
       x = "MPA coverage",
       title = "Large Fish Indicator") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0)) +
  ylim(-7, 0)


print(LFI_Med)
ggsave(here("figures/after_revision1/FINAL/LFI_barplot_Med_hashed.png"), dpi = 600, height = 6, width = 6)
