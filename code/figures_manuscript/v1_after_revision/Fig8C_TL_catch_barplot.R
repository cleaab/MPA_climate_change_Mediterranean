library(grid)

# Trophic level of landings weighted by the proportion of catch of each species - HC_C species ------------
# All species (for manuscript text) -----
fig_name = "All_species"
species_classif = read.csv(here("data-mpa-CC-paper/classification_species_commercial_class.csv")) %>%
  filter(! species_id %in% c(0,1,2))

species_name_selected = species_classif$species_name
species_id_selected = species_classif$species_id

TL_landings_current = read.csv(here("data-mpa-CC-paper/model_outputs/TL_landings/TL_landings_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_current = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_yield_by_species_NoCC_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_current = Yield_prop_current %>%
  left_join(TL_landings_current, by = c("species_name", "mpa_coverage", "scenario"))

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T))

TL_landings_baseline_NoMPA = unique(TL_landings_current_lit$TL_landings_all[TL_landings_current_lit$mpa_coverage == 0])

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(climate_scenario = "2006-2013")


TL_landings_current_Random = TL_landings_weighted_current %>%
  filter(grepl('Random', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2006-2013")


TL_landings_current_EEZ = TL_landings_weighted_current %>%
  filter(grepl('EEZ', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2006-2013")

# Future scenario ----

TL_landings_future = read.csv(here("data-mpa-CC-paper/model_outputs/TL_landings/TL_landings_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_future = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_yield_by_species_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_future = Yield_prop_future %>%
  left_join(TL_landings_future, by = c("species_name", "mpa_coverage", "scenario"))


TLcatch_Med = TL_landings_weighted_future %>%
  filter(mpa_coverage %in% c(0,10)) %>%
  rename(mpa_scenario = scenario) %>%
  group_by(mpa_coverage, mpa_scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "S3-Random EEZ",
                                        grepl("Random", mpa_scenario) ~ "S2-Random basin",
                                        mpa_scenario %in% c("Existing network") ~ "S1-Current",
                                        mpa_scenario %in% c("Micheli") ~ "S4-Micheli",
                                        mpa_scenario %in% c("Mazor scenario 8") ~ "S5-Mazor GFCM",
                                        mpa_scenario %in% c("Mazor scenario 9") ~ "S6-Mazor SAUP",
                                        TRUE ~ mpa_scenario)) %>%
  group_by(mpa_coverage, mpa_scenario_group) %>%
  summarise(TLcatch_change_mean = mean(TL_change, na.rm = T),
            TLcatch_change_sd = sd(TL_change, na.rm = T)) %>%
  ungroup() %>%
  mutate(mpa_coverage = case_when(mpa_coverage == 0 ~ "0%",
                                  TRUE ~ "10%")) %>%
  mutate(mpa_scenario_group = case_when(mpa_coverage == "0%" ~ NA,
                                        TRUE ~ mpa_scenario_group)) %>%
  distinct(TLcatch_change_mean, .keep_all = T) 

# PLOT barplot
mpa_colors <- c("S1-Current" = "gold", 
                "S2-Random basin" = "gray60", 
                "S3-Random EEZ" = "black",
                "S4-Micheli" = "darkorange", 
                "S5-Mazor GFCM" = "darkslateblue", 
                "S6-Mazor SAUP" = "darkorchid")

TLcatch_Med <- TLcatch_Med %>%
  mutate(xpos = if_else(mpa_coverage == "0%", 1, 1.6))

TL_catch_Med <- ggplot() +
  geom_col(
    data = filter(TLcatch_Med, mpa_coverage == "0%"),
    aes(x = xpos, y = TLcatch_change_mean),
    fill = "grey80",
    width = 0.25
  ) +
  geom_col(
    data = filter(TLcatch_Med, mpa_coverage == "10%"),
    aes(x = xpos, y = TLcatch_change_mean, fill = mpa_scenario_group),
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_errorbar(
    data = TLcatch_Med %>%
      filter(mpa_coverage == "10%") %>%
      mutate(
        ymin = if_else(
          mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
          TLcatch_change_mean - TLcatch_change_sd,
          NA_real_
        ),
        ymax = if_else(
          mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
          TLcatch_change_mean + TLcatch_change_sd,
          NA_real_
        )
      ),
    aes(
      x = xpos,
      ymin = ymin,
      ymax = ymax,
      group = mpa_scenario_group
    ),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  scale_x_continuous(
    breaks = c(1, 1.6),
    labels = c("0%", "10%")
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_fill_manual(name = "MPA scenario",
                    values = mpa_colors) +
  theme_bw() +
  labs(y = "Change in TL (%)",
       x = "MPA coverage",
       title = "Trophic level of catches") 

print(TL_catch_Med)

# Commercial species ------
fig_name = "Species of high commercial interest"
species_classif = read.csv(here("data-mpa-CC-paper/classification_species_commercial_class.csv")) %>%
  filter(commercial_class %in% c("HC", "C")) %>%
  filter(! species_id %in% c(0,1,2))

species_name_selected = species_classif$species_name
species_id_selected = species_classif$species_id

TL_landings_current = read.csv(here("data-mpa-CC-paper/model_outputs/TL_landings/TL_landings_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_current = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_yield_by_species_NoCC_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_current = Yield_prop_current %>%
  left_join(TL_landings_current, by = c("species_name", "mpa_coverage", "scenario"))

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T))

TL_landings_baseline_NoMPA = unique(TL_landings_current_lit$TL_landings_all[TL_landings_current_lit$mpa_coverage == 0])

# Future scenario ----

TL_landings_future = read.csv(here("data-mpa-CC-paper/model_outputs/TL_landings/TL_landings_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_future = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_yield_by_species_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_future = Yield_prop_future %>%
  left_join(TL_landings_future, by = c("species_name", "mpa_coverage", "scenario"))

TLcatch_HC_Med = TL_landings_weighted_future %>%
  filter(mpa_coverage %in% c(0,10)) %>%
  rename(mpa_scenario = scenario) %>%
  group_by(mpa_coverage, mpa_scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "S3-Random EEZ",
                                        grepl("Random", mpa_scenario) ~ "S2-Random basin",
                                        mpa_scenario %in% c("Existing network") ~ "S1-Current",
                                        mpa_scenario %in% c("Micheli") ~ "S4-Micheli",
                                        mpa_scenario %in% c("Mazor scenario 8") ~ "S5-Mazor GFCM",
                                        mpa_scenario %in% c("Mazor scenario 9") ~ "S6-Mazor SAUP",
                                        TRUE ~ mpa_scenario)) %>%
  group_by(mpa_coverage, mpa_scenario_group) %>%
  summarise(TLcatch_change_mean = mean(TL_change, na.rm = T),
            TLcatch_change_sd = sd(TL_change, na.rm = T)) %>%
  ungroup() %>%
  mutate(mpa_coverage = case_when(mpa_coverage == 0 ~ "0%",
                                  TRUE ~ "10%")) %>%
  mutate(mpa_scenario_group = case_when(mpa_coverage == "0%" ~ NA,
                                        TRUE ~ mpa_scenario_group)) %>%
  distinct(TLcatch_change_mean, .keep_all = T) 

# PLOT barplot
TLcatch_HC_Med <- TLcatch_HC_Med %>%
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

# ---- Plot 1 (run right before building plot 1) ----
bar0  <- filter(TLcatch_HC_Med, mpa_coverage == "0%")
hatch <- make_hatch(bar0)


TL_catch_HC_Med <- ggplot() +
  # 0% bar: light fill + outline, own legend entry via linetype
  geom_col(
    data = bar0,
    aes(x = xpos, y = TLcatch_change_mean, linetype = "No MPA, RCP8.5"),
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
    data = filter(TLcatch_HC_Med, mpa_coverage == "10%"),
    aes(x = xpos, y = TLcatch_change_mean, fill = mpa_scenario_group),
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_errorbar(
    data = TLcatch_HC_Med %>%
      filter(mpa_coverage == "10%") %>%
      mutate(
        ymin = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                       TLcatch_change_mean - TLcatch_change_sd, NA_real_),
        ymax = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                       TLcatch_change_mean + TLcatch_change_sd, NA_real_)
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
  labs(y = "Change in TL (%)",
       x = "MPA coverage",
       title = "C) TROPHIC LEVEL of CATCHES",
       subtitle = "Species of high commercial interest") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0)) +
  
  ylim(c(-1.15, 0))


# Low commercial species ---------------------
fig_name = "Species of minor commercial interest"
species_classif = read.csv(here("data-mpa-CC-paper/classification_species_commercial_class.csv")) %>%
  filter(commercial_class %in% c("LC", "NC")) %>%
  filter(! species_id %in% c(0,1,2))

species_name_selected = species_classif$species_name
species_id_selected = species_classif$species_id

TL_landings_current = read.csv(here("data/model_outputs/TL_landings/TL_landings_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_current = read.csv(here("data/model_outputs/Biomass_yield_by_species_NoCC_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_current = Yield_prop_current %>%
  left_join(TL_landings_current, by = c("species_name", "mpa_coverage", "scenario"))

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T))

TL_landings_baseline_NoMPA = unique(TL_landings_current_lit$TL_landings_all[TL_landings_current_lit$mpa_coverage == 0])

# Future scenario ----

TL_landings_future = read.csv(here("data/model_outputs/TL_landings/TL_landings_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_future = read.csv(here("data/model_outputs/Biomass_yield_by_species_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_future = Yield_prop_future %>%
  left_join(TL_landings_future, by = c("species_name", "mpa_coverage", "scenario"))

TLcatch_LC_Med = TL_landings_weighted_future %>%
  filter(mpa_coverage %in% c(0,10)) %>%
  rename(mpa_scenario = scenario) %>%
  group_by(mpa_coverage, mpa_scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "S3-Random EEZ",
                                        grepl("Random", mpa_scenario) ~ "S2-Random basin",
                                        mpa_scenario %in% c("Existing network") ~ "S1-Current",
                                        mpa_scenario %in% c("Micheli") ~ "S4-Micheli",
                                        mpa_scenario %in% c("Mazor scenario 8") ~ "S5-Mazor GFCM",
                                        mpa_scenario %in% c("Mazor scenario 9") ~ "S6-Mazor SAUP",
                                        TRUE ~ mpa_scenario)) %>%
  group_by(mpa_coverage, mpa_scenario_group) %>%
  summarise(TLcatch_change_mean = mean(TL_change, na.rm = T),
            TLcatch_change_sd = sd(TL_change, na.rm = T)) %>%
  ungroup() %>%
  mutate(mpa_coverage = case_when(mpa_coverage == 0 ~ "0%",
                                  TRUE ~ "10%")) %>%
  mutate(mpa_scenario_group = case_when(mpa_coverage == "0%" ~ NA,
                                        TRUE ~ mpa_scenario_group)) %>%
  distinct(TLcatch_change_mean, .keep_all = T) 

# PLOT barplot
TLcatch_LC_Med <- TLcatch_LC_Med %>%
  mutate(xpos = if_else(mpa_coverage == "0%", 1, 1.6))

# ---- Hatch lines for the 0% bar (visual angle, scaled to the axis) ----
# ---- Plot 2 (run right before building plot 2) ----
bar0  <- filter(TLcatch_LC_Med, mpa_coverage == "0%")
hatch <- make_hatch(bar0)

TL_catch_LC_Med <- ggplot() +
  # 0% bar: light fill + outline, own legend entry via linetype
  geom_col(
    data = bar0,
    aes(x = xpos, y = TLcatch_change_mean, linetype = "No MPA, RCP8.5"),
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
    data = filter(TLcatch_LC_Med, mpa_coverage == "10%"),
    aes(x = xpos, y = TLcatch_change_mean, fill = mpa_scenario_group),
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_errorbar(
    data = TLcatch_LC_Med %>%
      filter(mpa_coverage == "10%") %>%
      mutate(
        ymin = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                       TLcatch_change_mean - TLcatch_change_sd, NA_real_),
        ymax = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                       TLcatch_change_mean + TLcatch_change_sd, NA_real_)
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
  labs(y = "Change in TL (%)",
       x = "MPA coverage",
       title = "C) TROPHIC LEVEL of CATCHES",
       subtitle = "Species of minor commercial interest") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0)) +
  
  ylim(c(-1.15, 0))


# Both plots together:
library(ggpubr)
TL_catch <- ggarrange(TL_catch_HC_Med, TL_catch_LC_Med,
                      ncol=2,
                      nrow=1,
                      common.legend = T,
                      legend = "right",
                      align = "hv") 

print(TL_catch)

# Save figure
ggsave(here("figures/after_revision1/FINAL/Fig8C_TLcatch_barplot_hashed.png"), dpi = 600, height = 6, width = 10)
