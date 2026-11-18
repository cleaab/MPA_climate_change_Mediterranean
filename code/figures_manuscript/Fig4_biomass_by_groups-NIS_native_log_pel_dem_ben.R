library(tidyverse)
library(sf)
library(ncdf4)
library(here)
library(osmose)
library(ggplot2)
library(dplyr)
library(cowplot)
library(here)
library(ggpubr)
library(grid)  # for text_grob()
library(ggpattern)

# A3b. Biomass by groups - Barplots - all species
lit.scenarios.name <- c("Mazor scenario 8",
                        "Mazor scenario 9",
                        "Micheli",
                        "Existing network")

random.scenarios.name <- c("Random 11",
                           "Random 12",
                           "Random 13",
                           "Random 14",
                           "Random 15",
                           "Random 16",
                           "Random 17",
                           "Random 18",
                           "Random 19",
                           "Random 20",
                           "EEZ-conservation 1",
                           "EEZ-conservation 2",
                           "EEZ-conservation 3",
                           "EEZ-conservation 4",
                           "EEZ-conservation 5",
                           "EEZ-conservation 6",
                           "EEZ-conservation 7",
                           "EEZ-conservation 8",
                           "EEZ-conservation 9",
                           "EEZ-conservation 10")

MPA_scenarios_name <- c("S5-Mazor GFCM",
                        "S6-Mazor SAUP",
                        "S4-Micheli",
                        "S1-Current")
# BIOMASS by groups ----------------------------------------------------------

sp_group_levels <- c("Large pelagic", "Medium pelagic", "Large demersal", "Medium demersal", "Large benthic", "Medium benthic", "Small benthic", "Small pelagic", "Macroinvertebrates", "Small demersal")

legend_scenarios <- c("Existing network", "Random 11", "EEZ-conservation 1", "Micheli", "Mazor scenario 8", "Mazor scenario 9")

group_levels <- c("Existing network", "Random", "EEZ-scale conservation", "Micheli", "Mazor scenario 8", "Mazor scenario 9")

legend_labels <- c("S1-Current", "S2-Random basin", "S3-Random EEZ", "S4-Micheli", "S5-Mazor GFCM", "S6-Mazor SAUP")


####

species_classif = read.csv(here("data-mpa-CC-paper/classification_species.csv")) %>%
  #filter(! species_id %in% c(0,1,2)) %>%
  mutate(local = case_when(species_name %in% c("Etrumeusteres", #renamed Etrumeus golanii
                                                         "Sauridaundosquamis", # renamed Saurida lessepsianus
                                                         "Stephanolepisdiaspros",
                                                         "Upeneusmoluccensis") ~ "Lessepsian",
                           TRUE ~ "Native")) %>%
  dplyr::select(species_id, species_name, group_article1, size_class, vertical_position, local)


# Load spatial biomass maps RCP8.5 2070-2100
mpa.future.Dir = here("data-mpa-CC-paper/model_outputs/spatial_mpa_10/CC_2100/")

mpa_scenarios = c("Existing_network",
                  "Micheli",
                  "Mazor_scenario_8",
                  "Mazor_scenario_9",
                  "EEZ-conservation-rep1",
                  "EEZ-conservation-rep2",
                  "EEZ-conservation-rep3",
                  "EEZ-conservation-rep4",
                  "EEZ-conservation-rep5",
                  "EEZ-conservation-rep6",
                  "EEZ-conservation-rep7",
                  "EEZ-conservation-rep8",
                  "EEZ-conservation-rep9",
                  "EEZ-conservation-rep10",
                  "Random-rep11",
                  "Random-rep12",
                  "Random-rep13",
                  "Random-rep14",
                  "Random-rep15",
                  "Random-rep16",
                  "Random-rep17",
                  "Random-rep18",
                  "Random-rep19",
                  "Random-rep20")

# Mediterranean GFCM areas calculated from main script - Med_grid_GSA_EEZ_df
Med_areas = Med_grid_GSA_EEZ_df %>%
  #st_drop_geometry() %>%
  mutate(FAO_areas = case_when(GSA %in% c("GSA 1",
                                          "GSA 2",
                                          "GSA 3",
                                          "GSA 4",
                                          "GSA 5",
                                          "GSA 6",
                                          "GSA 7",
                                          "GSA 8",
                                          "GSA 9",
                                          "GSA 10",
                                          "GSA 11.1",
                                          "GSA 11.2") ~ "Western Mediterranean",
                               GSA %in% c("GSA 17",
                                          "GSA 18") ~ "Adriatic Sea",
                               GSA %in% c("GSA 12",
                                          "GSA 13",
                                          "GSA 14",
                                          "GSA 15",
                                          "GSA 16",
                                          "GSA 19",
                                          "GSA 20",
                                          "GSA 21") ~ "Central Mediterranean",
                               GSA %in% c("GSA 22",
                                          "GSA 23",
                                          "GSA 24",
                                          "GSA 25",
                                          "GSA 26",
                                          "GSA 27") ~ "Eastern Mediterranean",
                               TRUE ~ NA)) %>%
  dplyr::select(cell_ID, VALUE, GSA, ecoregions, FAO_areas) %>%
  mutate(cell_ID = as.numeric(cell_ID)) %>%
  arrange(cell_ID) 

# Store results for all scenarios
B_mpa_future_computed = TRUE
if (B_mpa_future_computed == FALSE){
  B_mpa_future_all_scens <- NULL
  
  for (mpa_scenario in mpa_scenarios) {
    
    nc <- nc_open(
      file.path(mpa.future.Dir, mpa_scenario, "Biomass_mpa_scenario.nc")
    )
    
    B_mpa_future <- ncvar_get(nc, varid = "Biomass")
    nc_close(nc)
    
    # Mean over replicates
    B_mpa_future <- apply(
      B_mpa_future,
      c(1, 2, 3),
      mean,
      na.rm = TRUE
    )
    
    # Keep species dimension
    B_mpa_future <- B_mpa_future[
      1:190,
      1:83,
      1:101
    ]
    
    # Reverse latitude/whatever your original [,83:1] was doing
    B_mpa_future <- B_mpa_future[, 83:1, , drop = FALSE]
    
    if (is.null(B_mpa_future_all_scens)) {
      
      # dimensions: x × y × species × scenario
      B_mpa_future_all_scens <- array(
        NA,
        dim = c(
          dim(B_mpa_future),
          length(mpa_scenarios)
        )
      )
    }
    
    # Add this scenario
    B_mpa_future_all_scens[, , , 
                           which(mpa_scenarios == mpa_scenario)] <- 
      B_mpa_future
  }
  
  # SAVE in ncdf4 form:
  # Dimensions
  x_dim <- ncdim_def(
    name = "x",
    units = "index",
    vals = 1:dim(B_mpa_future_all_scens)[1]
  )
  
  y_dim <- ncdim_def(
    name = "y",
    units = "index",
    vals = 1:dim(B_mpa_future_all_scens)[2]
  )
  
  species_id_list = species_classif$species_id
  species_dim <- ncdim_def(
    name = "species",
    units = "index",
    vals = species_id_list
  )
  
  scenario_dim <- ncdim_def(
    name = "scenario",
    units = "index",
    vals = 1:length(mpa_scenarios)
  )
  
  # Variable
  biomass_var <- ncvar_def(
    name = "Biomass",
    units = "tonnes",
    dim = list(x_dim, y_dim, species_dim, scenario_dim),
    missval = NA,
    prec = "double"
  )
  
  # Create file
  nc <- nc_create(
    file.path(mpa.future.Dir, "B_mpa_future_all_scens.nc"),
    biomass_var
  )
  
  # Write array
  ncvar_put(
    nc,
    biomass_var,
    B_mpa_future_all_scens
  )
  
  # Add scenario names as an attribute
  ncatt_put(
    nc,
    "scenario",
    "scenario_names",
    paste(mpa_scenarios, collapse = ",")
  )
  
  # Add species IDs
  ncatt_put(
    nc,
    "species",
    "species_ids",
    paste(species_id_list, collapse = ",")
  )
  
  nc_close(nc)
}

# if above files already computed: START FROM HERE ------------------------------

# LOAD BIOMASS MPA - CC All scens
nc = nc_open(file.path(mpa.future.Dir, "B_mpa_future_all_scens.nc"))
B_mpa_future <- ncvar_get(nc, varid = "Biomass")
nc_close(nc)

# LOAD BIOMASS NoMPA - NoCC:
nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Biomass_nompa_scenario_noCC.nc"))
B_nompa_present <- ncvar_get(nc, varid = "Biomass")
nc_close(nc)

# LOAD BIOMASS NOMPA - CC:
nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Biomass_nompa_scenario_CC.nc"))
B_nompa_future <- ncvar_get(nc, varid = "Biomass")
nc_close(nc)

# Colours for plot
mpa_colors <- c("S1-Current" = "gold", 
                "S2-Random basin" = "gray60", 
                "S3-Random EEZ" = "black",
                "S4-Micheli" = "darkorange", 
                "S5-Mazor GFCM" = "darkslateblue", 
                "S6-Mazor SAUP" = "darkorchid")

# Fig 4A - Native vs Lessepsian species ----------------------------------------

local_exotic_groups = unique(species_classif$local)
# ---- Shared hatch settings for LOG-scale bars (units: decades) ----
angle_log <- 45      # visual angle in degrees (lower = flatter, higher = steeper)
gap_log   <- 0.12    # target spacing between stripes, in decades (smaller = denser)

panel_w_log <- 640   # panel width in px
panel_h_log <- 726   # panel height in px
x_span_log  <- 1.17  # x-axis range in data units
y_span_log  <- log10(1300)  # y-axis range in decades (axis runs from 1 to 1300)

make_hatch_log <- function(bar0, value_col = "B_change_mean") {
  x0 <- bar0$xpos - bar_w / 2
  x1 <- bar0$xpos + bar_w / 2
  ymin_log <- 0                              # bar starts at 1 (= 10^0)
  ymax_log <- log10(bar0[[value_col]])       # bar top in log10 units
  H <- ymax_log - ymin_log
  
  # nudge the gap so the bar height is a whole number of gaps (bottom-left corner hit exactly)
  n   <- max(1, round(H / gap_log))
  gap <- H / n
  
  # slope from the visual angle, snapped so a stripe also hits the top-right corner
  slope_target <- tan(angle_log * pi / 180) * (y_span_log / x_span_log) * (panel_w_log / panel_h_log)
  m     <- max(1, round(slope_target * (x1 - x0) / gap))
  slope <- m * gap / (x1 - x0)
  
  # stripes anchored at the top edge, running down past the bottom-left corner
  cc_seq <- ymax_log - gap * (0:(n + m))
  
  bind_rows(lapply(cc_seq, function(cc) {
    xa <- max(x0, x0 + (ymin_log - cc) / slope)
    xb <- min(x1, x0 + (ymax_log - cc) / slope)
    if (xb - xa < 1e-9) return(NULL)
    ya <- min(max(cc + slope * (xa - x0), ymin_log), ymax_log)
    yb <- min(max(cc + slope * (xb - x0), ymin_log), ymax_log)
    data.frame(x = xa, xend = xb,
               y    = 10^ya,    # back to data units
               yend = 10^yb)
  }))
}

plot_list_local_exotic = list()
for (i in 1:length(local_exotic_groups)){
  species_grouping = species_classif %>%
    filter(!species_id %in% c(0,1,2)) %>%
    filter(local == local_exotic_groups[i])
  
  species_id_list = species_grouping$species_id
  species_name_list = species_grouping$species_name
  
  # Process MPA + CC 
  tot_B_mpa_future = B_mpa_future[1:190, 1:83, species_id_list+1, 1:24]
  tot_B_mpa_future = apply(tot_B_mpa_future, c(1,2,4), sum)
  
  tot_B_mpa_future_matrix <- matrix(
    tot_B_mpa_future,
    nrow = 190 * 83,
    ncol = 24
  )
  
  nscen = seq(1, 24, 1)
  colnames(tot_B_mpa_future_matrix) = paste0("scen", nscen)
  
  B_mpa_future_group = cbind(Med_areas, tot_B_mpa_future_matrix)
  
  # Process NoMPA + future
  B_nompa_future <- apply(B_nompa_future, c(1,2,3), mean, na.rm = T) # mean over reps
  
  tot_B_nompa_future = B_nompa_future[1:190, 1:83, species_id_list+1]
  tot_B_nompa_future = apply(tot_B_nompa_future, c(1,2), sum)
  
  tot_B_nompa_future2 = tot_B_nompa_future[,83:1]
  tot_B_nompa_future2_Vect = as.vector(tot_B_nompa_future2)
  
  # Process NoMOA + present
  B_nompa_present <- apply(B_nompa_present, c(1,2,3), mean, na.rm = T) # mean over reps
  
  tot_B_nompa_present = B_nompa_present[1:190, 1:83, species_id_list+1]
  tot_B_nompa_present = apply(tot_B_nompa_present, c(1,2), sum)
  
  tot_B_nompa_present2 = tot_B_nompa_present[,83:1]
  tot_B_nompa_present2_Vect = as.vector(tot_B_nompa_present2)
  
  # Join with MPA
  B_mpa_future_group$B_nompa_present = tot_B_nompa_present2_Vect
  B_mpa_future_group$B_nompa_future = tot_B_nompa_future2_Vect
  
  # Make plot
  B_allMed = B_mpa_future_group %>%
    st_drop_geometry() %>%
    pivot_longer(6:29, names_to = "mpa_scenario", values_to = "B_mpa_future") %>%
    mutate(mpa_scenario_group = case_when(mpa_scenario %in% c(paste0("scen", 5:14)) ~ "S3-Random EEZ",
                                          mpa_scenario %in% c(paste0("scen", 15:24)) ~ "S2-Random basin",
                                          mpa_scenario %in% c(paste0("scen", 1)) ~ "S1-Current",
                                          mpa_scenario %in% c(paste0("scen", 2)) ~ "S4-Micheli",
                                          mpa_scenario %in% c(paste0("scen", 3)) ~ "S5-Mazor GFCM",
                                          mpa_scenario %in% c(paste0("scen", 4)) ~ "S6-Mazor SAUP",
                                          TRUE ~ mpa_scenario)) %>%
    group_by(mpa_scenario, mpa_scenario_group) %>%
    summarise(B_mpa_future = sum(B_mpa_future, na.rm = T),
              B_nompa_present = sum(B_nompa_present, na.rm = T),
              B_nompa_future = sum(B_nompa_future, na.rm = T)) %>%
    ungroup() %>%
    mutate(diff_B = B_nompa_future - B_nompa_present,
           delta_B = (B_nompa_future - B_nompa_present)/B_nompa_present * 100,
           diff_B_mpa = B_mpa_future - B_nompa_present,
           delta_B_mpa = (B_mpa_future - B_nompa_present)/B_nompa_present * 100) %>%
    pivot_longer(c(delta_B, delta_B_mpa), names_to = "mpa_coverage", values_to = "B_change") %>%
    mutate(mpa_coverage = case_when(mpa_coverage == "delta_B_mpa" ~ "10%",
                                    TRUE ~ "0%")) %>%
    group_by(mpa_coverage, mpa_scenario_group) %>%
    summarise(B_change_mean = mean(B_change, na.rm = T),
              B_change_sd = sd(B_change, na.rm = T)) %>%
    ungroup() %>%
    mutate(mpa_scenario_group = case_when(mpa_coverage == "0%" ~ NA,
                                          TRUE ~ mpa_scenario_group)) %>%
    distinct(B_change_mean, .keep_all = T) 
  
  # Barplot
  B_local_exotic_allMed <- B_allMed %>%
    mutate(xpos = if_else(mpa_coverage == "0%", 1, 1.6))
  # ---- Hatch lines for the 0% bar (drawn in data coordinates) ----
  bar0   <- filter(B_local_exotic_allMed, mpa_coverage == "0%")
  bar_w  <- 0.25
  hatch  <- make_hatch_log(bar0)
  
  B_Med_local_exotic <- ggplot() +
    # 0% bar: light fill + outline, own legend entry via linetype
    geom_col(
      data = bar0,
      aes(x = xpos, y = B_change_mean, linetype = "No MPA, RCP8.5"),
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
      data = filter(B_local_exotic_allMed, mpa_coverage == "10%"),
      aes(x = xpos, y = B_change_mean, fill = mpa_scenario_group),
      position = position_dodge(width = 0.7),
      width = 0.6
    ) +
    geom_errorbar(
      data = B_local_exotic_allMed %>%
        filter(mpa_coverage == "10%") %>%
        mutate(
          ymin = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                         B_change_mean - B_change_sd, NA_real_),
          ymax = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                         B_change_mean + B_change_sd, NA_real_)
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
    labs(y = "Biomass change (%) [log-scale]",
         x = "MPA coverage",
         title = paste0(local_exotic_groups[i])) +
    theme(
      legend.title = element_text(size = 14),
      legend.text  = element_text(size = 12),
      legend.key.size = unit(1, "cm")   # makes the color swatches bigger too
    ) +
    scale_y_log10(
      breaks = c(1, 10, 100, 1000),
      limits = c(1, 1300)
    )  
  
  plot_list_local_exotic[[i]] <- B_Med_local_exotic
}


local_lessepsian_plot <- ggarrange(
  plotlist = plot_list_local_exotic,
  ncol = 2,
  nrow = 1,
  common.legend = TRUE,   # set to FALSE if each plot should keep its own legend
  legend = "right"       # only used if common.legend = TRUE
)

local_lessepsian_plot <- annotate_figure(
  local_lessepsian_plot,
  top = text_grob(
    "A) Native vs. Lessepsian species",
    x = 0,        # left edge
    hjust = 0,    # align text to that edge
    face = "bold",
    size = 16
  )
)

local_lessepsian_plot <- local_lessepsian_plot +
  theme(plot.margin = margin(b = 15))

print(local_lessepsian_plot)
# Save to file 
ggsave(filename = here(paste0("figures/after_revision1/FINAL/Fig4_revised_local_lessepsian_logscale_hashed.png")), 
       plot = local_lessepsian_plot,
       dpi = 1000, height = 5, width = 10)

# Fig 4B - Native species by vertical habitat ----------------------------------

habitat_groups = unique(species_classif$vertical_position)
plot_list = list()
for (i in 1:length(habitat_groups)){
  species_grouping = species_classif %>%
    filter(local == "Native") %>%
    filter(!species_id %in% c(0,1,2)) %>%
    filter(vertical_position == habitat_groups[i])
  
  species_id_list = species_grouping$species_id
  species_name_list = species_grouping$species_name
  
  # Process MPA + CC 
  tot_B_mpa_future = B_mpa_future[1:190, 1:83, species_id_list+1, 1:24]
  tot_B_mpa_future = apply(tot_B_mpa_future, c(1,2,4), sum)
  
  tot_B_mpa_future_matrix <- matrix(
    tot_B_mpa_future,
    nrow = 190 * 83,
    ncol = 24
  )
  
  nscen = seq(1, 24, 1)
  colnames(tot_B_mpa_future_matrix) = paste0("scen", nscen)
  
  B_mpa_future_group = cbind(Med_areas, tot_B_mpa_future_matrix)
  
  # Process NoMPA + future
  B_nompa_future <- apply(B_nompa_future, c(1,2,3), mean, na.rm = T) # mean over reps
  
  tot_B_nompa_future = B_nompa_future[1:190, 1:83, species_id_list+1]
  tot_B_nompa_future = apply(tot_B_nompa_future, c(1,2), sum)
  
  tot_B_nompa_future2 = tot_B_nompa_future[,83:1]
  tot_B_nompa_future2_Vect = as.vector(tot_B_nompa_future2)
  
  # Process NoMOA + present
  B_nompa_present <- apply(B_nompa_present, c(1,2,3), mean, na.rm = T) # mean over reps
  
  tot_B_nompa_present = B_nompa_present[1:190, 1:83, species_id_list+1]
  tot_B_nompa_present = apply(tot_B_nompa_present, c(1,2), sum)
  
  tot_B_nompa_present2 = tot_B_nompa_present[,83:1]
  tot_B_nompa_present2_Vect = as.vector(tot_B_nompa_present2)
  
  # Join with MPA
  B_mpa_future_group$B_nompa_present = tot_B_nompa_present2_Vect
  B_mpa_future_group$B_nompa_future = tot_B_nompa_future2_Vect
  
  # Make plot
  B_allMed = B_mpa_future_group %>%
    st_drop_geometry() %>%
    pivot_longer(6:29, names_to = "mpa_scenario", values_to = "B_mpa_future") %>%
    mutate(mpa_scenario_group = case_when(mpa_scenario %in% c(paste0("scen", 5:14)) ~ "S3-Random EEZ",
                                          mpa_scenario %in% c(paste0("scen", 15:24)) ~ "S2-Random basin",
                                          mpa_scenario %in% c(paste0("scen", 1)) ~ "S1-Current",
                                          mpa_scenario %in% c(paste0("scen", 2)) ~ "S4-Micheli",
                                          mpa_scenario %in% c(paste0("scen", 3)) ~ "S5-Mazor GFCM",
                                          mpa_scenario %in% c(paste0("scen", 4)) ~ "S6-Mazor SAUP",
                                          TRUE ~ mpa_scenario)) %>%
    group_by(mpa_scenario, mpa_scenario_group) %>%
    summarise(B_mpa_future = sum(B_mpa_future, na.rm = T),
              B_nompa_present = sum(B_nompa_present, na.rm = T),
              B_nompa_future = sum(B_nompa_future, na.rm = T)) %>%
    ungroup() %>%
    mutate(diff_B = B_nompa_future - B_nompa_present,
           delta_B = (B_nompa_future - B_nompa_present)/B_nompa_present * 100,
           diff_B_mpa = B_mpa_future - B_nompa_present,
           delta_B_mpa = (B_mpa_future - B_nompa_present)/B_nompa_present * 100) %>%
    pivot_longer(c(delta_B, delta_B_mpa), names_to = "mpa_coverage", values_to = "B_change") %>%
    mutate(mpa_coverage = case_when(mpa_coverage == "delta_B_mpa" ~ "10%",
                                    TRUE ~ "0%")) %>%
    group_by(mpa_coverage, mpa_scenario_group) %>%
    summarise(B_change_mean = mean(B_change, na.rm = T),
              B_change_sd = sd(B_change, na.rm = T)) %>%
    ungroup() %>%
    mutate(mpa_scenario_group = case_when(mpa_coverage == "0%" ~ NA,
                                          TRUE ~ mpa_scenario_group)) %>%
    distinct(B_change_mean, .keep_all = T) 
  
  # Barplot
  B_allMed <- B_allMed %>%
    mutate(xpos = if_else(mpa_coverage == "0%", 1, 1.6))

  # ---- Hatch lines for the 0% bar (drawn in data coordinates) ----
  bar0   <- filter(B_allMed, mpa_coverage == "0%")
  bar_w  <- 0.25
  x0 <- bar0$xpos - bar_w/2
  x1 <- bar0$xpos + bar_w/2
  ymin_bar <- min(0, bar0$B_change_mean)
  ymax_bar <- max(0, bar0$B_change_mean)
  
  slope <- 20    # steepness of the stripes (increase = steeper)
  gap   <- 1.2   # vertical distance between stripes (in % units)
  
  hatch <- bind_rows(lapply(
    seq(ymin_bar - slope * (x1 - x0), ymax_bar, by = gap),
    function(cc) {
      xs <- seq(x0, x1, length.out = 200)
      ys <- cc + slope * (xs - x0)
      ok <- ys >= ymin_bar & ys <= ymax_bar
      if (!any(ok)) return(NULL)
      data.frame(x = xs[ok][1], xend = tail(xs[ok], 1),
                 y = ys[ok][1], yend = tail(ys[ok], 1))
    }
  ))
  
  # ---- Custom legend key: grey box with diagonal stripes ----
  draw_key_hatch <- function(data, params, size) {
    grid::grobTree(
      grid::rectGrob(gp = grid::gpar(fill = "grey95", col = "grey30")),
      grid::gTree(
        children = grid::gList(
          grid::segmentsGrob(
            x0 = grid::unit(seq(-1, 1, by = 0.25), "npc"),
            x1 = grid::unit(seq(-1, 1, by = 0.25) + 1, "npc"),
            y0 = grid::unit(0, "npc"), y1 = grid::unit(1, "npc"),
            gp = grid::gpar(col = "grey30", lwd = 1)
          )
        ),
        vp = grid::viewport(clip = "on")
      )
    )
  }
  
  B_Med_group <- ggplot() +
    # 0% bar: light fill + outline, own legend entry via linetype
    geom_col(
      data = bar0,
      aes(x = xpos, y = B_change_mean, linetype = "No MPA, RCP8.5"),
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
      data = filter(B_allMed, mpa_coverage == "10%"),
      aes(x = xpos, y = B_change_mean, fill = mpa_scenario_group),
      position = position_dodge(width = 0.7),
      width = 0.6
    ) +
    geom_errorbar(
      data = B_allMed %>%
        filter(mpa_coverage == "10%") %>%
        mutate(
          ymin = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                         B_change_mean - B_change_sd, NA_real_),
          ymax = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                         B_change_mean + B_change_sd, NA_real_)
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
    labs(y = "Biomass change (%)",
         x = "MPA coverage",
         title = paste0(habitat_groups[i])) +
    ylim(c(-10, 16))

  plot_list[[i]] <- B_Med_group
}

plot_list_rearranged = c(plot_list[1], plot_list[3], plot_list[2])
combined_plot_pel_dem_ben <- ggarrange(
  plotlist = plot_list_rearranged,
  ncol = 3,
  nrow = 1,
  common.legend = TRUE,   # set to FALSE if each plot should keep its own legend
  legend = "none"       # only used if common.legend = TRUE
)

combined_plot_pel_dem_ben <- annotate_figure(
  combined_plot_pel_dem_ben,
  top = text_grob(
    "B) Native species by vertical habitat",
    x = 0,        # left edge
    hjust = 0,    # align text to that edge
    face = "bold",
    size = 16
  )
)
print(combined_plot_pel_dem_ben)

# Save to file
ggsave(filename = here(paste0("figures/after_revision1/FINAL/Fig4_revised_pel_dem_ben_hashed.png")), 
       plot= combined_plot_pel_dem_ben,
       dpi = 1000, height = 5, width = 10)

# Calculate MPA effect under future climate for paper:
B_allMed = B_mpa_future_group %>%
  st_drop_geometry() %>%
  pivot_longer(6:29, names_to = "mpa_scenario", values_to = "B_mpa_future") %>%
  mutate(mpa_scenario_group = case_when(mpa_scenario %in% c(paste0("scen", 5:14)) ~ "S3-Random EEZ",
                                        mpa_scenario %in% c(paste0("scen", 15:24)) ~ "S2-Random basin",
                                        mpa_scenario %in% c(paste0("scen", 1)) ~ "S1-Current",
                                        mpa_scenario %in% c(paste0("scen", 2)) ~ "S4-Micheli",
                                        mpa_scenario %in% c(paste0("scen", 3)) ~ "S5-Mazor GFCM",
                                        mpa_scenario %in% c(paste0("scen", 4)) ~ "S6-Mazor SAUP",
                                        TRUE ~ mpa_scenario)) %>%
  group_by(mpa_scenario, mpa_scenario_group) %>%
  summarise(B_mpa_future = sum(B_mpa_future, na.rm = T),
            B_nompa_present = sum(B_nompa_present, na.rm = T),
            B_nompa_future = sum(B_nompa_future, na.rm = T)) %>%
  ungroup() %>%
  mutate(diff_B = B_nompa_future - B_nompa_present,
         delta_B = (B_nompa_future - B_nompa_present)/B_nompa_present * 100,
         diff_B_mpa = B_mpa_future - B_nompa_present,
         delta_B_mpa = (B_mpa_future - B_nompa_present)/B_nompa_present * 100) %>%
  mutate(diff_delta = delta_B_mpa - delta_B) %>%
  group_by(mpa_scenario_group) %>%
  summarize(diff_delta_mean = mean(diff_delta),
            diff_delta_sd = sd(diff_delta))


# Combine both A and B for Fig 4 -----------------------------------------------

Fig4 <- ggarrange(
  plotlist = list(local_lessepsian_plot,
  combined_plot_pel_dem_ben),
  ncol = 1,
  nrow = 2,
  heights = c(1, 1),   # adjust if one panel needs more space
  legend = "none"
)

ggsave(filename = here(paste0("figures/after_revision1/FINAL/Fig4_revised_A_B_log_hashed.png")), 
       plot = Fig4,
       dpi = 1000, height = 10, width = 10)
