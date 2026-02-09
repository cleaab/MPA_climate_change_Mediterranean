rm(list=ls())
library(tidyverse)
library(ncdf4)
library(here)

# load cutoff age/size
cutoff = read.csv("D:/Documents/OSMOSE/osmose-med-Article2/Config/osm_param-output.csv", sep=";") %>%
  rename(species_index = X..Main.output.parameters, cutoff_age = X) %>%
  filter(grepl("output.cutoff.age.sp", species_index)) %>%
  mutate(species_index = gsub(".*\\.", "", species_index)) %>%# keep only species name
  mutate(species_index = substr(species_index, 3, 7)) %>% # keep only species number
  mutate(species_index = as.integer(species_index))

Lmax_csv = TRUE
if (Lmax_csv == FALSE){
  
  # NoCC Nompa --------------------------------------------------------------
  
  data_dir <- here("data/model_outputs/Lmax/NoCC_Nompa/restart/")
  
  first_year = (24*140)-1
  last_year = (24*200)-1
  
  time_steps = seq(first_year, last_year, by = 24)
  length(time_steps)
  
  for (t in time_steps){
    # Open the NetCDF file
    file_base <- file.path(paste0(data_dir, "/med_snapshot_step", t, ".nc."))
    files <- paste0(file_base, 0:29)
    
    for (i in seq_along(files)) {
      nc <- nc_open(files[i])
      
      # Extract variables
      species <- ncvar_get(nc, "species")
      abundance <- ncvar_get(nc, "abundance")
      length_cm <- ncvar_get(nc, "length")
      age <- ncvar_get(nc, "age")
      
      # Combine into a data frame
      school_data <- data.frame(
        species_index = species,
        abundance = abundance,
        length_cm = length_cm,
        age = age,
        time_step = t,
        replicate = i)
      
      # Compute cumulative abundance by species_index, sorted by length
      school_data_cum <- school_data %>%
        left_join(cutoff, by = "species_index") %>%
        filter(age >= cutoff_age) %>%
        group_by(species_index) %>%
        arrange(length_cm, .by_group = TRUE) %>%
        mutate(
          cum_abundance = cumsum(abundance),
          total_abundance = sum(abundance),
          cum_abund_frac = cum_abundance / total_abundance,
          L95 = ifelse(cum_abundance >= 0.95 * total_abundance, length_cm, NA),
          L99 = ifelse(cum_abundance >= 0.99 * total_abundance, length_cm, NA)
        )
      
      # Optionally extract the first L95 and L99 per species_index
      L_thresholds <- school_data_cum %>%
        group_by(species_index) %>%
        summarize(
          L95 = length_cm[which.min(is.na(L95))],
          L99 = length_cm[which.min(is.na(L99))],
          .groups = "drop"
        ) 
      
      
      if (i == 1){
        L_thresholds_all = L_thresholds
      } else{
        L_thresholds_all = rbind(L_thresholds_all, L_thresholds)
      }
      
    }
    
    if (t == time_steps[1]){
      Lmax_present_nompa = L_thresholds_all
    } else{
      Lmax_present_nompa = rbind(Lmax_present_nompa, L_thresholds_all)
      
    }
  }
  
  
  write.csv(Lmax_present_nompa, here("data/model_outputs/Lmax/Lmax_nompa_present.csv"))
  
  
  ## With MPA CC scenario S5-Mazor GFCM  --------------------
  data_dir <- here("data/model_outputs/Lmax/CC_Mazor_scenario_8_10/restart/")
  
  first_year = (24*140)-1
  last_year = (24*200)-1
  
  time_steps = seq(first_year, last_year, by = 24)
  length(time_steps)
  
  for (t in time_steps){
    # Open the NetCDF file
    file_base <- file.path(paste0(data_dir, "/med_snapshot_step", t, ".nc."))
    files <- paste0(file_base, 0:29)
    
    for (i in seq_along(files)) {
      nc <- nc_open(files[i])
      
      # Extract variables
      species <- ncvar_get(nc, "species")
      abundance <- ncvar_get(nc, "abundance")
      length_cm <- ncvar_get(nc, "length")
      age <- ncvar_get(nc, "age")
      
      # Combine into a data frame
      school_data <- data.frame(
        species_index = species,
        abundance = abundance,
        length_cm = length_cm,
        age = age,
        time_step = t,
        replicate = i)
      
      # Compute cumulative abundance by species_index, sorted by length
      school_data_cum <- school_data %>%
        left_join(cutoff, by = "species_index") %>%
        filter(age >= cutoff_age) %>%
        group_by(species_index) %>%
        arrange(length_cm, .by_group = TRUE) %>%
        mutate(
          cum_abundance = cumsum(abundance),
          total_abundance = sum(abundance),
          cum_abund_frac = cum_abundance / total_abundance,
          L95 = ifelse(cum_abundance >= 0.95 * total_abundance, length_cm, NA),
          L99 = ifelse(cum_abundance >= 0.99 * total_abundance, length_cm, NA)
        )
      
      # Optionally extract the first L95 and L99 per species_index
      L_thresholds <- school_data_cum %>%
        group_by(species_index) %>%
        summarize(
          L95 = length_cm[which.min(is.na(L95))],
          L99 = length_cm[which.min(is.na(L99))],
          .groups = "drop"
        ) 
      
      
      if (i == 1){
        L_thresholds_all = L_thresholds
      } else{
        L_thresholds_all = rbind(L_thresholds_all, L_thresholds)
      }
      
    }
    
    if (t == time_steps[1]){
      Lmax_future_mpa = L_thresholds_all
    } else{
      Lmax_future_mpa = rbind(Lmax_future_mpa, L_thresholds_all)
      
    }
  }
  
  
  
  write.csv(Lmax_future_mpa, here("data/model_outputs/Lmax/Lmax_mpa_future.csv"))
  
  ## No MPA future -------------------------------------------------------------------
  data_dir <- here("data/model_outputs/Lmax/CC_Nompa/restart/")
  
  first_year = (24*140)-1
  last_year = (24*200)-1
  
  time_steps = seq(first_year, last_year, by = 24)
  length(time_steps)
  
  for (t in time_steps){
    # Open the NetCDF file
    file_base <- file.path(paste0(data_dir, "/med_snapshot_step", t, ".nc."))
    files <- paste0(file_base, 0:29)
    
    for (i in seq_along(files)) {
      nc <- nc_open(files[i])
      
      # Extract variables
      species <- ncvar_get(nc, "species")
      abundance <- ncvar_get(nc, "abundance")
      length_cm <- ncvar_get(nc, "length")
      age <- ncvar_get(nc, "age")
      
      # Combine into a data frame
      school_data <- data.frame(
        species_index = species,
        abundance = abundance,
        length_cm = length_cm,
        age = age,
        time_step = t,
        replicate = i)
      
      # Compute cumulative abundance by species_index, sorted by length
      school_data_cum <- school_data %>%
        left_join(cutoff, by = "species_index") %>%
        filter(age >= cutoff_age) %>%
        group_by(species_index) %>%
        arrange(length_cm, .by_group = TRUE) %>%
        mutate(
          cum_abundance = cumsum(abundance),
          total_abundance = sum(abundance),
          cum_abund_frac = cum_abundance / total_abundance,
          L95 = ifelse(cum_abundance >= 0.95 * total_abundance, length_cm, NA),
          L99 = ifelse(cum_abundance >= 0.99 * total_abundance, length_cm, NA)
        )
      
      # Optionally extract the first L95 and L99 per species_index
      L_thresholds <- school_data_cum %>%
        group_by(species_index) %>%
        summarize(
          L95 = length_cm[which.min(is.na(L95))],
          L99 = length_cm[which.min(is.na(L99))],
          .groups = "drop"
        ) 
      
      
      if (i == 1){
        L_thresholds_all = L_thresholds
      } else{
        L_thresholds_all = rbind(L_thresholds_all, L_thresholds)
      }
      
    }
    
    if (t == time_steps[1]){
      Lmax_future_nompa = L_thresholds_all
    } else{
      Lmax_future_nompa = rbind(Lmax_future_nompa, L_thresholds_all)
      
    }
  }
  
  write.csv(Lmax_future_nompa, here("data/model_outputs/Lmax/Lmax_nompa_future.csv"))
  
}


# Lmax for all scens
if (Lmax_csv == FALSE){
  
  # NoCC Nompa --------------------------------------------------------------
  
  data_dir <- here("data/model_outputs/Lmax/NoCC_Nompa/restart/")
  
  first_year = (24*140)-1
  last_year = (24*200)-1
  
  time_steps = seq(first_year, last_year, by = 24)
  length(time_steps)
  
  for (t in time_steps){
    # Open the NetCDF file
    file_base <- file.path(paste0(data_dir, "/med_snapshot_step", t, ".nc."))
    files <- paste0(file_base, 0:29)
    
    for (i in seq_along(files)) {
      nc <- nc_open(files[i])
      
      # Extract variables
      species <- ncvar_get(nc, "species")
      abundance <- ncvar_get(nc, "abundance")
      length_cm <- ncvar_get(nc, "length")
      age <- ncvar_get(nc, "age")
      
      # Combine into a data frame
      school_data <- data.frame(
        species_index = species,
        abundance = abundance,
        length_cm = length_cm,
        age = age,
        time_step = t,
        replicate = i)
      
      # Compute cumulative abundance by species_index, sorted by length
      school_data_cum <- school_data %>%
        left_join(cutoff, by = "species_index") %>%
        filter(age >= cutoff_age) %>%
        group_by(species_index) %>%
        arrange(length_cm, .by_group = TRUE) %>%
        mutate(
          cum_abundance = cumsum(abundance),
          total_abundance = sum(abundance),
          cum_abund_frac = cum_abundance / total_abundance,
          L95 = ifelse(cum_abundance >= 0.95 * total_abundance, length_cm, NA),
          L99 = ifelse(cum_abundance >= 0.99 * total_abundance, length_cm, NA)
        )
      
      # Optionally extract the first L95 and L99 per species_index
      L_thresholds <- school_data_cum %>%
        group_by(species_index) %>%
        summarize(
          L95 = length_cm[which.min(is.na(L95))],
          L99 = length_cm[which.min(is.na(L99))],
          .groups = "drop"
        ) 
      
      
      if (i == 1){
        L_thresholds_all = L_thresholds
      } else{
        L_thresholds_all = rbind(L_thresholds_all, L_thresholds)
      }
      
    }
    
    if (t == time_steps[1]){
      Lmax_present_nompa = L_thresholds_all
    } else{
      Lmax_present_nompa = rbind(Lmax_present_nompa, L_thresholds_all)
      
    }
  }
  
  
  write.csv(Lmax_present_nompa, here("data/model_outputs/Lmax/Lmax_nompa_present.csv"))
  
  
  ## With MPA CC scenario S5-Mazor GFCM  --------------------
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
  
  
  for (mpa_scenario in mpa_scenarios){
    data_dir <- here(paste0("data/model_outputs/Lmax/CC_Mpa/", mpa_scenario, "/restart/"))
    
    first_year = (24*140)-1
    last_year = (24*200)-1
    
    time_steps = seq(first_year, last_year, by = 24)
    length(time_steps)
    
    for (t in time_steps){
      # Open the NetCDF file
      file_base <- file.path(paste0(data_dir, "/med_snapshot_step", t, ".nc."))
      files <- paste0(file_base, 0:29)
      
      for (i in seq_along(files)) {
        nc <- nc_open(files[i])
        
        # Extract variables
        species <- ncvar_get(nc, "species")
        abundance <- ncvar_get(nc, "abundance")
        length_cm <- ncvar_get(nc, "length")
        age <- ncvar_get(nc, "age")
        
        # Combine into a data frame
        school_data <- data.frame(
          species_index = species,
          abundance = abundance,
          length_cm = length_cm,
          age = age,
          time_step = t,
          replicate = i)
        
        # Compute cumulative abundance by species_index, sorted by length
        school_data_cum <- school_data %>%
          left_join(cutoff, by = "species_index") %>%
          filter(age >= cutoff_age) %>%
          group_by(species_index) %>%
          arrange(length_cm, .by_group = TRUE) %>%
          mutate(
            cum_abundance = cumsum(abundance),
            total_abundance = sum(abundance),
            cum_abund_frac = cum_abundance / total_abundance,
            L95 = ifelse(cum_abundance >= 0.95 * total_abundance, length_cm, NA),
            L99 = ifelse(cum_abundance >= 0.99 * total_abundance, length_cm, NA)
          )
        
        # Optionally extract the first L95 and L99 per species_index
        L_thresholds <- school_data_cum %>%
          group_by(species_index) %>%
          summarize(
            L95 = length_cm[which.min(is.na(L95))],
            L99 = length_cm[which.min(is.na(L99))],
            .groups = "drop"
          ) 
        
        
        if (i == 1){
          L_thresholds_all = L_thresholds
        } else{
          L_thresholds_all = rbind(L_thresholds_all, L_thresholds)
        }
        nc_close(nc)
      }
      
      if (t == time_steps[1]){
        Lmax_future_mpa = L_thresholds_all
      } else{
        Lmax_future_mpa = rbind(Lmax_future_mpa, L_thresholds_all)
        
      }
    }
    
    Lmax_future_mpa$mpa_scenario = mpa_scenario
    #write.csv(Lmax_future_mpa, here(paste0("data/model_outputs/Lmax/Lmax_mpa_future_", mpa_scenario, ".csv")))
    
    if (mpa_scenario == mpa_scenarios[1]){
      Lmax_future_mpa_all_scens = Lmax_future_mpa
    } else{
      Lmax_future_mpa_all_scens = rbind(Lmax_future_mpa_all_scens, Lmax_future_mpa)
      
    }
  }
 
  
  write.csv(Lmax_future_mpa_all_scens, here("data/model_outputs/Lmax/Lmax_mpa_future_all_scens.csv"))
  
  ## No MPA future -------------------------------------------------------------------
  data_dir <- here("data/model_outputs/Lmax/CC_Nompa/restart/")
  
  first_year = (24*140)-1
  last_year = (24*200)-1
  
  time_steps = seq(first_year, last_year, by = 24)
  length(time_steps)
  
  for (t in time_steps){
    # Open the NetCDF file
    file_base <- file.path(paste0(data_dir, "/med_snapshot_step", t, ".nc."))
    files <- paste0(file_base, 0:29)
    
    for (i in seq_along(files)) {
      nc <- nc_open(files[i])
      
      # Extract variables
      species <- ncvar_get(nc, "species")
      abundance <- ncvar_get(nc, "abundance")
      length_cm <- ncvar_get(nc, "length")
      age <- ncvar_get(nc, "age")
      
      # Combine into a data frame
      school_data <- data.frame(
        species_index = species,
        abundance = abundance,
        length_cm = length_cm,
        age = age,
        time_step = t,
        replicate = i)
      
      # Compute cumulative abundance by species_index, sorted by length
      school_data_cum <- school_data %>%
        left_join(cutoff, by = "species_index") %>%
        filter(age >= cutoff_age) %>%
        group_by(species_index) %>%
        arrange(length_cm, .by_group = TRUE) %>%
        mutate(
          cum_abundance = cumsum(abundance),
          total_abundance = sum(abundance),
          cum_abund_frac = cum_abundance / total_abundance,
          L95 = ifelse(cum_abundance >= 0.95 * total_abundance, length_cm, NA),
          L99 = ifelse(cum_abundance >= 0.99 * total_abundance, length_cm, NA)
        )
      
      # Optionally extract the first L95 and L99 per species_index
      L_thresholds <- school_data_cum %>%
        group_by(species_index) %>%
        summarize(
          L95 = length_cm[which.min(is.na(L95))],
          L99 = length_cm[which.min(is.na(L99))],
          .groups = "drop"
        ) 
      
      
      if (i == 1){
        L_thresholds_all = L_thresholds
      } else{
        L_thresholds_all = rbind(L_thresholds_all, L_thresholds)
      }
      
    }
    
    if (t == time_steps[1]){
      Lmax_future_nompa = L_thresholds_all
    } else{
      Lmax_future_nompa = rbind(Lmax_future_nompa, L_thresholds_all)
      
    }
  }
  
  write.csv(Lmax_future_nompa, here("data/model_outputs/Lmax/Lmax_nompa_future.csv"))
  
}



# Wilcox test (need to keep all the replicates):
Lmax_present_nompa = read.csv(here("data/model_outputs/Lmax/Lmax_nompa_present.csv"))
Lmax_future_nompa = read.csv(here("data/model_outputs/Lmax/Lmax_nompa_future.csv")) 
  
species_index = unique(Lmax_future_nompa$species_index)

for (i in species_index) {
  present_vals <- subset(Lmax_present_nompa, species_index == i)$L95
  
  future_vals <- subset(Lmax_future_nompa, species_index == i)$L95
  
  print(present_vals)
  print(future_vals)
  
  # check if normal
  normal_test_present = shapiro.test(present_vals)$p.value
  normal_test_future = shapiro.test(future_vals)$p.value
  
  print(normal_test_present)
  print(normal_test_future)
  
  if (normal_test_future > 0.05 & normal_test_present > 0.05){
    
    p_value = t.test(present_vals, future_vals)$p.value
    
  } else{
    p_value = wilcox.test(present_vals, future_vals)$p.value
  }
  
  if (i == species_index[1]){
    p_values = p_value
  } else{
    p_values = c(p_values, p_value)
  }
  
  
}

compare_means = data.frame(species_index)
compare_means$p_value = p_values
compare_means = compare_means %>%
  mutate(significative = case_when(p_value < 0.05 ~ "yes",
                                   TRUE ~ "no"))
table(compare_means$significative)
length(unique(compare_means$species_index))

# Plot barplot changes Nompas:
Lmax_present_nompa = read.csv(here("data/model_outputs/Lmax/Lmax_nompa_present.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_mean = mean(L95),
            L95_sd = sd(L95),
            L99_mean = mean(L99),
            L99_sd = sd(L99)) 


Lmax_future_nompa = read.csv(here("data/model_outputs/Lmax/Lmax_nompa_future.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_future_mean = mean(L95),
            L95_future_sd = sd(L95),
            L99_future_mean = mean(L99),
            L99_future_sd = sd(L99)) 

species_classif = read.csv(here("data/classification_species.csv")) %>%
  dplyr::select(species_id, species_name, group_article1, updated_commercial_interest, size_class, vertical_position, vulnerability_cat)

Lmax = Lmax_present_nompa %>%
  left_join(Lmax_future_nompa, by = "species_index") %>%
  left_join(compare_means, by = "species_index") %>%
  mutate(diff_L95 = L95_future_mean - L95_mean) %>%
  mutate(delta_L95 = (L95_future_mean - L95_mean)/L95_mean * 100) %>%
  mutate(change_category = case_when(diff_L95 < 0 ~ "decrease",
                                     diff_L95 > 0 ~ "increase",
                                     TRUE ~ "stable")) %>%
  rename(species_id = species_index) %>%
  left_join(species_classif, by = "species_id") %>%
  filter(significative == "yes") %>%
  filter(! species_id %in% c(0,1,2)) %>%
  mutate(species_name = case_when(species_name == "Etrumeusteres" ~ "Etrumeusgolanii",
                                  TRUE ~ species_name)) %>%
  mutate(species_name = fct_reorder(species_name, diff_L95))   # <-- this line
  
table(Lmax$significative)
table(Lmax$change_category)

table(Lmax$change_category[Lmax$significative == "yes"])

ggplot(Lmax) +
  geom_bar(aes(x = diff_L95, y = factor(species_name), fill = change_category), stat="identity") +
  labs(x = "Difference in Lmax (cm)", y = " ")

ggsave(here("figures/Lmax_updated/FigS4_2_Change_Lmax_future_present.png"), dpi =300, width = 10, height = 10)


ggplot(Lmax) +
  geom_bar(aes(x = delta_L95, y = factor(species_name), fill = change_category), stat="identity") +
  labs(x = "Relative change in Lmax (%)", y = " ")


ggsave(here("figures/Lmax_updated/Delta_Lmax_future_present.png"), dpi =300, width = 10, height = 10)

# Plot barplot changes Nompas/Mpas: -----------------------------------------------
Lmax_present_nompa = read.csv(here("data/model_outputs/Lmax/Lmax_nompa_present.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_mean = mean(L95),
            L95_sd = sd(L95),
            L99_mean = mean(L99),
            L99_sd = sd(L99)) 
  

Lmax_future_nompa = read.csv(here("data/model_outputs/Lmax/Lmax_nompa_future.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_future_mean = mean(L95),
            L95_future_sd = sd(L95),
            L99_future_mean = mean(L99),
            L99_future_sd = sd(L99)) 

Lmax_future_mpa = read.csv(here("data/model_outputs/Lmax/Lmax_mpa_future.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_future_mean = mean(L95),
            L95_future_sd = sd(L95),
            L99_future_mean = mean(L99),
            L99_future_sd = sd(L99))

# all scens - mean accross spatial cells and across scenarios (all same weight)
Lmax_future_mpa = read.csv(here("data/model_outputs/Lmax/Lmax_mpa_future_all_scens.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_future_mean = mean(L95),
            L95_future_sd = sd(L95),
            L99_future_mean = mean(L99),
            L99_future_sd = sd(L99))

length(unique(Lmax_future_mpa$mpa_scenario))

# all scens - mean accross spatial cells and across scenarios - first mean across S2 and S3 and then mean across 6 MPA scenarios
Lmax_future_mpa_2 = read.csv(here("data/model_outputs/Lmax/Lmax_mpa_future_all_scens.csv")) %>%
  dplyr::select(-X) %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "EEZ-conservation",
                                        grepl("Random", mpa_scenario) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%
  group_by(species_index, mpa_scenario_group) %>%
  summarize(L95_future_mean_6 = mean(L95),
            L95_future_sd_6 = sd(L95),
            L99_future_mean_6 = mean(L99),
            L99_future_sd_6 = sd(L99)) %>%
  ungroup() %>%
  group_by(species_index) %>%
  summarize(L95_future_mean = mean(L95_future_mean_6),
            L95_future_sd = sd(L95_future_sd_6),
            L99_future_mean = mean(L99_future_mean_6),
            L99_future_sd = sd(L99_future_sd_6))

Lmax_future_mpa = Lmax_future_mpa_2


species_classif = read.csv(here("data/classification_species.csv")) %>%
  dplyr::select(species_id, species_name, group_article1, updated_commercial_interest, size_class, vertical_position, vulnerability_cat) %>%
  mutate(group_article1 = case_when(group_article1 == "Cephalopod" ~ "Cephalopods",
                                    group_article1 == "Crustacean" ~ "Crustaceans",
                                    TRUE ~ group_article1))

Lmax_nompa = Lmax_present_nompa %>%
  left_join(Lmax_future_nompa, by = "species_index") %>%
  left_join(compare_means, by = "species_index") %>%
  mutate(diff_L95 = L95_future_mean - L95_mean) %>%
  mutate(delta_L95 = (L95_future_mean - L95_mean)/L95_mean * 100) %>%
  mutate(change_category = case_when(diff_L95 < 0 ~ "decrease",
                                     diff_L95 > 0 ~ "increase",
                                     TRUE ~ "stable")) %>%
  rename(species_id = species_index) %>%
  left_join(species_classif, by = "species_id") %>%
  filter(significative == "yes") %>%
  filter(! species_id %in% c(0,1,2)) %>%
  mutate(scenario = "without_MPAs")

Lmax_mpa = Lmax_present_nompa %>%
  left_join(Lmax_future_mpa, by = "species_index") %>%
  left_join(compare_means, by = "species_index") %>%
  mutate(diff_L95 = L95_future_mean - L95_mean) %>%
  mutate(delta_L95 = (L95_future_mean - L95_mean)/L95_mean * 100) %>%
  mutate(change_category = case_when(diff_L95 < 0 ~ "decrease",
                                     diff_L95 > 0 ~ "increase",
                                     TRUE ~ "stable")) %>%
  rename(species_id = species_index) %>%
  left_join(species_classif, by = "species_id") %>%
  filter(significative == "yes") %>%
  filter(! species_id %in% c(0,1,2)) %>%
  mutate(scenario = "with_MPAs")

Lmax_mitigation = rbind(Lmax_nompa, Lmax_mpa) %>%
  mutate(species_name = fct_reorder(species_name, diff_L95))  # <-- this line


ggplot(Lmax_mitigation) +
  geom_bar(aes(x = diff_L95, y = factor(species_name), fill = scenario), position="dodge", stat="identity")

ggsave(here("figures/Lmax_updated/Diff_Lmax_mitigation_MPA.png"), dpi =300, width = 10, height = 10)


ggplot(Lmax_mitigation) +
  geom_bar(aes(x = delta_L95, y = factor(species_name), fill = scenario), position="dodge", stat="identity")

ggsave(here("figures/Lmax_updated/Delta_Lmax_mitigation_MPA.png"), dpi =300, width = 10, height = 10)


#Plot change and MPA impact: --------------------------------------------------
change_cats = Lmax_mitigation %>%
  filter(scenario == "without_MPAs") %>%
  dplyr::select(species_name, change_category)

Lmax_mitigation2 = Lmax_mitigation %>%
  dplyr::select(species_name, diff_L95, significative, scenario, vulnerability_cat, group_article1, size_class, updated_commercial_interest, vertical_position) %>%
  pivot_wider(names_from = scenario, values_from = diff_L95) %>%
  mutate(MPA_impact = with_MPAs - without_MPAs) %>%
  left_join(change_cats, by = "species_name") %>%
  mutate(MPA_effect = case_when(MPA_impact > 0 ~ "mitigation",
                                MPA_impact < 0 ~ "amplification",
                                TRUE ~ "stable")) %>%
  mutate(species_name = fct_reorder(species_name, MPA_impact)) %>%
  # <-- this line
  mutate(group_article1 = case_when(group_article1 == "Cephalopod" ~ "Cephalopods",
                                    group_article1 == "Crustacean" ~ "Crustaceans",
                                    TRUE ~ group_article1))

table(Lmax_mitigation2$MPA_effect[Lmax_mitigation2$change_category == "decrease"])

unique(Lmax_mitigation2$species_name[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "positive"])
unique(Lmax_mitigation2$species_name[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "negative"])

table(Lmax_mitigation2$size_class[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "negative"])
table(Lmax_mitigation2$size_class[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "positive"])

table(Lmax_mitigation2$vertical_position[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "negative"])
table(Lmax_mitigation2$vertical_position[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "positive"])

table(Lmax_mitigation2$group_article1[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "negative" & Lmax_mitigation2$updated_commercial_interest %in% c("HC", "C")])
table(Lmax_mitigation2$group_article1[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "positive" & Lmax_mitigation2$updated_commercial_interest %in% c("HC", "C")])

unique(Lmax_mitigation2$species_name[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "negative" & Lmax_mitigation2$updated_commercial_interest %in% c("HC")])
unique(Lmax_mitigation2$species_name[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "positive" & Lmax_mitigation2$updated_commercial_interest %in% c("HC")])

table(Lmax_mitigation2$vulnerability_cat[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "negative"])
table(Lmax_mitigation2$vulnerability_cat[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "positive"])


table(Lmax_mitigation2$updated_commercial_interest[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "negative"])
table(Lmax_mitigation2$updated_commercial_interest[Lmax_mitigation2$change_category == "decrease" & Lmax_mitigation2$MPA_effect == "positive"])

table(Lmax_mitigation2$MPA_effect[Lmax_mitigation2$change_category == "increase"])

ggplot(Lmax_mitigation2) +
  geom_bar(aes(x = MPA_impact, y = factor(species_name), fill = change_category), stat="identity") 

ggsave(here("figures/Lmax_updated/Impact_MPA_Lmax.png"), dpi =300, width = 10, height = 10)

# MPA impact by groups:
groups = unique(species_classif$group_article1)

for (g in groups){
  Lmax_mitigation2 = Lmax_mitigation %>%
    dplyr::select(species_name, diff_L95, significative, scenario) %>%
    pivot_wider(names_from = scenario, values_from = diff_L95) %>%
    mutate(MPA_impact = with_MPAs - without_MPAs) %>%
    left_join(change_cats, by = "species_name") %>%
    left_join(species_classif, by = "species_name") %>%
    filter(group_article1 %in% g) %>%
    mutate(species_name = fct_reorder(species_name, MPA_impact))  # <-- this line
  
  
  ggplot(Lmax_mitigation2) +
    geom_bar(aes(x = MPA_impact, y = factor(species_name), fill = change_category), stat="identity") +
    labs(x = "MPA impact", y = "", title = g)
  
  ggsave(here(paste0("figures/Lmax_updated/Impact_MPA_Lmax_", g, ".png")), dpi =300, width = 10, height = 10)

}


# Doughnut plots - by groups (fig 7 main text) ---------------------------------
groups = unique(Lmax_mitigation2$group_article1)
groups = c("Large-sized benthic fish", "Large-sized demersal fish", "Large-sized pelagic fish", "Cephalopods", "Medium-sized benthic fish", "Medium-sized demersal fish", "Medium-sized pelagic fish", "Crustaceans", "Small-sized benthic fish", "Small-sized demersal fish", "Small-sized pelagic fish")


# dummy plot
dummy_group <- groups[2]

Species_declining = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & group_article1 == dummy_group)

Species_group = length(species_classif$species_name[species_classif$group_article1 == dummy_group])
Prop_declining = round(nrow(Species_declining)/Species_group *100, 0)

category_counts <- table(Species_declining$MPA_effect)
category_props <- prop.table(category_counts)


# Plot the proportions
df_plot <- as.data.frame(category_props)
colnames(df_plot) <- c("Change", "Proportion")

# Compute labels
df_plot$label <- paste0(round(100 * df_plot$Proportion, 0), "%")

fig_name <- gsub("-sized", "", i)

# Plot: Donut Chart
legend_plot <- ggplot(df_plot, aes(x = 2, y = Proportion, fill = Change)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  xlim(0.5, 2.5) +  # Makes the hole in the middle
  theme_void() +
  #geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6) +
  scale_fill_manual(values = c("mitigation" = "purple", "amplification" = "darkgreen", "no effect" = "lightyellow")) +
  ggtitle(paste0(fig_name)) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  labs(fill = "MPA effect on \nLmax declines") +
  theme_bw(base_size = 16) +
  annotate("text", x = 0.5, y = 0, label = paste0(Prop_declining, "%"), size = 10, fontface = "bold") 

shared_legend <- cowplot::get_legend(legend_plot)

#######
plot_list <- list()
for (i in groups){
  Species_declining = Lmax_mitigation2 %>%
    filter(change_category == "decrease" & group_article1 == i)
  
  Species_group = length(species_classif$species_name[species_classif$group_article1 == i])
  Prop_declining = round(nrow(Species_declining)/Species_group *100, 0)
  
  # Species_increasing = Lmax_join %>%
  #   filter(change_category == "increase")
  ###PLOT - with mpas in the future
  # Count number of species in each category
  category_counts <- table(Species_declining$MPA_effect)
  category_props <- prop.table(category_counts)
  
  
  # Plot the proportions
  df_plot <- as.data.frame(category_props)
  colnames(df_plot) <- c("MPA_effect", "Proportion")
  print(i)
  print(df_plot)
  # Compute labels
  df_plot$label <- paste0(round(100 * df_plot$Proportion, 0), "%")
  
  fig_name <- gsub("-sized", "", i)
  
  
  # Plot: Donut Chart
  change_Lmax_future_mpas_plot <- ggplot(df_plot, aes(x = 2, y = Proportion, fill = MPA_effect)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    xlim(0.5, 2.5) +  # Makes the hole in the middle
    theme_void() +
    #geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6) +
    scale_fill_manual(values = c("mitigation" = "purple", "amplification" = "darkgreen", "no effect" = "lightyellow")) +
    ggtitle(paste0(fig_name)) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
    annotate("text", x = 0.5, y = 0, label = paste0(Prop_declining, "%"), size = 7, fontface = "bold") 
    
  plot_list[[i]] <- change_Lmax_future_mpas_plot
}


# ---- Combine into a 3 x 4 grid ----
plot_list[["legend"]] <- cowplot::plot_grid(shared_legend)

# ---- Combine into a 3 x 4 grid ----
final_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 4)

# ---- Save the plot ----

ggsave(here::here(paste0("figures/Lmax_updated/doughnughts/groups/Lmax_combined_figure_corrected_for_paper_v2_all_6_scens.png")),
       plot = final_plot, dpi = 600, height = 6, width = 7)
### --------------------------------
# Identify species that benefit from MPA --------------------------
species_benefitting_from_MPA = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")

species_benefitting_from_MPA_mean_24_scens = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")

species_benefitting_from_MPA_S5 = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")


## Calculate basin-wide mitigation effect across scenarios for paper ---------------
Lmax_future_mpa_2 = read.csv(here("data/model_outputs/Lmax/Lmax_mpa_future_all_scens.csv")) %>%
  dplyr::select(-X) %>%
  mutate(scenario = case_when(grepl("EEZ", mpa_scenario) ~ "EEZ-conservation",
                                        grepl("Random", mpa_scenario) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%
  group_by(species_index, scenario) %>%
  summarize(L95_future_mean = mean(L95),
            L95_future_sd = sd(L95),
            L99_future_mean = mean(L99),
            L99_future_sd = sd(L99)) 


Lmax_future_mpa = Lmax_future_mpa_2


species_classif = read.csv(here("data/classification_species.csv")) %>%
  dplyr::select(species_id, species_name, group_article1, updated_commercial_interest, size_class, vertical_position, vulnerability_cat) %>%
  mutate(group_article1 = case_when(group_article1 == "Cephalopod" ~ "Cephalopods",
                                    group_article1 == "Crustacean" ~ "Crustaceans",
                                    TRUE ~ group_article1))


Lmax_nompa = Lmax_present_nompa %>%
  left_join(Lmax_future_nompa, by = "species_index") %>%
  left_join(compare_means, by = "species_index") %>%
  mutate(diff_L95 = L95_future_mean - L95_mean) %>%
  mutate(delta_L95 = (L95_future_mean - L95_mean)/L95_mean * 100) %>%
  mutate(change_category = case_when(diff_L95 < 0 ~ "decrease",
                                     diff_L95 > 0 ~ "increase",
                                     TRUE ~ "stable")) %>%
  rename(species_id = species_index) %>%
  left_join(species_classif, by = "species_id") %>%
  filter(significative == "yes") %>%
  filter(! species_id %in% c(0,1,2)) %>%
  mutate(scenario = "without_MPAs")

Lmax_mpa = Lmax_present_nompa %>%
  left_join(Lmax_future_mpa, by = "species_index") %>%
  left_join(compare_means, by = "species_index") %>%
  mutate(diff_L95 = L95_future_mean - L95_mean) %>%
  mutate(delta_L95 = (L95_future_mean - L95_mean)/L95_mean * 100) %>%
  mutate(change_category = case_when(diff_L95 < 0 ~ "decrease",
                                     diff_L95 > 0 ~ "increase",
                                     TRUE ~ "stable")) %>%
  rename(species_id = species_index) %>%
  left_join(species_classif, by = "species_id") %>%
  filter(significative == "yes") %>%
  filter(! species_id %in% c(0,1,2)) %>%
  relocate(scenario, .after  = vulnerability_cat)

Lmax_mitigation = rbind(Lmax_nompa, Lmax_mpa) %>%
  mutate(species_name = fct_reorder(species_name, diff_L95))  # <-- this line

#Plot change and MPA impact: --------------------------------------------------
change_cats = Lmax_mitigation %>%
  filter(scenario == "without_MPAs") %>%
  dplyr::select(species_name, change_category)

Lmax_mitigation_nompa =  Lmax_mitigation %>%
  filter(scenario == "without_MPAs") %>%
  dplyr::select(species_name, diff_L95)
  

Lmax_mitigation_mpa = Lmax_mitigation %>%
  filter(scenario != "without_MPAs") %>%
  dplyr::select(species_name, diff_L95, significative, scenario, vulnerability_cat, group_article1, size_class, updated_commercial_interest, vertical_position)
  
  
Lmax_mitigation2 = Lmax_mitigation_mpa %>%
  rename(diff_L95_mpa = diff_L95) %>%
  left_join(Lmax_mitigation_nompa, by = "species_name") %>%
  mutate(MPA_impact = diff_L95_mpa - diff_L95) %>%
  left_join(change_cats, by = "species_name") %>%
  mutate(MPA_effect = case_when(MPA_impact > 0 ~ "mitigation",
                                MPA_impact < 0 ~ "amplification",
                                TRUE ~ "stable")) %>%
  mutate(species_name = fct_reorder(species_name, MPA_impact)) %>%
  # <-- this line
  mutate(group_article1 = case_when(group_article1 == "Cephalopod" ~ "Cephalopods",
                                    group_article1 == "Crustacean" ~ "Crustaceans",
                                    TRUE ~ group_article1))


# Doughnut plots - by groups (fig 7 main text) ---------------------------------
groups = unique(Lmax_mitigation2$group_article1)
groups = c("Large-sized benthic fish", "Large-sized demersal fish", "Large-sized pelagic fish", "Cephalopods", "Medium-sized benthic fish", "Medium-sized demersal fish", "Medium-sized pelagic fish", "Crustaceans", "Small-sized benthic fish", "Small-sized demersal fish", "Small-sized pelagic fish")

#######
plot_list <- list()
for (i in groups){
  print(i)
  Species_group = length(species_classif$species_name[species_classif$group_article1 == i])
  
  Species_declining = Lmax_mitigation2 %>%
    filter(change_category == "decrease" & group_article1 == i) %>%
    distinct(scenario, species_name) %>%
    count(scenario, name = "n_decrease") 
  
  n_decrease = unique(Species_declining$n_decrease)
  Prop_declining = round(n_decrease/Species_group *100, 0)
  print(Prop_declining)
  
  # Species_increasing = Lmax_join %>%
  #   filter(change_category == "increase")
  ###PLOT - with mpas in the future
  # Count number of species in each category
  
  Species_declining = Lmax_mitigation2 %>%
    filter(change_category == "decrease" & group_article1 == i) %>%
    group_by(species_name) %>%
    mutate(mean_MPA_impact = mean(MPA_impact)) # check - ok same as method 1
  
  Species_declining = Lmax_mitigation2 %>%
    filter(change_category == "decrease" & group_article1 == i) %>%
    group_by(scenario) %>%
    count(MPA_effect, name = "MPA_impact") %>%
    mutate(n_decrase = n_decrease) %>%
    mutate(Mitigation_impact = MPA_impact/n_decrease * 100) %>%
    ungroup()
   
  
  # Species_declining_mitigation = Species_declining %>%
  #   filter(MPA_effect == "mitigation") %>%
  #   summarize(mean_6_scens = mean(Mitigation_impact),
  #          sd_6_scens = sd(Mitigation_impact),
  #          species_group = i)
  
  # Calculate average proportion of species that MPAs help increase or decrease Lmax and associated sd across MPA scenarios
  # mean_MPA_mitigation_impact = mean(Species_declining_mitigation$Mitigation_impact)
  # print(mean_MPA_mitigation_impact)
  # 
  # sd_MPA_mitigation_impact = sd(Species_declining_mitigation$Mitigation_impact)
  # print(sd_MPA_mitigation_impact)
  

  # Plot the proportions -----------------------------------
  if ((length(unique(Species_declining$MPA_effect)) > 1) == TRUE){
    Species_declining_mean_6_scens = Species_declining %>%
      dplyr::select(scenario, MPA_effect, Mitigation_impact) %>%
      pivot_wider(names_from = MPA_effect, values_from = Mitigation_impact) %>%
      mutate(across(c(mitigation, amplification), ~ replace_na(.x, 0))) %>%
      
      mutate(mean_mitigation = mean(mitigation),
             sd_mitigation = sd(mitigation)) %>%
      mutate(mean_amplification = mean(amplification),
             sd_amplification = sd(amplification)) %>%
      mutate(species_group = i)
    
    # Compute labels
    Species_declining_mean_6_scens$label <- paste0(round(Species_declining_mean_6_scens$mean_mitigation, 0), "%")
    
    df_plot = Species_declining_mean_6_scens %>%
      pivot_longer(c(mean_mitigation, mean_amplification), names_to = "MPA_effect", values_to = "Proportion") %>%
      mutate(MPA_effect = case_when(MPA_effect == "mean_mitigation" ~ "mitigation",
                                    TRUE ~ "amplification")) %>%
      distinct(MPA_effect, .keep_all = T)
    
  } else{
    
    MPA_effect = unique(Species_declining$MPA_effect)
    print(MPA_effect)
   
    if (MPA_effect == "amplification"){
      Proportion = c(100, 0)
      MPA_effect = c("amplification", "mitigation")
      df_plot <- data.frame(MPA_effect, Proportion)
      
    }
  
  }

  fig_name <- gsub("-sized", "", i)
  

  # Plot: Donut Chart
  change_Lmax_future_mpas_plot <- ggplot(df_plot, aes(x = 2, y = Proportion, fill = MPA_effect)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    xlim(0.5, 2.5) +  # Makes the hole in the middle
    theme_void() +
    #geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6) +
    scale_fill_manual(values = c("mitigation" = "purple", "amplification" = "darkgreen", "no effect" = "lightyellow")) +
    ggtitle(paste0(fig_name)) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
    annotate("text", x = 0.5, y = 0, label = paste0(Prop_declining, "%"), size = 7, fontface = "bold") 
  
  plot_list[[i]] <- change_Lmax_future_mpas_plot
  
  
  if (i == groups[1]){
    Species_declining_mitigation_all_groups = Species_declining_mean_6_scens
  } else{
    Species_declining_mitigation_all_groups = rbind(Species_declining_mitigation_all_groups, Species_declining_mean_6_scens)
  }
}


# ---- Combine into a 3 x 4 grid ----
plot_list[["legend"]] <- cowplot::plot_grid(shared_legend)

# ---- Combine into a 3 x 4 grid ----
final_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 4)

# ---- Save the plot ----

ggsave(here::here(paste0("figures/Lmax_updated/doughnughts/groups/Lmax_combined_figure_corrected_for_paper_v2_all_6_scens_mean_proportions.png")),
       plot = final_plot, dpi = 600, height = 6, width = 7)


write.csv(Species_declining_mitigation_all_groups, here(paste0("figures/Lmax_updated/doughnughts/groups/Proportions_Lmax_all_6_scens_mean_proportions_corrected.csv")))

### --------------------------------

species_benefitting_from_MPA = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")

species_benefitting_from_MPA_mean_24_scens = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")

species_benefitting_from_MPA_S5 = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")

