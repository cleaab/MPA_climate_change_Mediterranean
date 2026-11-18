library(tidyverse)
library(mFD)
library(factoextra)
library(cluster)
library(sf)
library(ncdf4)
library(here)
library(osmose)
library(FactoMineR)

# Compute functional group clusters --------------------------------------------

clusters_csv <- here(
  "data-mpa-CC-paper/clusters_6_groups_Linf_TL_vd_rf_lifepsan_no_amphihaline_with_traits.csv"
)

if (file.exists(clusters_csv)) {
  message("File exists — skipping: ", clusters_csv)
} else {
  
  osmose_med_dir = ("D:/Documents/OSMOSE/osmose-med-articleMPA-CC/") 
  
  # growth parameters
  OSMOSE_MED_Von_Bertalanffy_params_K = read.table(paste0(osmose_med_dir, "Config/osm_param-species.csv"), sep = ";", row.names = NULL, header= FALSE) %>%
    filter(grepl("species.K", V1)) %>%
    mutate(V1 = gsub(".*\\.", "", V1)) %>%# keep only species name
    rename(species_id = V1, K = V2) %>%
    mutate(species_id = substr(species_id, 3, 7)) # keep only species number
  
  OSMOSE_MED_Von_Bertalanffy_params_Linf = read.table(paste0(osmose_med_dir, "Config/osm_param-species.csv"), sep = ";", row.names = NULL, header= FALSE) %>%
    filter(grepl("species.lInf", V1)) %>%
    mutate(V1 = gsub(".*\\.", "", V1)) %>%# keep only species name
    rename(species_id = V1, LInf = V2) %>%
    mutate(species_id = substr(species_id, 3, 7)) # keep only species number
  
  
  OSMOSE_MED_Von_Bertalanffy_params_Legg = read.table(paste0(osmose_med_dir, "Config/osm_param-species.csv"), sep = ";", row.names = NULL, header= FALSE) %>%
    filter(grepl("species.egg.size", V1)) %>%
    mutate(V1 = gsub(".*\\.", "", V1)) %>%# keep only species name
    rename(species_id = V1, Legg = V2) %>%
    mutate(species_id = substr(species_id, 3, 7)) # keep only species number
  
  OSMOSE_MED_Von_Bertalanffy_params_egg_weight = read.table(paste0(osmose_med_dir, "Config/osm_param-species.csv"), sep = ";", row.names = NULL, header= FALSE) %>%
    filter(grepl("species.egg.weight", V1)) %>%
    mutate(V1 = gsub(".*\\.", "", V1)) %>%# keep only species name
    rename(species_id = V1, Wegg = V2) %>%
    mutate(species_id = substr(species_id, 3, 7)) # keep only species number
  
  OSMOSE_MED_lifespan = read.table(paste0(osmose_med_dir, "Config/osm_param-species.csv"), sep = ";", row.names = NULL, header= FALSE) %>%
    filter(grepl("species.lifespan", V1)) %>%
    mutate(V1 = gsub(".*\\.", "", V1)) %>%# keep only species name
    rename(species_id = V1, lifespan = V2) %>%
    mutate(species_id = substr(species_id, 3, 7)) # keep only species number
  
  
  OSMOSE_MED_Lmat = read.table(paste0(osmose_med_dir, "Config/osm_param-species.csv"), sep = ";", row.names = NULL, header= FALSE) %>%
    filter(grepl("species.maturity.size", V1)) %>%
    mutate(V1 = gsub(".*\\.", "", V1)) %>%# keep only species name
    rename(species_id = V1, Lmat = V2) %>%
    mutate(species_id = substr(species_id, 3, 7)) # keep only species number
  
  OSMOSE_MED_relative_fecundity = read.table(paste0(osmose_med_dir, "Config/osm_param-species.csv"), sep = ";", row.names = NULL, header= FALSE) %>%
    filter(grepl("species.relativefecundity", V1)) %>%
    mutate(V1 = gsub(".*\\.", "", V1)) %>%# keep only species name
    rename(species_id = V1, relative_fecundity = V2) %>%
    mutate(species_id = substr(species_id, 3, 7)) # keep only species number
  
  
  # predation ratios
  OSMOSE_MED_predation_ratios_max = read.table(paste0(osmose_med_dir, "Config/osm_param-predation.csv"), sep = ",", row.names = NULL, header= FALSE) %>%
    filter(grepl("predation.predPrey.sizeRatio.max", V1)) %>%
    separate(V1, into = c("species_id", "pred_rate_juvenile_max", "pred_rate_adult_max"), sep = ";") %>%
    mutate(species_id = gsub(".*\\.", "", species_id)) %>% # keep only species name
    mutate(species_id = substr(species_id, 3, 7)) # keep only species number
  
  OSMOSE_MED_predation_ratios_min = read.table(paste0(osmose_med_dir, "Config/osm_param-predation.csv"), sep = ",", row.names = NULL, header= FALSE) %>%
    filter(grepl("predation.predPrey.sizeRatio.min", V1)) %>%
    separate(V1, into = c("species_id", "pred_rate_juvenile_min", "pred_rate_adult_min"), sep = ";") %>%
    mutate(species_id = gsub(".*\\.", "", species_id)) %>% # keep only species name
    mutate(species_id = substr(species_id, 3, 7)) # keep only species number
  
  
  # species classification - used throughout thesis
  sp_order_habitat = read.csv(here("data-mpa-CC-paper/classification_species.csv")) %>%
    mutate(species_id = as.character(species_id)) %>%
    rename(vertical_distribution = vertical_position) %>%
    dplyr::select(species_id, species_name, vertical_distribution, order)
  
  # Trophic levels
  osmose_outputs = read_osmose(here("data-mpa-CC-paper/model_outputs/output_noCC/Trophic"))
  TL = osmose_outputs$meanTL
  mean_TL = apply(TL, c(2), mean, na.rm = T)
  
  ## ----------------------------------------------------------------
  species_traits = sp_order_habitat %>%
    left_join(OSMOSE_MED_Von_Bertalanffy_params_K, by = "species_id") %>%
    left_join(OSMOSE_MED_Von_Bertalanffy_params_Linf, by = "species_id") %>%
    left_join(OSMOSE_MED_Von_Bertalanffy_params_Legg, by = "species_id") %>%
    left_join(OSMOSE_MED_Von_Bertalanffy_params_egg_weight, by = "species_id") %>%
    left_join(OSMOSE_MED_lifespan, by = "species_id") %>%
    left_join(OSMOSE_MED_Lmat, by = "species_id") %>%
    left_join(OSMOSE_MED_relative_fecundity, by = "species_id") %>%
    left_join(OSMOSE_MED_predation_ratios_max, by = "species_id") %>%
    left_join(OSMOSE_MED_predation_ratios_min, by = "species_id") %>%
    
    mutate(LInf = as.numeric(LInf)) %>%
    mutate(K = as.numeric(K)) %>%
    mutate(Legg = as.numeric(Legg)) %>%
    mutate(Wegg = as.numeric(Wegg)) %>%
    mutate(lifespan = as.numeric(lifespan)) %>%
    mutate(Lmat = as.numeric(Lmat)) %>%
    mutate(relative_fecundity = as.numeric(relative_fecundity)) %>%
    mutate(pred_rate_juvenile_max = as.numeric(pred_rate_juvenile_max)) %>%
    mutate(pred_rate_juvenile_min = as.numeric(pred_rate_juvenile_min)) %>%
    mutate(pred_rate_adult_min = as.numeric(pred_rate_adult_min)) %>%
    mutate(pred_rate_adult_max = as.numeric(pred_rate_adult_max)) %>%
    mutate(TL = mean_TL) %>%
    filter(! is.na(TL)) %>% # remove species that crash
    filter(! order == "Krill") %>% # remove euphausids
    filter(! species_id %in% c(0,1,2)) # remove amphihaline species
  
  # traits matrix
  traits_matrix = column_to_rownames(species_traits, var = "species_name") %>%
    dplyr::select(K, LInf, vertical_distribution, lifespan, Lmat, relative_fecundity, TL) %>%
    mutate(vertical_distribution = as.factor(vertical_distribution)) 
  
  trait_name <- colnames(traits_matrix)
  traits_cat = data.frame(trait_name) %>%
    mutate(trait_type = case_when(trait_name %in% c("vertical_distribution") ~ "N",
                                  TRUE ~ "Q"))
  
  # Only select the traits you are keeping
  traits_matrix <- traits_matrix[, c("LInf", "vertical_distribution", "lifespan", "relative_fecundity", "TL")]
  
  famd_result <- FAMD(traits_matrix, graph = T)
  var <- get_famd_var(famd_result)
  
  # Cos2 shows how well a variable is represented
  head(var$cos2)
  famd_result$eig
  
  fviz_screeplot(famd_result)
  fviz_famd_var(famd_result, repel = TRUE)
  head(var$contrib)
  
  trait_scores <- famd_result$ind$coord[, 1:3]  # here keeping first 3 dimensions
  
  dist_matrix <- dist(trait_scores)  # Euclidean distance by default
  hc <- hclust(dist_matrix, method = "ward.D2")  # better for clustering from PCA
  
  plot(hc, labels = rownames(traits_matrix), main = "Hierarchical Clustering of Species")
  
  clusters <- cutree(hclust(dist(trait_scores), method = "ward.D2"), k = 6)
  
  clusters[which(clusters==1)]
  clusters[which(clusters==2)]
  clusters[which(clusters==3)]
  clusters[which(clusters==4)]
  clusters[which(clusters==5)]
  clusters[which(clusters==6)]
  
  cluster_ids <- sort(unique(clusters))
  write.table(clusters, file=here("data-mpa-CC-paper/clusters_6_groups_Linf_TL_vd_rf_lifepsan_no_amphihaline_with_traits.csv"), quote=FALSE,sep=",", dec=".", row.names=TRUE)
  
}

# Compute feve  ----------------------------------------------------------------

feve_NoCC <- here(
  "data-mpa-CC-paper/clusters_6_groups_Linf_TL_vd_rf_lifepsan_no_amphihaline_with_traits.csv"
)

if (file.exists(feve_NoCC)) {
  message("File exists — skipping: ", feve_NoCC)
} else {
  # run from here if clusters already computed --------------------------------------------------------
  clusters = read.csv(here("data-mpa-CC-paper/clusters_6_groups_Linf_TL_vd_rf_lifepsan_no_amphihaline_with_traits.csv")) %>%
    dplyr::select(-X) %>%
    rename(fe = cluster) %>%
    mutate(species_id = as.character(species_id))
  
  
  fe_osmose = species_traits %>%
    left_join(clusters, by = "species_id") %>%
    dplyr::select(species_id, fe) %>%
    mutate(species_id = as.numeric(species_id))
  
  fe_i = unique(fe_osmose$fe)
  unique(fe_i)
  
  
  ####### BIOMASS
  ### NOCC ------------------------------
  
  nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Biomass_nompa_scenario_noCC.nc"))
  biom_noCC = ncvar_get(nc, "Biomass")
  biom_noCC = apply(biom_noCC, c(1,2,3), mean, na.rm = T)
  
  biom_all_fe = c()
  fe_name = c()
  
  for (i in fe_i){
    fe_id = fe_osmose %>%
      filter(fe == i)
    
    fe_sp_id = unique(fe_id$species_id)
    
    biom_sp = biom_noCC[1:190, 1:83, fe_sp_id]
    biom_fe = apply(biom_sp, c(1,2), sum, na.rm = T)
    biom_fe2 = biom_fe[,83:1]
    biom_fe_vect = as.vector(biom_fe2)
    
    
    if (i == fe_i[1]){
      biom_all_fe = data.frame(biom_fe_vect)
      fe_name = i
    } else{
      biom_all_fe = cbind(biom_all_fe, biom_fe_vect)
      fe_name = c(fe_name, i)
    }
  }
  
  colnames(biom_all_fe) = paste0("Cluster_", fe_name)
  biom_all_fe = biom_all_fe %>%
    rownames_to_column("cell_ID")
  
  
  Med_grid = st_read(here("data-mpa-CC-paper/Med_polygon.gpkg")) %>%
    st_drop_geometry() %>%
    rownames_to_column("cell_ID")
  
  occ_fe <- Med_grid %>%
    left_join(biom_all_fe, by = "cell_ID") %>%
    filter(VALUE == 0) %>%
    dplyr::select(-VALUE)
  
  
  occ_fe_matrix = column_to_rownames(occ_fe, var = "cell_ID")
  occ_fe_matrix = as.matrix(occ_fe_matrix)
  
  asb_fe_osmose_summ <- mFD::asb.sp.summary(asb_sp_w = occ_fe_matrix)
  
  
  # Calculate trait centroids for each clusters
  
  # Calculate mean trait scores per cluster
  clusters <- cutree(hclust(dist(trait_scores), method = "ward.D2"), k = 6)
  cluster_ids <- sort(unique(clusters))
  
  cluster_traits <- sapply(cluster_ids, function(i) {
    sp <- names(clusters[clusters == i])
    colMeans(trait_scores[sp, , drop = FALSE])
  })
  
  cluster_traits <- t(cluster_traits)
  rownames(cluster_traits) <- paste0("Cluster_", cluster_ids)
  
  
  # Use clusters as meta-species with mFD
  
  colnames(cluster_traits) = c("PC1", "PC2", "PC3")
  
  alpha_fd_indices_osmose_clusters_feve_nompa_noCC <- mFD::alpha.fd.multidim(
    sp_faxes_coord   = cluster_traits[ ,c("PC1", "PC2")],
    asb_sp_w         = occ_fe_matrix,
    ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                         "fspe", "fide"),
    scaling          = TRUE,
    check_input      = TRUE,
    details_returned = TRUE)
  
  
  # Plot
  
  # fd_ind_values_osmose_fe_clusters_11
  feve_biomass_noCC_nompa <- alpha_fd_indices_osmose_clusters_feve_nompa_noCC$"functional_diversity_indices"
  
  Feve_biomass_noCC_nompa = feve_biomass_noCC_nompa %>%
    rownames_to_column("cell_ID") %>%
    dplyr::select(cell_ID, feve) %>%
    rename(feve_noCC_nompa = feve)
  
  write.csv(Feve_biomass_noCC_nompa, here("data-mpa-CC-paper/model_outputs/functional_evenness_biomass/Feve_biomass_nompa_noCC.csv"))
  
}
  

#### CC ------------------------------------------------------
feve_CC <- here(
  "data-mpa-CC-paper/clusters_6_groups_Linf_TL_vd_rf_lifepsan_no_amphihaline_with_traits.csv"
)

if (file.exists(feve_CC)) {
  message("File exists — skipping: ", feve_CC)
} else {
  nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Biomass_nompa_scenario_CC.nc"))
  biom_CC_nompa = ncvar_get(nc, "Biomass")
  biom_CC_nompa = apply(biom_CC_nompa, c(1,2,3), mean, na.rm = T)
  
  biom_all_fe = c()
  fe_name = c()
  
  for (i in fe_i){
    fe_id = fe_osmose %>%
      filter(fe == i)
    
    fe_sp_id = unique(fe_id$species_id)
    
    biom_sp = biom_CC_nompa[1:190, 1:83, fe_sp_id]
    biom_fe = apply(biom_sp, c(1,2), sum, na.rm = T)
    biom_fe2 = biom_fe[,83:1]
    biom_fe_vect = as.vector(biom_fe2)
    
    
    if (i == fe_i[1]){
      biom_all_fe = data.frame(biom_fe_vect)
      fe_name = i
    } else{
      biom_all_fe = cbind(biom_all_fe, biom_fe_vect)
      fe_name = c(fe_name, i)
    }
  }
  
  colnames(biom_all_fe) = paste0("Cluster_", fe_name)
  biom_all_fe = biom_all_fe %>%
    rownames_to_column("cell_ID")
  
  
  Med_grid = st_read(here("data-mpa-CC-paper/Med_polygon.gpkg")) %>%
    st_drop_geometry() %>%
    rownames_to_column("cell_ID")
  
  occ_fe <- Med_grid %>%
    left_join(biom_all_fe, by = "cell_ID") %>%
    filter(VALUE == 0) %>%
    dplyr::select(-VALUE)
  
  
  occ_fe_matrix = column_to_rownames(occ_fe, var = "cell_ID")
  occ_fe_matrix = as.matrix(occ_fe_matrix)
  
  asb_fe_osmose_summ <- mFD::asb.sp.summary(asb_sp_w = occ_fe_matrix)
  
  
  # Calculate trait centroids for each clusters
  
  # Calculate mean trait scores per cluster
  clusters <- cutree(hclust(dist(trait_scores), method = "ward.D2"), k = 6)
  cluster_ids <- sort(unique(clusters))
  
  cluster_traits <- sapply(cluster_ids, function(i) {
    sp <- names(clusters[clusters == i])
    colMeans(trait_scores[sp, , drop = FALSE])
  })
  
  cluster_traits <- t(cluster_traits)
  rownames(cluster_traits) <- paste0("Cluster_", cluster_ids)
  
  
  # Use clusters as meta-species with mFD
  
  colnames(cluster_traits) = c("PC1", "PC2", "PC3")
  
  alpha_fd_indices_osmose_clusters_feve_nompa_CC <- mFD::alpha.fd.multidim(
    sp_faxes_coord   = cluster_traits[ ,c("PC1", "PC2")],
    asb_sp_w         = occ_fe_matrix,
    ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                         "fspe", "fide"),
    scaling          = TRUE,
    check_input      = TRUE,
    details_returned = TRUE)
  
  
  
  feve_biomass_CC_nompa <- alpha_fd_indices_osmose_clusters_feve_nompa_CC$"functional_diversity_indices"
  
  Feve_biomass_CC_nompa = feve_biomass_CC_nompa %>%
    rownames_to_column("cell_ID") %>%
    dplyr::select(cell_ID, feve) %>%
    rename(feve_CC_nompa = feve)
  
  write.csv(Feve_biomass_CC_nompa, here("data-mpa-CC-paper/model_outputs/functional_evenness_biomass/Feve_biomass_nompa_CC.csv"))
}


#### CC with all 24 scenarios -------------------------------------------------

feve_CC_mpa <- here(
  "data-mpa-CC-paper/model_outputs/functional_evenness_biomass/Feve_biomass_mpa_CC_all_scenarios.csv"
)

if (file.exists(feve_CC_mpa)) {
  message("File exists — skipping: ", feve_CC_mpa)
} else {
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
    
    nc = nc_open(here(paste0("data-mpa-CC-paper/model_outputs/spatial_mpa_10/CC_2100/", mpa_scenario, "/Biomass_mpa_scenario.nc")))
    biomass_mpa_future <- ncvar_get(nc, varid = "Biomass")
    biomass_mpa_future <- apply(biomass_mpa_future, c(1,2,3), mean, na.rm = T) # mean over reps
    
    biom_all_fe = c()
    fe_name = c()
    
    for (i in fe_i){
      fe_id = fe_osmose %>%
        filter(fe == i)
      
      fe_sp_id = unique(fe_id$species_id)
      
      biom_sp = biomass_mpa_future[1:190, 1:83, fe_sp_id]
      biom_fe = apply(biom_sp, c(1,2), sum, na.rm = T)
      biom_fe2 = biom_fe[,83:1]
      biom_fe_vect = as.vector(biom_fe2)
      
      
      if (i == fe_i[1]){
        biom_all_fe = data.frame(biom_fe_vect)
        fe_name = i
      } else{
        biom_all_fe = cbind(biom_all_fe, biom_fe_vect)
        fe_name = c(fe_name, i)
      }
    }
    
    colnames(biom_all_fe) = paste0("Cluster_", fe_name)
    biom_all_fe = biom_all_fe %>%
      rownames_to_column("cell_ID")
    
    Med_grid = st_read(here("data-mpa-CC-paper/Med_polygon.gpkg")) %>%
      st_drop_geometry() %>%
      rownames_to_column("cell_ID")
    
    occ_fe <- Med_grid %>%
      left_join(biom_all_fe, by = "cell_ID") %>%
      filter(VALUE == 0) %>%
      dplyr::select(-VALUE)
    
    
    occ_fe_matrix = column_to_rownames(occ_fe, var = "cell_ID")
    occ_fe_matrix = as.matrix(occ_fe_matrix)
    
    asb_fe_osmose_summ <- mFD::asb.sp.summary(asb_sp_w = occ_fe_matrix)
    
    
    # Calculate trait centroids for each clusters
    
    # Calculate mean trait scores per cluster
    clusters <- cutree(hclust(dist(trait_scores), method = "ward.D2"), k = 6)
    cluster_ids <- sort(unique(clusters))
    
    cluster_traits <- sapply(cluster_ids, function(i) {
      sp <- names(clusters[clusters == i])
      colMeans(trait_scores[sp, , drop = FALSE])
    })
    
    cluster_traits <- t(cluster_traits)
    rownames(cluster_traits) <- paste0("Cluster_", cluster_ids)
    
    
    # Use clusters as meta-species with mFD
    
    colnames(cluster_traits) = c("PC1", "PC2", "PC3")
    
    alpha_fd_indices_osmose_clusters <- mFD::alpha.fd.multidim(
      sp_faxes_coord   = cluster_traits[ ,c("PC1", "PC2")],
      asb_sp_w         = occ_fe_matrix,
      ind_vect         = c("feve"),
      scaling          = TRUE,
      check_input      = TRUE,
      details_returned = TRUE)
    
    
    feve_biomass_CC_mpa <- alpha_fd_indices_osmose_clusters$"functional_diversity_indices"
    feve_biomass_CC_mpa$mpa_scenario = mpa_scenario
    feve_biomass_CC_mpa = feve_biomass_CC_mpa %>%
      rownames_to_column("cell_ID")
    
    if (mpa_scenario == mpa_scenarios[1]){
      feve_biomass_CC_mpa_all_scenarios = feve_biomass_CC_mpa
    } else{
      feve_biomass_CC_mpa_all_scenarios = rbind(feve_biomass_CC_mpa_all_scenarios, feve_biomass_CC_mpa)
    }
  }
  
  length(unique((feve_biomass_CC_mpa_all_scenarios$mpa_scenario)))
  write.csv(feve_biomass_CC_mpa_all_scenarios, here("data-mpa-CC-paper/model_outputs/functional_evenness_biomass/Feve_biomass_mpa_CC_all_scenarios.csv"))
  
  
}



### if feve computed - run from here 

Feve_biomass_noCC_nompa = read.csv(here("data-mpa-CC-paper/model_outputs/functional_evenness_biomass/Feve_biomass_nompa_noCC.csv")) %>%
  dplyr::select(-X) 

Feve_biomass_CC_nompa = read.csv(here("data-mpa-CC-paper/model_outputs/functional_evenness_biomass/Feve_biomass_nompa_CC.csv")) %>%
  dplyr::select(-X) 

Feve_biomass_CC_mpa_all_scenarios = read.csv(here("data-mpa-CC-paper/model_outputs/functional_evenness_biomass/Feve_biomass_mpa_CC_all_scenarios.csv")) %>%
  dplyr::select(-X) %>%
  rename(feve_CC_mpa = feve) 


# Calculate MPA effect on basin-wide biomass for paper
Feve_all_scenarios = Feve_biomass_CC_mpa_all_scenarios %>%
  left_join(Feve_biomass_noCC_nompa, by = "cell_ID") %>%
  left_join(Feve_biomass_CC_nompa, by = "cell_ID") %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "EEZ-conservation",
                                        grepl("Random", mpa_scenario) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%
  
  group_by(mpa_scenario, mpa_scenario_group) %>%
  # basin-wide sum
  summarize(feve_noCC_nompa = sum(feve_noCC_nompa, na.rm = T),
            feve_CC_nompa = sum(feve_CC_nompa, na.rm = T),
            feve_CC_mpa = sum(feve_CC_mpa, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(diff_feve = feve_CC_nompa - feve_noCC_nompa,
         delta_feve = (feve_CC_nompa - feve_noCC_nompa)/feve_noCC_nompa * 100,
         diff_feve_mpa = feve_CC_mpa - feve_noCC_nompa,
         delta_feve_mpa = (feve_CC_mpa - feve_noCC_nompa)/feve_noCC_nompa * 100) %>%
  
  mutate(delta_feve = case_when(feve_noCC_nompa == 0 & feve_CC_nompa == 0 ~ 0,
                                TRUE ~ delta_feve)) %>%
  
  mutate(diff_delta = delta_feve_mpa - delta_feve) %>%
  
  group_by(mpa_scenario_group) %>%
  summarize(delta_feve_6 = mean(delta_feve, na.rm = T),
            delta_feve_mpa_6 = mean(delta_feve_mpa, na.rm = T),
            diff_delta_6 = mean(diff_delta, na.rm = T)) %>%
  
  ungroup() 


mean_diff_delta = mean(Feve_all_scenarios$diff_delta_6, na.rm = T)
print(mean_diff_delta)

sd_diff_delta = sd(Feve_all_scenarios$diff_delta_6, na.rm = T)
print(sd_diff_delta)


# Figures for paper
Feve_biomass_all_scenarios = Feve_biomass_CC_mpa_all_scenarios %>%

  left_join(Feve_biomass_noCC_nompa, by = "cell_ID") %>%
  left_join(Feve_biomass_CC_nompa, by = "cell_ID") %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "EEZ-conservation",
                                        grepl("Random", mpa_scenario) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%
  mutate(diff_feve = feve_CC_nompa - feve_noCC_nompa,
         delta_feve = (feve_CC_nompa - feve_noCC_nompa)/feve_noCC_nompa * 100,
         diff_feve_mpa = feve_CC_mpa - feve_noCC_nompa,
         delta_feve_mpa = (feve_CC_mpa - feve_noCC_nompa)/feve_noCC_nompa * 100) %>%
  
  mutate(delta_feve = case_when(feve_noCC_nompa == 0 & feve_CC_nompa == 0 ~ 0,
                             TRUE ~ delta_feve)) %>%
  
  mutate(diff_delta = delta_feve_mpa - delta_feve) %>%
  
  group_by(cell_ID, mpa_scenario_group) %>%
  summarize(delta_feve_6 = mean(delta_feve, na.rm = T),
            delta_feve_mpa_6 = mean(delta_feve_mpa, na.rm = T),
            diff_delta_6 = mean(diff_delta, na.rm = T)) %>%
  
  ungroup() %>%
  group_by(cell_ID) %>%
  summarize(delta_feve = mean(delta_feve_6, na.rm = T),
            delta_feve_mpa = mean(delta_feve_mpa_6, na.rm = T),
            diff_delta = mean(diff_delta_6, na.rm = T)) %>%
  ungroup() 

Spatial_feve_all_scenarios = Med_grid_GSA_EEZ_df %>%
  mutate(cell_ID = as.numeric(cell_ID)) %>%
  arrange(cell_ID) %>%
  left_join(Feve_biomass_all_scenarios, by = c("cell_ID")) %>%
  mutate(delta_feve_mpa = case_when(VALUE == 0 & is.na(delta_feve_mpa) ~ 0,
                                 TRUE ~ delta_feve_mpa),
         delta_feve = case_when(VALUE == -99 ~ NA,
                             TRUE ~ delta_feve),
         diff_delta = case_when(VALUE == 0 & is.na(diff_delta) ~ 0,
                                TRUE ~ diff_delta)) # so no NANs in the Med and NAN in other seas

#### PLOT
# Plot difference by GSA

all_vals <- c(Spatial_feve_all_scenarios$delta_feve,
              Spatial_feve_all_scenarios$delta_feve_mpa)

common_min <- min(all_vals, na.rm = TRUE)
common_max <- max(all_vals, na.rm = TRUE)

# Plot 1: MPA effect catch change
feve_change_noCC_CC <- ggplot(Spatial_feve_all_scenarios) +
  geom_sf(aes(fill = delta_feve), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +

  
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "deepskyblue4",
    midpoint = 0,
    limits = c(-30, 30),
    oob = scales::squish,
    name = expression(Delta~Feve~"(%)"),
    breaks = c(-30, -15, 0, 15, 30),
    labels = c("≤ -30%", -15, 0, 15, "≥ 30%")) +
  
  
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = "Relative change in total feve between current and future periods",
    fill = expression(Delta~Feve~"(%)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

print(feve_change_noCC_CC)
# Save the plot
ggsave(
  here(paste0("figures/functional_evenness/delta_change/Feve_biomass_change_noCC_CC_mean_6_scens.png")),
  plot = feve_change_noCC_CC,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 2: catch change before MPAs
feve_change_noCC_CC_with_mpas <- ggplot(Spatial_feve_all_scenarios) +
  geom_sf(aes(fill = delta_feve_mpa), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "deepskyblue4",
    midpoint = 0,
    limits = c(-50, 50),
    oob = scales::squish,
    name = expression(Delta~Feve~"(%)"),
    breaks = c(-50, -25, 0, 25, 50),
    labels = c("≤ -50%", -25, 0, 25, "≥ 50%")) +

  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Change in Feve with climate change and 10% MPAs - ", sp_name) ,
    fill = expression(Delta~Feve~"(%)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

print(feve_change_noCC_CC_with_mpas)
# Save the plot
ggsave(
  here(paste0("figures/functional_evenness/delta_change/Feve_biomass_change_noCC_CC_with_mpas_mean_6_scens.png")),
  plot = feve_change_noCC_CC_with_mpas,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 3: diff delta
common_min = min(Spatial_feve_all_scenarios$diff_delta, na.rm = T)
common_max = max(Spatial_feve_all_scenarios$diff_delta, na.rm = T)

map_effect_mpa_feve <- ggplot(Spatial_feve_all_scenarios) +
  
  geom_sf(aes(fill = diff_delta), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  scale_fill_gradient2(
    low = "darkgreen",
    mid = "lightyellow",
    high = "purple",
    midpoint = 0,
    limits = c(-10, 10),
    oob = scales::squish,
    name = expression(Delta~Feve~"(%)"),
    breaks = c(-10, -5, 0, 5, 10),
    labels = c("≤ -10%", -5, 0, 5, "≥ 10%")) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = "MPA impact of Feve change",
    fill = expression(Delta~Feve~"(%)")
  ) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Diff delta - ", sp_name),
    fill = expression(Delta~Feve~"(%)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

print(map_effect_mpa_feve)

# Save the plot
ggsave(
  here(paste0("figures/functional_evenness/delta_change/MPA_effect_feve_biomass_all_scens_mean_6_scens.png")),
  plot = map_effect_mpa_feve,
  dpi = 300,
  width = 10,
  height = 6
)

