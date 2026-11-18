library(ggpubr)
library(cowplot)

spatial_biomass_change = map_effect_biomass +
  #theme(plot.title = element_blank()) +
  labs(title = "A) BIOMASS",
       subtitle = "Without MPAs") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
        plot.subtitle = element_text(size = 11, hjust = 0))

spatial_biomass_change_MPAs = map_mpa_effect_biomass +
  theme(plot.title = element_blank()) +
  labs(subtitle = "MPA effect under future climate conditions") +
  theme(plot.subtitle = element_text(size = 11, hjust = 0))

Biomass_change <- ggarrange(spatial_biomass_change, spatial_biomass_change_MPAs, 
                          ncol=1,
                          nrow=2,
                          common.legend = F,
                          legend = "right",
                          align = "hv")


print(Biomass_change)

spatial_Feve_change = feve_change_noCC_CC +
  labs(title = "B) FUNCTIONAL EVENNESS",
       subtitle = "Without MPAs") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
        plot.subtitle = element_text(size = 11, hjust = 0))

spatial_Feve_change_MPAs = map_effect_mpa_feve +
  theme(plot.title = element_blank()) +
  labs(subtitle = "MPA effect under future climate conditions") +
  theme(plot.subtitle = element_text(size = 11, hjust = 0))  

Feve_change <- ggarrange(spatial_Feve_change, spatial_Feve_change_MPAs, 
                                 ncol=1,
                                 nrow=2,
                                 common.legend = F,
                                 legend = "right",
                                 align = "hv")
print(Feve_change)

fig3 <- plot_grid(Biomass_change, Feve_change, ncol = 2)

print(fig3)

# save file
ggsave(here("figures/after_revision1/FINAL/Fig3_spatial_biomass_Feve_all_scenarios_mean_6_scens_new_scale.png"), dpi = 1000, height = 5, width = 12)
