library(ggpubr)

spatial_yield_change = map_effect_yield +
  #theme(plot.title = element_blank()) +
  labs(title = "A) CATCH",
       subtitle = "Without MPAs") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
        plot.subtitle = element_text(size = 11, hjust = 0))

spatial_yield_change_MPAs = map_effect_mpa_yield +
  theme(plot.title = element_blank()) +
  labs(subtitle = "MPA mitigation potential") +
  theme(plot.subtitle = element_text(size = 11, hjust = 0))

catch_change <- ggarrange(spatial_yield_change, spatial_yield_change_MPAs, 
                          ncol=1,
                          nrow=2,
                          common.legend = F,
                          legend = "right",
                          align = "hv")


print(catch_change)

spatial_landed_value_change = map_catch_value +
  labs(title = "B) CATCH VALUE",
       subtitle = "Without MPAs") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
        plot.subtitle = element_text(size = 11, hjust = 0))

spatial_landing_value_change_MPAs = map_effect_mpas_catch_value +
  theme(plot.title = element_blank()) +
  labs(subtitle = "MPA mitigation potential") +
  theme(plot.subtitle = element_text(size = 11, hjust = 0))  

landed_value_change <- ggarrange(spatial_landed_value_change, spatial_landing_value_change_MPAs, 
                                 ncol=1,
                                 nrow=2,
                                 common.legend = F,
                                 legend = "right",
                                 align = "hv")
print(landed_value_change)

fig8AB <- plot_grid(catch_change, landed_value_change, ncol = 2)

print(fig8AB)
ggsave(here("figures/Fig8A_B_spatial_catch_catch_value.png"), dpi = 1000, height = 8, width = 15)



fig8_catch <- ggarrange(fig8AB,
                         TL_catch,
                         ncol = 1,
                         nrow = 2)
ggsave(here("figures/fig8_catch_section.png"), dpi = 1000, height = 12, width = 12)
ggsave(here("figures/fig8_catch_section_classif_4_Jan_2026.png"), dpi = 1000, height = 12, width = 12)
