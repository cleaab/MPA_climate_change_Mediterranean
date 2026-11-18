library(ggpubr)

fig_size_comm <- ggarrange(LFI_Med, TL_Med,
                   ncol=2,
                   nrow=1,
                   common.legend = T,
                   legend = "right",
                   align = "hv") 

print(fig_size_comm)

ggsave(here("figures/after_revision1/FINAL/Fig6_LFI_TL_community_barplot_same_scale_hashed.png"), dpi = 1000, height = 6, width = 10)
