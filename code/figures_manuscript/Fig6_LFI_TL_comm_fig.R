
library(ggpubr)
fig_size_comm <- ggarrange(LFI_Med_future_present, TL_community_Med_future_present,
                   ncol=2,
                   nrow=1,
                   common.legend = T,
                   legend = "right",
                   align = "hv") 

print(fig_size_comm)
#ggsave(here("figures/LFI_TL_community_for_paper_relative_change.png"), dpi = 600, height = 6, width = 10)
ggsave(here("figures/LFI_TL_community_for_paper_relative_change_v2.png"), dpi = 600, height = 6, width = 10)
ggsave(here("figures/after_revision1/Fig6_LFI_TL_community_diff_y.png"), dpi = 600, height = 6, width = 10)
