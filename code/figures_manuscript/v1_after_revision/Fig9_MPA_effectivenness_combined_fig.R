fig  <- ggarrange(Delta_comparison_plot,
                  plot_impact_diff_by_ecoregions,
                  ncol = 2,
                  nrow = 1)
print(fig)

ggsave(here("figures/after_revision1/FINAL/Fig9AB_MPA_effectivenness.png"), dpi = 1000, height = 5, width = 10)
