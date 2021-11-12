# PREREQ -----------------------------------------------------------------------

library(ggplot2)
source("slides/feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

eff = FeatureEffect$new(pred.bike, feature = c("season"), method = "pdp+ice")

# PLOT -------------------------------------------------------------------------

pd_cat = eff$plot() +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = 2, size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = 2, lty = 2, lwd = 1) +
  scale_y_continuous("Predicted number of bike rentals") + ggtitle("PD plot for a categorical feature")

p = eff$plot()
p$layers[[1]] = NULL
ice_cat = p + geom_path(aes(group = 1), alpha = 0.2) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = 2, size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = 2, lwd = 1, lty = 2) +
  scale_y_continuous("Predicted number of bike rentals") + ggtitle("ICE plot for a categorical feature")

ggsave("slides/feature-effects/figure/pdp_ice_cat.pdf", pd_cat + ice_cat, width = 8, height = 3)