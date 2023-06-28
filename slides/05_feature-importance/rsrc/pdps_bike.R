# PREREQ -----------------------------------------------------------------------

library(patchwork)
library(ggplot2)
theme_set(theme_bw())
source("../../03_feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp_tmp = FeatureEffect$new(pred.bike, "temp", method = "pdp")
pdp_ws = FeatureEffect$new(pred.bike, "windspeed", method = "pdp")
# PLOT -------------------------------------------------------------------------

ptmp = pdp_tmp$plot() +
  scale_x_continuous('Temperature', limits = c(0, NA)) +
  geom_hline(yintercept = mean(pdp_tmp$results$.value),
             color = "red", linetype="dashed") +
  scale_y_continuous('Predicted number of bike rentals',
                     limits = range(pdp_tmp$results$.value)) +
  xlim(range(bike$temp))

pws = pdp_ws$plot() +
  scale_x_continuous('Windspeed', limits = c(0, NA)) +
  geom_hline(yintercept = mean(pdp_ws$results$.value),
             color = "red", linetype="dashed") +
  scale_y_continuous('Predicted number of bike rentals',
                     limits = range(pdp_tmp$results$.value)) +
  xlim(range(bike$windspeed))

ggsave("../figure_man/pdps_bike.pdf", height = 2, width = 8, ptmp+pws)
