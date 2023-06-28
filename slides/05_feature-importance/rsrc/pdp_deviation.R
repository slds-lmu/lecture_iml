# PREREQ -----------------------------------------------------------------------

library(patchwork)
library(ggplot2)
theme_set(theme_bw())
source("../../03_feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp_tmp = FeatureEffect$new(pred.bike, "temp", method = "pdp")
pdp_tmp$results$.mean =  mean(pdp_tmp$results$.value)
pdp_ws = FeatureEffect$new(pred.bike, "windspeed", method = "pdp")
pdp_ws$results$.mean =  mean(pdp_ws$results$.value)
# PLOT -------------------------------------------------------------------------

ptmp = pdp_tmp$plot() +
  scale_x_continuous('Temperature', limits = c(0, NA)) +
  geom_hline(aes(yintercept = .mean),
             color = "red", linetype="dashed") +
  geom_ribbon(aes(ymin=.mean,ymax=.value), fill="blue", alpha=0.5) +
  scale_y_continuous('Predicted number of bike rentals',
                     limits = range(pdp_tmp$results$.value)) +
  xlim(range(bike$temp))

pws = pdp_ws$plot() +
  scale_x_continuous('Windspeed', limits = c(0, NA)) +
  geom_hline(aes(yintercept = .mean),
             color = "red", linetype="dashed") +
  geom_ribbon(aes(ymin=.mean,ymax=.value), fill="blue", alpha=0.5) +
  scale_y_continuous('Predicted number of bike rentals',
                     limits = range(pdp_tmp$results$.value)) +
  xlim(range(bike$windspeed))

ggsave("../figure_man/pdps_dev.pdf", height = 2, width = 8, ptmp+pws)
