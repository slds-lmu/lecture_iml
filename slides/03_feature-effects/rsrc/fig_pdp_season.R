# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(data.table)
library(patchwork)
theme_set(theme_bw())
source("anova_bike.R")

# DATA -------------------------------------------------------------------------

set.seed(123)

pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "season"), method = "pdp")

quant = as.data.table(bike)[, .(temp.q1 = quantile(temp, 0.25), temp.q3 = quantile(temp, 0.75)), by = season]

d.split = split(bike$temp, bike$season)
d = as.data.table(pdp.2feature$results)[, list(temp = temp, .value = .value - mean(.value)), by = season]

# PLOT -------------------------------------------------------------------------

pdp = pdp.2feature$plot() / ggplot(data = bike) + geom_density(aes(x = temp, col = season))

ggsave("../figure/pdp_season.pdf", pdp)
