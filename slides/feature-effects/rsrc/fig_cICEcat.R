# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(data.table)
source("anova_bike.R")
theme_set(theme_bw())

# DATA -------------------------------------------------------------------------

set.seed(123)
ids = sample(1:nrow(bike), 100)
pred.bike = Predictor$new(mod, data = bike[ids,])
pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp+ice")

eff = FeatureEffect$new(pred.bike, feature = c("season"), method = "pdp+ice")

d = as.data.table(eff$results)
d = d[, list(season = season, .value = .value - .value[season=="SPRING"]), by = c(".type", ".id")]

d = merge(d, data.frame(season_real = bike$season[ids], .id = 1:length(ids)),
  all.x = TRUE)

# PLOT -------------------------------------------------------------------------

cpdcat = ggplot(d[d$.type=="ice"], aes(x = season, y = .value)) +
  geom_path(aes(group = .id), alpha = 0.2) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = "gold", size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = "gold", lty = 2, lwd = 1.5) +
  scale_y_continuous('Predicted bike rentals')

ggsave("../figure/cICEcat.pdf", cpdcat, width = 5.5, height = 3)
