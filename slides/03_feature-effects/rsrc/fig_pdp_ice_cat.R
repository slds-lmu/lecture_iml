# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
source("anova_bike.R")
theme_set(theme_bw())

# DATA -------------------------------------------------------------------------
set.seed(123)
ids = sample(1:nrow(bike), 100)
pred.bike = Predictor$new(mod, data = bike[ids, ])
eff = FeatureEffect$new(pred.bike, feature = c("season"), method = "pdp+ice")

# PLOT -------------------------------------------------------------------------

pd_cat = eff$plot() +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = "gold", size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = "gold", lty = 2, lwd = 1) +
  scale_y_continuous("Predicted number of bike rentals") + ggtitle("PD plot for a categorical feature")

p = eff$plot()
df = data.frame(season_real = bike$season[ids], .id = 1:length(ids))
df = merge(p$data, df, all.x = TRUE)
ggplot(data = df, aes(x = season, y = .value)) +
  geom_line(mapping = aes(group = .id, col = season_real)) #+
  #geom_path(aes(group = .id), alpha = 0.2)

p = eff$plot()
p$layers[[1]] = NULL
ice_cat = p + geom_path(aes(group = .id), alpha = 0.2) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = "gold", size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = "gold", lwd = 1, lty = 2) +
  scale_y_continuous("Predicted number of bike rentals") + ggtitle("ICE plot for a categorical feature")

ggsave("../figure/pdp_ice_cat.pdf", pd_cat + ice_cat, width = 8, height = 3)
