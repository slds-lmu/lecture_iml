# PREREQ -----------------------------------------------------------------------

library(ggplot2)
theme_set(theme_bw())
source("slides/feature-effects/rsrc/anova_bike.R")

# DATA -------------------------------------------------------------------------
pred.sub = Predictor$new(mod, data = bike.x, y = bike$cnt)
ice = FeatureEffect$new(pred.sub, "temp", method = "pdp+ice")
pdp = FeatureEffect$new(pred.sub, "temp", method = "pdp")

mod = lm(mpg ~ cyl + drat + wt + gear + carb - 1, data = mtcars)
d = as.data.frame(summary(mod)$coefficients)
d$Var = paste("Feat.", 1:nrow(d))
colnames(d)[2] = "sd"
# one obs




# PLOT -------------------------------------------------------------------------
p1 = ice$plot() + xlab("Feature 1")
p2 = pdp$plot() + ylim(1e3, 8e3) + xlab("Feature 1")

p3 = ggplot(d, aes(x = reorder(Var, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = Estimate - sd, ymax = Estimate + sd), width = .2, position = position_dodge(.9)) +
  coord_flip() + xlab("Features") + ylab("Average Marginal Effect (AME)") +
  ggtitle("Global Effect (aggregated)", "AME (e.g., average slope)")

ggsave("slides/feature-effects/figure/feature-effect.pdf", p1 + p2 + p3)
