# PREREQ -----------------------------------------------------------------------
library(ggplot2)

# DATA -------------------------------------------------------------------------
set.seed(1234L)
x1 = runif(1000, -1, 1)
x2 = x1^2+rnorm(length(x1), sd = 0.6)
y = 5*x1 + -2*x2 + 3*x1*x2 + rnorm(length(x1))
d = data.frame(y, x1, x2)

rf = randomForest::randomForest(y~x1+x2, d = d)
pred = iml::Predictor$new(rf, data = d, y = y)

# PLOT -------------------------------------------------------------------------
plot(iml::FeatureEffects$new(pred, feature = c("x1","x2"), method = "ale")) + 
  patchwork::plot_annotation(title = "ALE")

ggsave('exercises/feature-effects/figure/ALE_Plot.pdf',height=3,width=6)
