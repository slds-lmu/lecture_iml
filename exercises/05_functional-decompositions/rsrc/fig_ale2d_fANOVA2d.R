# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("anova_bike.R")

# DATA -------------------------------------------------------------------------

pdp = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp", grid.size = 20)
ale = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "ale", grid.size = 20)

pdp_temp = FeatureEffect$new(pred.bike, feature = c("temp"), method = "pdp", grid.size = 20)
pdp_hum = FeatureEffect$new(pred.bike, feature = c("hum"), method = "pdp", grid.size = 20)

pdp_temp
pdp

pdp$plot()
pdp_temp$plot()
pdp_hum$plot()

# fANOVA_component = pdp - pdp_temp - pdp_hum + mean.pred

# This is something completely different:
# https://www.rdocumentation.org/packages/ftsa/versions/6.4/topics/FANOVA

# Function Interaction from iml package
# https://giuseppec.github.io/iml/reference/Interaction.html
# Problem: not usable here, because can only directly compute H-statistic, not the interaction component as a function

# See also fig_h-statistic.R in slides/05_functional-decompositions/rsrc/
# => H-statistic of 0.015

# See in source code of function Interaction from iml package:
# https://github.com/giuseppec/iml/blob/main/R/Interaction.R
# Computing centered PD-fcts. as well as H-statistic happens in function h.test, l. 253 ff.
# => question: Where does adjustment of dimensions take place? apparently before, but shouldn't this happen afterwards?
#
# see l. 172 ff.: partial_j / partial_jk etc. are the data samples for computing PDPs, i.e. the grid for all ICE curves
# afterwards, predictResults contains all values of all the ICE curves
# see l. 292 ff.: pd <- data.table::dcast computes PD-functions from ICE curves
#
# Or is adjusting dimensions happening automatically due to the data.table / the kind of objects used?
#
# Anyway, necessary to compute ICE / PDPs themselves, using feature effect objects is not enough!

# https://giuseppec.github.io/iml/reference/FeatureEffect.html    ???
# Result: Only need to calculate with the following data.tables (or data.frames ?) and afterwards plot them again
pdp$results
pdp_temp$results
pdp_hum$results

# PLOT -------------------------------------------------------------------------

pdp_plot <- pdp$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Bivariate PD") + scale_fill_viridis_b(expression(hat(y)))
fANOVA_component_plot <- fANOVA_component$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Component of classical fANOVA") + scale_fill_viridis_b(expression(hat(y)))
ale_plot <- ale$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Second order ALE") + scale_fill_viridis_b("ALE")

# ggsave("../figure/ale2d.pdf", pdp_plot + ale_plot, width = 8, height = 4)
