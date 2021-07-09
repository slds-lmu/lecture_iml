library(effects)
library(mgcViz)
library(mgcv)

data_bike = read.csv("slides/intro/R/day.csv")
data_bike$cnt = as.numeric(data_bike$cnt)
str(data_bike)

lm_mod = lm(cnt ~ (hum + temp)^2, data = data_bike)
sink(file = "slides/intro/figure/lm_output.txt")
summary(lm_mod)
sink(file = NULL)
png("slides/intro/figure/lm_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(Effect(c("temp"), lm_mod), type = "response")
dev.off()


glm_mod = glm(cnt ~ (hum + temp)^2, data = data_bike, family = Gamma(link = "inverse"))
sink(file = "slides/intro/figure/glm_output.txt")
summary(glm_mod)
sink(file = NULL)
png("slides/intro/figure/glm_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(Effect(c("temp"), glm_mod), type = "response")
dev.off()


gam_mod = mgcv::gam(cnt ~ s(hum, temp), data = data_bike, family = gaussian)
sink(file = "slides/intro/figure/gam_output.txt")
summary(gam_mod)
sink(file = NULL)

vis = getViz(gam_mod)
png("slides/intro/figure/gam_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(sm(vis, 1))
dev.off()



mboost_mod = mboost(cnt ~ bols(hum) + bols(temp) + bspatial(hum, temp), data = data_bike)
sink(file = "slides/intro/figure/mboost_output.txt")
summary(mboost_mod)
sink(file = NULL)
png("slides/intro/figure/mboost_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(mboost_mod)
dev.off()
