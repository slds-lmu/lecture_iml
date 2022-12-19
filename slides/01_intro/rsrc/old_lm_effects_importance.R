library(effects)
library(mgcViz)
library(mgcv)
library(rpart)
library(rpart.plot)
library(mboost)

load("../../../data/bike.RData")
bike$cnt = as.numeric(bike$cnt)
str(bike)

lm_mod = lm(cnt ~ (hum + temp)^2, data = bike)
sink(file = "../figure/lm_output.txt")
summary(lm_mod)
sink(file = NULL)
png("../figure/lm_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(Effect(c("temp"), lm_mod), type = "response")
dev.off()


glm_mod = glm(cnt ~ (hum + temp)^2, data = bike, family = Gamma(link = "inverse"))
sink(file = "../figure/glm_output.txt")
summary(glm_mod)
sink(file = NULL)
png("../figure/glm_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(Effect(c("temp"), glm_mod), type = "response")
dev.off()


gam_mod = mgcv::gam(cnt ~ s(hum, temp), data = bike, family = gaussian)
sink(file = "../figure/gam_output.txt")
summary(gam_mod)
sink(file = NULL)

vis = getViz(gam_mod)
png("../figure/gam_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(sm(vis, 1))
dev.off()



mboost_mod = mboost(cnt ~ bols(hum) + bols(temp) + bspatial(hum, temp), data = bike)
sink(file = "../figure/mboost_output.txt")
summary(mboost_mod)
sink(file = NULL)
png("../figure/mboost_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(mboost_mod)
dev.off()


rpart_mod = rpart(cnt ~ hum + temp, data = bike)
sink(file = "../figure/rpart_output.txt")
rpart_mod$call
sink(file = NULL)
sink(file = "../figure/rpart_output.txt", append = TRUE)
writeLines("\n")
rpart.rules(rpart_mod)
sink(file = NULL)
png("../figure/rpart_plot.png", width = 12, height = 12, units = "cm", res = 1200)
rpart.plot(rpart_mod)
dev.off()
