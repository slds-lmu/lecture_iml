library(effects)
library(mgcViz)


data_bike = read.csv("slides/intro/R/day.csv")
summary(data_bike)
lm_mod = lm(cnt ~ (hum + temp)^2, data = data_bike)
summary(lm_mod)


png("slides/intro/figure/lm_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(Effect(c("temp"), lm_mod), type = "response")
dev.off()


glm_mod = glm(cnt ~ (hum + temp)^2, data = data_bike, family = Gamma(link = "inverse"))
summary(glm_mod)
png("slides/intro/figure/glm_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(Effect(c("temp"), glm_mod), type = "response")
dev.off()


gam_mod = gam(cnt ~ s(hum) + s(temp) + s(hum, temp), data = data_bike, family = gaussian)
summary(gam_mod)
vis = getViz(gam_mod)
png("slides/intro/figure/gam_effect_plot_1.png", width = 12, height = 12, units = "cm", res = 1200)
plot(sm(vis, 1))
dev.off()
png("slides/intro/figure/gam_effect_plot_2.png", width = 12, height = 12, units = "cm", res = 1200)
plot(sm(vis, 2))
dev.off()
png("slides/intro/figure/gam_effect_plot_3.png", width = 12, height = 12, units = "cm", res = 1200)
plot(sm(vis, 3))
dev.off()
