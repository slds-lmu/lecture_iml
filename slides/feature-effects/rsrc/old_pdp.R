library(ggplot2)
library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggpubr)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("slides/feature-effects/rsrc/helpers.R")

pdf(file = "slides/feature-effects/figure_man/PD.pdf", width = 5, height = 4)
par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
  split = split, i = 3, col = "gray")

splitx = split(X, X$x)
i = 1
d = splitx[[i]]
col = "red"
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
  labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x[i], X.aggr$dL[i], type = "b", lwd = 3, col = col)

#
par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
  split = split, i = 3, col = "gray")

d = splitx[[2]]
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
  labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x[1:2], X.aggr$dL[1:2], type = "b", lwd = 3, col = col)

#
par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
  split = split, i = 3, col = "gray")

d = splitx[[3]]
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
  labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x, X.aggr$dL, type = "b", lwd = 3, col = col)
dev.off()

######################################################

pred.sub = Predictor$new(mod, data = bike, y = bike$cnt)
pdp = FeatureEffect$new(pred.sub, "temp", method = "ice")
p1 = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals', limits = c(0, 4000))

pdp = Partial$new(pred.bike, "temp", ice = TRUE, aggregation = "pdp")
p1 = pdp$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) + scale_y_continuous('Predicted number of bike rentals')
p1 + xlim(range(bike$temp))

######################################################

set.seed(123)
load("data/bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
pred.bike = Predictor$new(mod, data = bike[sample(1:nrow(bike), 100),])
bike.x = bike[names(bike) != 'cnt']


pred.sub = Predictor$new(mod, data = bike, y = bike$cnt)
pdp = FeatureEffect$new(pred.sub, "temp", method = "pdp+ice")
p1 = pdp$plot() + scale_x_continuous('Temperature') + scale_y_continuous('Predicted bike rentals')
p1
ggsave("slides/feature-effects/figure_man/pdp_num.pdf", p1, width = 6, height = 3)



# Compute the partial dependence for the first feature
eff = FeatureEffect$new(pred.bike,
  feature = c("season"),#, "mnth", "weathersit"),
  method = "pdp+ice")
pd_cat = eff$plot() + #(features = "season") +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = 2, size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = 2, lty = 2, lwd = 1) +
  scale_y_continuous("Predicted number of bike rentals") + ggtitle("PD plot for a categorical feature")

p = eff$plot() #(features = "season")
p$layers[[1]] = NULL
ice_cat = p + geom_path(aes(group = 1), alpha = 0.2) +
  geom_point() +
  #geom_boxplot(alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", aes(group = 1), pch = 4, col = 2, size = 2, stroke = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), col = 2, lwd = 1, lty = 2) +
  scale_y_continuous("Predicted number of bike rentals") + ggtitle("ICE plot for a categorical feature")

pd_cat + ice_cat

ggsave("slides/feature-effects/figure_man/pdp_ice_cat.pdf", pd_cat + ice_cat, width = 8, height = 3)
#eff$plot(features = c("season"))

# library(pdp)
# library(ggplot2)  # required to use autoplot
#
# mod = randomForest(cnt ~ ., data = bike)
# # Partial dependence of cmedv on lstat
# mod %>%
#   partial(pred.var = "season", train = bike) %>%
#   autoplot(rug = TRUE, train = bike)
#
# # ICE curves and c-ICE curves
# age.ice <- partial(mod, pred.var = "season", ice = TRUE)
#
# autoplot(age.ice, alpha = 0.5)
# autoplot(age.ice, center = TRUE, alpha = 0.5)




set.seed(123)
pred.bike = Predictor$new(mod, data = bike)
pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp")
pdp.2feature$plot() +
  geom_point(data = bike, mapping = aes(x = temp, y = hum), alpha = 0.5)


pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "season"), method = "pdp")
library(data.table)
quant = as.data.table(bike)[, .(temp.q1 = quantile(temp, 0.25), temp.q3 = quantile(temp, 0.75)), by = season]
pdp.2feature$plot() +
  geom_point(data = d, aes(x = x, y = y, col = col), alpha = 0.5)

#d.range = as.data.table(bike)[, .(temp.min = quantile(temp, 0), temp.max = quantile(temp, 1)), by = season]
d.split = split(bike$temp, bike$season)
d = as.data.table(pdp.2feature$results)[, list(temp = temp, .value = .value - mean(.value)), by = season]
d
library(patchwork)
pdp.2feature$plot()
ggplot(data = d) + geom_line(aes(x = temp, y = .value, col = season))

pdp.2feature$plot() / ggplot(data = bike) + geom_density(aes(x = temp, col = season))


subs = split(bike$temp, bike$season)

lapply(names(subs), function(fname) {
  fe = FeatureEffect$new(pred_sub, feature = fname, grid.size = grid_size, method = "pdp")
  ret = fe$results
  ret$node = rep(nodes_df[subs_ids[1], ".path"][[1]], times = nrow(ret))

  bpstats = data.frame(stats = boxplot.stats(subs[[fname]])$stats, feature = fname)
  bpstats$.value = fe$predict(bpstats$stats)
  outliers = subs[subs[[fname]] < bpstats$stats[1] |
      subs[[fname]] > bpstats$stats[5],]
  outliers$node = ret$node[1]
  outliers$.value = fe$predict(outliers[[fname]])
  ret$in_hinges = ret[[fname]] >= bpstats$stats[1] &
    ret[[fname]] <= bpstats$stats[5]
  ret$in_box = ret[[fname]] >= bpstats$stats[2] &
    ret[[fname]] <= bpstats$stats[4]
  return(list("pdp" = ret, "outliers" = outliers, "bpstats" = bpstats))
})



pdp = rbindlist(lapply(res, function(x) x$pdp))
outliers = rbindlist(lapply(res, function(x) x$outliers))
bpstats = rbindlist(lapply(res, function(x) x$bpstats))
p = ggplot(data = pdp,
    aes_string(x = fname, y = ".value",
      group = "node", color = "node")) +
    geom_line(data = pdp[pdp$in_hinges,]) +
    geom_line(data = pdp[pdp$in_box,], size = 2) +
    geom_point(data = outliers)
