# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(mlr3verse)
theme_set(theme_bw())
source("anova_bike.R")

# future::plan("multisession")
#
# set.seed(123)
# load("data/bike.RData")
# tsk = TaskRegr$new(id = "bike", backend = bike, target = "cnt")
#
# cv = rsmp("cv", folds = 4)
# res = resample(tsk, learner = lrn("regr.ranger"), resampling = cv)
# res$aggregate()
#
# lrn = AutoTuner$new(
#   learner = lrn("regr.ranger",
#     num.trees = 500,
#     mtry.ratio = to_tune(0, 1),
#     sample.fraction = to_tune(0, 1)),
#   #learner = lts("regr.ranger.default")$get_learner(),
#   # learner = lrn("regr.ksvm",
#   #   kernel = "rbfdot",
#   #   type = "eps-svr",
#   #   C = to_tune(1e-04, 1e4, logscale = TRUE),
#   #   sigma = to_tune(1e-04, 1e4, logscale = TRUE)),
#   resampling = cv,
#   measure = msr("regr.mse"),
#   terminator = trm("evals", n_evals = 100),
#   tuner = tnr("random_search")
#   #,  search_space = search_space
# )
#
# mod = lrn$train(tsk)
#
# pred.bike = Predictor$new(mod, data = bike)

# DATA -------------------------------------------------------------------------

set.seed(123)

# int = Interaction$new(pred.bike)
# int$plot()
#
# int.temp = Interaction$new(pred.bike, feature = "temp")
# int.temp$plot()

pdp.2feature = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp", grid.size = 30)

pdp.temp = FeatureEffect$new(pred.bike, feature = c("temp"), method = "pdp+ice", grid.size = 30)
pdp.hum = FeatureEffect$new(pred.bike, feature = c("hum"), method = "pdp+ice", grid.size = 30)

ptemp = pdp.temp$results[pdp.temp$results$.type == "pdp",]
phum = pdp.hum$results[pdp.hum$results$.type == "pdp",]
p2d = pdp.2feature$results

# ptemp$.value = ptemp$.value - mean(ptemp$.value)
# phum$.value = phum$.value - mean(phum$.value)
# p2d$.value = p2d$.value - mean(p2d$.value)
#
# p2d.sum = data.frame(
#   temp = ptemp[rep(1:nrow(ptemp), each = nrow(phum)), "temp"],
#   hum = phum[rep(1:nrow(phum), times = nrow(ptemp)), "hum"],
#   .value = ptemp[rep(1:nrow(ptemp), each = nrow(phum)), ".value"] + phum[rep(1:nrow(phum), times = nrow(ptemp)), ".value"])
#
# breaks = seq(round(min(p2d$.value, p2d.sum$.value), -2),
#   round(max(p2d$.value, p2d.sum$.value), -2), by = 500)
# ggplot(data = p2d, aes(x = temp, y = hum)) + geom_tile(aes(fill = .value)) +
#   scale_fill_viridis_b(breaks = breaks)
# ggplot(data = p2d.sum, aes(x = temp, y = hum)) + geom_tile(aes(fill = .value)) +
#   scale_fill_viridis_b(breaks = breaks)


#ggplot(data = p2d[p2d$hum == unique(p2d$hum)[1], ], aes(x = temp, y = .value)) + geom_line()

# plot(p2d$temp[p2d$hum == unique(p2d$hum)[1]], p2d$.value[p2d$hum == unique(p2d$hum)[1]], type = "l", ylim = range(p2d$.value))
# for(i in 1:length(unique(p2d$hum)))
# lines(p2d$temp[p2d$hum == unique(p2d$hum)[i]], p2d$.value[p2d$hum == unique(p2d$hum)[i]])


# PLOT -------------------------------------------------------------------------

#min = round_any(min(pdp.2feature$results$.value), 500, f = floor)
#max = round_any(max(pdp.2feature$results$.value), 500, f = ceiling)

pdp2d_bike = pdp.2feature$plot() +
  geom_point(data = bike, mapping = aes(x = temp, y = hum), alpha = 0.5) +
  ylab("Humidity") + xlab("Temperature") +
  scale_fill_viridis_b("Predicted number \nof bike rentals", n.breaks = 5) +
  ggtitle("2-dim PDP") +
  theme(legend.position = "bottom", legend.key.width = unit(1,"cm")) +
  #guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  #guides(fill = guide_legend(direction = "horizontal")) +
  NULL #+
  #geom_hline(yintercept = quantile(p2d$hum, type = 1), col = 1:5) +
  #geom_vline(xintercept = quantile(p2d$temp, type = 1), col = 1:5)

hum_vals = unique(p2d$hum)[sapply(quantile(bike$hum, type = 1), function(x) which.min(abs(unique(p2d$hum) - x)))]
pdp_temp = ggplot(data = p2d[p2d$hum %in% quantile(p2d$hum, type = 1), ],
  aes(x = temp, y = .value)) +
  geom_line(aes(group = as.factor(hum), col = as.factor(round(hum, 0)))) +
  scale_color_discrete("ICE at humidity") +
  ylab("Predicted number of bike rentals") +
  xlab("Temperature") +
  ggtitle("ICE for temp") + theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  NULL

temp_vals = unique(p2d$temp)[sapply(quantile(bike$temp, type = 1), function(x) which.min(abs(unique(p2d$temp) - x)))]
pdp_hum = ggplot(data = p2d[p2d$temp %in% quantile(p2d$temp, type = 1), ],
  aes(x = hum, y = .value)) +
  geom_line(aes(group = as.factor(temp), col = as.factor(round(temp, 0)))) +
  scale_color_discrete("ICE at temperature") +
  ylab("Predicted number of bike rentals") +
  xlab("Humidity") +
  ggtitle("ICE for humidity") + theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
  NULL

library(patchwork)
plot = pdp2d_bike + pdp_temp + pdp_hum + plot_layout(widths = c(1.5,1,1))
#+ plot_layout(design = layout)

ggsave("../figure/pdp2d_bike.pdf", height = 5, width = 12, plot = plot)
