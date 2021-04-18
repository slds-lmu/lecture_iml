library(ggplot2)
library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggpubr)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

######################################################
set.seed(10)

# Generate Pseudo Random Variables
x1 = runif(50, -5, 5)
x2 = x1 + rnorm(50, 0, 1)
df_observed = data.frame(x1, x2)
df_permuted = data.frame(expand.grid(x1, x2))
names(df_permuted) = c("x1", "x2")

# Generate Densities
marg_dens = density(x2)
marg_discrete = data.frame(x = marg_dens$x, y = marg_dens$y)
marg_discrete = rbind(marg_discrete, marg_discrete[1,])

eps = 0.3
df_cond = subset(df_observed, x1 < (0 + eps) & x1 > (0 - eps))
cond_dens = density(df_cond$x2)
cond_discrete = data.frame(x = cond_dens$x, y = cond_dens$y)
cond_discrete = rbind(cond_discrete, cond_discrete[1,])

# Plot Marginal
label1 = expression(atop(displaystyle(atop(
  paste("M plot averages f(",x[1],", ",X[2],")"),
  paste("over the conditional"))),
  paste("distribution of"~X[2]~"|"~X[1]~"="~x[1])
))
label2 = expression(paste("conditional distribution of"~X[2]~"|"~X[1]~"=0"))

ymid = median(with(df_observed, x2[x2>0-0.5 & x2<0+0.5]))
xcond = 0

p = ggplot() +
  geom_point(data = df_observed, aes(x1, x2), size = 1) +
  labs(x = expression(X[1]), y = expression(X[2]))

mplot = p +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_path(data = cond_discrete, aes(x = -5*y + xcond, y = x + ymid), alpha = 0.5, lwd = 2) +
  annotate(geom = "segment",
    x = 0,
    y = cond_discrete$x[nrow(cond_discrete)/2],
    xend = xcond + 1, yend = ymid - 4,
    arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "segment",
    x = -5*cond_discrete$y[400] + xcond,
    y = cond_discrete$x[400] + ymid,
    xend = -2.5, yend = 4,
    arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "label", x = xcond + 0.1, y = ymid - 5, label = label1,
    hjust = "left") +
  annotate(geom = "label", x = -5, y = 5, label = label2,
    hjust = "left") + ylim(c(-10,10))

# Plot Conditional
label3 = expression(atop(displaystyle(atop(
  paste("PD plot at"~X[1]~"averages f(",X[1],", ",X[2],")"),
  paste("over the marginal"))),
  paste("distribution of"~X[2])
))
label4 = expression(paste("marginal distribution of"~X[2]))

pdplot = p +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_path(data = marg_discrete, aes(x = -5*y + xcond, y = x + ymid), alpha = 0.5, lwd = 2) +
  annotate(geom = "segment",
    x = 0,
    y = marg_discrete$x[nrow(marg_discrete)/2],
    xend = xcond + 1, yend = ymid - 4,
    arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "segment",
    x = -5*marg_discrete$y[300] + xcond,
    y = marg_discrete$x[300] + ymid,
    xend = -2.5, yend = 4,
    arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "label", x = xcond + 0.1, y = ymid - 5, label = label3,
    hjust = "left") +
  annotate(geom = "label", x = -5, y = 5, label = label4,
    hjust = "left") + ylim(c(-10,10))

pgrid = p + ylim(c(-10,10)) +
  geom_point(data = df_permuted, aes(x1, x2, col = "artificial (grid points)"), alpha = 0.3, size = 1, shape = 16) +
  geom_point(data = df_observed, aes(x1, x2, col = "observed data points"), size = 1.5) +
  scale_color_manual(values = c(2,1)) +
  labs(colour = "Data Points") +
  theme(legend.position = c(0.2, 0.875), legend.title = element_blank())
    #legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"))

ggsave("../figure_man/ale_scatter.pdf", p + ylim(c(-10,10)), width = 5.5, height = 4)
ggsave("../figure_man/ale_scatter_grid.pdf", pgrid, width = 5.5, height = 4)
ggsave("../figure_man/ale_mplot.pdf", mplot, width = 5.5, height = 4)
ggsave("../figure_man/ale_pdplot.pdf", pdplot, width = 5.5, height = 4)


##
inv = seq(floor(min(x1)), ceiling(max(x1)), length.out = 5)
id = which(x1 >= inv[1] & x1 < inv[2])
p3 = p + geom_vline(xintercept = inv, colour = "black") +
  geom_segment(aes(x = inv[1], xend = inv[2], y = x2[id], yend = x2[id]), colour = "blue") +
  geom_point(aes(x = inv[1], y = x2[id]), colour = "blue") +
  geom_point(aes(x = inv[2], y = x2[id]), colour = "blue")

interval_lab = c(
  expression(z[0~",1"]),
  expression(z[1~",1"]),
  expression(z[2~",1"]),
  expression(z[3~",1"]),
  expression(z[4~",1"]))

# grid.arrange(p, p3 +
#     scale_x_continuous(sec.axis = sec_axis(~., breaks = inv, labels = interval_lab)), ncol = 2, respect = TRUE)

library(patchwork)
ale_interval = p + p3 + scale_x_continuous(sec.axis = sec_axis(~., breaks = inv, labels = interval_lab))

ggsave("../figure_man/ale_interval.pdf", ale_interval, width = 10/1.25, height = 4/1.25)













set.seed(123)
load("bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
set.seed(123)
pred.bike = Predictor$new(mod, data = bike)
bike.x = bike[names(bike) != 'cnt']

set.seed(123)
pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp", grid.size = 50)
pdp_plot <- pdp$plot() + scale_y_continuous('Univariate PD on humidity')
ale = FeatureEffect$new(pred.bike, "hum", method = "ale", grid.size = 50)
ale_plot <- ale$plot() + scale_y_continuous('First order ALE of humidity')
ale1d = grid.arrange(pdp_plot, ale_plot, nrow = 1, ncol = 2)

ggsave("../figure_man/ale1d.pdf", ale1d, width = 8, height = 4)
###################################################

set.seed(123)
pdp = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "pdp", grid.size = 10)
pdp_plot <- pdp$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Bivariate PD")
ale = FeatureEffect$new(pred.bike, feature = c("temp", "hum"), method = "ale", grid.size = 10)
ale_plot <- ale$plot() + theme(legend.position = "top", legend.key.width = unit(1.5,"cm")) + ggtitle("Second order ALE")
ale2d = grid.arrange(pdp_plot, ale_plot, nrow = 1, ncol = 2)

ggsave("../figure_man/ale2d.pdf", ale2d, width = 8, height = 4)
