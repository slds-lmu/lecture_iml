library(ggplot2)
library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggpubr)
theme_set(theme_bw())

######################################################

set.seed(10)
x1 = runif(20, -5, 5)
x2 = x1 + rnorm(20, 0, 1)
df_observed = data.frame(x1, x2)
df_permuted = data.frame(expand.grid(x1, x2))
names(df_permuted) = c("x1", "x2")

dens = density(x2)
d = data.frame(x = dens$x, y = dens$y)

p1 = ggplot() +
  geom_point(data = df_observed, aes(x1, x2),
    shape = 3, color = "red", size = 2, stroke = 2) +
  theme_bw() + ylim(range(d$x))

p2 = ggplot() +
  geom_point(data = df_permuted, aes(x1, x2),
    shape = 3, color = "green", size = 2, stroke = 1) +
  geom_point(data = df_observed, aes(x1, x2),
    shape = 3, color = "red", size = 2, stroke = 2) +
  theme_bw() + ylim(range(d$x))

dens2 = density(df_observed$x2)
p2 = p2 + geom_path(data = data.frame(x = dens2$x, y = dens2$y),
  aes(x = -5*y - 3, y = x)) +
  geom_line(data = NULL, aes(x = c(-3,-3), y = range(dens2$x))) +
  labs(x = expression(x[1]), y = expression(x[2]))

p = grid.arrange(p1, p2, ncol = 2, respect = TRUE)













library(ggplot2)
library(gridExtra)
theme_set(theme_bw())
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


