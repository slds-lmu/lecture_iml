# PREREQ -----------------------------------------------------------------------

library(ggplot2)
p = readRDS("../figure/ale_scatter.RDS")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------
set.seed(10)

# Generate Pseudo Random Variables
x1 = runif(50, -5, 5)
x2 = x1 + rnorm(50, 0, 1)
df_observed = data.frame(x1, x2)

eps = 0.3
df_cond = subset(df_observed, x1 < (0 + eps) & x1 > (0 - eps))
cond_dens = density(df_cond$x2)
cond_discrete = data.frame(x = cond_dens$x, y = cond_dens$y)
cond_discrete = rbind(cond_discrete, cond_discrete[1,])
ymid = median(with(df_observed, x2[x2>0-0.5 & x2<0+0.5]))
xcond = 0

# LABEL ------------------------------------------------------------------------

label5 = expression(atop(displaystyle(atop(
  paste("ALE plot at"~X[1]~"= 0 averages and"),
  paste("accumulates the local effect ", frac(delta~f(x[1]~X[2]),delta~x[1]), " over"))),
  paste("the conditional distribution of"~X[2]~"|"~X[1]~"="~x[1])
))
label6 = expression(paste("conditional distribution of"~X[2]~"|"~X[1]~"= 0"))

# PLOT -------------------------------------------------------------------------

aleplot = p +
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
  annotate(geom = "label", x = xcond + 0.1, y = ymid - 5, label = label5,
           hjust = "left") +
  annotate(geom = "label", x = -5, y = 5, label = label6,
           hjust = "left") + ylim(c(-10,10))

ggsave("../figure/ale_plot.pdf", aleplot, width = 7,
       height = 4)

