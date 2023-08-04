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

marg_dens = density(x2)
marg_discrete = data.frame(x = marg_dens$x, y = marg_dens$y)
marg_discrete = rbind(marg_discrete, marg_discrete[1,])
ymid = median(with(df_observed, x2[x2>0-0.5 & x2<0+0.5]))
xcond = 0

# LABEL ------------------------------------------------------------------------

label3 = expression(atop(displaystyle(atop(
  paste("PD plot at"~X[1]~"averages f(",X[1],", ",X[2],")"),
  paste("over the marginal"))),
  paste("distribution of"~X[2])
))
label4 = expression(paste("marginal distribution of"~X[2]))

# PLOT -------------------------------------------------------------------------

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

ggsave("../figure/ale_pdplot.pdf", pdplot, width = 5.5,
       height = 4)