# PREREQ -----------------------------------------------------------------------

library(ggplot2)
p = readRDS("slides/03_feature-effects/figure/ale_scatter.RDS")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

x1 = p$layers[[1]]$data$x1
x2 = p$layers[[1]]$data$x2
df_observed = data.frame(x1, x2)
marg_dens = density(x2)
marg_discrete = data.frame(x = marg_dens$x, y = marg_dens$y)
marg_discrete = rbind(marg_discrete, marg_discrete[1,])
ymid = median(with(df_observed, x2[x2>0-0.5 & x2<0+0.5]))
xcond = 0

# LABEL ------------------------------------------------------------------------

label3 = expression(atop(displaystyle(atop(
  paste("PD plot at"~x[S]~"averages f(",x[S],", ",x[-S],")"),
  paste("over the marginal"))),
  paste("distribution of"~x[-S])
))
label4 = expression(paste("marginal distribution of"~x[-S]))

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
           hjust = "left") + 
  xlab(expression(x[S])) + ylab(expression(x[-S]))

ggsave("slides/03_feature-effects/figure/ale_pdplot.pdf", pdplot, width = 5.5,
       height = 4)