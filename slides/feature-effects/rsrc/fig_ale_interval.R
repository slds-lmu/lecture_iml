# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("fig_ale_scatter.R")

# DATA -------------------------------------------------------------------------

inv = seq(floor(min(x1)), ceiling(max(x1)), length.out = 5)
id = which(x1 >= inv[1] & x1 < inv[2])

# LABEL ------------------------------------------------------------------------

interval_lab = c(
  expression(z[0~",1"]),
  expression(z[1~",1"]),
  expression(z[2~",1"]),
  expression(z[3~",1"]),
  expression(z[4~",1"]))

# PLOT -------------------------------------------------------------------------

ale_interval = p + geom_vline(xintercept = inv, colour = "black") +
  geom_segment(aes(x = inv[1], xend = inv[2], y = x2[id], yend = x2[id]), colour = "blue") +
  geom_point(aes(x = inv[1], y = x2[id]), colour = "blue") +
  geom_point(aes(x = inv[2], y = x2[id]), colour = "blue") +
  scale_x_continuous(sec.axis = sec_axis(~., breaks = inv, labels = interval_lab))

ggsave("../figure/ale_interval.pdf", p + ale_interval, 
       width = 10/1.25, height = 4/1.25)
