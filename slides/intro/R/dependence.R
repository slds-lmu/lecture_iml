# correlation
library(ggplot2)
library(pbv)
library(CopulaModel) # devtools::install_github("vincenzocoia/CopulaModel")
devtools::source_url("https://github.com/UBC-MDS/DSCI_551_stat-prob-dsci/blob/master/supplementary/ggjointmarg.R?raw=TRUE")

set.seed(1)
x = rnorm(200)
y = rnorm(200)
cor = 0.8
p21_layers = list(
  lims(x = c(-3,3), y = c(-3,3)),
  labs(x = expression(X[1]), y = expression(X[2]))
)

xy_seq = seq(-3, 3, length.out = 100)
grid = expand.grid(x = xy_seq, y= xy_seq)
data_cor = cbind(grid, z = dbvn2(grid$x, grid$y, cor))
sample_cor = data.frame(x = x, y = cor*x + sqrt(1 - cor^2) * y)
p1 = ggjointmarg(data_cor, dnorm, dnorm, .sample = sample_cor,
  p21_layers = p21_layers)

sample_norm = data.frame(x = x, y = y)
data_norm = cbind(grid, z = dbvn2(grid$x, grid$y, 0))
p2 = ggjointmarg(data_norm, dnorm, dnorm, .sample = sample_norm,
  p21_layers = p21_layers)

ggsave(filename = "slides/intro/figure/dependent.pdf", p1)
ggsave(filename = "slides/intro/figure/independent.pdf", p2)
