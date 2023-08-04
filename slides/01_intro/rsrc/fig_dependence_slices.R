library(ggplot2)
library(purrr)
library(patchwork)
#library(CopulaModel) # devtools::install_github("vincenzocoia/CopulaModel")
devtools::source_url("https://github.com/giuseppec/CopulaModel/blob/master/R/copcdfpdf.R?raw=TRUE")
devtools::source_url("https://github.com/UBC-MDS/DSCI_551_stat-prob-dsci/blob/master/supplementary/ggjointmarg.R?raw=TRUE")
theme_set(theme_bw() + theme(plot.margin = grid::unit(c(1,5.5,1,1), "pt")))


layout <- "
AAA#
AAAB
AAAB
AAAB
AAAC
AAAC
AAAC
AAA#
"

dind = function(x, y) dnorm(x) * dlnorm(y)

slice_vals = data.frame(
  y = c(0.5, 1, 1.5),
  x = c(0, 0.5, 1)
)

slices = list(
  geom_vline(xintercept = slice_vals$x, linetype = 2, col = 2:4),
  geom_hline(yintercept = slice_vals$y, linetype = 2, col = 2:4),
  lims(y = c(-1,4), x = c(-3,3)),
  labs(x = expression(X[1]), y = expression(X[2]))
)

grid = expand.grid(y = seq(-1, 4, length.out = 100),
  x = seq(-3, 3, length.out = 100))
data_ind = cbind(grid, z = dind(grid$x, grid$y))

p = ggjointmarg(data_ind, dnorm, dlnorm, p21_layers = slices)

p1 = ggplot(data_ind, aes(x)) +
  map(slice_vals$y, function(yval)
    stat_function(
      fun     = function(x) dind(x, yval),
      mapping = aes(colour = as.factor(yval))
    )
  ) +
  scale_colour_discrete(expression(X[2]~"values")) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3)) +
  ylab("Density") +
  ggtitle("Horizontal slices") +
  labs(x = expression(X[1]))

p2 = ggplot(data_ind, aes(y)) +
  map(slice_vals$x, function(xval)
    stat_function(
      fun     = function(y) dind(xval, y),
      mapping = aes(colour = as.factor(xval))
    )
  ) +
  scale_colour_discrete(expression(X[1]~"values")) +
  ylab("Density") +
  ggtitle("Vertical slices") +
  labs(x = expression(X[2]))

plot1 = p + p1 + p2 + plot_layout(design = layout) +
  plot_annotation(title = expression('Independent')) &
  theme(legend.position = 'right', plot.title = element_text(hjust = 0.5))

######

ddep = function(x, y) {
  u = pnorm(x)
  v = plnorm(y)
  d = dnorm(x) * dlnorm(y)
  d*dgum(u, v, 3)
  #dbvn2(x, y, 0.8)
}

# slice_vals = data.frame(
#   x = c(-0.5, 0, 0.5),
#   y = c(0.5, 1, 1.5)
# )

slices = list(
  geom_vline(xintercept = slice_vals$x, linetype = 2, col = 2:4),
  geom_hline(yintercept = slice_vals$y, linetype = 2, col = 2:4),
  lims(x = c(-3,3), y = c(-1,4)),
  labs(x = expression(X[1]), y = expression(X[2]))
)

grid = expand.grid(x = seq(-3, 3, length.out = 100),
  y = seq(-1, 4, length.out = 100))
data_dep = cbind(grid, z = ddep(grid$x, grid$y))

p = ggjointmarg(data_dep, dnorm, dlnorm, p21_layers = slices)

p1 = ggplot(data_dep, aes(x)) +
  map(slice_vals$y, function(yval)
    stat_function(
      fun     = function(x) ddep(x, yval),
      mapping = aes(colour = as.factor(yval))
    )
  ) +
  scale_colour_discrete(expression(X[2]~"values")) +
  ylab("Density") +
  ggtitle("Horizontal slices") +
  labs(x = expression(X[1]))

p2 = ggplot(data_dep, aes(y)) +
  map(slice_vals$x, function(xval)
    stat_function(
      fun     = function(y) ddep(xval, y),
      mapping = aes(colour = as.factor(xval))
    )
  ) +
  # scale_colour_manual(values = 2:4,
  #   labels = c(
  #     bquote(X[2] == .(slice_vals$x[1])),
  #     bquote(X[2] == .(slice_vals$x[2])),
  #     bquote(X[2] == .(slice_vals$x[3])))  )  +
  scale_colour_discrete(expression(X[1]~"values")) +
  ylab("Density") +
  ggtitle("Vertical slices") +
  labs(x = expression(X[2]))

#p | (p1 / p2)
plot2 = p + p1 + p2 + plot_layout(design = layout) +
  plot_annotation(title = expression('Dependent')) & # ~P(X[1]~"|"~X[2])!=P(X[1])
  theme(legend.position = 'right', plot.title = element_text(hjust = 0.5))

ggsave(filename = "../figure/independent_slice.pdf", plot1, width = 7, height = 3.5)
ggsave(filename = "../figure/dependent_slice.pdf", plot2, width = 7, height = 3.5)
