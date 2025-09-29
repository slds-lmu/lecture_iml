# PREREQ -----------------------------------------------------------------------
library(latex2exp)
library(patchwork)
library(ggplot2)
set.seed(3)

# DATA -----------------------------------------------------------------------
# Show off how a split in a feature can make distribution more homogeneous
n = 100
x2 = runif(n=n)
x1a = rnorm(n, mean = 0, sd = 1)
x1b = rnorm(n, mean = 4, sd = 4)
x1 = (x2 < 0.5) * 1  * x1a + (x2 >= 0.5) * x1b
df = data.frame(x1 = x1, x2, g = x2 > 0.5)

# PLOT -----------------------------------------------------------------------
p1 = ggplot(df) +
  geom_density(aes(x = x1)) +
  scale_x_continuous(TeX("Feature $x_1$")) +
  scale_y_continuous(TeX("Density of $x_1$"))

p2 = ggplot(df) +
  geom_density(aes(x = x1, fill = g, group = g), alpha = 0.1) +
  scale_fill_discrete(guide = "none") +
  scale_y_continuous("") +
  scale_x_continuous(TeX("Feature $x_1$")) +
  annotate("label", label = TeX("$x_2$ < 0.5"), x = 1.7, y = 0.25, size = 4) +
  annotate("label", label = TeX("$x_2$ >= 0.5"), x = 5, y = 0.1, size = 4)

df2 = df
df2$x1 = sample(df2$x1)
p_scatter = ggplot(data = df, aes(x = x1, y = x2)) +
  geom_point(size = 2)
pplot_marg = p_scatter +
  geom_point(data = df2, shape = 3, size = 2, color = "blue") +
  xlab(TeX("$x_1$")) + ylab(TeX("$x_2$"))

df3 = df
df3$x1 = (x2 < 0.5) * 1  * sample(x1a) + (x2 >= 0.5) * sample(x1b)
pplot_cond = p_scatter +
  geom_point(data = df3, shape = 3, size = 2, color = "blue") +
  geom_hline(yintercept = 0.5, lty = 2) +
  xlab(TeX("$x_1$")) + ylab(TeX("$x_2$"))

res = ((pplot_marg + pplot_cond) / (p1 + p2) + plot_layout(heights = c(2, 1))) & theme(plot.margin = unit(c(0,0,0,0), "pt"))

ggsave('../figure_man/conditional_sampling.pdf', width=7, height=4)
