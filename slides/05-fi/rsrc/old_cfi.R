library("iml")
library("mlr3")
library("mlr3verse")
library("ggplot2")
library("iml")
library("dplyr")
theme_set(theme_bw())

set.seed(123)

n = 1000
ntrain = 0.7 * n

x1 = rnorm(n)
x2 = x1 + rnorm(n, sd=0.01)
x3 = rnorm(n)
x4 = rnorm(n)
y = x3 + rnorm(n, sd=0.1)

data = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y=y)

write.csv(data, 'extrapolation.csv')

lp = '../pysrc/'

df = read.csv('../pysrc/df_res.csv')
colnames(df)[3] <- "importance"

#df = df[df$type != 'conditional sage',]

p = ggplot(data=df, aes(x=reorder(type, importance), y=importance, fill=reorder(feature, importance))) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=q.05, ymax=q.95), width=.2, position=position_dodge(.9))

p = p + labs(x='IML technique', y='importance', fill='feature')
p + coord_flip()

ggsave('../figure_man/cfi_pfi.pdf', width=6, height=2)


library(latex2exp)
set.seed(3)

# Show off how a split in a feature can make distribution more homogeneous
n = 100
x2 = runif(n=n)
x1a = rnorm(n, mean = 0, sd = 1)
x1b = rnorm(n, mean = 4, sd = 4)
x1 = (x2 < 0.5) * 1  * x1a + (x2 >= 0.5) * x1b
df = data.frame(x1 = x1, x2, g = x2 > 0.5)
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

# # explain conditional permutation scheme
# p2 = ggplot(data, aes(x=x1, y=x2)) + geom_hex(bins = 20) + scale_fill_viridis_c() #+ theme_bw()
# p2
#
# data_perm = data.frame(data)
# data_perm$x1 = data_perm$x1[sample(nrow(data_perm))]
#
# p3 = ggplot(data_perm, aes(x=x1, y=x2)) +
#   geom_hex(bins = 20) + scale_fill_viridis_c() #+ theme_bw()
# p3
#
# data_perm = data
# breaks = quantile(data_perm$x2, seq(0,1,by=0.1))
# breaks = seq(min(data_perm$x2), max(data_perm$x2), length = 20)
# data_perm$condition_x2 = findInterval(data_perm$x2, breaks)
# dlist = split(data_perm, data_perm$condition_x2)
# dlist = lapply(dlist, function(x) {
#   x$x1 = x$x1[sample(nrow(x))]
#   return(x)
# })
# data_perm = data.table::rbindlist(dlist)
#
# #data_perm[, x1_sample := sample(x1), by = condition_x2]
# #data_perm$x1 = data_perm$x1[sample(nrow(data_perm))]
# p4 = ggplot(data_perm, aes(x=x1, y=x2)) +
#   geom_hex(bins = 20) + scale_fill_viridis_c() #+ theme_bw()
# p4 = p4 + geom_hline(yintercept = breaks)
#
# p2 + p4 & theme(legend.position = "bottom", aspect.ratio=1)
