# PREREQ -----------------------------------------------------------------------
library("iml")
library("mlr3")
library("mlr3verse")
library("ggplot2")
library("iml")
library("dplyr")
theme_set(theme_bw())

# DATA -----------------------------------------------------------------------
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

lp = '../../05_feature-importance/pysrc/'

df = read.csv(paste0(lp, 'df_res.csv'))
colnames(df)[3] <- "importance"

#df = df[df$type != 'conditional sage',]

# PLOT -----------------------------------------------------------------------
p = ggplot(data=df, aes(x=reorder(type, importance), y=importance, fill=reorder(feature, importance))) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=q.05, ymax=q.95), width=.2, position=position_dodge(.9))

p = p + labs(x='IML technique', y='importance', fill='feature')
p + coord_flip()

ggsave('../figure_man/cfi_pfi.pdf', width=6, height=2)

