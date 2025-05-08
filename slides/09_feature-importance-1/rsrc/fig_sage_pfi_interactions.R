# PREREQ -----------------------------------------------------------------------
library(ggplot2)
library(latex2exp)
theme_set(theme_bw())

# DATA -----------------------------------------------------------------------
# interactions

n = 10000
ntrain = 0.7*n

x1 = sample(c(-1, 1), n, replace=TRUE)
x2 = sample(c(-1, 1), n, replace=TRUE)
x3 = sample(c(-1, 1), n, replace=TRUE)
x4 = sample(c(-1, 1), n, replace=TRUE)

y = x1 * x2 + x3 + rnorm(n)

data = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y=y)

# basepath = "~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/"
# rpath = paste(basepath, "rsrc/", sep='')
# pypath = paste(basepath, "pysrc/", sep='')

write.csv("df_interactions.csv")

df_res = read.csv("../pysrc/df_interactions_res.csv")
colnames(df_res)[3] <- "importance"

# PLOT -----------------------------------------------------------------------
p = ggplot(data=df_res, aes(x=type, y=importance, fill=reorder(feature, -importance))) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin=q.05, ymax=q.95), width=.2, position=position_dodge(.9)) + theme_bw()
p = p + labs(x='IML technique', y='importance (sqrt scale)', fill='feature')
# p = p + scale_y_continuous(trans='sqrt')
p = p + coord_flip()
p

ggsave("../figure_man/sage_pfi_interactions.pdf", height=1.5, width=3)
