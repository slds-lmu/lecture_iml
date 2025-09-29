library(ggplot2)
library(latex2exp)
theme_set(theme_bw())
#path = "~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/"
#setwd(path)

df = read.csv('../pysrc/df_res2.csv')
colnames(df)[3] <- "importance"
#df$importance = abs(df$importance)

x_lab = gsub(" u ", " \\\\cup ", levels(as.factor(df$type)))
#x_lab = gsub(" v\\(-", " \\\\newline v\\(-", x_lab)
x_lab = gsub(" v", " \\\\, v", x_lab)
x_lab = TeX(paste0("$", gsub(" u ", " \\\\cup ", x_lab, "$")))
#TeX(gsub(" u ", " $\\cup$ ", df$type))
#x_lab = 1:4
#x_lab = TeX(paste0("$", 1:12, "\\cdot $"))
p = ggplot(data=df, aes(x=type, y=importance, fill=reorder(feature, -importance))) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=q.05, ymax=q.95), width=.2, position=position_dodge(.9))
p = p + labs(x='IML technique', y='importance (sqrt scale)', fill='feature')
p = p + scale_y_continuous(trans='sqrt') +
  scale_x_discrete(labels = unname(x_lab)) +
  NULL
p = p + coord_flip()
p

ggsave('../figure_man/sage_variants.pdf', width=6, height=2)


# interactions

# interactions

n = 10000
ntrain = 0.7*n

x1 = sample(c(-1, 1), n, replace=TRUE)
x2 = sample(c(-1, 1), n, replace=TRUE)
x3 = sample(c(-1, 1), n, replace=TRUE)
x4 = sample(c(-1, 1), n, replace=TRUE)

y = x1 * x2 + x3 + rnorm(n)

data = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y=y)

write.csv(data, 'df_interactions.csv')


df_res = read.csv('df_interactions_res.csv')
colnames(df_res)[3] <- "importance"

p = ggplot(data=df_res, aes(x=type, y=importance, fill=reorder(feature, -importance))) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin=q.05, ymax=q.95), width=.2, position=position_dodge(.9)) + theme_bw()
p = p + labs(x='IML technique', y='importance (sqrt scale)', fill='feature')
# p = p + scale_y_continuous(trans='sqrt')
p = p + coord_flip()
p

ggsave("../figure_man/pfi_interactions.pdf", height=1.5, width=3)
