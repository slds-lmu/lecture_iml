library(ggplot2)

path = "~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/"
setwd(path)

df = read.csv(paste(path,'pysrc/df_res2.csv', sep=''))
colnames(df)[3] <- "importance"


p = ggplot(data=df, aes(x=type, y=importance, fill=reorder(feature, -importance))) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin=q.05, ymax=q.95), width=.2, position=position_dodge(.9)) + theme_bw()
p = p + labs(x='IML technique', y='importance (sqrt scale)', fill='feature')
p = p + scale_y_continuous(trans='sqrt')
p = p + coord_flip()
p

sp = paste(path, 'figure_man/sage_variants.pdf', sep='')

ggsave(sp, width=8, height=4)


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

basepath = "~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/"
rpath = paste(basepath, "rsrc/", sep='')
pypath = paste(basepath, "pysrc/", sep='')

write.csv(data, paste(rpath, 'df_interactions.csv', sep=''))


df_res = read.csv(paste(pypath, 'df_interactions_res.csv', sep=''))
colnames(df_res)[3] <- "importance"

p = ggplot(data=df_res, aes(x=type, y=importance, fill=reorder(feature, -importance))) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin=q.05, ymax=q.95), width=.2, position=position_dodge(.9)) + theme_bw()
p = p + labs(x='IML technique', y='importance (sqrt scale)', fill='feature')
# p = p + scale_y_continuous(trans='sqrt')
p = p + coord_flip()
p

ggsave("../figure_man/pfi_interactions.pdf", height=1.5, width=3)