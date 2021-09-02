library(ggplot2)

path = "~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/"
setwd(path)

df = read.csv(paste(path,'pysrc/df_res2.csv', sep=''))
colnames(df)[3] <- "importance"


p = ggplot(data=df, aes(x=type, y=importance, fill=reorder(feature, -importance))) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin=q.05, ymax=q.95), width=.2, position=position_dodge(.9)) + theme_bw()
p = p + labs(x='IML technique', y='importance', fill='feature')
p = p + coord_flip()
p

sp = paste(path, 'figure_man/sage_variants.pdf', sep='')

ggsave(sp, width=8, height=4)
