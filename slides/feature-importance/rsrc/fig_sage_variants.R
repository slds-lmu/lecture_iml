# PREREQ -----------------------------------------------------------------------
library(ggplot2)
library(latex2exp)
theme_set(theme_bw())
#path = "~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/"
#setwd(path)
#path = "slides/feature-importance/"

# DATA -----------------------------------------------------------------------
df = read.csv("../pysrc/df_res2.csv")
colnames(df)[3] <- "importance"
#df$importance = abs(df$importance)

x_lab = gsub(" u ", " \\\\cup ", levels(as.factor(df$type)))
#x_lab = gsub(" v\\(-", " \\\\newline v\\(-", x_lab)
x_lab = gsub(" v", " \\\\, v", x_lab)
x_lab = TeX(paste0("$", gsub(" u ", " \\\\cup ", x_lab, "$")))
#TeX(gsub(" u ", " $\\cup$ ", df$type))
#x_lab = 1:4
#x_lab = TeX(paste0("$", 1:12, "\\cdot $"))

# PLOT -----------------------------------------------------------------------
p = ggplot(data=df, aes(x=type, y=importance, fill=reorder(feature, -importance))) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=q.05, ymax=q.95), width=.2, position=position_dodge(.9))
p = p + labs(x='IML technique', y='importance (sqrt scale)', fill='feature')
p = p + scale_y_continuous(trans='sqrt') +
  scale_x_discrete(labels = unname(x_lab)) +
  NULL
p = p + coord_flip()
p

sp = paste('../figure_man/sage_variants.pdf')

ggsave(sp, width=6, height=2)
