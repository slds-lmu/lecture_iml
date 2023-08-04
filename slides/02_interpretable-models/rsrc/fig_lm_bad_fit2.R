library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

set.seed(123)

x1 = c(-10,-5,-1.4,runif(197, 0, 10))
x2 = numeric()
for(i in 1:200){
  if(x1[i] <=5){
    x2[i] = runif(1, 0, 9)
  } else {
    x2[i] = runif(1,1,10)
  }
}
df = data.frame(x1,x2)

p = ggplot(data = df, aes(x1, x2), size = 1.5) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggsave("../figure/lm_bad_fit2.pdf", p,
       width = 5, height = 3)
