library("ggplot2")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

set.seed(123)
load("../../../data/bike.RData")
# bike = na.omit(bike)

mod = lm(cnt ~ season+temp+hum+windspeed+days_since_2011, 
         data = bike)
sum_mod = as.data.frame(summary(mod)$coefficients)
sum_mod$`t value` = abs(sum_mod$`t value`)
sum_mod = sum_mod[order(sum_mod$`t value`),]
sum_mod = sum_mod[rownames(sum_mod)!="(Intercept)",]
features = factor(rownames(sum_mod), levels = rownames(sum_mod))
t_statistics = sum_mod$`t value`

ggplot(sum_mod, aes(y = features, x = t_statistics)) +
  geom_point(size = 3)

ggsave('../figure/t_stat.pdf', width=3, height=1.5)
