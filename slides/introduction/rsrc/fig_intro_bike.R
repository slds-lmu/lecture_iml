library(ggpubr)
library(ggcorrplot)
theme_set(theme_bw())
load("../../../data/bike.RData")

p = ggplot(data = bike, aes(y = cnt)) #+ ylim(c(0, 10000))

feats = setdiff(colnames(bike), "cnt")
  #setdiff(colnames(bike)[sapply(bike, is.numeric)], "cnt")

feats = c("yr", "season", "mnth", "weekday", "holiday", "workingday", "weathersit", "temp", "hum", "windspeed", "days_since_2011")

plot = lapply(feats, function(x) {
  if (is.numeric(bike[,x])) {
    p +
      geom_point(aes_string(x = x, col = "yr"))# +
      #stat_cor(method = "pearson", aes_string(x = x, col = "yr"))
  } else {
    if(x != "weathersit") {
      p + geom_boxplot(aes_string(x = x, col = "yr")) + coord_flip()
    } else {
      p + geom_boxplot(aes_string(x = x, col = "yr")) + coord_flip() +
        scale_y_continuous(guide = guide_axis(n.dodge = 2))
    }

    # if(nlevels(bike[,x]) > 2) {
    #   p + geom_boxplot(aes_string(x = x, col = "yr")) + #coord_flip() +
    #     scale_x_discrete(guide = guide_axis(n.dodge = 2))
    # } else {
    #   p + geom_boxplot(aes_string(x = x, col = "yr"))
    # }
  }
})

dens = ggplot(data = bike) + geom_density(aes(x = cnt))

plot = append(plot, list(dens), after = 0)

res = ggarrange(plotlist = plot, common.legend = TRUE, legend = "right")

ggsave("../figure/intro_bike.pdf", res, width = 10, height = 5)
