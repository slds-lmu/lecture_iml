library(knitr)
library(ggpubr)
library(xtable)
library(ggeffects)
load("../../../data/bike.RData")
source("helper.R")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# Data prep
X = bike[setdiff(colnames(bike), c("yr", "weekday", "mnth", "cnt", "weathersit", "workingday", "holiday"))]
y = bike[,'cnt']
dat = cbind(X, y)
