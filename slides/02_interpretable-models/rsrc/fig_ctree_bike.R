library(rpart)
library(rpart.plot)
library(partykit)

load("../../../data/bike.RData")

set.seed(1234)

# Data prep
X = bike[c("yr", "weekday", "mnth", "weathersit", "workingday", "holiday")]
y = bike[,'cnt']
dat = cbind(X, y)


## ctree
c_tree = ctree(y~., data = dat, 
               control = ctree_control(maxdepth = 3, maxsurrogate = 0))
pdf("../figure/bike_ctree.pdf", width = 6, height = 4)
plot(c_tree)
dev.off()


## mob
func <- function(y, x, start = NULL, weights = NULL, offset = NULL) {
  glm(y ~ x, start = start)
  }

mob_tree = mob(y ~ yr | weekday + mnth + weathersit + workingday + holiday, 
               data = dat, fit = func,
               control = mob_control(maxdepth = 3))
pdf("../figure/bike_mob.pdf", width = 6, height = 4)
plot(mob_tree)
dev.off()