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
               control = ctree_control(maxdepth = 2, maxsurrogate = 0))
pdf("../figure/bike_ctree.pdf", width =11, height = 7)
plot(c_tree)
dev.off()

# rmse
pred = predict(c_tree, newdata = dat)
rmse = mean((pred-dat$y)^2)/2

## mob
func <- function(y, x, start = NULL, weights = NULL, offset = NULL) {
  glm(y ~ x, start = start)
  }

mob_tree = mob(y ~ weathersit + holiday | yr + weekday + mnth + workingday, 
               data = dat, fit = func,
               control = mob_control(maxdepth = 3))
pdf("../figure/bike_mob.pdf", width = 22, height = 7)
plot(mob_tree)
dev.off()

# rmse
pred2 = predict(mob_tree, newdata = dat)
rmse2 = mean((pred2-dat$y)^2)/2
