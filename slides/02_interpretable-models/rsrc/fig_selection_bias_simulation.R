library(rpart)
library(rpart.plot)
library(partykit)

set.seed(1234)

n = 200
y = rnorm(n, 0, 1)
x3 = apply(rmultinom(n, 1,  rep(0.125, 8)), 2, which.max)
x = data.frame(x1 = round(rnorm(n, 0, 1), 4),
  x2 = as.factor(rbinom(n, 1, prob = c(0.5,0.5))),
  x3 = as.factor(x3))

## normal tree
tree = rpart(y~x1+x2+x3, data = x)
pdf("../figure/selection_bias_simulation_tree.pdf", width = 6, height = 4)
rpart.plot(tree)
dev.off()

## ctree
c_tree = ctree(y~x1+x2+x3, data = x)
pdf("../figure/selection_bias_simulation_ctree.pdf", width = 3, height = 2)
plot(c_tree)
dev.off()
