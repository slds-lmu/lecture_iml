source("bike_example_Data.R")

####################################################################################################
# Decision tree EXAMPLE
####################################################################################################
library(rpart)
library(rpart.plot)

# fit decision tree
tree = rpart(y~., data = dat, control = rpart.control(maxdepth = 3, maxsurrogate = 0))

# create tree plot
pdf("../figure/tree.pdf", width = 6, height = 4)
rpart.plot(tree)
dev.off()

# create feature importance table
xtable(data.frame(tree$variable.importance/sum(tree$variable.importance)*100))
