source("bike_example_Data.R")
#remotes::install_github("schalkdaniel/compboost", "dev")
library(compboost)
library("mlr3verse")
library("mlr3learners")
library(rpart)
library(rpart.plot)
library(xtable)
options("scipen"=100, "digits"=4)
require(gridExtra)

#### fit compboost model with linear and centered splines ----------------------
set.seed(31415)
cboost = Compboost$new(data = dat, target = "y", learning_rate = 0.1,
                       loss = LossQuadratic$new())#, oob_fraction = 0.2)

cboost$addComponents("temp", df = 4)
cboost$addComponents("days_since_2011", df = 4)

# model
p = list() # storage for every iteration
for(i in 1:5){
  cboost$train(i, trace = i)
  
  # create feature importance plot
  p[[i]] = plotFeatureImportance(cboost) + theme_bw()
}

# FI plot
ggsave("../figure/compboost_pfi.pdf", 
       p[[5]], 
       width = 4.5, height = 2.625)

# single risks
print(xtable(data.frame(iteration = 1:5, risk=cboost$getInbagRisk()[1:5])), 
      include.rownames = FALSE)

# Output
for(i in 1:5) p[[i]] = p[[i]]$data
p = lapply(p, function(x) x = x[order(x$baselearner),])
p
mat = data.frame(iteration = 1:5, baselearner = NA, risk_reduction = NA)
mat[1,2:3] = c(p[[1]]$baselearner, p[[1]]$risk_reduction)
for(i in 2:5){
  if(length(p[[i]]$risk_reduction)==1){
    mat[i,2:3] = c(p[[i]]$baselearner, 
                   p[[i]]$risk_reduction-p[[i-1]]$risk_reduction)
  }else if(length(p[[i]]$risk_reduction)==2 & 
           length(p[[i-1]]$risk_reduction)==1){
    index = (1:2)[p[[i]]$baselearner != p[[i-1]]$baselearner]
    mat[i,2:3] = c(p[[i]]$baselearner[index], p[[i]]$risk_reduction[index])
  }else{
    index = (1:2)[p[[i]]$risk_reduction != p[[i-1]]$risk_reduction]
    mat[i,2:3] = c(p[[i]]$baselearner[index], 
                   p[[i]]$risk_reduction[index]-p[[i-1]]$risk_reduction[index])
  }
}
mat[,3] = as.numeric(mat[,3])
mat_ex = mat
mat_ex$risk_reduction = cboost$getInbagRisk()[1:5]
mat_ex$new_risk = cboost$getInbagRisk()[2:6]
names(mat_ex)[3] = "old_risk"

print(xtable(mat_ex, align = "cclrr"), include.rownames=FALSE) # for task
print(xtable(mat, align = "cclr"), include.rownames=FALSE) # for solution
print(xtable(p[[5]][,-3], align = "clr"), include.rownames=FALSE) # for solution



#### fit tree model ------------------------------------------------------------
# fit decision tree
tree = rpart(y~temp+days_since_2011, data = dat, 
             control = rpart.control(maxsurrogate = 0))

# create tree plot
pdf("../figure/tree.pdf", width = 6, height = 4)
rpart.plot(tree)
dev.off()

# create feature importance table
xtable(data.frame(feature_importance = tree$variable.importance))


sum= print(summary(tree))
