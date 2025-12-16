library(mlr)
library(iml)
library(StatMatch)
library(glmnet)

set.seed(123)
source("helpers.R")
svmmod = readRDS("model_svm.rds")

###---- Get data ----
credit = read.csv("german_credit_data.csv", row.names = 1, stringsAsFactors = TRUE)
names(credit)
# omit rows with NA entries
credit = na.omit(credit)
# join groups with small frequencies
levels(credit$Purpose) = c("others", "car", "others", "others",
  "furniture", "radio/TV", "others", "others")
levels(credit$Saving.accounts) = c("little", "moderate", "rich", "rich")
# Colnames to lower
names(credit) = tolower(names(credit))
# Drop levels
credit = droplevels.data.frame(credit)

###---- Define xinterest and training/test data ----
# Separate xinterest from training dataset
id = which.min(predict(svmmod, newdata = credit)$data$prob.good)
x.interest = credit[id,]
x.interest$risk =  NULL
credit = credit[-id,]

###---- prediction as new target
pred = Predictor$new(svmmod, data = credit, class = "good")
pred.pred = pred$predict(x.interest)
newcredit = credit
newcredit[["risk"]] = NULL
gowsim = 1 - c(StatMatch::gower.dist(x.interest, newcredit))
predgood = pred$predict(newdata = credit)[[1]]


### lasso model
k = 3
surmod = glmnet::glmnet(newcredit, y = predgood, weights = gowsim, intercept = TRUE, standardize = TRUE, type.multinomial = "grouped")
X.recode = iml:::recode(newcredit, x.interest)
x.interest.recode = iml:::recode(x.interest, x.interest)

best.index =  max(which(surmod$df == k))
res =  iml:::extract.glmnet.effects(
          surmod$beta, best.index, iml:::recode(x.interest, x.interest),
          x.interest
        )
res =  res[res$beta != 0, ]
pred.surmod.interest = predict(surmod, newx = as.matrix(x.interest.recode))[, best.index, drop = FALSE]

### randomForest
traindata = cbind(newcredit, predgood)
rngmod = ranger::ranger(predgood ~ ., data = traindata, case.weights = gowsim)
predict(rngmod, x.interest)$predictions
gammod = mgcv::gam(predgood ~ s(age) + sex + job + housing + saving.accounts + checking.account + s(credit.amount) + s(duration) + purpose,
          data = traindata, weights = gowsim)
summary(gammod)

# predictions for x.interest
pred.pred  # original
pred.surmod.interest # lasso
predict(gammod, x.interest) # gam

