library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(gower)

set.seed(123)

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
x.interest = credit[1,]
x.interest$risk <- NULL
credit = credit[-1,]

###--- Train model-----
set.seed(123)
task = makeClassifTask(id = "credit",
  data = credit, target = "risk")
lrn = makeLearner("classif.svm", predict.type = "prob")
mod = train(lrn, task)
pred = Predictor$new(mod, data = credit, class = "bad")

###--- Get explanation----
x.interest
pred$predict(x.interest)
n_features_lime = 5
lim = LocalModel$new(pred, x.interest = x.interest, k = n_features_lime, dist.fun = "gower", 
  kernel.width = 0.2)
a = plot(lim)
a 
ggsave(plot = a, filename = "../figure/lime_credit.pdf", height = 2.5, width = 4.5)

#################################################
