set.seed(2022L)
library("e1071") # SVM 
library("gridExtra") # to plot two ggplots next to each other

dataset = read.csv(file = "exercises/local-explanations/rsrc/datasets/wheat_seeds.csv")
dataset$Type = as.factor(dataset$Type)
table(dataset$Type)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

dataset  = dataset[c("Perimeter", "Asymmetry.Coeff", "Type")]
dataset$Perimeter = min_max_norm(dataset$Perimeter)
dataset$Asymmetry.Coeff = min_max_norm(dataset$Asymmetry.Coeff)

traindata = dataset[sample(seq_len(nrow(dataset)),
                           round(0.6*nrow(dataset)), replace = TRUE),]

# Fit a svm to the data
mod = svm(Type ~ ., data = traindata)
dataset$Type = NULL

# Compute counterfactual for first observation
x_interest = data.frame(Perimeter = 0.31, Asymmetry.Coeff = 0.37)

# Parameters for method
points_per_feature = 50L
n_points = 1000L