
library(featureImportance)
library(ggplot2)


# Example dataset
n = 100
set.seed(123)
#X = data.frame("x1" = runif(n), "x2" = runif(n), "x3" = runif(n))
#y = X$x1 + 2*X$x2 + X$x3 + X$x1*X$x2
#data = cbind(X, y)
#features = names(X)


# bike rental dataset
load("../../../data/bike.RData")


# train an ML model
features = c( "hum","temp", "windspeed")
bike_reduced = bike[sample(1:nrow(bike), size = n),]
X = bike_reduced[,features]
#X$workingday = ifelse(X$workingday=="WORKING DAY", 1, 0)
y = bike_reduced$cnt
data = cbind(X,y)
rf = randomForest::randomForest(y~., data)
mlr::measureRSQ(data$y, rf$predicted)
rf$importance


#---------------------------------------------------------------------------------------------------
# calculate exact shapley values for one observation (i)

i = 1
n.perm = 100 # set number of permutations for shapley calculations
random_sample = data[sample((1:nrow(data)), n.perm),]

# create feature permutations
perm_features = featureImportance:::generatePermutations(names(X))


# calculate shapley values for all features
shap_exact = lapply(features, function(x_interest){
  marg_contr = featureImportance:::generateMarginalContribution(x_interest, perm_features)

  feat = lapply(marg_contr, function(x){
    #browser()
    data_w_j = as.data.frame(matrix(as.numeric(rep(X[i,],n.perm)), nrow = n.perm, byrow = TRUE, dimnames = list(c(1:n.perm), features)))
    data_wo_j = as.data.frame(matrix(as.numeric(rep(X[i,],n.perm)), nrow = n.perm, byrow = TRUE, dimnames = list(c(1:n.perm), features)))
    mc_with_f = x$with.f
    mc_without_f = x$without.f

    # replace feature values which are not in S (and j)
    data_w_j[,setdiff(features,mc_with_f)] = random_sample[,setdiff(features,mc_with_f)]
    data_wo_j[,setdiff(features,mc_without_f)] = random_sample[,setdiff(features,mc_without_f)]

    # calculate prediction with ML model for subset including j and without j (value function)
    data_w_j$pred = predict(rf, newdata = data_w_j)
    data_wo_j$pred = predict(rf, newdata = data_wo_j)

    # calculate difference between two predictions (marginal contribution of feature j for this coalition)
    mean(data_w_j$pred)-mean(data_wo_j$pred)


  })
  # since all combinations are calculated here weights are already accounted for and we can calculate the mean to receive
  # shapley value for feature j
  mean(unlist(feat))
})

# shapley values of all 3 features
shapley_values = unlist(shap_exact)

# sum over shapley values plus empty set is the actual prediction of the ML model (only exact for all possible permutations)
sum(shapley_values) + mean(predict(rf, newdata = random_sample))
predict(rf, X[i,])

# plot shapley values
p_shap = ggplot(data = data.frame("feature" = paste(features,"=",round(X[i,],2)), "phi" = shapley_values)) +
  geom_bar(aes(x = phi, y = feature), stat = "identity") +
  ylab("") + ggtitle(paste("Actual prediction: ", round(predict(rf, X[i,]),2), ";\nAverage prediction: ", round(mean(predict(rf, newdata = random_sample)),2)))+
  theme_bw()

# library(fastshap)
# sh = setNames(as.data.frame(t(shapley_values)), colnames(X))
# class(sh) = append(class(sh),"explain")
# mean_pred = mean(predict(rf, newdata = random_sample))
# force_plot(sh, baseline = mean_pred, feature_values = X[i,])

ggsave("../figure/shapley2shap.pdf", p_shap, width = 4, height = 2.5)

#---------------------------------------------------------------------------------------------------
# kernel shap

# generate feature permutations for all features
mc.list = lapply(features, function(x) featureImportance:::generateMarginalContribution(x, perm_features))
mc = unlist(mc.list, recursive = FALSE)

# get all unique sets
values = unique(unname(unlist(mc, recursive = FALSE)))
# remove empty and full set due to missigness and efficiency axiom (weights of kernel will otherwise be infinite)
values = values[!is.na(values)]
#values = values[unlist(lapply(values,function(x) !all(x == features)))]
values = values[-which(sapply(values, function(x) length(x)== length(features)))]

model_matrix = lapply(values, function(x){
  #browser()
  data_perm = as.data.frame(matrix(as.numeric(rep(X[i,],n.perm)), nrow = n.perm, byrow = TRUE, dimnames = list(c(1:n.perm), features)))
  # replace all absent features by random_sample feature values
  data_perm[,setdiff(features,x)] = random_sample[,setdiff(features,x)]
  # predict on permuted dataset
  data_perm$pred = predict(rf, newdata = data_perm)
  #calculate kernel weights
  p = length(features)
  n.z = length(x)
  weights = (p-1)/(choose(p, n.z)* n.z* (p-n.z))

  # return binary feature matrix Z, average prediction for this coaltion and according weights
  as.data.frame(matrix(c(as.integer(features %in% x), mean(data_perm$pred), weights), ncol = ncol(data)+1, dimnames = list(NULL, c(features,"pred", "weights"))))

})

model_matrix = do.call("rbind", model_matrix)
#model_matrix[c(2,5), "weights"] = 1/6
#model_matrix[-c(2,5), "weights"] = 1/9

# calculate Weighted linear regression
#mod = lm(pred~x1+x2+x3, data = model_matrix, weights = weights)

# sum over coefficients and shapley values itself are not exactly the same but close
# intercept and sum should be fullfilled by axioms of shapley -> how do we include these constraints?
#sum(mod$coefficients)

#predict(rf, X[i,])

# sum of coefficients should sum up to constr
constr = predict(rf, X[i,]) - mean(predict(rf, X))

# estimate according to https://stats.stackexchange.com/a/177430/13525 (with the difference that we want the sum of coefficients to be constr and not 1)
mod = lm(I(pred - constr*model_matrix[,3])~I(model_matrix[,1]-model_matrix[,3])+I(model_matrix[,2]-model_matrix[,3])-1, data = model_matrix, weights = weights)
shap = c(mod$coefficients, constr - sum(mod$coefficients))

shap
shapley_values
