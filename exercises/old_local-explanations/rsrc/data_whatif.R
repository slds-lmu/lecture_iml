df = read.csv(file = "exercises/local-explanations/rsrc/datasets/wheat_seeds.csv")
table(df$Type)

# Create a binary classification task
df$Type = as.factor(ifelse(df$Type == "0", 1, df$Type))
table(df$Type)

# Fit a random forest to the data
mod = randomForest::randomForest(Type ~ ., data = df)
df$Type = NULL
# Compute counterfactual for first observation
x_interest = df[1,]