library(randomForest)
library(ggplot2)
source("exercises/feature-effects/rsrc/get_bounds().R")
source("exercises/feature-effects/rsrc/calculate_ale().R")
source("exercises/feature-effects/rsrc/prepare_ale().R")

# Set up your working directory using swd() and get the dataset file. 
df = read.csv(file = 'exercises/feature-effects/rsrc/datasets/wheat_seeds.csv')

# Split the dataset to 70% train data and 30% test data.
set.seed(100)
train = sample(nrow(df), 0.7 * nrow(df), replace = FALSE)
trainData = df[train, ]
testData = df[-train, ]

# Normalize the target to be between 0 and 1.
min_max_norm = function(x) {
  (x - min(x)) / (max(x) - min(x))
}

trainData$Type = min_max_norm(trainData$Type)

# Build a Random Forest model.
model = randomForest(Type ~ ., data = trainData, mtry = 4, importance = TRUE)

# Use the first feature from the dataset and set up
# 4 intervals to test the get_bounds function
bounds = get_bounds(df, 1, 4)

# Test the calculate_ale function, with centered = FALSE
uncentered_ale = calculate_ale(model, df, 1, 4, FALSE)

#Test the prepare_ale function, with centered = TRUE
prepared_ale = prepare_ale(model, df, 1, 4, TRUE)

ggplot(data = prepared_ale, mapping = aes(x = x, y = y)) + 
  geom_line() + 
  geom_point()

ggsave('exercises/feature-effects/figure/example_ale.pdf',width=4,height=3)
