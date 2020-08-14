# Logistic Regression

# Importing the dataset
dataset = read.csv('Breast_cancer_data.csv')
dataset = dataset[1:6]

# Encoding the target feature as factor
dataset$diagnosis= factor(dataset$diagnosis, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$diagnosis, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[1:5] = scale(training_set[1:5])
test_set[1:5] = scale(test_set[1:5])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = diagnosis ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[1:5])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[,6 ], y_pred > 0.5)


