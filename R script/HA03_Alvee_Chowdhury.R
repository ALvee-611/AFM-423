# Answer:1
library(readr)
library(tidyverse)
library(modelr)
library(broom)
library(glmnet)
library(caret)

leukemia = read_csv("leukemia.csv", progress = FALSE)
leukemia <- as.tibble(leukemia)

y = as.factor(leukemia$class)
X = as.matrix(leukemia[, -1])

# a)
set.seed(20745516)

# b) 
# Ridge penalty
ridge <- glmnet(X, y, alpha = 0,family = "binomial")

# LASSO
LASSO <- glmnet(X, y, alpha = 1,family = "binomial")

par(mfrow = c(1, 2))
plot(ridge, xvar = "lambda", label = TRUE, main = "Ridge")
plot(LASSO, xvar = "lambda", label = TRUE, main = "LASSO")

# c)
# cross-validation to tune a logistic regression with a LASSO penalty
LASSO_cv = cv.glmnet(X, y, alpha = 1, family = "binomial", nfolds = 5)

# Storing both the value of lambda that minimizes the deviance, as well as the 
# value of lambda that has a deviance within one standard error.
lambda_lasso = c(LASSO_cv$lambda.min, LASSO_cv$lambda.1se)

# plot of the deviances for each value of lambda
plot(LASSO_cv,main="Lasso")

# create a grid for use with the train() function in the caret package. 
L_lambda <- expand.grid(alpha = 1,lambda = lambda_lasso)

# Use the train() function to get cross-validated classification accuracy
# for these two values of lambda
LASSO_training_data <- train(X, y, method = "glmnet",
                             trControl = trainControl(method = "cv", number = 5),
                             tuneGrid = L_lambda)

# d) 

# cross-validation to tune a logistic regression with a ridge penalty
ridge_cv = cv.glmnet(X, y, alpha = 0, family = "binomial", nfolds = 5)

# Storing both the value of lambda that minimizes the deviance, as well as the 
# value of lambda that has a deviance within one standard error.
lambda_ridge = c(ridge_cv$lambda.min, ridge_cv$lambda.1se)

# plot of the deviances for each value of lambda
plot(ridge_cv,main="Ridge")

# create a grid for use with the train() function in the caret package. 
r_lambda <- expand.grid(alpha = 0,lambda = lambda_ridge)

# Use the train() function to get cross-validated classification accuracy
# for these two values of lambda
ridge_training_data <- train(X, y, method = "glmnet",
                             trControl = trainControl(method = "cv", number = 5),
                             tuneGrid = r_lambda)

# e)

# cross-validation to tune k-nearest neighbors using the train() function 
# in the caret package package
knn_training_data = train(X, y, method = "knn", preProc = c("center", "scale"),
                          trControl = trainControl(method = "cv", number = 5))


lambda_values <- c(lambda_lasso,lambda_ridge,5,7,9)
results_ridge <- ridge_training_data$results
results_lasso <- LASSO_training_data$results
results_knn <- knn_training_data$results

Accuracy <- c(results_lasso$Accuracy,results_ridge$Accuracy,
              results_knn$Accuracy)
AccuracySD <- c(results_lasso$AccuracySD,results_ridge$AccuracySD,
                results_knn$AccuracySD)

results = data.frame( 
  c("LASSO", "LASSO", "Ridge","Ridge","KNN","KNN","KNN"), 
  round(lambda_values,3),
  round(Accuracy,3),round(AccuracySD,3)) 

colnames(results) = c("Models", "lambda/k values", "Accuracy",
                      "standard deviation of Accuracy")
rownames(results) = NULL 

knitr::kable(results,align="c",caption = "Summary of the 7 models")

### Answer 2

set.seed(42)
library(caret)
library(ISLR)
library(randomForest)
index = createDataPartition(College$Outstate, p = 0.75, list = FALSE)
college_trn = College[index, ]
college_tst = College[-index, ]

# checking the dataset
str(College)

cv_5 <- trainControl(method = "cv", number = 5)

idn = 123456789
set.seed(idn)

# An additive linear model.
linear_fit<- train(Outstate ~ ., data = college_trn, method = "lm", 
                   trControl = cv_5)
# An elastic net model using additive predictors using a tuneLength of 10.
elastic_fit<-train(Outstate ~ ., data = college_trn, method = "glmnet", 
                   trControl = cv_5, tuneLength = 10)
# An elastic net model that also considers all two-way interactions. Use a tuneLength of 10.
elastic_all<-train(Outstate ~ . ^ 2, data = college_trn, method = "glmnet", 
                   trControl = cv_5, tuneLength = 10)
# A well-tuned KNN model
knn<- train(Outstate ~ ., data = college_trn, method = "knn", 
            trControl = cv_5, tuneLength = 30, 
            preProcess = c("center", "scale"))
# A well-tuned KNN model that also considers all two-way interactions
knn_all<- train(Outstate ~ . ^ 2, data = college_trn, method = "knn", 
                trControl = cv_5, tuneLength = 30, 
                preProcess = c("center", "scale"))
# A default-tuned random forest.
random_forest<- train(Outstate ~ ., data = college_trn, method = "rf", 
                      trControl = cv_5)

# function to evaluate RMSE
RMSE <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# helper function to extract the row with the best tuning parameters
CV_results <- function(model) {
  a = which(rownames(model$results) == rownames(model$bestTune))
  b = model$results[a, ]
  rownames(b) = NULL
  b
}

cv_RMSE <- c(
  CV_results(linear_fit)$RMSE,
  CV_results(elastic_fit)$RMSE,
  CV_results(elastic_all)$RMSE,
  CV_results(knn)$RMSE,
  CV_results(knn_all)$RMSE,
  CV_results(random_forest)$RMSE)

Test_RMSE <- c(
  RMSE(actual = college_tst$Outstate, predicted = predict(linear_fit, newdata = college_tst)),
  RMSE(actual = college_tst$Outstate, predicted = predict(elastic_fit, newdata = college_tst)),
  RMSE(actual = college_tst$Outstate, predicted = predict(elastic_all, newdata = college_tst)),
  RMSE(actual = college_tst$Outstate, predicted = predict(knn, newdata = college_tst)),
  RMSE(actual = college_tst$Outstate, predicted = predict(knn_all, newdata = college_tst)),
  RMSE(actual = college_tst$Outstate, predicted = predict(random_forest, newdata = college_tst)))

r2 = data.frame( 
  c("additive linear model", "elastic net model",
    "elastic net model (all interactions)","well-tuned KNN",
    "well-tuned KNN(with interactions)","default-tuned random forest"),
  round(cv_RMSE,2),round(Test_RMSE,2)) 

colnames(r2) = c("Models", "CV RMSE", "Test RMSE")
rownames(r2) = NULL 

knitr::kable(r2,align="c",caption = "Summary of the 6 models")

### Answer 6
# a)
# number of observations:
nrow(leukemia)
#number of predictors:
ncol(leukemia)

# g)
# the best tuning parameters for elastic net model
elastic_fit$bestTune

# the best tuning parameters for elastic net model (with interactions)
elastic_all$bestTune

# j) 
# out-of-state tuition at, say, University of Illinois at Urbana-Champaign
a<-filter(College,rownames(College) == "University of Illinois - Urbana")
select(a,Outstate)
