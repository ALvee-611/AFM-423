## Name: Alvee Jawad Chowdhury
## Student ID: 20745516


library(caret)
library(mlbench)

# Answer 1

set.seed(42)
sim_trn = mlbench.spirals(n = 2500, cycles = 1.5, sd = 0.125)
sim_trn = data.frame(sim_trn$x, class = as.factor(sim_trn$classes))
sim_tst = mlbench.spirals(n = 10000, cycles = 1.5, sd = 0.125)
sim_tst = data.frame(sim_tst$x, class = as.factor(sim_tst$classes))


uin = 20745516
set.seed(uin)

cv_5 = trainControl(method = "cv", number = 5)

glm_cv_time = system.time({
  sim_glm_cv  = train(
    class ~ .,
    data = sim_trn,
    trControl = cv_5,
    method = "glm")
})

tree_cv_time = system.time({
  sim_tree_cv = train(
    class ~ .,
    data = sim_trn,
    trControl = cv_5,
    method = "rpart")
})

glm_cv_time["elapsed"]
tree_cv_time["elapsed"]

grid = expand.grid(mtry = c(1, 2))
OOB  = trainControl(method = "oob")

# random forest model using a 5-fold cross-validation procedure
rf_cv_time = system.time({
  rf_cv_model = train(
    class ~ .,
    data = sim_trn,
    trControl = cv_5,
    tuneGrid = grid)
})

# random forest model using OOB samples
rf_oob_time = system.time({
  rf_oob_model = train(
    class ~ .,
    data = sim_trn,
    trControl = OOB,
    tuneGrid = grid)
})


tune = c(NA, sim_tree_cv$bestTune$cp, rf_oob_model$bestTune$mtry,
         rf_cv_model$bestTune$mtry)

time_taken = c(glm_cv_time["elapsed"], 
               tree_cv_time["elapsed"], 
               rf_oob_time["elapsed"], 
               rf_cv_time["elapsed"])

resampled_accuracy = c(max(sim_glm_cv$results$Accuracy),
                       max(sim_tree_cv$results$Accuracy),
                       max(rf_oob_model$results$Accuracy), 
                       max(rf_cv_model$results$Accuracy))

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

glm_cv_accuracy = accuracy(predicted = predict(sim_glm_cv, sim_tst),
                           actual    = sim_tst$class)

tree_cv_accuracy = accuracy(predicted = predict(sim_tree_cv, sim_tst),
                            actual    = sim_tst$class)

rf_cv_accuracy = accuracy(predicted = predict(rf_cv_model, sim_tst),
                          actual    = sim_tst$class)

rf_oob_accuracy = accuracy(predicted = predict(rf_oob_model, sim_tst),
                           actual    = sim_tst$class)

test_accuracy = c(glm_cv_accuracy, tree_cv_accuracy,
                  rf_cv_accuracy, rf_oob_accuracy)

results = data.frame(
  c("Logistic Model with CV", "Single Decision Tree using a 5-fold CV",
    "random forest model using a OOB",
    "random forest model using a 5-fold"),
  tune,
  time_taken,
  resampled_accuracy,
  test_accuracy
)

colnames(results) = c("Method", "Best Tune", "Elapsed",
                      "Resampled Accuracy", "Test Accuracy")
knitr::kable(results,align="c",caption = "Summary of the 4 models")



### Answer 2

library(ISLR)
Hitters = na.omit(Hitters)

uin = 20745516
set.seed(uin)
hit_idx = createDataPartition(Hitters$Salary, p = 0.6, list = FALSE)
hit_trn = Hitters[hit_idx,]
hit_tst = Hitters[-hit_idx,]

gbm_grid = expand.grid(interaction.depth = c(1, 2),
                       n.trees = c(500, 1000, 1500),
                       shrinkage = c(0.001, 0.01, 0.1),
                       n.minobsinnode = 10)
# a) Tune a boosted tree model by using the following tuning grid and
#    5-fold cross-validation procedure.

boosted_tree_model = train(Salary ~ ., data = hit_trn,
                           method = "gbm",
                           trControl = cv_5,
                           verbose = FALSE,
                           tuneGrid = gbm_grid)

# b) Tune a random forest model by using OOB resampling and all possible values
#    of mtry

rf_grid = expand.grid(mtry = 1:(ncol(hit_trn) - 1))
OOB  = trainControl(method = "oob")

random_forest_model  = train(Salary ~ ., data = hit_trn,
                             method = "rf",
                             trControl = OOB,
                             tuneGrid = rf_grid)

Bagged_tree_model = train(Salary ~ ., data = hit_trn,
                          method = "rf",
                          trControl = OOB,
                          tuneGrid = data.frame(mtry = (ncol(hit_trn) - 1)))

calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

boosted_tree_test_rmse = calc_rmse(predicted = predict(boosted_tree_model,
                                                       hit_tst),
                                   actual    = hit_tst$Salary)

random_forest_test_rmse = calc_rmse(predicted = predict(random_forest_model,
                                                        hit_tst),
                                    actual    = hit_tst$Salary)

Bagged_tree_test_rmse = calc_rmse(predicted = predict(Bagged_tree_model,
                                                      hit_tst),
                                  actual    = hit_tst$Salary)

resampled_rmse = c(min(boosted_tree_model$results$RMSE),
                   min(random_forest_model$results$RMSE),
                   min(Bagged_tree_model$results$RMSE))

results_2 = data.frame(
  c("Boosted Tree Model", "Random Forest Model", "Bagged Tree Model"),
  resampled_rmse,
  c(boosted_tree_test_rmse, random_forest_test_rmse, Bagged_tree_test_rmse)
)
colnames(results_2) = c("Method", "Resampled RMSE", "Test RMSE")
knitr::kable(results_2,align="c",caption = "Summary of the 3 models")

### Answer 3

library(ISLR)
library(caret)
uin = 20745516
set.seed(uin)
oj_idx = createDataPartition(OJ$Purchase, p = 0.5, list = FALSE)
oj_trn = OJ[oj_idx,]
oj_tst = OJ[-oj_idx,]


# Tuning a SVM with a linear kernel to the training data by using a
# 5-fold cross-validation procedure.

lin_grid = expand.grid(C = c(2 ^ (-5:5)))

svm_linear_tune <- train(Purchase ~ ., data = oj_trn,
                         method = "svmLinear",
                         trControl = trainControl(method = 'cv', number = 5),
                         preProcess = c('center', 'scale'),
                         tuneGrid = lin_grid)

svm_linear_tune

# test accuracy 
l<-postResample(predict(svm_linear_tune, oj_tst), oj_tst$Purchase)
l

# b) a SVM with a polynomial kernel to the training data by using a 
#    5-fold cross-validation procedure
library(e1071)
svm_polynomial_tune <- svm(Purchase ~ ., data = oj_trn,
                           method = 'polynomial', degree = 2,
                           trControl = trainControl(method = 'cv', number = 5),
                           preProcess = c('center', 'scale'))

summary(svm_polynomial_tune)


# test accuracy 
r_1 = postResample(predict(svm_polynomial_tune, oj_tst), oj_tst$Purchase)
r_1

# c) a SVM with a radial kernel to the training data by using a 5-fold
#    cross-validation procedure and using the following grid of values for
#    C and sigma

rad_grid = expand.grid(C = c(2 ^ (-2:3)), sigma = c(2 ^ (-3:1)))

svm_radial_tune <- train(Purchase ~ ., data = oj_trn,
                         method = 'svmRadial',
                         trControl = trainControl(method = 'cv', number = 5),
                         preProcess = c('center', 'scale'),
                         tuneGrid = rad_grid)
svm_radial_tune


# test accuracy 
r_2 = postResample(predict(svm_radial_tune, oj_tst), oj_tst$Purchase)
r_2

# d) Tune a random forest model by using a 5-fold cross-validation procedure
rf_model = train(Purchase ~ .,
                 data = oj_trn,
                 trControl = trainControl(method = 'cv', number = 5))

rf_model

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

rf_accuracy = accuracy(predicted = predict(rf_model, oj_tst),
                       actual    = oj_tst$Purchase)

rf_accuracy

a<- postResample(predict(svm_polynomial_tune, oj_trn), oj_trn$Purchase)
test_accuracy_2 = c(l[1],r_1[1],r_2[1],rf_accuracy)
resampled_accuracy_2 = c(max(svm_linear_tune$results$Accuracy),
                         max(a[1]),
                         max(svm_radial_tune$results$Accuracy), 
                         max(rf_model$results$Accuracy))


results_3 = data.frame(
  c("SVM", "SVM",
    "SVM",
    "random forest"),
  c("linear","polynomial","radial","N/A"),
  resampled_accuracy_2,
  test_accuracy_2
)

colnames(results_3) = c("Model","Kernel", "Resampled Accuracy", "Test Accuracy")
knitr::kable(results_3,align="c",caption = "Summary of the 4 models")


### Answer 4

# a)
# time taken for rf oob method
rf_oob_time["elapsed"]
# time taken for rf 5-fold cv
rf_cv_time["elapsed"]

# Diiference:
rf_cv_time["elapsed"] / rf_oob_time["elapsed"]


#### Answer: 5
# a)
random_forest_model$bestTune

# b)
library(lattice)
library(ggplot2)
plot(boosted_tree_model)

# c)
library(randomForest)
varImpPlot(random_forest_model$finalModel,
           main = "variable importance for the tuned random forest model")

# d)
library(gbm)
plot(varImp(boosted_tree_model),
     main = "Variable Importance for Boosted tree model")

# e) According to the random forest model, the three most important predictors
#    are:
names(importance(random_forest_model$finalModel)
      [order(importance(random_forest_model$finalModel), decreasing = TRUE),]
      [1:3])

# f) According to the boosted tree model, the three most important predictors
#    are:
library(gbm)
rownames(varImp(boosted_tree_model)$importance)[order(varImp(boosted_tree_model)
                                                      $importance$Overall,
                                                      decreasing = TRUE)][1:3]
