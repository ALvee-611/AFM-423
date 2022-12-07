library(caret)
library(flextable)
library(MASS)
library(e1071)



# Answer:1


train_data_q1 <- read.csv("h2q1-trn-data.csv")
test_data_q1 <- read.csv("h2q1-tst-data.csv")

index_train <- nrow(train_data_q1)
index_test <- nrow(test_data_q1)


classifier_1 = function (x1,above,below){
  ifelse(x1>0,above,below)
}

classifier_2 = function (x1,x2,above,below){
  ifelse(x2>x1+1,above,below)
}

classifier_3 = function (x1,x2,above,below){
  ifelse (((x2 > x1+1) | (x2 < x1-1)), above, below)
}

classifier_4 = function (x1,x2,above,below){
  ifelse (((x2 > (x1+1)^2) | (x2 < -(x1-1)^2)), above, below)
}

# For classifier 1

trn_pred_c1 <- classifier_1(x=train_data_q1$x1,above="dodgerblue",below="darkorange")
tst_pred_c1 <- classifier_1(x=test_data_q1$x1,above="dodgerblue",below="darkorange")

# For classifier 2

trn_pred_c2 <- classifier_2(x1=train_data_q1$x1,x2=train_data_q1$x2,above="dodgerblue",below="darkorange")
tst_pred_c2 <- classifier_2(x1=test_data_q1$x1,x2=test_data_q1$x2,above="dodgerblue",below="darkorange")

# For classifier 3

trn_pred_c3 <- classifier_3(x1=train_data_q1$x1,x2=train_data_q1$x2,above="dodgerblue",below="darkorange")
tst_pred_c3 <- classifier_3(x1=test_data_q1$x1,x2=test_data_q1$x2,above="dodgerblue",below="darkorange")

# For classifier 4

trn_pred_c4 <- classifier_4(x1=train_data_q1$x1,x2=train_data_q1$x2,above="dodgerblue",below="darkorange")
tst_pred_c4 <- classifier_4(x1=test_data_q1$x1,x2=test_data_q1$x2,above="dodgerblue",below="darkorange")

# Calculating error rates

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

# For classifier 1 (Train error)
trn_error1<-calc_class_err (actual = train_data_q1$y,predicted = trn_pred_c1)

# For classifier 2 (Train error)
trn_error2<-calc_class_err (predicted = trn_pred_c2, actual = train_data_q1$y)

# For classifier 3 (Train error)
trn_error3<-calc_class_err (predicted = trn_pred_c3, actual = train_data_q1$y)

# For classifier 4 (Train error)
trn_error4<-calc_class_err (predicted = trn_pred_c4, actual = train_data_q1$y)


# For classifier 1 (Test error)
tst_error1<-calc_class_err (predicted = tst_pred_c1, actual = test_data_q1$y)

# For classifier 2 (Test error)
tst_error2<-calc_class_err (predicted = tst_pred_c2, actual = test_data_q1$y)

# For classifier 3 (Test error)
tst_error3<-calc_class_err (predicted = tst_pred_c3, actual = test_data_q1$y)

# For classifier 4 (Test error)
tst_error4<-calc_class_err (predicted = tst_pred_c4, actual = test_data_q1$y)

# Forming table:

table <- data.frame(
  Classifier = c("Classifier 1", "Classifier 2", "Classifier 3",
                 "Classifier 4"),
  Classifier_train_error = c(trn_error1, trn_error2,trn_error3,trn_error4),
  Classifier_test_error = c(tst_error1, tst_error2, tst_error3,tst_error4))
t<-delete_part(flextable(table), part = "header")
t<- add_header(t,top=T,Classifier_train_error=" train error rate",
               Classifier_test_error= 'test error rate')

m<- colformat_num(t,j=c(2,3),digits = 4)
autofit(align(m,align = "center", part = "all"))


# Answer: 2


trn_data_q2 <- read.csv("h2q1-trn-data.csv",as.is = 1,header=TRUE)
tst_data_q2 <- read.csv("h2q1-tst-data.csv",as.is=1,header=TRUE)

model_1 = glm(y ~ 1, family = "binomial",data = trn_data_q2) 
model_2 <- glm(y~.,family="binomial",data = trn_data_q2)
model_3 <- glm(y~.+I(x1^2)+I(x2^2),family="binomial",data = trn_data_q2)
model_4 <- glm(y~x1+x2+I(x1^2)+I(x2^2)+x1*x2,family="binomial",data = trn_data_q2)

# create an R function to calculate the error rates for trained the logistic regression 

calc_lr_error = function(model, data) { 
  
  predicted = ifelse(predict(model, data,type = "response") > 0.5, 
                     
                     yes = "dodgerblue", no = "darkorange") 
  
  mean(data$y != predicted) 
  
} 

models = list(model_1,model_2,model_3,model_4) 

results = data.frame( 
  
  c("model 1", "model 2", "model 3", "model 4"), 
  
  sapply(models, calc_lr_error, data = trn_data_q2), 
  
  sapply(models, calc_lr_error, data = tst_data_q2) 
  
) 

colnames(results) = c("Model", "Train Error Rate", "Test Error Rate") 

knitr::kable(results,caption = "Summary of the test and error rates of the models") 


# Answer: 3

set.seed(42)

make_sim_data = function(n_obs = 25) {
  x1 = runif(n = n_obs, min = 0, max = 2)
  x2 = runif(n = n_obs, min = 0, max = 4)
  prob = exp(1 + 2 * x1 - 1 * x2) / (1 + exp(1 + 2 * x1 - 1 * x2))
  y = rbinom(n = n_obs, size = 1, prob = prob)
  data.frame(y, x1, x2)
}


number_of_simulations = 1000
predict_data = data.frame(x1 = 1, x2=1)
predicted = matrix(0, nrow = number_of_simulations, ncol = 3)
for (i in 1:number_of_simulations){
  
  sim_data = make_sim_data()
  
  # The 3 models that we will fit  are as follow:
  Mod_1 <- glm(y~1,family="binomial",data = sim_data)
  Mod_2 <- glm(y~x1+x2,family="binomial",data = sim_data)
  Mod_3 <- glm(y~x1+x2+I((x1)^2)+I((x2)^2)+x1*x2,family="binomial",data = sim_data)
  
  # Saving the above in a matrix so that we can use it later to calculate bias, MSE and variance
  predicted[i, 1] = predict(Mod_1,predict_data,type = "response")
  predicted[i, 2]=  predict(Mod_2, predict_data,type = "response")
  predicted[i, 3] = predict(Mod_3, predict_data,type = "response")
}

# true funciton p(x) as defined by DGP 

p = function(x) {
  with(x, exp(1 + 2 * x1 - 1 * x2) / (1 + exp(1 + 2 * x1 - 1 * x2))
  ) 
} 


# For squared bias
bias = function(predict, actual) {
  mean(predict) - actual
}

MSE = function(predict, actual) {
  mean((predict - actual) ^ 2)
}

var = function(predict) {
  mean((predict - mean(predict)) ^ 2)
}

b<-apply(predicted, 2, bias, p(x = predict_data)) 
v<-apply(predicted, 2, var)
M<-apply(predicted, 2, MSE, p(x = predict_data))

results = data.frame( 
  
  c("Intercept Only", "Additive", "Second Order"), 
  
  round(M, 5), 
  
  round(b ^ 2, 5), 
  
  round(v, 5) 
  
) 
colnames(results) = c("Logistic Regression Model", 
                      
                      "Mean Squared Error", 
                      
                      "Bias Squared", 
                      
                      "Variance") 

rownames(results) = NULL 

knitr::kable(results,caption = "Summary of the Squared Bias, MSE and Variance of the logistic models")  


# Answer 4

library("FNN")

set.seed(314)

train_data_q4 <- read.csv("wisc-trn.csv",as.is=1,header = T)
test_data_q4 <- read.csv("wisc-tst.csv",as.is=1,header = T)


train_data_q4$class <- as.factor(train_data_q4$class)
test_data_q4$class <- as.factor(test_data_q4$class)

trn = train_data_q4
tst =test_data_q4

# M is 1 and B is 0
trn$class = as.numeric(trn$class)-1
tst$class = as.numeric(tst$class)-1

# training data
X_trn = train_data_q4[, -1]
y_trn = train_data_q4$class

# testing data
X_tst = test_data_q4[, -1]
y_tst = test_data_q4$class

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

possible_k <- seq(from = 1, to = 51, by = 2)

err_k = rep(x = 0, times = length(possible_k))
trn_err_k = rep(x = 0, times = length(possible_k))
for (i in seq_along(possible_k)) {
  pred = knn(train = X_trn, test = X_tst, cl = y_trn, k = possible_k[i])
  trn_pred =  knn(train = X_trn, test = X_trn, cl = y_trn, k = possible_k[i])
  err_k[i] = calc_class_err(y_tst, pred)
  trn_err_k[i] = calc_class_err(y_trn, trn_pred)
}

# plot error vs choice of k
plot(x=possible_k,y=err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20,
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors")
par(new=TRUE)
plot( x=possible_k,y=trn_err_k, type = "b", col = "green", cex = 1, pch = 20,
      xaxt='n',yaxt='n',ann=FALSE)

trn = train_data_q4
tst =test_data_q4

# M is 1 and B is 0
trn$class = as.numeric(as.factor(trn$class))-1
tst$class = as.numeric(as.factor(tst$class))-1

#  an additive logistic regression that considers only two predictors, 
# radius and symmetry:
additive_model <- glm(class~radius + symmetry, family = "binomial", data=trn)

cut_off <- c(0.1,0.5,0.9)

glm_pred_1 = ifelse(predict(additive_model, type = "response") > 0.1, "M", "B")
glm_pred_2 = ifelse(predict(additive_model, type = "response") > 0.5, "M", "B")
glm_pred_3 = ifelse(predict(additive_model, type = "response") > 0.9, "M", "B")

library(caret)
train_tab = table(predicted = glm_pred_1, actual = train_data_q4$class)
train_con_mat = confusionMatrix(train_tab, positive = "M")

Model1<-c(train_con_mat$overall["Accuracy"],
          train_con_mat$byClass["Sensitivity"],
          train_con_mat$byClass["Specificity"])


train_tab2 = table(predicted = glm_pred_2, actual = train_data_q4$class)
train_con_mat2 = confusionMatrix(train_tab2, positive = "M")

Model2<-c(train_con_mat2$overall["Accuracy"],
          train_con_mat2$byClass["Sensitivity"],
          train_con_mat2$byClass["Specificity"])

train_tab3 = table(predicted = glm_pred_3, actual = train_data_q4$class)
train_con_mat3 = confusionMatrix(train_tab3, positive = "M")

Model3<-c(train_con_mat$overall["Accuracy"],
          train_con_mat3$byClass["Sensitivity"],
          train_con_mat3$byClass["Specificity"])


ConFusionTable = data.frame( 
  
  cut_off, 
  
  c(Model1[1], Model2[1], Model3[1]), 
  
  c(Model1[2], Model2[2], Model3[2]), 
  
  c(Model1[3], Model2[3], Model3[3]) 
  
) 
colnames(ConFusionTable) = c(" cut-off for predicted probability", 
                             
                             "Accuracy", 
                             
                             "Sensitivity", 
                             
                             "Specificity") 

rownames(ConFusionTable) = NULL 

knitr::kable(ConFusionTable,align = 'c',caption = "Summary of the Test Accuracy, Test Sensitivity and Test Specificity of the logistic models") 





# Answer:5

library(ellipse)

trn_5 <- read.csv("h2q5-trn.csv")
tst_5 <- read.csv("h2q5-tst.csv")

trn_5$y <- as.factor(trn_5$y)
tst_5$y <- as.factor(tst_5$y)


featurePlot(x = trn_5[, c("x1", "x2")],
            y = trn_5$y, plot = "ellipse",
            auto.key = list(columns = 4))


library(MASS)
library(e1071)
library(nnet)

# (a) Additive Logistic Model
add_logModel <- multinom(y~.,family = "binomial", data = trn_5,trace=FALSE)

# (b) LDA (with Priors estimated from data)
lda = lda(y ~ ., data = trn_5)

# (c) LDA with Flat Prior
lda_flat = lda(y ~ ., data = trn_5, prior = c(1, 1, 1, 1) / 4 )

# (d) QDA (with Priors estimated from data)
qda = qda(y ~ ., data = trn_5)

# (e) QDA with Flat Prior
qda_flat = qda(y ~ ., data = trn_5, prior = c(1, 1, 1, 1) / 4 )

# (f) Naive Bayes (with Priors estimated from data)
nb = naiveBayes(y ~ ., data = trn_5)



# To calculate Test error and train error rates


calc_lr_error = function(predicted, actual) { 
  
  mean(actual$y != predicted) 
  
} 

log_predict_trn = predict(add_logModel,trn_5,type = "prob")
log_predict_tst = predict(add_logModel,tst_5,type = "prob")

nb_trn_pred = predict(nb, trn_5)
nb_tst_pred = predict(nb, tst_5)

qda_trn_pred = predict(qda, trn_5)$class
qda_tst_pred = predict(qda, tst_5)$class

qda_flat_trn_pred = predict(qda_flat, trn_5)$class
qda_flat_tst_pred = predict(qda_flat, tst_5)$class

lda_trn_pred = predict(lda, trn_5)$class
lda_tst_pred = predict(lda, tst_5)$class

lda_flat_trn_pred = predict(lda_flat, trn_5)$class
lda_flat_tst_pred = predict(lda_flat, tst_5)$class


model_list = list(add_logModel,lda,lda_flat,qda,qda_flat,nb) 
predictlist_trn = list (log_predict_trn,lda_trn_pred,lda_flat_trn_pred,qda_trn_pred,qda_flat_trn_pred,nb_trn_pred)
predictlist_tst = list (log_predict_tst,lda_tst_pred,lda_flat_tst_pred,qda_tst_pred,qda_flat_tst_pred,nb_tst_pred)
results_q5 = data.frame( 
  
  c("Additive Logistic Model", "LDA (with Priors estimated from data)",
    "LDA with Flat Prior", "QDA (with Priors estimated from data)",
    " QDA with Flat Prior","Naive Bayes (with Priors estimated from data)"), 
  
  sapply(predictlist_trn, calc_lr_error, actual = trn_5), 
  
  sapply(predictlist_tst, calc_lr_error, actual = tst_5) 
  
) 

colnames(results_q5) = c("Model", "Train Error Rate", "Test Error Rate") 

knitr::kable(results_q5, caption = "Summary of the Test and Train error rates of the models") 
