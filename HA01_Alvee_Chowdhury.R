# Answer 1

# Set up

library(flextable)

set.seed(42)
h1q1 <- read.csv("h1q1.csv")
train_index = sample(1:nrow(h1q1), size = round(0.5 * nrow(h1q1)))
train_data = h1q1[train_index, ]
test_data = h1q1[-train_index, ]

#Fitting the 4 models:

Model_1 <- lm(y ~ .,data=train_data)
Model_2 <- lm(y ~ . + I(a^2) + I(b^2) + I(c^2), data= train_data)
Model_3 <- lm(y ~ . ^ 2 + I(a^2) + I(b^2) + I(c^2), data=train_data)
Model_4 <- lm( y ~ a * b * c * d + I(a^2) + I(b^2) + I(c^2), data= train_data)

# Calculates RMSE
rmse = function (actual,predicted){
  sqrt(mean((actual - predicted)^2))
}
get_rmse = function (model,data, response){
  rmse (actual = subset(data, select = response, drop = T),
        predicted = predict(model,data))
}
# Calculates the number of parameters, excluding the variance parameter
# of the model:
parameter_number = function (model) {
  length(coef(model)) - 1
}

# Now let us obtain the train RMSE,test RMSE and number of parameters
# (excluding variance parameter) for each model defined above:

# (a) train RMSE
model_1_train_RMSE <- get_rmse (model = Model_1, data = train_data,
                                response = 'y')
model_2_train_RMSE <- get_rmse (model = Model_2, data = train_data,
                                response = 'y')
model_3_train_RMSE <- get_rmse (model = Model_3, data = train_data,
                                response = 'y')
model_4_train_RMSE <- get_rmse (model = Model_4, data = train_data,
                                response = 'y')
# (b) test RMSE
model_1_test_RMSE <- get_rmse (model = Model_1, data = test_data,
                               response = 'y')
model_2_test_RMSE <- get_rmse (model = Model_2, data = test_data,
                               response = 'y')
model_3_test_RMSE <- get_rmse (model = Model_3, data = test_data,
                               response = 'y')
model_4_test_RMSE <- get_rmse (model = Model_4, data = test_data,
                               response = 'y')
# (c) the number of parameters, excluding the variance parameter:
Model_1_number <- parameter_number(Model_1)
Model_2_number <- parameter_number(Model_2)
Model_3_number <- parameter_number(Model_3)
Model_4_number <- parameter_number(Model_4)


table <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  model_train_RMSE = c(model_1_train_RMSE, model_2_train_RMSE,
                       model_3_train_RMSE,
                       model_4_train_RMSE),
  model_test_RMSE = c(model_1_test_RMSE, model_2_test_RMSE, model_3_test_RMSE,
                      model_4_test_RMSE),
  model_number = c (Model_1_number,Model_2_number,Model_3_number,
                    Model_4_number))
t<-delete_part(flextable(table), part = "header")
t<- add_header(t,top=T,model_train_RMSE="model train RMS",
               model_test_RMSE= 'model test RMSE',
               model_number="model number")

m<- colformat_num(t,j=c(2,3),digits = 7)
j<- colformat_num(m,j=c(4),digits = 0)
autofit(align(j,align = "center", part = "all"))


# Answer 2


#Set up:

library(readr)
library(tibble)
library(MASS)

data(Boston)
Boston = as_tibble(Boston)
set.seed(42)
boston_index = sample(1:nrow(Boston), size = 400)
train_boston = Boston[boston_index, ]
test_boston = Boston[-boston_index, ]

# Fitting Model
fit = lm(medv ~ . ^ 2, data = train_boston)

# fitting 2 more models (one smaller and one bigger than fit):

fit_smaller <- lm(medv ~ . , data = train_boston)
fit_larger <- lm(medv ~ . ^ 2 + I(crim^2)+I(zn^2)+I(indus^2)+I(chas^2)+I(nox^2),
                 data = train_boston)

#Defining RMSE function and complexity function:

# Calculates RMSE
rmse = function (actual,predicted){
  sqrt(mean((actual - predicted)^2))
}
get_rmse = function (model,data, response){
  rmse (actual = subset(data, select = response, drop = T),
        predicted = predict(model,data))
}
# Calculates the number of parameters, excluding the variance parameter
# of the model:
parameter_number = function (model) {
  length(coef(model)) - 1
}

# Now let us obtain the train RMSE,test RMSE and number of parameters 
# (excluding variance parameter) for each model defined above:

# (a) train RMSE
fit_train_RMSE <- get_rmse (model = fit, data = train_boston,
                            response = 'medv')
fit_smaller_train_RMSE <- get_rmse (model = fit_smaller, data = train_boston,
                                    response = 'medv')
fit_larger_train_RMSE <- get_rmse (model = fit_larger, data = train_boston,
                                   response = 'medv')
# (b) test RMSE
fit_test_RMSE <- get_rmse (model = fit, data = test_boston, response = 'medv')
fit_smaller_test_RMSE <- get_rmse (model = fit_smaller, data = test_boston,
                                   response = 'medv')
fit_larger_test_RMSE <- get_rmse (model = fit_larger, data = test_boston,
                                  response = 'medv')
# (c) the number of parameters, excluding the variance parameter:
fit_number <- parameter_number(fit)
fit_smaller_number <- parameter_number(fit_smaller)
fit_larger_number <- parameter_number(fit_larger)

library(flextable)
table <- data.frame(
  Model = c("fit_smaller", "fit","fit_larger"),
  model_train_RMSE = c(fit_smaller_train_RMSE,fit_train_RMSE, 
                       fit_larger_train_RMSE),
  model_test_RMSE = c(fit_smaller_test_RMSE,fit_test_RMSE,
                      fit_larger_test_RMSE),
  model_number = c (fit_smaller_number,fit_number,fit_larger_number))

t<-delete_part(flextable(table), part = "header")
t<- add_header(t,top=T,model_train_RMSE="model train RMS",
               model_test_RMSE= 'model test RMSE',
               model_number="number of parameters")

m<- colformat_num(t,j=c(2,3),digits = 7)
j<- colformat_num(m,j=c(4),digits = 0)
autofit(align(j,align = "center", part = "all"))


# Answer 3

# Set up:

library(FNN)
library(MASS)
library(flextable)

train_data <- read.csv("h1q3-train-data.csv")
test_data <- read.csv("h1q3-test-data.csv")
X_train <- train_data["x"]
X_test <- test_data["x"]
y_train <- train_data["y"]
y_test <- test_data["y"]
#testing_data <- data.frame(test = seq(min(X_train), max(X_train), by = 0.01))
k = seq(5, 50, by = 5)

# Fitting 10 nearest neighbors models:

for (i in c(1:10)){
  Model_name <- paste("Model_", i, sep = "")
  assign(Model_name, knn.reg(train = X_train, test = X_test, y = y_train,
                             k = k[i]))
}

# Hence we have 10 nearest neighbors models. Now we need to calculate test RMSE
# and train RMSE for each:

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
# define helper function for getting knn.reg predictions
# note: this function is highly specific to this situation and dataset
make_knn_pred = function(k = 1, training, predicting) {
  pred = FNN::knn.reg(train = training["x"], test = predicting["x"],
                      y = training$y, k = k)$pred
  act = predicting$y
  rmse(predicted = pred, actual = act)
}
# get requested train RMSEs
knn_trn_rmse = sapply(k, make_knn_pred,training = train_data,
                      predicting = train_data)
# get requested test RMSEs
knn_tst_rmse = sapply(k, make_knn_pred, training = train_data,
                      predicting = test_data)
# determine "best" k
best_k = k[which.min(knn_tst_rmse)]
# find overfitting, underfitting, and "best"" k
fit_status = ifelse(k < best_k, "Over", ifelse(k == best_k, "Best", "Under"))

# Now to tabulate our findings and conclusion:

# summarize results
knn_results = data.frame(k,
                         round(knn_trn_rmse, 2),
                         round(knn_tst_rmse, 2),
                         fit_status
)

colnames(knn_results) = c("k", "Train RMSE", "Test RMSE", "Fit?")
t<-flextable(knn_results)
m<- colformat_num(t,j=1,digits = 0)
j<- colformat_num(m,j=c(2,3),digits = 2)
autofit(align(j,align = "center", part = "all"))



# Answer 4

# Set up:
library(FNN)
library(MASS)
library(flextable)
library(dplyr)

train_data <- read.csv("h1q4-train-data.csv")
test_data <- read.csv("h1q4-test-data.csv")
X_train <- train_data[,!names(train_data) %in% c("y")]
X_test <- test_data[,!names(test_data) %in% c("y")]
y_train <- train_data["y"]
y_test <- test_data["y"]
k = c(1,5,25)


# Fitting 6 nearest neighbors models:

# Scaled Model
for (i in c(1:3)){
  Model_name <- paste("ScaledModel_", i, sep = "")
  assign(Model_name, knn.reg(train = scale(X_train), test =scale( X_test),
                             y = y_train, k = k[i]))
}
# Unscaled Model
for (i in c(1:3)){
  Model_name <- paste("UnscaledModel_", i, sep = "")
  assign(Model_name, knn.reg(train = X_train, test = X_test,
                             y = y_train, k = k[i]))
}

# Hence we have 6 nearest neighbors models. Now we need to calculate test RMSE
# and train RMSE for each:

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
pred = function (training,predicting,k){
  knn.reg(train = training, test = predicting, y = y_train, k = k)$pred
}

# test RMSE (Unscaled)
Unscaled_model_1_train_RMSE = rmse ((pred(training= X_train,
                                          predicting = X_test, k = 1)), 
                                    actual = y_test$y)
Unscaled_model_2_train_RMSE = rmse ((pred(training= X_train,
                                          predicting = X_test, k = 5)),
                                    actual = y_test$y)
Unscaled_model_3_train_RMSE = rmse ((pred(training= X_train,
                                          predicting = X_test, k = 25)),
                                    actual = y_test$y)
# test RMSE (scaled)
scaled_model_1_train_RMSE = rmse (predicted= pred(training= scale(X_train),
                                                  predicting = scale(X_test),
                                                  k = 1), actual = y_test$y)
scaled_model_2_train_RMSE = rmse ((pred(training= scale(X_train),
                                        predicting = scale(X_test), k = 5)),
                                  actual = y_test$y)
scaled_model_3_train_RMSE = rmse ((pred(training= scale(X_train),
                                        predicting = scale(X_test), k = 25)),
                                  actual = y_test$y)

library(flextable)
table <- data.frame(
  kValue = c(1,1,5,5,25,25),
  model_test_RMSE = c(Unscaled_model_1_train_RMSE,scaled_model_1_train_RMSE,
                      Unscaled_model_2_train_RMSE,scaled_model_2_train_RMSE,
                      Unscaled_model_3_train_RMSE,scaled_model_3_train_RMSE),
  scaleCheck = c ("No","Yes","No","Yes","No","Yes"))

t<-delete_part(flextable(table), part = "header")
t<- add_header(t,top=T,kValue="k",model_test_RMSE= 'test RMSE',
               scaleCheck="Scaled?")
m<- colformat_num(t,j=c(1),digits = 0)

j<- colformat_num(m,j=c(2),digits = 2)

autofit(align(j,align = "center", part = "all"))


# Answer 5


#Set up:

library(ISLR)
library(FNN)
library(flextable)

auto = Auto[, !names(Auto) %in% c("name")]
set.seed(42)
auto_idx = sample(1:nrow(auto), size = round(0.5 * nrow(auto)))
auto_trn = auto[auto_idx, ]
auto_tst = auto[-auto_idx, ]


X_train <- auto_trn[,!names(auto_trn) %in% c("mpg")]
X_test <- auto_tst[,!names(auto_tst) %in% c("mpg")]
mpg_train <- auto_trn["mpg"]
mpg_test <- auto_tst["mpg"]

# Fitting the additive linear model:

LinearModel<-lm(mpg ~ ., data = auto_trn)

# Fitting a KNN Model:

Model<- knn.reg(train = scale(X_train), test = scale(X_test), y = mpg_train,
                k = 5)

# Calculating test RMSE for both models:

# For linear model

# Calculates RMSE
rmse = function (actual,predicted){
  sqrt(mean((actual - predicted)^2))
}

get_rmse = function (model,data, response){
  rmse (actual = subset(data, select = response, drop = T),
        predicted = predict(model,data))
}

LinearModel_RMSE <- get_rmse (model = LinearModel, data = auto_tst,
                              response = 'mpg')

# For KNN

# get requested test RMSEs

scaled_pred = knn.reg(train = scale(X_train), test = scale(X_test),
                      y = mpg_train, k = 5)$pred

KNN_RMSE <- rmse(predicted = scaled_pred, actual = mpg_test$mpg)

# Sumarising everything:

table <- data.frame(
  Model = c("Linear Model", "KNN Model"),
  model_test_RMSE = c(LinearModel_RMSE, KNN_RMSE))

t<-delete_part(flextable(table), part = "header")
t<- add_header(t,top=T,Model="Model", model_test_RMSE= 'model test RMSE')

m<- colformat_num(t,j=c(2),digits = 3)
autofit(align(m,align = "center", part = "all"))

