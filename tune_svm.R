library(tidyverse)
library(knitr)
library(ggcorrplot)
library(ggsoccer)
library(glmnet)
library(stringr)
library(dplyr)
library(e1071)

load("train.RData")

train<- train|>
  filter(subEventName != "Hand pass" & subEventName != "Launch") |>
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

# making the model matrix subset based on selected predictors from forward selection
x <- model.matrix( ~ -1 + is_goal +eventSec2 + x_meter + y_meter + distance_to_goal_line + subEventName + skilled_foot +  passes_is_key_Pass + matchPeriod + is_CA + passes_is_through,
                   data = train)
x_train<- as.data.frame(x)

col_names<- colnames(x_train)[6:ncol(x_train)]
x_train[col_names]<- lapply(x_train[col_names] , factor)
x_train['is_goal']<- lapply(x_train["is_goal"] , factor)

train_model_data<- x_train %>%
  mutate_if(is.numeric, scale)

tune_degree_linear <- tune.svm(is_goal ~ . , data = train_model_data,
                               type = "C-classification",
                               kernel = "linear", 
                               gamma = 10^(-5:-1),
                               cost = 10^(-3:1),
                               coef0 = c(0.1,1,10))
save(tune_degree_linear, file = "tune_degree_linear")

tune_degree_quadratic<- tune.svm(is_goal ~ . , data = train_model_data,
                                 type = "C-classification",
                                 kernel = "polynomial", 
                                 degree = 2, 
                                 gamma = 10^(-5:-1),
                                 cost = 10^(-3:1),
                                 coef0 = c(0.1,1,10))
save(tune_degree_quadratic, file = "tune_degree_quadratic")

tune_degree_cubic <- tune.svm(is_goal ~ . , data = train_model_data,
                              type = "C-classification",
                              kernel = "polynomial", 
                              degree = 3, 
                              gamma = 10^(-5:-1),
                              cost = 10^(-3:1),
                              coef0 = c(0.1,1,10))
save(tune_degree_cubic, file = "tune_degree_cubic")
