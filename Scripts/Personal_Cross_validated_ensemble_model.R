# SL.ranger SL.kernel.knn SL.glmnet SL.xgboost SL.svm
# ranger  num.trees = 1000, mtry =floor(2*sqrt(ncol(full_data_train))),
# kernel.knn k = 1 glmnet nlambda = 200, alpha = 1
# MODELS ------------------------------------------------------------------


slglmnet1_long <- SL.glmnet(Y = full_data_train$LONGITUDE,
                            X = full_data_train %>% 
                              select(-LONGITUDE,-LATITUDE),
                            newX = full_data_validation %>% 
                              select(-LONGITUDE,-LATITUDE),
                            family = gaussian(),
                            nlambda = 200,
                            alpha = 1,
                            loss = "mae",
                            obsWeights = obsWeights) 

slglmnet2_long <- SL.glmnet(Y = full_data_train$LONGITUDE,
                            X = full_data_train %>% 
                              select(-LONGITUDE,-LATITUDE),
                            newX = full_data_validation %>% 
                              select(-LONGITUDE,-LATITUDE),
                            family = gaussian(),
                            nlambda = 200,
                            alpha = 0,
                            loss = "mae",
                            obsWeights = obsWeights) 

slknn1_long<- SL.kernelKnn(Y = full_data_train$LONGITUDE,
                           X = full_data_train %>% 
                             select(-LONGITUDE,-LATITUDE),
                           newX = full_data_validation %>% 
                             select(-LONGITUDE,-LATITUDE),
                           family = gaussian(),
                           k = 1,
                           obsWeights = obsWeights,
                           method = "manhattan") 

SL_xgboost_long <- SL.xgboost(Y = full_data_train$LONGITUDE,
                              X = full_data_train %>% 
                                select(-LONGITUDE,-LATITUDE),
                              newX = full_data_validation %>% 
                                select(-LONGITUDE,-LATITUDE),
                              obsWeights = obsWeights,
                              family = gaussian(),verbose = TRUE,nthread = 3,
                              ntrees = 500)
postResample(SL_xgboost_long$pred,full_data_validation$LONGITUDE)

learners_ranger$names = create.Learner(base_learner = "SL.ranger",
                                       params = list(num.trees = 1000, mtry = 34))

ranger_long <- ranger(LONGITUDE~.,data = full_data_train %>% select(-LATITUDE),
                      verbose = TRUE)

# sl_svm_longitude = SL.svm(
#   Y = full_data_train$LONGITUDE, 
#   X = full_data_train %>% select(-LONGITUDE,-LATITUDE),
#   # newX = full_data_validation %>% select(-LONGITUDE,-LATITUDE),
#   family = gaussian())
X_long_train <-  full_data_train %>% select(-LONGITUDE,-LATITUDE)


# ensemble long -----------------------------------------------------

xgboost_long <- predict(SL_xgboost_long$fit,X_long_train)
lmnet1_long <- predict(slglmnet1_long$fit,X_long_train)
glmnet2_long <- predict(slglmnet2_long$fit,X_long_train)
ranger_long_pred <- predictions(predict(ranger_long,X_long_train))
Y <- full_data_train$LONGITUDE

predictors_long_f2 <- tibble(
  xgboost_long = xgboost_long, 
  lmnet1_long = lmnet1_long, 
  glmnet2_long = glmnet2_long, 
  ranger_long = ranger_long_pred, 
  Y = Y)

ensemble_long <- ranger(formula = Y~.,data = predictors_long_f2)

# predictions long -------------------------------------------------------------

X_long <-  full_data_validation %>% select(-LONGITUDE,-LATITUDE)


xgboost_long <- predict(SL_xgboost_long$fit,X_long)
lmnet1_long <- predict(slglmnet1_long$fit,X_long)
glmnet2_long <- predict(slglmnet2_long$fit,X_long)
ranger_long_pred <- predictions(predict(ranger_long,X_long))


predictors_long_f2 <- tibble(
  xgboost_long = xgboost_long, 
  lmnet1_long = lmnet1_long, 
  glmnet2_long = glmnet2_long, 
  ranger_long = ranger_long_pred)

predicciones <- predictions(predict(object = ensemble_long,
                                    predictors_long_f2))

postResample(predicciones,
             full_data_validation$LONGITUDE)


# models lat ------------------------------------------------------------

slglmnet1_lat <- SL.glmnet(Y = full_data_train$LATITUDE,
                            X = full_data_train %>% 
                              select(-LONGITUDE,-LATITUDE),
                            newX = full_data_validation %>% 
                              select(-LONGITUDE,-LATITUDE),
                            family = gaussian(),
                            nlambda = 200,
                            alpha = 1,
                            loss = "mae",
                            obsWeights = obsWeights) 

slglmnet2_lat <- SL.glmnet(Y = full_data_train$LATITUDE,
                            X = full_data_train %>% 
                              select(-LONGITUDE,-LATITUDE),
                            newX = full_data_validation %>% 
                              select(-LONGITUDE,-LATITUDE),
                            family = gaussian(),
                            nlambda = 200,
                            alpha = 0,
                            loss = "mae",
                            obsWeights = obsWeights) 

slknn1_lat<- SL.kernelKnn(Y = full_data_train$LATITUDE,
                           X = full_data_train %>% 
                             select(-LONGITUDE,-LATITUDE),
                           newX = full_data_validation %>% 
                             select(-LONGITUDE,-LATITUDE),
                           family = gaussian(),
                           k = 1,
                           obsWeights = obsWeights,
                           method = "manhattan") 

SL_xgboost_lat <- SL.xgboost(Y = full_data_train$LATITUDE,
                              X = full_data_train %>% 
                                select(-LONGITUDE,-LATITUDE),
                              newX = full_data_validation %>% 
                                select(-LONGITUDE,-LATITUDE),
                              obsWeights = obsWeights,
                              family = gaussian(),verbose = TRUE,nthread = 3,
                              ntrees = 300)
postResample(SL_xgboost_lat$pred,full_data_validation$LATITUDE)

learners_ranger$names = create.Learner(base_learner = "SL.ranger",
                                       params = list(num.trees = 1000, mtry = 34))

ranger_lat <- ranger(LATITUDE~.,data = full_data_train %>% select(-LONGITUDE),
                      verbose = TRUE)

# sl_svm_longitude = SL.svm(
#   Y = full_data_train$LONGITUDE, 
#   X = full_data_train %>% select(-LONGITUDE,-LATITUDE),
#   # newX = full_data_validation %>% select(-LONGITUDE,-LATITUDE),
#   family = gaussian())
X_lat_train <-  full_data_train %>% select(-LONGITUDE,-LATITUDE)


# ensemble lat -----------------------------------------------------

xgboost_lat <- predict(SL_xgboost_lat$fit,X_lat_train)
lmnet1_lat <- predict(slglmnet1_lat$fit,X_lat_train)
glmnet2_lat <- predict(slglmnet2_lat$fit,X_lat_train)
ranger_lat_pred <- predictions(predict(ranger_lat,X_lat_train))
Y <- full_data_train$LATITUDE

predictors_lat_f2 <- tibble(
  xgboost_lat = xgboost_lat, 
  lmnet1_lat = lmnet1_lat, 
  glmnet2_lat = glmnet2_lat, 
  ranger_lat = ranger_lat_pred, 
  Y = Y)

ensemble_lat <- ranger(formula = Y~.,data = predictors_lat_f2)

# predictions  -------------------------------------------------------------

X_lat <-  full_data_validation %>% select(-LONGITUDE,-LATITUDE)


xgboost_lat <- predict(SL_xgboost_lat$fit,X_lat)
lmnet1_lat <- predict(slglmnet1_lat$fit,X_lat)
glmnet2_lat <- predict(slglmnet2_lat$fit,X_lat)
ranger_lat_pred <- predictions(predict(ranger_lat,X_lat))


predictors_lat_f2 <- tibble(
  xgboost_lat = xgboost_lat, 
  lmnet1_lat = lmnet1_lat, 
  glmnet2_lat = glmnet2_lat, 
  ranger_lat = ranger_lat_pred)

predicciones <- predictions(predict(object = ensemble_lat,
                                    predictors_lat_f2))

postResample(predicciones,
             full_data_validation$LATITUDE)
