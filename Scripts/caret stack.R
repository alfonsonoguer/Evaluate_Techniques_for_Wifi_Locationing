cl <-  makeCluster(2)
registerDoParallel(cl)

myControl <- trainControl(method="repeatedcv",
                                number=10, 
                                repeats=0,
                                savePredictions=FALSE, 
                                classProbs=FALSE)

stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

full_data_train <- full_data_dummy %>% filter(source == "training") %>%
  select(starts_with("WAP"),contains("dummy"), LONGITUDE, LATITUDE)

full_data_validation <- full_data_dummy %>% filter(source == "validation") %>%
  select(starts_with("WAP"),contains("dummy"), LONGITUDE, LATITUDE)


# SL.ranger SL.kernel.knn SL.glmnet SL.xgboost SL.svm
# ranger  num.trees = 1000, mtry =floor(2*sqrt(ncol(full_data_train))),
# kernel.knn k = 1 glmnet nlambda = 200, alpha = 1


caret_models_longitude <- caretList(
  LONGITUDE~.,
  full_data_train,
  tuneList=list(
    glmnet=caretModelSpec(method="glmnet",alpha = 1, lambda = 200),
    ranger=caretModelSpec("ranger", mtry = 35, ntree = 1000),
    knn=caretModelSpec("knn",k=1),
    xgbTree=caretModelSpec("xgbTree",nrounds = 2000),
    svmRadial=caretModelSpec("svmRadial")
  ),
  trControl=myControl
)
saveRDS(object = caret_models_longitude,file =
          "Models/models_r2/caret_models_longitude.RDS")

caret_stack_longitude_glm <- caretStack(multi_mod, method = "glm",
                                    metric = "MAE", trControl = stackControl)

saveRDS(object = caret_stack_longitude_glm,file =
          "Models/models_r2/caret_stack_longitude_glm.RDS")

postResample(predict(caret_stack_longitude_glm,full_data_validation),
             full_data_validation$LONGITUDE)

caret_stack_longitude_rf <- caretStack(multi_mod, method = "rf",
                                    metric = "MAE", trControl = stackControl)

saveRDS(object = caret_stack_longitude_rf,file =
          "Models/models_r2/caret_stack_longitude_rf.RDS")

postResample(predict(caret_stack_longitude_rf,full_data_validation),
             full_data_validation$LONGITUDE)

caret_models_latitude <- caretList(
  LATITUDE~.,
  full_data_train,
  tuneList=list(
    glmnet=caretModelSpec(method="glmnet",alpha = 1, lambda = 200),
    ranger=caretModelSpec("ranger", mtry = 35, ntree = 1000),
    knn=caretModelSpec("glm",k=1),
    xgbTree=caretModelSpec("xgbTree",nrounds = 2000),
    svmRadial=caretModelSpec("svmRadial")
  ),
  trControl=myControl
)
saveRDS(object = caret_models_latitude,file =
          "Models/models_r2/caret_models_latitude.RDS")

caret_stack_latitude_glm <- caretStack(multi_mod, method = "glm",
                                       metric = "MAE", trControl = stackControl)

saveRDS(object = caret_stack_latitude_glm,file =
          "Models/models_r2/caret_stack_latitude_glm.RDS")

postResample(predict(caret_stack_latitude_glm,full_data_validation),
             full_data_validation$LATITUDE)

caret_stack_latitude_rf <- caretStack(multi_mod, method = "rf",
                                       metric = "MAE", trControl = stackControl)

saveRDS(object = caret_stack_latitude_rf,file =
          "Models/models_r2/caret_stack_latitude_rf.RDS")

postResample(predict(caret_stack_latitude_rf,full_data_validation),
             full_data_validation$LATITUDE)

