options(mc.cores = 2)
set.seed(123, "L'Ecuyer-CMRG")
cvc <- SuperLearner.CV.control(V = 3)
obsWeights = rep.int(x = 1,times = nrow(full_data_train))
# SL.ranger SL.kernel.knn SL.glmnet SL.xgboost SL.svm
# ranger  num.trees = 1000, mtry =floor(2*sqrt(ncol(full_data_train))),
# kernel.knn k = 1 glmnet nlambda = 200, alpha = 1

obsWeights <- full_data_train$WAP001
obsWeights = replicate(nrow(full_data_train), 0)
full_data_train <- full_data_dummy %>% filter(source == "training") %>%
  select(starts_with("WAP"),contains("dummy"), LONGITUDE, LATITUDE)

full_data_validation <- full_data_dummy %>% filter(source == "validation") %>%
  select(starts_with("WAP"),contains("dummy"), LONGITUDE, LATITUDE)

set_bart_machine_num_cores(2)
bart_long <- bartMachine(X = as.data.frame(full_data_train %>%
                                             select(-LONGITUDE,-LATITUDE)),
                         y = full_data_train$LONGITUDE)
  



slsvm1 <- SL.glmnet(Y = full_data_train$LONGITUDE,
                       X = full_data_train %>% 
                         select(-LONGITUDE,-LATITUDE),
                       newX = full_data_validation %>% 
                         select(-LONGITUDE,-LATITUDE),
                       family = gaussian(),
                       obsWeights = obsWeights)  

print(postResample(slglmnet1$pred,
                   full_data_validation$LONGITUDE))


for(i in seq(0,1, length = 5)){
  
  print(paste("alpha = ", i))
  
  slglmnet1 <- SL.glmnet(Y = full_data_train$LONGITUDE,
                      X = full_data_train %>% 
                        select(-LONGITUDE,-LATITUDE),
                      newX = full_data_validation %>% 
                        select(-LONGITUDE,-LATITUDE),
                      family = gaussian(),
                      nlambda = 200,
                      alpha = i,
                      loss = "mae",
                      obsWeights = obsWeights)  

  print(postResample(slglmnet1$pred,
                     full_data_validation$LONGITUDE))
}

for(i in c(2,2.5,3)){
  print(paste("iteration ", i, " start"))
slran1 <- SL.ranger(Y = full_data_train$LONGITUDE,
                    X = full_data_train %>% 
                      select(-LONGITUDE,-LATITUDE),
                    newX = full_data_validation %>% 
                      select(-LONGITUDE,-LATITUDE),
                    family = gaussian(),
                    num.trees = 1000,
                    obsWeights = obsWeights,
                    num.threads = 3,
                    mtry =floor(2*sqrt(ncol(full_data_train))))  

print(postResample(slran1$pred,
             full_data_validation$LONGITUDE))
}





for(i in c(1,3,5)){
  print(paste("iteration ", i, " start"))
slran1 <- SL.kernelKnn(Y = full_data_train$LONGITUDE,
                    X = full_data_train %>% 
                      select(-LONGITUDE,-LATITUDE),
                    newX = full_data_validation %>% 
                      select(-LONGITUDE,-LATITUDE),
                    family = gaussian(),
                    k = i,
                    obsWeights = obsWeights,
                    method = "manhattan",
                    mtry =floor(2*sqrt(ncol(full_data_train))))  

print(postResample(slran1$pred,
             full_data_validation$LONGITUDE))
}


parallel::clusterExport(cluster, create_ranger_tunned$names)
SL_RANGER_long = SuperLearner(Y = full_data_train$LONGITUDE,
                              X = full_data_train %>% 
                                select(-LONGITUDE,-LATITUDE),
                              newX = full_data_validation %>% 
                                select(-LONGITUDE,-LATITUDE),
                              family = gaussian(),
                              SL.library = create_ranger_tunned$names,
                              cvControl = cvc)

# Make a snow cluster
# Again, replace 2 with num_cores to use all available cores.
cluster = parallel::makeCluster(2)

# Check the cluster object.
cluster

parallel::clusterEvalQ(cluster, library(SuperLearner))

SL_RANGER_long= SuperLearner(Y = full_data_train$LONGITUDE,
                                       X = full_data_train %>% 
                                         select(-LONGITUDE,-LATITUDE),
                                       newX = full_data_validation %>% 
                                         select(-LONGITUDE,-LATITUDE),
                                       family = gaussian(),
                                       SL.library = create_ranger_tunned$names,
                                       cvControl = cvc)



sl_lasso = SuperLearner(Y = y_train, X = x_train, family = binomial(),
                        SL.library = "SL.glmnet")

SL_xgboost_long <- SL.xgboost(Y = full_data_train$LONGITUDE,
                               X = full_data_train %>% 
                                 select(-LONGITUDE,-LATITUDE),
                               newX = full_data_validation %>% 
                                 select(-LONGITUDE,-LATITUDE),
                               obsWeights = obsWeights,
                               family = gaussian(),verbose = TRUE,nthread = 3,
                               ntrees = 2000)

train

# SL_bart_long <- SL.bartMachine(Y = full_data_train$LONGITUDE,
#                                X = as.data.frame(full_data_train %>%
#                                                    select(-LONGITUDE,-LATITUDE)),
#                                newX = full_data_validation %>%
#                                  select(-LONGITUDE,-LATITUDE),
#                                obsWeights = obsWeights,
#                                family = gaussian(),verbose = TRUE)
SL_xgboost_lat <- SL.xgboost(Y = full_data_train$LATITUDE,
                               X = full_data_train %>% 
                               select(-LONGITUDE,-LATITUDE),
                               newX = full_data_validation %>% 
                               select(-LONGITUDE,-LATITUDE),
                               obsWeights = obsWeights,
                               family = gaussian(),verbose = TRUE,nthread = 3,
                             ntrees = 500
                               )



install.packages('bartMachine')
library(bartMachine)
options(java.parameters = "-Xmx500m")
  
predictrions_SLranger_long <- predict.SuperLearner(object = SL_RANGER_long,
                     newdata = full_data_dummy %>%
                       filter(source == 'validation') %>% 
                       select(contains("WAP"),contains("dummy"),
                              LATITUDE))

predictrions_SLranger_long <- as_data_frame(predictrions_SLranger_long)

postResample(predictrions_SLranger_long$pred[,1],
             full_data_dummy[
               full_data_dummy$source == 'validation','LONGITUDE']$LONGITUDE)
postResample(SL_xgboost_loong$pred,
             full_data_validation$LONGITUDE)
postResample(SL_xgboost_lat$pred,
             full_data_validation$LATITUDE)



# ensemble model long -----------------------------------------------------
SL.ranger_1()

learners_ranger$names = create.Learner(base_learner = "SL.ranger",
                                 params = list(num.trees = 1000, mtry = 34))
learners_knn = create.Learner(base_learner = "SL.kernelKnn",
                              params = list(k=1))
learners_glmnet = create.Learner(base_learner = "SL.glmnet",
                                 params = list(nlambda = 200, alpha = 1))
learners_xgboost = create.Learner(base_learner = "SL.xgboost",
                                  params = list(ntrees = 2200))

cv_sl_ranger_longitude = CV.SuperLearner(
  Y = full_data_train$LONGITUDE, 
  X = full_data_train %>% select(-LONGITUDE,-LATITUDE),
  # newX = full_data_validation %>% select(-LONGITUDE,-LATITUDE),
  family = gaussian(), obsWeights = obsWeights,
  V = 10, SL.library = c(learners_ranger$names[1],
                         num.threads = 4,
                         verbose = TRUE,
                         parallel = 'multicore'))
saveRDS(object = cv_sl_ranger_longitude,file =
          "Models/models_r2/cv_sl_ranger_longitude.RDS")


cv_sl_knn_longitude = CV.SuperLearner(
  Y = full_data_train$LONGITUDE, 
  X = full_data_train %>% select(-LONGITUDE,-LATITUDE),
  # newX = full_data_validation %>% select(-LONGITUDE,-LATITUDE),
  family = gaussian(), obsWeights = obsWeights,
  V = 10, SL.library = c(learners_knn$names,
                         num.threads = 4,
                         verbose = TRUE,
                         parallel = 'multicore'))
saveRDS(object = cv_sl_knn_longitude,file =
          "Models/models_r2/cv_sl_knn_longitude.RDS")


cv_sl_glmnet_longitude = CV.SuperLearner(
  Y = full_data_train$LONGITUDE, 
  X = full_data_train %>% select(-LONGITUDE,-LATITUDE),
  # newX = full_data_validation %>% select(-LONGITUDE,-LATITUDE),
  family = gaussian(), obsWeights = obsWeights,
  V = 10, SL.library = c(learners_glmnet$names,
                         num.threads = 4,
                         verbose = TRUE,
                         parallel = 'multicore'))
saveRDS(object = cv_sl_glmnet_longitude,file =
          "Models/models_r2/cv_sl_glmnet_longitude.RDS")


cv_sl_xgboost_longitude = CV.SuperLearner(
  Y = full_data_train$LONGITUDE, 
  X = full_data_train %>% select(-LONGITUDE,-LATITUDE),
  # newX = full_data_validation %>% select(-LONGITUDE,-LATITUDE),
  family = gaussian(), obsWeights = obsWeights,
  V = 10, SL.library = c(learners_xgboost$names,
                         num.threads = 4,
                         verbose = TRUE,
                         parallel = 'multicore'))
saveRDS(object = cv_sl_xgboost_longitude,file =
          "Models/models_r2/cv_sl_xgboost_longitude.RDS")


cv_sl_svm_longitude = CV.SuperLearner(
  Y = full_data_train$LONGITUDE, 
  X = full_data_train %>% select(-LONGITUDE,-LATITUDE),
  # newX = full_data_validation %>% select(-LONGITUDE,-LATITUDE),
  family = gaussian(), obsWeights = obsWeights,
  V = 10, SL.library = c("SL.svm", num.threads = 4,
                         verbose = TRUE,
                         parallel = 'multicore'))
saveRDS(object = cv_sl_svm_longitude,file =
          "Models/models_r2/cv_sl_svm_longitude.RDS")
