
# get the data ------------------------------------------------------------

pacman::p_load(readr,caret,tidyverse,parallel,doParallel)

cl <-  makeCluster(2)
registerDoParallel(cl)

full_data <- read_rds("Data/FULL_DATA_CLEAN.RDS")
full_data_dummy <-  read_rds("Data/FULL_DATA_CLEAN_DUMMY.RDS")
data_validation <- full_data %>% filter(source=='validation')
data_train <- full_data %>% filter(source == "training")

data_train_dummy <- full_data_dummy %>% filter(source == "training") %>%
  select(contains("WAP"),contains("dummy"), LONGITUDE, LATITUDE, FLOOR)

data_validation_dummy <- full_data_dummy %>% filter(source == "validation") %>%
  select(contains("WAP"),contains("dummy"), LONGITUDE, LATITUDE, FLOOR)


myControl <- trainControl(method="repeatedcv",
                          number=3, 
                          repeats=0,
                          savePredictions=FALSE, 
                          classProbs=FALSE)

stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

# general predictions only cascading building -----------------------------

# ranger

# Building_ranger <- train(form = BUILDINGID~.,
#                    data = data_train %>% select(starts_with("WAP"),BUILDINGID),
#                    method = "ranger",
#                    trControl = myControl,
#                    tuneLength = 4)
# saveRDS(object = Building_ranger, file =
#           "Models/FINAL_MODELS/Building_ranger.RDS")
Building_ranger <- read_rds("Models/FINAL_MODELS/Building_ranger.RDS")

predictions_Building_ranger <- predict(Building_ranger,data_validation_dummy %>% select(starts_with('WAP')))
postResample(predictions_Building_ranger, data_validation$BUILDINGID)
confusionMatrix(predictions_Building_ranger, data_validation$BUILDINGID)

# LONGITUDE_ranger <- train(form = LONGITUDE~.,
#                     data = data_train %>% select(starts_with("WAP"),LONGITUDE),
#                     method = "ranger",
#                     trControl = myControl,
#                     tuneLength = 4)
# saveRDS(object = LONGITUDE_ranger, file =
#           "Models/FINAL_MODELS/LONGITUDE_ranger.RDS")
LONGITUDE_ranger <- read_rds(
  "Models/FINAL_MODELS/LONGITUDE_ranger.RDS")

predictions_LONGITUDE_ranger <- predict(LONGITUDE_ranger,data_validation_dummy %>% select(starts_with('WAP')))
postResample(predictions_LONGITUDE_ranger, data_validation$LONGITUDE)

# LATITUDE_ranger <- train(form = LATITUDE~.,
#                     data = data_train %>% select(starts_with("WAP"),LATITUDE),
#                     method = "ranger",
#                     trControl = myControl,
#                     tuneLength = 4)
# saveRDS(object = LATITUDE_ranger, file =
#           "Models/FINAL_MODELS/LATITUDE_ranger.RDS")
LATITUDE_ranger <- read_rds(
  "Models/FINAL_MODELS/LATITUDE_ranger.RDS")

predictions_LATITUDE_ranger <- predict(LATITUDE_ranger,data_validation_dummy %>% select(starts_with('WAP')))
postResample(predictions_LATITUDE_ranger, data_validation$LATITUDE)

# FLOOR_ranger <- train(form = FLOOR~.,
#                    data = data_train %>% select(starts_with('WAP'),FLOOR),
#                    method = "ranger",
#                    trControl = myControl,
#                    tuneLength = 5)
# saveRDS(object = FLOOR_ranger, file =
#           "Models/FINAL_MODELS/FLOOR_ranger.RDS")

FLOOR_ranger <- read_rds(
  "Models/FINAL_MODELS/FLOOR_ranger.RDS")

predictions_FLOOR_ranger <- predict(FLOOR_ranger,data_validation_dummy %>% select(starts_with('WAP')))
postResample(predictions_FLOOR_ranger, data_validation$FLOOR)

# xgboost

# Building_xgb <- train(form = BUILDINGID~.,
#                         data = data_train %>% select(starts_with("WAP"),
#                                                            BUILDINGID),
#                          method = "xgbTree",
#                          trControl = myControl,
#                          tuneLength = 5)
# saveRDS(object = Building_xgb, file =
#           "Models/FINAL_MODELS/Building_xgb.RDS")

Building_xgb <- read_rds(
  "Models/FINAL_MODELS/Building_xgb.RDS")

predictions_Building_xgb <- predict(Building_xgb,data_validation_dummy %>% select(starts_with('WAP')))
postResample(predictions_Building_xgb, data_validation$BUILDINGID)
confusionMatrix(predictions_Building_xgb, data_validation$BUILDINGID)

# cl <-  makeCluster(2)
# registerDoParallel(cl)
# LONGITUDE_xgb <- train(form = LONGITUDE~.,
#                           data = data_train %>% select(contains('WAP'),
#                                                              LONGITUDE),
#                           method = "xgbTree",
#                           trControl = myControl,
#                           tuneLength = 5)
# saveRDS(object = LONGITUDE_xgb, file =
#           "Models/FINAL_MODELS/LONGITUDE_xgb.RDS")
LONGITUDE_xgb <- read_rds(
  "Models/FINAL_MODELS/LONGITUDE_xgb.RDS")

problematic<- which(data_validation_dummy$highestWAP %in% c('WAP268', 'WAP323'))
# temp_a <- which(!data_validation_dummy[problematic[1],] == -200)
# temp_b <- which(!data_validation_dummy[problematic[2],] == -200)
# max(data_validation_dummy[problematic[1],temp_a] %>% select(starts_with('WAP'),-'WAP268'))
# which(data_validation_dummy[problematic[1],13]==-74)
data_validation_dummy[problematic[2],]$highestWAP <- 'WAP032'
data_validation_dummy[problematic[1],]$highestWAP <- 'WAP019'
predictions_LONGITUDE_xgb <- predict(LONGITUDE_xgb,data_validation_dummy %>% select(contains('WAP')))
postResample(predictions_LONGITUDE_xgb, data_validation$LONGITUDE)
# worse results go go ranger o retry manually

# cl <-  makeCluster(2)
# registerDoParallel(cl)
# LATITUDE_xgb <- train(form = LATITUDE~.,
#                       data = data_train %>% 
#                         select(contains('WAP'),LATITUDE),
#                       method = "xgbTree",
#                       trControl = myControl,
#                       tuneLength = 5)
# saveRDS(object = LATITUDE_xgb, file =
#           "Models/FINAL_MODELS/LATITUDE_xgb.RDS")
LATITUDE_xgb <- read_rds(
  "Models/FINAL_MODELS/LATITUDE_xgb.RDS")

predictions_LATITUDE_xgb <- predict(LATITUDE_xgb,data_validation_dummy %>% select(contains('WAP')))
postResample(predictions_LATITUDE_xgb, data_validation$LATITUDE)
# way worse results
# cl <-  makeCluster(2)
# registerDoParallel(cl)
# FLOOR_xgb <- train(form = FLOOR~.,
#                    data = data_train %>% select(contains('WAP'),FLOOR),
#                    method = "xgbTree",
#                    trControl = myControl,
#                    tuneLength = 5)
# saveRDS(object = FLOOR_xgb, file =
#           "Models/FINAL_MODELS/FLOOR_xgb.RDS")
# FLOOR_xgb <- read_rds(
#   "Models/FINAL_MODELS/FLOOR_xgb.RDS")
# 
# predictions_FLOOR_xgb <- predict(FLOOR_xgb,data_validation_dummy %>% select(contains('WAP')))
# postResample(predictions_FLOOR_xgb, data_validation$FLOOR)


# floor cascading long and lat building

# FLOOR_xgb_full <- train(form = FLOOR~.,
#                         data = data_train_dummy %>% select(-starts_with("dummy")),
#                         method = "xgbTree",
#                         trControl = myControl,
#                         tuneLength = 5)
# saveRDS(object = FLOOR_xgb_full, file =
#           "Models/FINAL_MODELS/FLOOR_xgb_full.RDS")
# FLOOR_xgb_full <- read_rds(
#   "Models/FINAL_MODELS/FLOOR_xgb_full.RDS")


# FLOOR_ranger_full <- train(form = FLOOR~.,
#                            data = data_train_dummy %>% select(-starts_with("dummy")),
#                            method = "ranger",
#                            trControl = myControl,
#                            tuneLength = 5)
# saveRDS(object = FLOOR_ranger_full, file =
#           "Models/FINAL_MODELS/FLOOR_ranger_full.RDS")

FLOOR_ranger_full <- read_rds(
  "Models/FINAL_MODELS/FLOOR_ranger_full.RDS")
data_validation_cascade <- data_validation_dummy %>% select(contains('WAP'))
data_validation_cascade %>% mutate(BUILDINGID = predictions_Building_ranger,
                                   LATITUDE = ,
                                   LONGITUDE = ,)
predictions_FLOOR_ranger_full <- predict(FLOOR_ranger_full,data_validation_cascade)
postResample(predictions_FLOOR_ranger_full, data_validation$FLOOR)
confusionMatrix(predictions_FLOOR_ranger_full, data_validation$FLOOR)

# LATITUDE_BY BUILDING ----------------------------------------------------
LATITUDE_BY_BUILDING_RANGER <- list()
for (j in 0:2) {
  TRAIN <- full_data_dummy %>%
    filter(BUILDINGID == j,source == 'training') %>%
    select(contains("WAP"),starts_with("dummy"),LATITUDE)
  LATITUDE_BY_BUILDING_RANGER[[paste("LATITUDE_building",j,sep = "_")]] <-
    train(form = LATITUDE~.,
          data = TRAIN,
          method = "ranger",
          trControl = myControl,
          tuneLength = 5)
}
saveRDS(object = LATITUDE_BY_BUILDING_RANGER, file =
          "Models/FINAL_MODELS/LATITUDE_BY_BUILDING_RANGER.RDS")
LATITUDE_BY_BUILDING_XGB <- list()
for (j in 0:2) {
  TRAIN <- full_data_dummy %>%
    filter(BUILDINGID == j,source == 'training') %>%
    select(-LONGITUDE,-FLOOR)
  LATITUDE_BY_BUILDING_XGB[[paste("LATITUDE_building",j,sep = "_")]] <-
    train(form = LATITUDE~.,
          data = TRAIN,
          method = "xgbTree",
          trControl = myControl,
          tuneLength = 5)
}
saveRDS(object = LATITUDE_BY_BUILDING_XGB, file =
          "Models/FINAL_MODELS/LATITUDE_BY_BUILDING_XGB.RDS")


# Longitude predicting with lat,building ----------------------------------


LONGITUDE_full_xgb <- train(form = LONGITUDE~.,
                            data = data_train_dummy %>%
                              select(contains('WAP'), starts_with("dummy"),LATITUDE,LONGITUDE),
                            method = "xgbTree",
                            trControl = myControl,
                            tuneLength = 5)
saveRDS(object = LONGITUDE_full_xgb, file =
          "Models/FINAL_MODELS/LONGITUDE_full_xgb.RDS")

LONGITUDE_full_ranger <- train(form = LONGITUDE~.,
                               data = data_train_dummy %>%
                                 select(contains('WAP'), starts_with("dummy"),LATITUDE,LONGITUDE),
                               method = "ranger",
                               trControl = myControl,
                               tuneLength = 5)
saveRDS(object = LONGITUDE_full_ranger, file =
          "Models/FINAL_MODELS/LONGITUDE_full_ranger.RDS")
