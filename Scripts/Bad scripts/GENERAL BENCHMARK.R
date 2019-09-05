# libraries ---------------------------------------------------------------


pacman::p_load(readr,caret,tidyverse,ggplot2,plotly, lubridate, ranger,
               DataExplorer,plotly,gdata,splitstackshape,class,FNN)

low_int_to_NA <- function(x){  
  ifelse((x==100 || x <= -90 ),return(NA),return(x))
}


# Coments -----------------------------------------------------------------

# first 2 classification problems building (solved by random randomforest
# and floor needs to be improved study errors.
# train/test is not correctly stratified.

# Get and preporcess the data ------------------------------------------------

train <- read_csv("Data/trainingData.csv")
train$BUILDINGID <- as.factor(train$BUILDINGID)
train$FLOOR <- as.factor(train$FLOOR)

model_building <- train %>% select(contains("WAP"),BUILDINGID,FLOOR,LONGITUDE,
                                   LATITUDE)
str_train <- stratified(model_building,
                        group =  c("BUILDINGID", "FLOOR"),size =.25,
                        bothSets = TRUE)

test <- read_csv("Data/validationData.csv")

test$BUILDINGID <- as.factor(test$BUILDINGID)
test$FLOOR <- as.factor(test$FLOOR)
length(unique(test$LONGITUDE))
length(unique(train$LONGITUDE))

fulldata <- rbind(train,test)
out_general <- stratified(fulldata,group =  c("BUILDINGID", "FLOOR"),size =.25,
                          bothSets = TRUE)
out_B1 <- stratified(indt = fulldata %>% filter(BUILDINGID == 1),
                     group = "FLOOR", size =.25, bothSets = TRUE)
names(out_B1) <- c("TEST01","TRAIN01")


# benchmark ranger building floor longitude,latitude ----------------------
str_train$SAMP2 <- as_tibble(str_train$SAMP2)
str_train$SAMP1 <- as_tibble(str_train$SAMP1)
results <- tibble()
benchmark_building_CV_TRAIN <- ranger(formula = BUILDINGID~.,
                                      data = str_train$SAMP2 %>%
                                        select(-FLOOR,-LONGITUDE,-LATITUDE))
saveRDS(object =benchmark_building_CV_TRAIN, 
        file = "Models/benchmark_building_CV_TRAIN.rds")
results_CV_TRAIN <- tibble(building = (predictions(
  predict(benchmark_building_CV_TRAIN,
          str_train$SAMP1))))
confusionMatrix(str_train$SAMP1$BUILDINGID,results_CV_TRAIN$building)

results_validation <- tibble(building = (predictions(
  predict(benchmark_building_CV_TRAIN,
          test))))
confusionMatrix(test$BUILDINGID,results_validation$building)


# benchmark FLOOR ---------------------------------------------------------
# floor GENERAL

benchmark_FLOOR_CV_TRAIN <- ranger(formula = FLOOR~.,
                                   data = str_train$SAMP2 %>%
                                     select(contains("WAP"),FLOOR))
saveRDS(object =benchmark_building_CV_TRAIN, 
        file = "Models/benchmark_FLOOR_CV_TRAIN.rds")
results_CV_TRAIN$FLOOR_GENERAL <-predictions(predict(benchmark_FLOOR_CV_TRAIN,
                                                     str_train$SAMP1))
confusionMatrix(str_train$SAMP1$FLOOR,results_CV_TRAIN$FLOOR_GENERAL)

results_validation$FLOOR_GENERAL <- predictions(
  predict(benchmark_FLOOR_CV_TRAIN, test))
confusionMatrix(test$FLOOR,results_validation$FLOOR_GENERAL)

# FLOOR BUILDING 0 --------------------------------------------------------
# floor by BUILDING
# floor building 0
data_b0_F_CV <- str_train$SAMP2 %>% filter(BUILDINGID == 0) %>%
  select(contains("WAP"),FLOOR)
test_b0_F_CV <- str_train$SAMP1 %>% filter(BUILDINGID == 0)

validation_b0 <- test %>% filter(BUILDINGID == 0)
validation_b0$FLOOR <- factor(validation_b0$FLOOR)

benchmark_FLOOR_B0_CV_TRAIN <- ranger(FLOOR ~ .,
                                      data = data_b0_F_CV)
saveRDS(object =benchmark_FLOOR_B0_CV_TRAIN, 
        file = "Models/benchmark_FLOOR_B0_CV_TRAIN.rds")
results_CV_TRAIN_B0_FLOOR <- predictions(predict(benchmark_FLOOR_B0_CV_TRAIN,
                                                 data = str_train$SAMP1 %>% 
                                                   filter(BUILDINGID == 0)))
confusionMatrix(factor(test_b0_F_CV$FLOOR),
                factor(results_CV_TRAIN_B0_FLOOR))

validation_building_0_floor <- predict(benchmark_FLOOR_B0_CV_TRAIN,
                                       data = validation_b0)
confusionMatrix(validation_b0$FLOOR,
                factor(validation_building_0_floor$predictions))

# FLOOR BUILDING 1 ------------------------------------------------------------


# floor building 1
data_b1_F_CV <- str_train$SAMP2 %>% filter(BUILDINGID == 1) %>%
  select(contains("WAP"),FLOOR)
test_b1_F_CV <- str_train$SAMP1 %>% filter(BUILDINGID == 1)
validation_b1 <- test %>% filter(BUILDINGID == 1)
validation_b1$FLOOR <- factor(validation_b1$FLOOR)
benchmark_FLOOR_B1_CV_TRAIN <- ranger(FLOOR ~ .,
                                      data = data_b1_F_CV)
saveRDS(object =benchmark_FLOOR_B1_CV_TRAIN, 
        file = "Models/benchmark_FLOOR_B1_CV_TRAIN.rds")
results_CV_TRAIN_B1_FLOOR <- predictions(predict(benchmark_FLOOR_B1_CV_TRAIN,
                                                 data = test_b1_F_CV))
confusionMatrix(factor(results_CV_TRAIN_B1_FLOOR),
                factor(test_b1_F_CV$FLOOR))

validation_building_1_floor <- predict(benchmark_FLOOR_B1_CV_TRAIN,
                                       data = validation_b1)

confusionMatrix(validation_b1$FLOOR,
                factor(validation_building_1_floor$predictions))
# FLOOR BUILDING 2 ------------------------------------------------------------
# floor building 2
data_b2_F_CV <- str_train$SAMP2 %>% filter(BUILDINGID == 2) %>%
  select(contains("WAP"),FLOOR)
test_b2_F_CV <- str_train$SAMP1 %>% filter(BUILDINGID == 2)
validation_b2 <- test %>% filter(BUILDINGID == 2)
validation_b2$FLOOR <- factor(validation_b2$FLOOR)
benchmark_FLOOR_B2_CV_TRAIN <- ranger(FLOOR ~ .,
                                      data = data_b2_F_CV)
saveRDS(object = benchmark_FLOOR_B2_CV_TRAIN,
        file = "Models/benchmark_FLOOR_B2_CV_TRAIN.rds")
results_CV_TRAIN_B2_FLOOR <- predictions(predict(benchmark_FLOOR_B2_CV_TRAIN,
                                                 data = test_b2_F_CV))
confusionMatrix(factor(results_CV_TRAIN_B2_FLOOR),
                factor(test_b2_F_CV$FLOOR))

validation_building_2_floor <- predict(benchmark_FLOOR_B2_CV_TRAIN,
                                       data = validation_b2)

confusionMatrix(validation_b2$FLOOR,
                factor(validation_building_2_floor$predictions))

# LONGITUDE GENERAL ------------------------------------------------------
# longitude general

benchmark_LONGITUDE<- ranger(formula = LONGITUDE~.,
                                 data = str_train$SAMP2 %>% 
                                   # filter(BUILDINGID == 0) %>%
                                   select(-BUILDINGID,-FLOOR,-LATITUDE))
saveRDS(object =benchmark_building_CV_TRAIN, 
        file = "Models/benchmark_LONGITUDE.rds")
results_B0_LONGITUDE <-
  predictions(predict(benchmark_LONGITUDE,
                      str_train$SAMP1))
postResample(str_train$SAMP1$LONGITUDE,results_B0_LONGITUDE)

results_validation$LONGITUDE <- predictions(
  predict(benchmark_LONGITUDE, test))
postResample(test$LONGITUDE,results_validation$LONGITUDE)



# LONGITUDE BUILDING 0 ------------------------------------------------------
# longitude b0

benchmark_LONGITUDE_B0 <- ranger(formula = LONGITUDE~.,
                                 data = str_train$SAMP2 %>% 
                                   filter(BUILDINGID == 0) %>%
                                   select(-BUILDINGID,-FLOOR,-LATITUDE))
saveRDS(object =benchmark_LONGITUDE_B0, 
        file = "Models/benchmark_LONGITUDE_B0.rds")
results_B0_LONGITUDE <-
  predictions(predict(benchmark_LONGITUDE_B0,
                      str_train$SAMP1 %>% filter(BUILDINGID == 0)))
LONGITUDE_B0_train<- str_train$SAMP1 %>%
  filter(BUILDINGID==0) %>% select(LONGITUDE)
postResample(LONGITUDE_B0_train$LONGITUDE,results_B0_LONGITUDE)

results_validation_LONGITUDE_B0 <- predictions(
  predict(benchmark_LONGITUDE_B0, test %>% filter(BUILDINGID==0) ))
LONGITUDE_B0_validation <- test %>% filter(BUILDINGID==0) %>% select(LONGITUDE)
postResample(LONGITUDE_B0_validation$LONGITUDE,results_validation_LONGITUDE_B0)


# LONGITUDE BUILDING 1 ------------------------------------------------------
# longitude b1

benchmark_LONGITUDE_B1 <- ranger(formula = LONGITUDE~.,
                                 data = str_train$SAMP2 %>% 
                                   filter(BUILDINGID == 1) %>%
                                   select(-BUILDINGID,-FLOOR,-LATITUDE))
saveRDS(object =benchmark_LONGITUDE_B1, 
        file = "Models/benchmark_LONGITUDE_B1.rds")
results_B1_LONGITUDE <-
  predictions(predict(benchmark_LONGITUDE_B1,
                      str_train$SAMP1 %>% filter(BUILDINGID == 1)))
LONGITUDE_B1_train<- str_train$SAMP1 %>%
  filter(BUILDINGID==1) %>% select(LONGITUDE)
postResample(LONGITUDE_B1_train$LONGITUDE,results_B1_LONGITUDE)

results_validation_LONGITUDE_B1 <- predictions(
  predict(benchmark_LONGITUDE_B1, test %>% filter(BUILDINGID==1) ))
LONGITUDE_B1_validation <- test %>% filter(BUILDINGID==1) %>% select(LONGITUDE)
postResample(LONGITUDE_B1_validation$LONGITUDE,results_validation_LONGITUDE_B1)

# LONGITUDE BUILDING 2 ------------------------------------------------------
# longitude b2

benchmark_LONGITUDE_B2 <- ranger(formula = LONGITUDE~.,
                                 data = str_train$SAMP2 %>% 
                                   filter(BUILDINGID == 2) %>%
                                   select(-BUILDINGID,-FLOOR,-LATITUDE))
saveRDS(object =benchmark_LONGITUDE_B2, 
        file = "Models/benchmark_LONGITUDE_B2.rds")
results_B2_LONGITUDE <-
  predictions(predict(benchmark_LONGITUDE_B2,
                      str_train$SAMP1 %>% filter(BUILDINGID == 2)))
LONGITUDE_B2_train<- str_train$SAMP1 %>%
  filter(BUILDINGID==2) %>% select(LONGITUDE)
postResample(LONGITUDE_B2_train$LONGITUDE,results_B2_LONGITUDE)

results_validation_LONGITUDE_B2 <- predictions(
  predict(benchmark_LONGITUDE_B2, test %>% filter(BUILDINGID==2) ))
LONGITUDE_B2_validation <- test %>% filter(BUILDINGID==2) %>% select(LONGITUDE)
postResample(LONGITUDE_B2_validation$LONGITUDE,results_validation_LONGITUDE_B2)

# LATITUDE GENERAL ------------------------------------------------------
# LATITUDE general

benchmark_LATITUDE_CV_TRAIN <- ranger(formula = LATITUDE~.,
                                      data = str_train$SAMP2 %>%
                                        select(-BUILDINGID,-FLOOR,-LATITUDE))
saveRDS(object =benchmark_building_CV_TRAIN, 
        file = "Models/benchmark_LATITUDE_CV_TRAIN.rds")
results_CV_TRAIN$LATITUDE_GENERAL <-
  predictions(predict(benchmark_LATITUDE_CV_TRAIN,str_train$SAMP1))
postResample(str_train$SAMP1$LATITUDE,results_CV_TRAIN$LATITUDE_GENERAL)

results_validation$LATITUDE_GENERAL <- predictions(
  predict(benchmark_LATITUDE_CV_TRAIN, test))
postResample(test$LATITUDE,results_validation$LATITUDE_GENERAL)

# LATITUDE BUILDING 1 ------------------------------------------------------
# LATITUDE b1

benchmark_LATITUDE_B1 <- ranger(formula = LATITUDE~.,
                                data = str_train$SAMP2 %>% 
                                  filter(BUILDINGID == 1) %>%
                                  select(-BUILDINGID,-FLOOR,-LONGITUDE))
saveRDS(object =benchmark_LATITUDE_B1, 
        file = "Models/benchmark_LATITUDE_B1.rds")
results_B1_LATITUDE <-
  predictions(predict(benchmark_LATITUDE_B1,
                      str_train$SAMP1 %>% filter(BUILDINGID == 1)))
LATITUDE_B1_train<- str_train$SAMP1 %>%
  filter(BUILDINGID==1) %>% select(LATITUDE)
postResample(LATITUDE_B1_train$LATITUDE,results_B1_LATITUDE)

results_validation_LATITUDE_B1 <- predictions(
  predict(benchmark_LATITUDE_B1, test %>% filter(BUILDINGID==1) ))
LATITUDE_B1_validation <- test %>% filter(BUILDINGID==1) %>% select(LATITUDE)
postResample(LATITUDE_B1_validation$LATITUDE,results_validation_LATITUDE_B1)

# LATITUDE BUILDING 2 ------------------------------------------------------
# LATITUDE b2

benchmark_LATITUDE_B2 <- ranger(formula = LATITUDE~.,
                                data = str_train$SAMP2 %>% 
                                  filter(BUILDINGID == 2) %>%
                                  select(-BUILDINGID,-FLOOR,-LONGITUDE))
saveRDS(object =benchmark_LATITUDE_B2, 
        file = "Models/benchmark_LATITUDE_B2.rds")
results_B2_LATITUDE <-
  predictions(predict(benchmark_LATITUDE_B2,
                      str_train$SAMP1 %>% filter(BUILDINGID == 2)))
LATITUDE_B2_train<- str_train$SAMP1 %>%
  filter(BUILDINGID==2) %>% select(LATITUDE)
postResample(LATITUDE_B2_train$LATITUDE,results_B2_LATITUDE)

results_validation_LATITUDE_B2 <- predictions(
  predict(benchmark_LATITUDE_B2, test %>% filter(BUILDINGID==2) ))
LATITUDE_B2_validation <- test %>% filter(BUILDINGID==2) %>% select(LATITUDE)
postResample(LATITUDE_B2_validation$LATITUDE,results_validation_LATITUDE_B2)

