# libraries ---------------------------------------------------------------

install.packages("xgboost", repos=c("http://dmlc.ml/drat/",
                                    getOption("repos")), type="source")

pacman::p_load(readr,caret,tidyverse,ggplot2,plotly, ranger,
               plotly,gdata,splitstackshape,
               doParallel,xgboost,seewave,arules,FNN,bbplot,
               SuperLearner,tictoc)
pacman::p_load(caret,caretEnsemble)
library('gdata')
# get the data ------------------------------------------------------------

training <- read_csv("Data/trainingData.csv")
validation <- read_csv("Data/validationData.csv")

full_data <- read_rds("Data/FULL_DATA_CLEAN.RDS")
full_data_dummy <-  read_rds("Data/FULL_DATA_CLEAN_DUMMY.RDS")

data_train_dummy <- full_data_dummy %>% filter(source == "training") %>%
  select(contains("WAP"),contains("dummy"), LONGITUDE, LATITUDE)

data_validation_dummy <- full_data_dummy %>% filter(source == "validation") %>%
  select(contains("WAP"),contains("dummy"), LONGITUDE, LATITUDE)

set.seed('123')
# we preproces no need to run ---------------------------------------------


# we merge and index for easier cleaning

full_data <- combine(training,validation)

# we remove duplicates

full_data <- distinct(full_data)

# we structure the data correctly 

factors<-c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION",
           "USERID", "PHONEID", "source")
full_data[,factors] <-lapply(full_data[,factors], as.factor)
rm(factors)

numeric<-c("LONGITUDE", "LATITUDE")
full_data[,numeric]<-lapply(full_data[,numeric], as.numeric)
rm(numeric)

# we remove timestamp because we dont use it
full_data <- full_data %>% select(-TIMESTAMP)

# we remove useless WAPS

zerovar_training <- nearZeroVar(saveMetrics = TRUE,
                                x = full_data %>%
                                  filter(source == "training") %>%
                                  select(starts_with("WAP")))

zerovar_validation <- nearZeroVar(saveMetrics = TRUE,
                                  x = full_data %>%
                                    filter(source == "validation") %>% 
                                    select(starts_with("WAP")))
# wl which nos devuelve las posiciones de los WAPS con 0 variancia, lo que nos
# permite simplemente quitar las posiciones que no nos interesan 

full_data <- full_data[,-which(zerovar_training$zeroVar |
                                 zerovar_validation$zeroVar)]

# we remove rows were there are no change in the WAP signal
elements_by_row <- apply(full_data %>% select(starts_with("WAP")), 1, 
                         function(x){length(unique(x))})
# we see that there are 73 rows with no wifi signal
full_data <- full_data[elements_by_row != 1,]

# for easier filtering we change the WAPS of 100 to -200

WAPS <- names(full_data %>% select(starts_with("WAP")))
full_data[,WAPS] <- apply(full_data[,WAPS], 1:2, function(x){if_else(x==100,
                                                                    -200,
                                                                    x)})

# we remove signals higer than -20dB because they are imposible
rows_to_remove<- which(apply(full_data %>% select(starts_with('WAP')), 1,
                             function(x){max(x)>-20}))

full_data <- full_data[-rows_to_remove,]
rm(rows_to_remove)

# it will be interesting to plot which places have the higest WAP by building
# and floor so we can locate the ruter

pos_max_wap <- apply(full_data %>% select(starts_with('WAP')), 1,
                     function(x){which.max(x)})
list_of_WAPS <- sapply(pos_max_wap, function(x){return(names(full_data)[x])})
full_data<-full_data %>% mutate(highestWAP=list_of_WAPS)


# We predict ------------------------------------------------


# Prepare Parallel Process
cluster <- makeCluster(detectCores() - 1) # convention leave 1 core for OS
registerDoParallel(cluster)

mycontrol <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          allowParallel = TRUE)
# building

# model_BUILDING <- train(BUILDINGID~.,
#                        data= full_data %>% select(starts_with('WAP'),
#                                                   BUILDINGID),
#                        method ="ranger",
#                        trControl = mycontrol)

# model_BUILDING_ranger <- ranger(BUILDINGID~.,
#                                 data= full_data %>% select(contains('WAP'),
#                                                            BUILDINGID),
#                                 importance = 'permutation',verbose = TRUE)
# model_BUILDING_ranger <- ranger(BUILDINGID~.,
#                                 data= full_data %>%
#                                   filter(source == 'training') %>% 
#                                   select(contains('WAP'), BUILDINGID),
#                                 importance = 'permutation',verbose = TRUE)
# saveRDS(object = model_BUILDING_ranger,file =
#           "Models/models_r2/model_BUILDING_ranger.RDS")
model_BUILDING_ranger <- read_rds(path = 
                                 "Models/models_r2/model_BUILDING_ranger.RDS")

BUILDINGID <- predictions(predict(model_BUILDING_ranger,full_data %>% 
                                    filter(source != 'training') %>% select(contains("WAP"))))
edificio<- tibble(predictions = BUILDINGID,values =
         full_data %>%  filter(source == 'validation') %>%  select(BUILDINGID))

confusionMatrix(edificio$predictions,edificio$values$BUILDINGID)


# dummyfy building and RF for coordinates ---------------------------------
# 
# dummy <- dummyVars(~ BUILDINGID, data = full_data, fullRank = TRUE)
# 
# dummy_building <- as_tibble(predict(dummy, full_data))
# 
# names(dummy_building) <- c("dummyB0","dummyB1") 
# 
# full_data_dummy <- as_tibble(bind_cols(full_data,dummy_building))

# latitude

# general_model_latitude_ranger <- 
#   ranger(LATITUDE~., full_data_dummy %>% filter(source == 'training') %>% 
#            select(contains('WAP'), starts_with("dummy"),LATITUDE),
#          verbose = TRUE)
# saveRDS(object = general_model_latitude_ranger,file =
#           "Models/models_r2/general_model_latitude_ranger_dummy_high.RDS")
general_model_latitude_ranger <- read_rds(path = 
          "Models/models_r2/general_model_latitude_ranger_dummy_high.RDS")
results_latitude <- predictions(predict(general_model_latitude_ranger,
                                        data = full_data_dummy %>% 
                                          filter(source == "validation")))
temporal <-  full_data_dummy %>%  filter(source == "validation")

latitude_validation <- tibble(latitude_val = temporal$LATITUDE,
                              lat_ranger_dummy_high = results_latitude)

postResample(latitude_validation$latitude_val,
             latitude_validation$lat_ranger_dummy_high)

# longitude

# general_model_longitude_ranger <-
#   ranger(LONGITUDE~., full_data_dummy %>% filter(source == 'training') %>%
#            select(contains('WAP'), starts_with("dummy"),LONGITUDE),
#          verbose = TRUE)
# saveRDS(object = general_model_longitude_ranger,file =
#           "Models/models_r2/general_model_longitude_ranger_dummy_high.RDS")
general_model_longitude_ranger.RDS <- read_rds(path = 
          "Models/models_r2/general_model_longitude_ranger_dummy_high.RDS")

results_longitude <- predictions(predict(general_model_longitude_ranger,
                                        data = full_data_dummy %>% 
                                          filter(source == "validation")))
temporal <-  full_data_dummy %>%  filter(source == "validation")

longitude_validation <- tibble(longitude_val = temporal$LONGITUDE,
                              lon_ranger_dummy_high = results_longitude)

postResample(longitude_validation$longitude_val,
             longitude_validation$lon_ranger_dummy_high)

# floor

validation_dataset <- full_data_dummy  %>%  filter(source == "validation") %>% 
  select(contains("WAP"),starts_with("dummy"))

validation_data<- bind_cols(validation_dataset,
                            LONGITUDE = longitude_validation$longitude_val,
                            LATITUDE = latitude_validation$latitude_val)

# model floor
# general_model_floor_ranger_long_lat_high_dummy <- 
#   ranger(FLOOR~., full_data_dummy %>% filter(source == 'training') %>% 
#            select(contains('WAP'), starts_with("dummy"), LONGITUDE,
#                   LATITUDE,FLOOR),
#          verbose = TRUE)
# saveRDS(object = general_model_floor_ranger_long_lat_high_dummy,file =
#           "Models/models_r2/floor_model_lon_lat_ranger_dummy_high.RDS")
general_model_floor_ranger_long_lat_high_dummy <- read_rds(path = 
          "Models/models_r2/floor_model_lon_lat_ranger_dummy_high.RDS")
results_floor <- predictions(
  predict(general_model_floor_ranger_long_lat_high_dummy,
          data = data_validation_dummy))

temporal <-  full_data_dummy %>%  filter(source == "validation")

floor_validation <- tibble(FLOOR = temporal$FLOOR,
                           floor_ranger_dummy_high_long_lat = 
                             results_floor)

postResample(floor_validation$FLOOR,
             floor_validation$floor_ranger_dummy_high_long_lat)
