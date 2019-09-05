# libraries ---------------------------------------------------------------

install.packages("xgboost", repos=c("http://dmlc.ml/drat/",
                                    getOption("repos")), type="source")

pacman::p_load(readr,caret,tidyverse,ranger,
               doParallel,xgboost,SuperLearner,parallel)

validation <- read_csv("Data/validationData.csv")

data_validation <- full_data %>% filter(source == 'validation')

# we only keep the WAPS WE CARE ABOUT

WAPS <- names(full_data %>% select(starts_with("WAP")))

test_predict <- data_validation %>% select(contains('WAP'))

# for easier filtering we change the WAPS of 100 to -200


full_data[,WAPS] <- apply(full_data[,WAPS], 1:2,
                          function(x){if_else(x==100, -200, x)})

# it will be interesting to plot which places have the higest WAP by building
# and floor so we can locate the ruter

pos_max_wap <- apply(full_data %>% select(starts_with('WAP')), 1,
                     function(x){which.max(x)})
list_of_WAPS <- sapply(pos_max_wap, function(x){return(names(full_data)[x])})
full_data<-full_data %>% mutate(highestWAP=list_of_WAPS)

# predicting

model_BUILDING_ranger <- read_rds(path = 
                                    "Models/models_r2/model_BUILDING_ranger.RDS")


pred_building <- predictions(predict(model_BUILDING_ranger,
                                  test_predict))
test_predict <- test_predict %>% mutate(BUILDINGID = pred_building)
postResample(data_validation$BUILDINGID,test_predict$BUILDINGID)


# general_model_latitude_ranger <-
#   ranger(LATITUDE~., data_train  %>%
#            select(contains('WAP'),LATITUDE),
#          verbose = TRUE,num.trees = 2000)
# saveRDS(object = general_model_latitude_ranger,file =
#           "Models/models_r2/general_model_latitude_ranger_dummy_high.RDS")

# general_model_latitude_ranger_full_data <-
#   ranger(LATITUDE~., data_train  %>%
#            select(contains('WAP'),LATITUDE),
#          verbose = TRUE,num.trees = 2000)
# saveRDS(object = general_model_latitude_ranger_full_data,file =
#           "Models/models_r2/general_model_latitude_ranger_full_data.RDS")

general_model_latitude_ranger <- read_rds(path = 
                                            "Models/models_r2/general_model_latitude_ranger_dummy_high.RDS")
general_model_latitude_ranger_full_data <- read_rds(path = 
                                            "Models/models_r2/general_model_latitude_ranger_full_data.RDS")
pred_LAT <- predictions(predict(general_model_latitude_ranger,
                                     test_predict))
test_predict <- test_predict %>% mutate(LATITUDE = pred_LAT)
postResample(data_validation$LATITUDE,test_predict$LATITUDE)


# longitude

# need to dummy for long

# general_model_longitude_ranger <-
#   ranger(LONGITUDE~., full_data_dummy %>% filter(source == 'training') %>%
#            select(contains('WAP'), starts_with("dummy"),LONGITUDE),
#          verbose = TRUE, num.trees = 1000 )
# saveRDS(object = general_model_longitude_ranger,file =
#           "Models/models_r2/general_model_longitude_ranger.RDS")
general_model_longitude_ranger_full_data <-
  ranger(LONGITUDE~., full_data_dummy %>% filter(source == 'training') %>%
           select(contains('WAP'), starts_with("dummy"),LONGITUDE),
         verbose = TRUE, num.trees = 1000 )
saveRDS(object = general_model_longitude_ranger_full_data,file =
          "Models/models_r2/general_model_longitude_ranger_full_data.RDS")
general_model_longitude_ranger <- read_rds(path = 
                                                 "Models/models_r2/general_model_longitude_ranger_full_data.RDS")
# need to dummyfy building here

pred_LONG <- predictions(
  predict(general_model_longitude_ranger,
          data = full_data_dummy %>% filter(source == "validation")))

test_predict <- test_predict %>% mutate(LONGITUDE = pred_LONG)
postResample(data_validation$LONGITUDE,test_predict$LONGITUDE)



# model floor
# dummy works better
# general_model_floor_ranger_long_lat_high_dummy <-
#   ranger(FLOOR~., full_data_dummy %>% filter(source == 'training') %>%
#            select(contains('WAP'), starts_with("dummy"),LONGITUDE,LATITUDE,FLOOR),
#          num.trees = 1000, verbose = TRUE)
# saveRDS(object = general_model_floor_ranger_long_lat_high_dummy,file =
#           "Models/models_r2/floor_model_lon_lat_ranger_dummy_high.RDS")
general_model_floor_ranger_long_lat_high_dummy <-
  read_rds(
    path = "Models/models_r2/floor_model_lon_lat_ranger_dummy_high.RDS")

predfloor <- predictions(
  predict(general_model_floor_ranger_long_lat_high_dummy,
          data = test_predict))

test_predict <- test_predict %>% mutate(FLOOR = predfloor)
postResample(data_validation$FLOOR,test_predict$FLOOR)

# second model full cascade

# latitude

# for (j in 0:2) {
#   TRAIN <- full_data %>%
#     filter(BUILDINGID == j,source == 'training') %>%
#     select(contains("WAP"),LONGITUDE)
#   lista_modelo_por_edificio[[paste('LONGITUDE',"building",j,sep = "_")]] <-
#     ranger(formula = as.formula(LONGITUDE~.),  TRAIN)
# }
saveRDS(lista_modelo_por_edificio,
        file = "Models/FINAL_MODELS/lista_modelo_por_edificio_lat.RDS")

# need to dummy for long

general_model_longitude_ranger_with_lat <-
  ranger(LONGITUDE~., full_data_dummy %>% filter(source == 'training') %>%
           select(contains('WAP'), starts_with("dummy"),LONGITUDE,LATITUDE),
         verbose = TRUE, num.trees = 1000 )
saveRDS(object = general_model_longitude_ranger_with_lat,file =
          "Models/models_r2/general_model_longitude_ranger_with_lat")

predlong <- predictions(
  predict(general_model_longitude_ranger_with_lat,
          data = test_predict2))

test_predict <- test_predict %>% mutate(FLOOR = predfloor)
postResample(data_validation$LONGITUDE,predlong)

test_predict2 <- bind_cols(test_predict2,full_data_dummy %>% 
                             filter(source == 'validation') %>%
                             select(starts_with('dummy')))

