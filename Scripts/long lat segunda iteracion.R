# latitude

general_model_latitude_ranger_wlong <-
  ranger(LATITUDE~., full_data_dummy %>% filter(source == 'training') %>%
           select(contains('WAP'), starts_with("dummy"), LONGITUDE, LATITUDE),
         verbose = TRUE)
# saveRDS(object = general_model_latitude_ranger,file =
#           "Models/models_r2/general_model_latitude_ranger_dummy_high.RDS")
# general_model_latitude_ranger.RDS <- 
#   read_rds(path = 
#              "Models/models_r2/general_model_latitude_ranger_dummy_high.RDS")
results_latitude_wlong <- predictions(predict(
  general_model_latitude_ranger_wlong, data = validation_data))

latitude_validation$lat_ranger_dummy_high_wlong <- results_latitude_wlong
  
postResample(latitude_validation$latitude_val,
             latitude_validation$lat_ranger_dummy_high_wlong)

# longitude

general_model_longitude_ranger_wlat <-
  ranger(LONGITUDE~., full_data_dummy %>% filter(source == 'training') %>%
           select(contains('WAP'), starts_with("dummy"),LONGITUDE,LATITUDE),
         verbose = TRUE)
# saveRDS(object = general_model_longitude_ranger,file =
#           "Models/models_r2/general_model_longitude_ranger_dummy_high.RDS")
# general_model_latitude_ranger.RDS <-
#   read_rds(path = 
#          "Models/models_r2/general_model_lalongitude_ranger_dummy_high.RDS")

results_longitude_wlat <- predictions(predict(
  general_model_longitude_ranger_wlat,
  data = validation_data))


longitude_validation$lon_ranger_dummy_high_wlat <- results_longitude_wlat

postResample(longitude_validation$longitude_val,
             longitude_validation$lon_ranger_dummy_high_wlat)

# floor

validation_dataset <- full_data_dummy  %>%  filter(source == "validation") %>% 
  select(contains("WAP"),starts_with("dummy"))

validation_data2 <- bind_cols(validation_dataset,
                              LONGITUDE = 
                                longitude_validation$lon_ranger_dummy_high_wlat,
                              LATITUDE =
                                latitude_validation$lat_ranger_dummy_high_wlong)

# model floor
# general_model_floor_ranger_long_lat_high_dummy <- 
#   ranger(FLOOR~., full_data_dummy %>% filter(source == 'training') %>% 
#            select(contains('WAP'), starts_with("dummy"), LONGITUDE,
#                   LATITUDE,FLOOR),
#          verbose = TRUE)
# saveRDS(object = general_model_floor_ranger_long_lat_high_dummy,file =
#           "Models/models_r2/floor_model_lon_lat_ranger_dummy_high.RDS")
general_model_floor_ranger_long_lat_high_dummy <- 
  read_rds(path = "Models/models_r2/floor_model_lon_lat_ranger_dummy_high.RDS")
results_floor2 <- predictions(
  predict(general_model_floor_ranger_long_lat_high_dummy,
          data = validation_data2))

temporal <-  full_data_dummy %>%  filter(source == "validation")

floor_validation$floor_ranger_dummy_high_long_lat_Round_2 <- results_floor2

postResample(floor_validation$FLOOR,
             floor_validation$floor_ranger_dummy_high_long_lat_Round_2)