xgb <- dummyVars(~ BUILDINGID,
                 data = full_data, fullRank = TRUE)

dummy_xgb <- as_tibble(predict(xgb, full_data))

names(dummy_xgb)[1:2] <- c("dummyB0","dummyB1") 

WAPS <- full_data %>% select(starts_with("WAP")) %>% names()
full_data_xgb <- full_data
full_data_xgb[,WAPS] <- full_data_xgb[,WAPS] + 200

dataxgb <- as_tibble(bind_cols(full_data_xgb %>% select(starts_with("WAP"),LATITUDE,
                                                    LONGITUDE,source),
                               dummy_xgb))

dataxgb_train_x<- dataxgb %>% filter(source == 'training') %>% select(-source,
                                                                    -LONGITUDE)
dataxgb_train_y<- dataxgb %>% filter(source == 'training') %>% select(LONGITUDE)

dtrain <- xgb.DMatrix(label = dataxgb_train_y$LONGITUDE,
                      data = as.matrix(dataxgb_train_x))

dataxgb_valid_x<- dataxgb %>% filter(source == 'validation') %>% select(-source,
                                                                    -LONGITUDE)
dataxgb_valid_y<- dataxgb %>% filter(source == 'validation') %>% 
  select(LONGITUDE)

dvalid <- xgb.DMatrix(label = dataxgb_valid_y$LONGITUDE,
                      data = as.matrix(dataxgb_valid_x))


params <- list(booster = "gbtree", objective = "reg:linear", eta=0.3,
               gamma=0, max_depth=20, min_child_weight=1, subsample=1,
               colsample_bytree=1)

xgbcv <- xgb.train( params = params, data = dtrain, nrounds = 1000,
                    nfold = 5, showsd = T, stratified = T, print_every_n = 10,
                    maximize = F,verbose = 1)




# PREDICTING BY BUILDING --------------------------------------------------

nombres <- c("LONGITUDE","LATITUDE")
lista_modelo_por_edificio <- list()
equation <- c("LONGITUDE~.","LATITUDE~.")
lista_modelo_xgb_por_edificio <- list()
WAPS <- names( full_data %>% select(starts_with("WAP")) )
full_data_xgb <- full_data
full_data_xgb[,WAPS] <- full_data_xgb[,WAPS] + 200

params <- list(booster = "gbtree", objective = "reg:linear", eta=0.2,
               gamma=0, max_depth=10, min_child_weight=1, subsample=1,
               colsample_bytree=1)

for (z in 1:2){
  i <- nombres[z]
  for (j in 0:2) {
    print(j)
    TRAIN <- full_data_xgb %>%
      filter(BUILDINGID == j,source == 'training') %>%
      select(starts_with("WAP"))
    Label <- full_data_xgb %>%  filter(BUILDINGID == j,source == 'training')
    dtrain <- xgb.DMatrix(label = Label[,i],
                          data = as.matrix(TRAIN))
    lista_modelo_xgb_por_edificio[[paste(i,"building",j,sep = "_")]] <-
      xgb.train( params = params, data = dtrain, nrounds = 800,
                 nfold = 5, showsd = T, stratified = T, print_every_n = 10,
                 maximize = F)
  }
}

saveRDS(object = lista_modelo_xgb_por_edificio,file =
          "Models/models_r2/lista_modelo_xgb_por_edificio_long")

resultados_xgb_por_edificio <- list()
postresample_xgb_por_edificio <- list()

for (j in 0:2) {
  for (z in 1:2){
    i <- nombres[z]
    validation_int <- full_data_xgb %>% filter(BUILDINGID == j,
                                               source == 'validation')
    dvalid <- xgb.DMatrix(label = validation_int[,i],
                          data = as.matrix(validation_int %>%
                                             select(starts_with("WAP"))))
    predictions <- predict(
      lista_modelo_xgb_por_edificio[[paste(i,"building",j,sep = "_")]], dvalid)
    
    tibblename<- bind_cols(validation_int %>%  select(i,'FLOOR'),
                           prediction = predictions)
    resultados_xgb_por_edificio[[paste(i,"building",j,sep = "_")]] <- tibblename
    postresample_xgb_por_edificio[[paste(i,"building",j,sep = "_")]] <- 
      postResample(tibblename[,i],tibblename$prediction)
  }
}

longitudebybuilding_xgb <- 
  as_tibble()
latitudebybuilding_xgb <-
  as_tibble(combine(resultados_xgb_por_edificio$LATITUDE_building_0$prediction,
                    resultados_xgb_por_edificio$LATITUDE_building_1$prediction,
                    resultados_xgb_por_edificio$LATITUDE_building_2$prediction,
                    names = c("B0","B1","B2")))

names(longitudebybuilding_xgb) <-
  c("LONGITUDE","FLOOR","prediction_long","source")  
names(latitudebybuilding_xgb) <-
  c("LATITUDE","FLOOR","prediction_lat","source")  

results_by_building_xgb <- 
  cbind(longitudebybuilding_xgb,latitudebybuilding_xgb[,c(1,3)])

results_by_building_xgb <-as_tibble( 
  results_by_building_xgb %>% mutate(errorlong = 
                                       LONGITUDE - prediction_long,
                                     errorlat = LATITUDE - prediction_lat))


ggplot(data = results_by_building_xgb %>% filter(source == 'B2') ) +
  geom_jitter(aes(x=LATITUDE,y = LONGITUDE),color = "blue") + 
  geom_jitter(aes(x=prediction_lat ,y = prediction_long), color = "red")

postResample(results_by_building_xgb$LONGITUDE,
             results_by_building_xgb$prediction_long)
postResample(results_by_building_xgb$LATITUDE,
             results_by_building_xgb$prediction_lat)
