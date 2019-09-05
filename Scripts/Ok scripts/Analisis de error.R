# BUILDING 0 --------------------------------------------------------------

str_train$SAMP2 %>% filter(BUILDINGID == 0) %>% ggplot(aes(x=LATITUDE,
                                                           y=LONGITUDE)) + 
  geom_point()


validation_int0 <- as.data.frame(test %>% filter(BUILDINGID == 0))
resultadoslong_0 <- predictions(predict(lista_modelos_edificio2$LONGITUDE_0,
                                        data = validation_int0 ))
resultadoslat_0 <- predictions(predict(lista_modelos_edificio2$LATITUDE_0,
                                        data = validation_int0 ))
resultados_test_edificio2$LONGITUDE_0$predictions
dataforerrors_b0 <- tibble(predLONGITUDE = resultadoslong_0,
                           LONGITUDE = validation_int0$LONGITUDE,
                           predLATITUDE = resultadoslat_0,
                           LATITUDE = validation_int0$LATITUDE,
                           FLOOR = validation_int0$FLOOR,
                           USERID = validation_int0$USERID)

datagraph0 <- dataforerrors_b0 %>% mutate(errorLONG = 
                                           abs(LONGITUDE - predLONGITUDE),
                                         errorLAT =
                                           abs(LATITUDE - predLATITUDE))

ggplot(data = datagraph0) + geom_histogram(aes(x = errorLAT, fill = FLOOR),
                                           binwidth = 1) +
  labs(title = "HIST ERROR B0 LATITUDE")

ggplot(data = datagraph0) + geom_histogram(aes(x = errorLONG, fill = FLOOR),
                                           binwidth = 1) +
  labs(title = "HIST ERROR B0 LONGITUDE")

datagraph0 %>% arrange(desc(errorLAT)) %>% filter(errorLAT > 12 ) %>% 
  ggplot() +
  geom_jitter(aes(x = LATITUDE, y = LONGITUDE , color = FLOOR)) + 
  labs(title = "SCATER ERROR B0 LATITUDE")

datagraph0 %>% arrange(desc(errorLONG)) %>% filter(errorLONG > 15 ) %>% 
  ggplot() +
   geom_jitter(aes(x = LATITUDE, y = LONGITUDE , color = USERID)) + 
  labs(title = "SCATER ERROR B0 LONGITUDE")

# SCATER OF BIG ERROR POSITION VS PREDICTION
datagraph0 %>% arrange(desc(errorLAT)) %>% #filter(errorLAT > 12 ) %>%
  ggplot() +
  geom_point(aes(x = LATITUDE, y = LONGITUDE ), color = "chartreuse4") + 
  geom_point(aes(x = predLATITUDE, y = predLONGITUDE), color = "red") + 
  labs(title = "position green vs prediction red")



# BUILDING 1 --------------------------------------------------------------

validation_int1 <- as.data.frame(test %>% filter(BUILDINGID == 1))
resultadoslong_1 <- predictions(predict(lista_modelos_edificio2$LONGITUDE_1,
                                        data = validation_int1 ))
resultadoslat_1 <- predictions(predict(lista_modelos_edificio2$LATITUDE_1,
                                       data = validation_int1 ))
resultados_test_edificio2$LONGITUDE_1$predictions
dataforerrors_b1 <- tibble(predLONGITUDE = resultadoslong_1,
                           LONGITUDE = validation_int1$LONGITUDE,
                           predLATITUDE = resultadoslat_1,
                           LATITUDE = validation_int1$LATITUDE,
                           FLOOR = validation_int1$FLOOR,
                           USERID = validation_int1$USERID)

datagraph1 <- dataforerrors_b1 %>% mutate(errorLONG = 
                                           abs(LONGITUDE - predLONGITUDE),
                                         errorLAT =
                                           abs(LATITUDE - predLATITUDE))

datagraph1 %>% arrange(desc(errorLONG)) %>% filter(errorLONG > 15 ) %>% 
  ggplot() +
  geom_jitter(aes(x = LATITUDE, y = LONGITUDE , color = USERID)) + 
  labs(title = "SCATER ERROR B1 LONGITUDE")
ggplot(data = datagraph1) + geom_histogram(aes(x = errorLONG, fill = FLOOR),
                                          binwidth = 1) +
  labs(title = "HIST ERROR B1 LONGITUDE")

datagraph1 %>% arrange(desc(errorLAT)) %>% filter(errorLAT > 12 ) %>% 
  ggplot() +
  geom_jitter(aes(x = LATITUDE, y = LONGITUDE , color = FLOOR)) + 
  labs(title = "SCATER ERROR B1 LATITUDE")
ggplot(data = datagraph1) + geom_histogram(aes(x = errorLAT, fill = FLOOR),
                                          binwidth = 1) +
  labs(title = "HIST ERROR B1 LATITUDE")

datagraph1 %>% arrange(desc(errorLAT)) %>% #filter(errorLAT > 12 ) %>%
  ggplot() +
  geom_point(aes(x = LATITUDE, y = LONGITUDE ), color = "chartreuse4") + 
  geom_point(aes(x = predLATITUDE, y = predLONGITUDE), color = "red") + 
  labs(title = "position green vs prediction red")

# BUILDING 2 --------------------------------------------------------------


validation_int2 <- as.data.frame(test %>% filter(BUILDINGID == 2))
resultadoslong_2 <- predictions(predict(lista_modelos_edificio2$LONGITUDE_2,
                                        data = validation_int2 ))
resultadoslat_2 <- predictions(predict(lista_modelos_edificio2$LATITUDE_2,
                                       data = validation_int2 ))
resultados_test_edificio2$LONGITUDE_2$predictions
dataforerrors_b2 <- tibble(predLONGITUDE = resultadoslong_2,
                           LONGITUDE = validation_int2$LONGITUDE,
                           predLATITUDE = resultadoslat_2,
                           LATITUDE = validation_int2$LATITUDE,
                           FLOOR = validation_int2$FLOOR,
                           USERID = validation_int2$USERID)

datagraph2 <- dataforerrors_b2 %>% mutate(errorLONG = 
                                           abs(LONGITUDE - predLONGITUDE),
                                         errorLAT =
                                           abs(LATITUDE - predLATITUDE))

datagraph2 %>% arrange(desc(errorLONG)) %>% filter(errorLONG > 15 ) %>% 
  ggplot() +
  geom_jitter(aes(x = LATITUDE, y = LONGITUDE , color = USERID)) + 
  labs(title = "SCATER ERROR B2 LONGITUDE")
ggplot(data = datagraph2) + geom_histogram(aes(x = errorLONG, fill = FLOOR),
                                          binwidth = 1) +
  labs(title = "HIST ERROR B2 LONGITUDE")

datagraph2 %>% arrange(desc(errorLAT)) %>% filter(errorLAT > 12 ) %>% 
  ggplot() +
  geom_jitter(aes(x = LATITUDE, y = LONGITUDE , color = FLOOR)) + 
  labs(title = "SCATER ERROR B2 LATITUDE")
ggplot(data = datagraph2) + geom_histogram(aes(x = errorLAT, fill = FLOOR),
                                          binwidth = 1) +
  labs(title = "HIST ERROR B2 LATITUDE")



datagraph2 %>% arrange(desc(errorLAT)) %>% #filter(errorLAT > 12 ) %>%
  ggplot() +
  geom_point(aes(x = LATITUDE, y = LONGITUDE ), color = "chartreuse4") + 
  geom_point(aes(x = predLATITUDE, y = predLONGITUDE), color = "red") + 
  labs(title = "position green vs prediction red")


train %>% filter(BUILDINGID == 2) %>% ggplot() +
  geom_point(aes(x = LATITUDE, y = LONGITUDE ), color = "chartreuse4")

# genero nuevos modelos para b2 y miro error
data_model_b2 <- train %>% filter(BUILDINGID == 2) %>%
  select(starts_with("WAP"), LONGITUDE, LATITUDE) 
model_lon_b2 <- ranger(formula = LONGITUDE ~ ., 
                       data = data_model_b2 %>% select(-LATITUDE))  
model_lat_b2 <- ranger(formula = LATITUDE ~ ., 
                       data = data_model_b %>% select(-LONGITUDE)) 

validation_int2b <- as.data.frame(test %>% filter(BUILDINGID == 2))
resultadoslong_2b <- predictions(predict(model_lon_b2,
                                        data = validation_int2b ))
resultadoslat_2b <- predictions(predict(model_lat_b2,
                                       data = validation_int2b ))
dataforerrors_b2b <- tibble(predLONGITUDE = resultadoslong_2b,
                           LONGITUDE = validation_int2b$LONGITUDE,
                           predLATITUDE = resultadoslat_2b,
                           LATITUDE = validation_int2b$LATITUDE,
                           FLOOR = validation_int2b$FLOOR,
                           USERID = validation_int2b$USERID)


model_lon_b2_knn <- ranger(formula = LONGITUDE ~ ., 
                       data = data_model_b2 %>% select(-LATITUDE))  
model_lat_b2_knn <- ranger(formula = LATITUDE ~ ., 
                       data = data_model_b %>% select(-LONGITUDE)) 

dataforerrors_b2b %>% 
  ggplot() +
  geom_point(aes(x = LATITUDE, y = LONGITUDE ), color = "chartreuse4") + 
  geom_point(aes(x = predLATITUDE, y = predLONGITUDE), color = "red") + 
  labs(title = "position green vs prediction red") +
  geom_point(data = data_model_b2,aes(x = LATITUDE, y = LONGITUDE),
             color = "blue")



data_model_knn <- data_model_b2 %>% select(starts_with("WAP"),LONGITUDE,
                                           LATITUDE)
ZerobarB1 <- nearZeroVar(data_model_knn,saveMetrics = TRUE)
data_model_knn <- data_model_knn[,!ZerobarB1$zeroVar]
names_train <- data_model_knn %>% select(starts_with("WAP")) %>% names()

names_test <- validation_int2b %>% select(starts_with("WAP")) %>% names()

errors2<-cbind(dataforerrors_b2b,
               data.frame(LONG_KNN = knn_LONG$pred,LAT_KNN = knn_LAT$pred))

errors2 %>% 
  ggplot() +
  geom_point(aes(x = LATITUDE, y = LONGITUDE ), color = "chartreuse4") + 
  geom_point(aes(x = predLATITUDE, y = predLONGITUDE), color = "red") +
  geom_point(data = data_model_b2,aes(x = LATITUDE, y = LONGITUDE),
             color = "yellow") +
  geom_point(aes(x = LAT_KNN, y = LONG_KNN), color = "blue") +
  labs(title = "position green vs prediction red vs knn blue") 
knn_resample_b2<- list(LONG = postResample(validation_int2b$LONGITUDE,
                                        errors2$LONG_KNN),
                    LAT = postResample(validation_int2b$LATITUDE,
                                       errors2$predLATITUDE))

print_loops_b0 <- tibble(LONG = resultados_test_edificio2$
                           LONGITUDE_0$predictions,
                         LAT = resultados_test_edificio2$
                           LATITUDE_0$predictions,
                         LONGITUDE = test %>% filter(BUILDINGID == 0) %>%
                           select(LONGITUDE),
                         LATITUDE = test %>% filter(BUILDINGID == 0) %>%
                           select(LATITUDE)
                         )

print_loops_b1 <- tibble(LONG = resultados_test_edificio2$
                           LONGITUDE_1$predictions,
                         LAT = resultados_test_edificio2$
                           LATITUDE_1$predictions,
                         LONGITUDE = test %>% 
                           filter(BUILDINGID == 1) %>% select(LONGITUDE),
                         LATITUDE = test %>% 
                           filter(BUILDINGID == 1) %>% select(LATITUDE)
                         )

print_loops_b2 <- tibble(LONG = resultados_test_edificio2$LONGITUDE_2$
                           predictions,
                         LAT = resultados_test_edificio2$LATITUDE_2$
                           predictions,
                         LONGITUDE = test %>% filter(BUILDINGID == 2) %>%
                           select(LONGITUDE),
                         LATITUDE = test %>% filter(BUILDINGID == 2) %>%
                           select(LATITUDE)
                         )

ggplotly(ggplot(data = print_loops_b2) +
           # geom_point(aes(x = LONG, y = LAT ),
           #            color = "red") + 
           geom_jitter(aes(x = LONGITUDE$LONGITUDE, y = LATITUDE$LATITUDE),
                      color = "chartreuse4") +
           geom_jitter(data = train %>% filter(BUILDINGID == 2),
                      aes(x = LONGITUDE, y = LATITUDE),
                      color = "yellow") +
           # geom_point(aes(x = LAT_KNN, y = LONG_KNN), color = "blue") +
           labs(title = "position green vs prediction red ") 
         )

LOOP_resample_B2<- list(LONG = postResample(pred = print_loops_b2$LONG,
                                         obs = print_loops_b2$LONGITUDE$
                                           LONGITUDE),
                    LAT = postResample(pred = print_loops_b2$LAT,
                                       obs = print_loops_b2$LATITUDE$LATITUDE)
                    )
