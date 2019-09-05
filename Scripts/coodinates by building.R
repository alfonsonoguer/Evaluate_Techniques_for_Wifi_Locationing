nombres <- c("LONGITUDE","LATITUDE")
lista_modelo_por_edificio <- list()
equation <- c("LONGITUDE~.","LATITUDE~.")

  for (j in 0:2) {
    TRAIN <- full_data %>%
      filter(BUILDINGID == j,source == 'training') %>%
      select(contains("WAP"),LATITUDE)
    lista_modelo_por_edificio[[paste('LATITUDE',"building",j,sep = "_")]] <-
      ranger(formula = as.formula(LATITUDE~.),  TRAIN)
  }


resultados_por_edificio <- list()

for (j in 0:2) {
  for (z in 1:2){
    i <- nombres[z]
    validation_int <- full_data %>% filter(BUILDINGID == j,
                                           source == 'validation')
    predictions <- predictions(predict(
      lista_modelo_por_edificio[[paste(i,"building",j,sep = "_")]],
      validation_int))
    tibblename<- bind_cols(validation_int %>%  select(i,'FLOOR'),
                           prediction = predictions)
    resultados_por_edificio[[paste(i,"building",j,sep = "_")]] <- tibblename
  }
}


# we plot the error -------------------------------------------------------


longitudebybuilding <- 
  as_tibble(combine(resultados_por_edificio$LONGITUDE_building_0,
                    resultados_por_edificio$LONGITUDE_building_1,
                    resultados_por_edificio$LONGITUDE_building_2,
                    names = c("B0","B1","B2")))
latitudebybuilding <-
  as_tibble(combine(resultados_por_edificio$LATITUDE_building_0,
                    resultados_por_edificio$LATITUDE_building_1,
                    resultados_por_edificio$LATITUDE_building_2,
                    names = c("B0","B1","B2")))

names(longitudebybuilding) <- c("LONGITUDE","FLOOR","prediction_long","source")  
names(latitudebybuilding) <- c("LATITUDE","FLOOR","prediction_lat","source")  

results_by_building <- cbind(longitudebybuilding,latitudebybuilding[,c(1,3)])

results_by_building <-as_tibble( 
  results_by_building %>% mutate(errorlong = 
                                   LONGITUDE - prediction_long,
                                 errorlat = LATITUDE - prediction_lat))

results_by_building$errorlongbins <- discretize(
  x = results_by_building$errorlong, method = "fixed", 
  breaks = c(-Inf, -10,10, Inf), labels = c("undershoot","ok","overshoot") )
results_by_building$errorlatbins <- discretize(
  x = results_by_building$errorlat, method = "fixed", 
  breaks = c(-Inf, -10,10, Inf), labels = c("undershoot","ok","overshoot") )

# hist of error
ggplot(data = results_by_building) +
  geom_histogram(aes(x = errorlong, fill = source ), binwidth = 1) +
  facet_grid(rows = vars(source), scales = "free")

ggplot(data = results_by_building) +
  geom_histogram(aes(x = errorlat, fill = source ), binwidth = 1) +
  facet_grid(rows = vars(source), scales = "free")

{
ggplot(data = results_by_building %>% filter(source == 'B2',FLOOR == 1) ) +
  geom_jitter(aes(x=LATITUDE,y = LONGITUDE),color = "blue") + 
  geom_jitter(aes(x=prediction_lat ,y = prediction_long, color = errorlongbins))

ggplot(data = results_by_building %>% filter(source == 'B2',FLOOR == 2) ) +
  geom_jitter(aes(x=LATITUDE,y = LONGITUDE),color = "blue") + 
  geom_jitter(aes(x=prediction_lat ,y = prediction_long, color = errorlongbins))

ggplot(data = results_by_building %>% filter(source == 'B2',FLOOR == 3) ) +
  geom_jitter(aes(x=LATITUDE,y = LONGITUDE),color = "blue") + 
  geom_jitter(aes(x=prediction_lat ,y = prediction_long, color = errorlongbins))

ggplot(data = results_by_building %>% filter(source == 'B2',FLOOR == 4) ) +
  geom_jitter(aes(x=LATITUDE,y = LONGITUDE),color = "blue") + 
  geom_jitter(aes(x=prediction_lat,y = prediction_long, color = errorlongbins))}
