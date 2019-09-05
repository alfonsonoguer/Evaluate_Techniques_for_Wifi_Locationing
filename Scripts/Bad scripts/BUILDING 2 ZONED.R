full_data_b2 <- full_data %>% filter(BUILDINGID == 2 )

# for easier understanding we center arround 00
minlat <- min(full_data_b2$LATITUDE)
minlong <- min(full_data_b2$LONGITUDE)
full_data_b2$LONGITUDE <- full_data_b2$LONGITUDE - minlong
full_data_b2$LATITUDE  <-  full_data_b2$LATITUDE - minlat


# plot_ly(x = long~full_data_b2$LONGITUDE,
#         y = lat~full_data_b2$LATITUDE,
#         type = 'scatter',
#         mode = 'markers') %>% layout(xaxis = a, yaxis = a)



# training zones we want to include some margins to the model
B2_NORTH <- full_data_b2[full_data_b2$LONGITUDE > 30 &
                                full_data_b2$LATITUDE > 55,] 
B2_SOUTH <- full_data_b2[full_data_b2$LONGITUDE < 80 &
                                full_data_b2$LATITUDE < 55,] 
B2_EAST <- full_data_b2[full_data_b2$LONGITUDE > 95|
                               (full_data_b2$LATITUDE < 45 &
                                  full_data_b2$LONGITUDE > 65),]
B2_WEST <- full_data_b2[full_data_b2$LONGITUDE < 15|
                               (full_data_b2$LATITUDE > 75 &
                                  full_data_b2$LONGITUDE < 45),]


# graph to see the zone distribution
ggplot() + 
  geom_jitter(data = B2_NORTH, aes(x=LONGITUDE,y = LATITUDE), color ="red") +
  geom_jitter(data = B2_SOUTH, aes(x=LONGITUDE,y = LATITUDE), color ="green") 
  #geom_jitter(data = B2_EAST, aes(x=LONGITUDE,y = LATITUDE), color ="purple")+
  # geom_jitter(data = B2_WEST, aes(x=LONGITUDE,y = LATITUDE), color ="blue")

ggplot(data = combine(B2_NORTH,B2_SOUTH,B2_EAST,B2_WEST),
       aes(x=LONGITUDE,y = LATITUDE,color=source)) + 
  geom_jitter()

# Zoning 
full_data_b2$zone <- "other"
# north
full_data_b2[(full_data_b2$LONGITUDE > 40 &
                     full_data_b2$LONGITUDE < 100) & 
                    (full_data_b2$LATITUDE > 60 &
                       full_data_b2$LATITUDE < 120),"zone"] <- "north"

# south
full_data_b2[(full_data_b2$LONGITUDE > 9 &
                     full_data_b2$LONGITUDE < 73) &
                    (full_data_b2$LATITUDE > 0 &
                       full_data_b2$LATITUDE < 50),"zone"] <- "south"
# west 
full_data_b2[(full_data_b2$zone != "north" &
                     full_data_b2$zone != "south") &
                    (full_data_b2$LONGITUDE > 55),"zone"] <- "west"
# est 
full_data_b2[(full_data_b2$zone != "north" &
                     full_data_b2$zone != "south") &
                    (full_data_b2$LONGITUDE < 55),"zone"] <- "est"
# the data in the building 2 tends to the north
table(full_data_b2$zone)

ggplot(data=full_data_b2) + 
  geom_jitter(aes(x=LONGITUDE,y = LATITUDE,color = zone))



# modeling by zone --------------------------------------------------------


MODEL_B2_zone <- full_data_b2 %>% filter(source == 'training') %>% 
  select(contains("WAP"),zone)
validation_B2_zone <- full_data_b2 %>% filter(source == 'validation') %>% 
  select(contains("WAP"),zone)

model_B2_zone <- ranger(formula = as.formula(zone~.), MODEL_B2_zone)



pred_val_zones <- predictions(predict(object = model_B2_zone,
                                      validation_B2_zone))
confusionMatrix(data = as.factor(validation_B2_zone$zone),
                reference = as.factor(pred_val_zones))
zones <- tibble( values= validation_B2_zone$zone,
                     predictions = pred_val_zones)

comparison <- function(x){ifelse(x[1]==x[2],
                                         return(TRUE),
                                         return(FALSE))}
predictedwell <- apply(X = zones, MARGIN = 1, FUN = comparison)
full_data_b2$predictedwell <- predictedwell


plot_missed_zones <- validation_B2_zone <- full_data_b2 %>% 
  filter(source == 'validation')
plot_missed_zones$predictedwell <- predictedwell
# ploting the error predicting zone train
ggplot() + 
  geom_jitter(data=plot_missed_zones, aes(x=LONGITUDE,y = LATITUDE)) + 
  geom_jitter(data=plot_missed_zones %>% filter(predictedwell == FALSE),
              aes(x=LONGITUDE,y = LATITUDE), color = "red")

# full_data_b2 %>% filter(predictedwell == FALSE, zone == "north") %>% 
#   select(-starts_with("WAP"))

# ploting the error on validation
# Zoning test
test2$zone <- "other"
# north
test2[(test2$LONGITUDE > 40 & test2$LONGITUDE < 100) & 
         (test2$LATITUDE > 60 & test2$LATITUDE < 120),"zone"] <- "north"

# south
test2[(test2$LONGITUDE > 9 & test2$LONGITUDE < 73) &
         (test2$LATITUDE > 0 & test2$LATITUDE < 50),"zone"] <- "south"
# west 
test2[(test2$zone != "north" & test2$zone != "south") &
         (test2$LONGITUDE > 55),"zone"] <- "west"
# est 
test2[(test2$zone != "north" & test2$zone != "south") &
         (test2$LONGITUDE < 55),"zone"] <- "est"


zones_validation_b2 <- predictions(predict(object = model_B2_zone, test2))
confusionMatrix(data = as.factor(test2$zone),
                reference = as.factor(zones_validation_b2))

test2$zonepredictions <- zones_validation_b2

predictedwell_validation <- apply(X = test2 %>% select(zone,zonepredictions),
                                  MARGIN = 1, FUN = comparison)
test2$predictedwell <- predictedwell_validation

ggplot() + 
  geom_jitter(data=test2, aes(x=LONGITUDE,y = LATITUDE)) +
  geom_jitter(data=test2 %>% filter(predictedwell == FALSE),
              aes(x=LONGITUDE,y = LATITUDE), color = "red") +
  geom_jitter(data=full_data_b2, aes(x=LONGITUDE,y = LATITUDE), color = "blue") 
table(test2$zone)

train_long <- ranger(data = train %>% select(starts_with("WAP"),LONGITUDE),
                      LONGITUDE~.)
train_lat <- ranger(data = train %>% select(starts_with("WAP"),LATITUDE),
                      LATITUDE~.)


longitude_tree <- train(data = full_data_b2 %>% select(starts_with("WAP"),LONGITUDE),
                        LONGITUDE~. , method = "rpart",tuneLength=15)
latitude_tree <- train(data = full_data_b2 %>% select(starts_with("WAP"),LATITUDE),
                                                LATITUDE~. ,
                       method = "rpart",tuneLength=15)
results_tree_b2 <- tibble(pred_longitude = predict(longitude_tree,
                                                   full_data_b2),
                          pred_latitude = predict(latitude_tree,
                                                  full_data_b2))

results_general <- tibble(pred_longitude =  predictions(predict(train_long,
                                                                train)),
                          pred_latitude = predictions(predict(train_lat,
                                                              train)))

ggplot() + 
  geom_point(data=results_tree_b2, aes(x=pred_longitude,y = pred_latitude)) +
  geom_jitter(data=full_data_b2, aes(x=LONGITUDE,y = LATITUDE), color = "blue")


ggplot() + 
  geom_point(data=results_general, aes(x=pred_longitude,y = pred_latitude)) +
  geom_jitter(data=train, aes(x=LONGITUDE,y = LATITUDE), color = "blue") 


