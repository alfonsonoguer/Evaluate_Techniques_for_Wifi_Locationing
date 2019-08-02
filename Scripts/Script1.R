pacman::p_load(readr,caret,tidyverse,ggplot2,plotly, lubridate, ranger, rpart)


# Coments -----------------------------------------------------------------

# first 2 classification problems building (solved by random randomforest
# and floor needs to be improved study errors.

# 



# Get and preporcess the data ------------------------------------------------

train <- read_csv("Data/trainingData.csv")
train$is.train <- 1

model_building <- train %>% select(contains("WAP"),BUILDINGID,FLOOR,LONGITUDE,
                                   LATITUDE)
model_building$FLOOR <- as.factor(model_building$FLOOR)
model_building$BUILDINGID <- as.factor(model_building$BUILDINGID)
model_building[, tail(names(model_building))]
test <- read_csv("Data/validationData.csv")
test$is.train <- 0
fulldata <- rbind(train,test)
glimpse(fulldata)

fulldata <- fulldata %>% unite(col = "ubication",
                               ... =c("FLOOR","BUILDINGID",
                                      "SPACEID"#,"RELATIVEPOSITION"
                                      ),
                               remove = TRUE)
fulldata$ubication <- as.factor(fulldata$ubication)

fulldata[,tail(names(fulldata),n = 12)]
fulldata$TIMESTAMP <- as_datetime(fulldata$TIMESTAMP)
trainWAP <- model_building %>% select(contains("WAP"))

tc <- trainControl(method = "CV",number = 10)

model_building_01 <- train(x = model_building %>% select(contains("WAP")),
                           y = as.factor(model_building$BUILDINGID),
                           method = "rf",
                           trControl = tc,
                           tuneLength = 4
                           )


# Ranger for MVP ----------------------------------------------------------
# building
data_building <- model_building %>% select(contains("WAP"),BUILDINGID) 
data_building$BUILDINGID <- as.factor(data_building$BUILDINGID)
ranger_model_building <- ranger(BUILDINGID ~ ., data = data_building)
pred.ranger.building <- predict(ranger_model_building, data = test) 

table(test$BUILDINGID, pred.ranger.building$predictions)
# floor building 0
data_building_0_floor <- model_building %>% filter(BUILDINGID == 0) %>%
  select(contains("WAP"),FLOOR)
ranger_model_building_0_floor <- ranger(FLOOR ~ .,
                                        data = data_building_0_floor)
pred.ranger.building_0_floor <- predict(ranger_model_building_0_floor,
                                        data = test %>% 
                                          filter(BUILDINGID == 0))
test_building0floor <- (test%>% filter(BUILDINGID == 0) %>%
  select(FLOOR))
confusionMatrix(as.factor(test_building0floor$FLOOR),
                factor(pred.ranger.building_0_floor$predictions))
# Confusion Matrix and Statistics
{
#   Confusion Matrix and Statistics
#   
#   Reference
#   Prediction   0   1   2   3
#   0  70   4   4   0
#   1   3 198   7   0
#   2   0   5 158   2
#   3   0   0   5  80
#   
#   Overall Statistics
#   
#   Accuracy : 0.944           
#   95% CI : (0.9211, 0.9619)
#   No Information Rate : 0.3862          
#   P-Value [Acc > NIR] : < 2.2e-16       
#   
#   Kappa : 0.9207          
#   
#   Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3
# Sensitivity            0.9589   0.9565   0.9080   0.9756
# Specificity            0.9827   0.9696   0.9807   0.9890
# Pos Pred Value         0.8974   0.9519   0.9576   0.9412
# Neg Pred Value         0.9934   0.9726   0.9569   0.9956
# Prevalence             0.1362   0.3862   0.3246   0.1530
# Detection Rate         0.1306   0.3694   0.2948   0.1493
# Detection Prevalence   0.1455   0.3881   0.3078   0.1586
# Balanced Accuracy      0.9708   0.9631   0.9444   0.9823
}

# floor building 1
data_building_1_floor <- model_building %>% filter(BUILDINGID == 1) %>%
  select(contains("WAP"),FLOOR)
ranger_model_building_1_floor <- ranger(FLOOR ~ .,
                                        data = data_building_1_floor)
pred.ranger.building_1_floor <- predict(ranger_model_building_1_floor,
                                        data = test %>% 
                                          filter(BUILDINGID == 1))
test_building1floor <- (test%>% filter(BUILDINGID == 1) %>%
  select(FLOOR))
confusionMatrix(as.factor(test_building1floor$FLOOR),
                factor(pred.ranger.building_1_floor$predictions))
# Confusion Matrix and Statistics
{
#   Confusion Matrix and Statistics
#   
#   Reference
#   Prediction  0  1  2  3
        #   0  22  2  5  1
        #   1   4 91 45  3
        #   2   0  1 77  9
        #   3   0  0  4 43
#   
#   Overall Statistics
#   
#   Accuracy : 0.759           
#   95% CI : (0.7071, 0.8057)
#   No Information Rate : 0.4267          
#   P-Value [Acc > NIR] : < 2.2e-16       
#   
#   Kappa : 0.6558          
#   
#   Mcnemar's Test P-Value : 8.572e-10       
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3
# Sensitivity           0.84615   0.9681   0.5878   0.7679
# Specificity           0.97153   0.7559   0.9432   0.9841
# Pos Pred Value        0.73333   0.6364   0.8851   0.9149
# Neg Pred Value        0.98556   0.9817   0.7545   0.9500
# Prevalence            0.08469   0.3062   0.4267   0.1824
# Detection Rate        0.07166   0.2964   0.2508   0.1401
# Detection Prevalence  0.09772   0.4658   0.2834   0.1531
# Balanced Accuracy     0.90884   0.8620   0.7655   0.8760
}


# floor building 2
data_building_2_floor <- model_building %>% filter(BUILDINGID == 2) %>%
  select(contains("WAP"),FLOOR)
ranger_model_building_2_floor <- ranger(FLOOR ~ .,
                                        data = data_building_2_floor)
pred.ranger.building_2_floor <- predict(ranger_model_building_2_floor,
                                        data = test %>% 
                                          filter(BUILDINGID == 2))
test_building2floor <- (test%>% filter(BUILDINGID == 2) %>%
  select(FLOOR))
confusionMatrix(as.factor(test_building2floor$FLOOR),
                factor(pred.ranger.building_2_floor$predictions))
# Confusion Matrix and Statistics
{
#   Confusion Matrix and Statistics
#   
#   Reference
#   Prediction   0   1   2   3   4
#   0  22   2   0   0   0
#   1   3 104   3   1   0
#   2   0   4  31  19   0
#   3   0   0   0  39   1
#   4   2   0   0  12  25
#   
#   Overall Statistics
#   
#   Accuracy : 0.8246          
#   95% CI : (0.7737, 0.8682)
#   No Information Rate : 0.4104          
#   P-Value [Acc > NIR] : < 2.2e-16       
#   
#   Kappa : 0.7636          
#   
#   Mcnemars Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
# Sensitivity           0.81481   0.9455   0.9118   0.5493  0.96154
# Specificity           0.99170   0.9557   0.9017   0.9949  0.94215
# Pos Pred Value        0.91667   0.9369   0.5741   0.9750  0.64103
# Neg Pred Value        0.97951   0.9618   0.9860   0.8596  0.99563
# Prevalence            0.10075   0.4104   0.1269   0.2649  0.09701
# Detection Rate        0.08209   0.3881   0.1157   0.1455  0.09328
# Detection Prevalence  0.08955   0.4142   0.2015   0.1493  0.14552
# Balanced Accuracy     0.90326   0.9506   0.9067   0.7721  0.95184
}



# floor building 1 error analysis -----------------------------------------


erroranalysisF1B1 <- test %>% select(-contains("WAP"),-LONGITUDE,
                                     -LATITUDE,-is.train) %>% 
  filter(BUILDINGID == 1) # %>% 
  # mutate(predictions = pred.ranger.building_1_floor$predictions) %>%
  # select(-BUILDINGID)

erroranalysisF1B1$Predictions <- pred.ranger.building_1_floor$predictions
erroranalysisF1B1$misplaced <- erroranalysisF1B1$FLOOR != 
  erroranalysisF1B1$Predictions
# sum(erroranalysisF1B1$misplaced)
errors_to_plot <- erroranalysisF1B1 %>% filter(misplaced == TRUE)
errors_to_plot %>% group_by(USERID,PHONEID) %>% summarise(n())