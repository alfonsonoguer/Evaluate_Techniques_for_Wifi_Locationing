pacman::p_load(readr,caret,tidyverse,ggplot2,plotly, lubridate, ranger,
               DataExplorer,ggbiplot)


# Coments -----------------------------------------------------------------

# first 2 classification problems building (solved by random randomforest
# and floor needs to be improved study errors.

# 



# Get and preporcess the data ------------------------------------------------

train <- read_csv("Data/trainingData.csv")

# create_report(train %>% select(-contains("WAP")))

model_building <- train %>% select(contains("WAP"),BUILDINGID,FLOOR,LONGITUDE,
                                   LATITUDE)
model_building$FLOOR <- as.factor(model_building$FLOOR)
model_building$BUILDINGID <- as.factor(model_building$BUILDINGID)
model_building[, tail(names(model_building))]
test <- read_csv("Data/validationData.csv")
# the test data is using diferent phones than the train.
unique(test$PHONEID) %in% unique(test$PHONEID)

summary(test%>% select(-contains('WAP')))
# test$is.train <- 0
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

# tc <- trainControl(method = "CV",number = 10)
# 
# model_building_01 <- train(x = model_building %>% select(contains("WAP")),
#                            y = as.factor(model_building$BUILDINGID),
#                            method = "rf",
#                            trControl = tc,
#                            tuneLength = 4
#                            )

# Ranger for MVP ----------------------------------------------------------
#predicting Building
{# building
data_building <- model_building %>% select(contains("WAP"),BUILDINGID) 
data_building$BUILDINGID <- as.factor(data_building$BUILDINGID)
ranger_model_building <- ranger(BUILDINGID ~ ., data = data_building)
pred.ranger.building <- predict(ranger_model_building, data = test) 

table(test$BUILDINGID, pred.ranger.building$predictions)}
# Predicting FLOOR
{
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
test_building1WAP<- test %>% filter(BUILDINGID == 1) %>%
                       select(starts_with("WAP"))
test_building1floor <- (test%>% filter(BUILDINGID == 1) %>%
                          select(FLOOR))
data_building_1_floor <- model_building %>% filter(BUILDINGID == 1) %>%
  select(contains("WAP"),FLOOR)

# WE BIN THE INTESITY SIGNALS
intesidad <- function(x){
  return(findInterval(x,vec = c(-90,-80,-70,-60,-30,0)))
}
data_building_1_floor_binned <- as_tibble(apply(data_building_1_floor %>%
                                  select(contains("WAP")),
                                2,
                                intesidad))
# las columnes con 100 y menos de -90 son iguales ya que no conectan a internet
data_building_1_floor_binned[data_building_1_floor_binned==6]<-0
data_building_1_floor_binned$FLOOR <- data_building_1_floor$FLOOR
# we remove the 0 variance columns
ZerobarB1 <- nearZeroVar(data_building_1_floor,saveMetrics = TRUE)
data_building_1_floor <- data_building_1_floor[,!ZerobarB1$zeroVar]

ZerobarB1_BINNED <- nearZeroVar(data_building_1_floor_binned,
                                saveMetrics = TRUE)
data_building_1_floor_binned <- data_building_1_floor_binned[
  ,!ZerobarB1_BINNED$zeroVar]

# modelos sin PCA
ranger_model_building_1_floor <- ranger(FLOOR ~ .,
                                        data = data_building_1_floor)
pred.ranger.building_1_floor <- predict(ranger_model_building_1_floor,
                                        data = test %>% 
                                          filter(BUILDINGID == 1))
confusionMatrix(as.factor(test_building1floor$FLOOR),
                factor(pred.ranger.building_1_floor$predictions))
# Confusion Matrix and Statistics 
{
#   Confusion Matrix and Statistics
#   
#   Reference
#   Prediction  0  1  2  3
#   0 24  1  5  0
#   1  1 93 46  3
#   2  0  1 78  8
#   3  0  0  4 43
#   
#   Overall Statistics
#   
#   Accuracy : 0.7752          
#   95% CI : (0.7244, 0.8207)
#   No Information Rate : 0.4332          
#   P-Value [Acc > NIR] : < 2.2e-16       
#   
#   Kappa : 0.6781          
#   
#   Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3
# Sensitivity           0.96000   0.9789   0.5865   0.7963
# Specificity           0.97872   0.7642   0.9483   0.9842
# Pos Pred Value        0.80000   0.6503   0.8966   0.9149
# Neg Pred Value        0.99639   0.9878   0.7500   0.9577
# Prevalence            0.08143   0.3094   0.4332   0.1759
# Detection Rate        0.07818   0.3029   0.2541   0.1401
# Detection Prevalence  0.09772   0.4658   0.2834   0.1531
# Balanced Accuracy     0.96936   0.8715   0.7674   0.8902
}
# modelo sin PCA con binning

ranger_model_building_1_floor_bin <- ranger(FLOOR ~ .,
                                        data = data_building_1_floor_binned)
pred.ranger.building_1_floor_bin <- predict(ranger_model_building_1_floor_bin,
                                        data = test %>% 
                                          filter(BUILDINGID == 1))
confusionMatrix(as.factor(test_building1floor$FLOOR),
                factor(pred.ranger.building_1_floor_bin$predictions))
#   Confusion Matrix and Statistics
{
#   
#   Reference
#   Prediction  0  1  2  3
#   0 15  9  6  0
#   1  4 87 50  2
#   2  0  2 71 14
#   3  0  1 19 27
#   
#   Overall Statistics
#   
#   Accuracy : 0.6515          
#   95% CI : (0.5953, 0.7047)
#   No Information Rate : 0.4756          
#   P-Value [Acc > NIR] : 4.083e-10       
#   
#   Kappa : 0.4931          
#   
#   Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3
# Sensitivity           0.78947   0.8788   0.4863  0.62791
# Specificity           0.94792   0.7308   0.9006  0.92424
# Pos Pred Value        0.50000   0.6084   0.8161  0.57447
# Neg Pred Value        0.98556   0.9268   0.6591  0.93846
# Prevalence            0.06189   0.3225   0.4756  0.14007
# Detection Rate        0.04886   0.2834   0.2313  0.08795
# Detection Prevalence  0.09772   0.4658   0.2834  0.15309
# Balanced Accuracy     0.86870   0.8048   0.6935  0.77607
}

# Modelo PCA

# we run a PCA
prin_comp <- prcomp(data_building_1_floor %>% select(-FLOOR))
# biplot(prin_comp, scale = 0)
#compute standard deviation of each principal component
std_dev <- prin_comp$sdev
#compute variance
pr_var <- std_dev^2
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
# 91 componentes explican aproximadamente el 95% de la variancia
B1_floor_PCA <- data.frame(data_building_1_floor %>% select(FLOOR),
                           prin_comp$x)
# WE MODEL
ranger_model_building_1_floor_PCA <- ranger(FLOOR ~ .,
                                        data = B1_floor_PCA[,1:91])
B1_PCA_TEST <-  as.data.frame( predict(prin_comp, newdata = test_building1WAP))

pred.ranger.building_1_floor_PCA <- predict(ranger_model_building_1_floor_PCA,
                                        data = B1_PCA_TEST)
confusionMatrix(as.factor(test_building1floor$FLOOR),
                factor(pred.ranger.building_1_floor_PCA$predictions))
# Confusion Matrix and Statistics 
{
#   Confusion Matrix and Statistics
#   
#   Reference
#   Prediction  0  1  2  3
#   0 23  2  4  1
#   1  9 81 52  1
#   2  0  1 74 12
#   3  0  0  9 38
#   
#   Overall Statistics
#   
#   Accuracy : 0.7036          
#   95% CI : (0.6491, 0.7541)
#   No Information Rate : 0.4528          
#   P-Value [Acc > NIR] : < 2.2e-16       
#   
#   Kappa : 0.5814          
#   
#   Mcnemar's Test P-Value : 4.589e-11       
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3
# Sensitivity           0.71875   0.9643   0.5324   0.7308
# Specificity           0.97455   0.7220   0.9226   0.9647
# Pos Pred Value        0.76667   0.5664   0.8506   0.8085
# Neg Pred Value        0.96751   0.9817   0.7045   0.9462
# Prevalence            0.10423   0.2736   0.4528   0.1694
# Detection Rate        0.07492   0.2638   0.2410   0.1238
# Detection Prevalence  0.09772   0.4658   0.2834   0.1531
# Balanced Accuracy     0.84665   0.8431   0.7275   0.8477
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

}

# floor building 1 error analysis -----------------------------------------


erroranalysisF1B1 <- test %>% select(-contains("WAP")) %>% 
  filter(BUILDINGID == 1) # %>% 
  # mutate(predictions = pred.ranger.building_1_floor$predictions) %>%
  # select(-BUILDINGID)

erroranalysisF1B1$Predictions <- pred.ranger.building_1_floor$predictions
erroranalysisF1B1$misplaced <- erroranalysisF1B1$FLOOR != 
  erroranalysisF1B1$Predictions
# sum(erroranalysisF1B1$misplaced)
errors_to_plot <- erroranalysisF1B1 %>% filter(misplaced == TRUE)
errors_to_plot %>% group_by(USERID,PHONEID) %>% summarise(n())
errors_to_plot %>% group_by(FLOOR,Predictions) %>% summarise(n())

errors_to_plot %>% filter(FLOOR == 1 & Predictions == 2)

summary(errors_to_plot)
ggplot(errors_to_plot, aes(x=LATITUDE,y=LONGITUDE))+
  geom_point()
ggplot(errors_to_plot, aes(x=LATITUDE))+
  geom_histogram(binwidth = 1,aes(fill = LONGITUDE))
ggplot(errors_to_plot, aes(x=LONGITUDE))+
  geom_histogram(binwidth = 1,aes(fill = LONGITUDE))

create_report(errors_to_plot %>% filter(FLOOR == 1 & Predictions == 2) %>% 
                select(-FLOOR,-BUILDINGID,-SPACEID,-RELATIVEPOSITION,
                       -USERID, - Predictions))
