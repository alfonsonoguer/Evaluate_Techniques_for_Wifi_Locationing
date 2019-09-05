# libraries ---------------------------------------------------------------


pacman::p_load(readr,caret,tidyverse,ggplot2,plotly, lubridate, ranger,
               DataExplorer,plotly,gdata,splitstackshape,rpart,C50)

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
                               select(-FLOOR,LONGITUDE,LATITUDE))
saveRDS(object =benchmark_building_CV_TRAIN, 
        file = "Models/benchmark_building_CV_TRAIN.rds")
results_CV_TRAIN <- tibble(building = (predictions(
  predict(benchmark_building_CV_TRAIN,
          str_train$SAMP1))))
confusionMatrix(str_train$SAMP1$BUILDINGID,results$building)

results_validation <- tibble(building = (predictions(
  predict(benchmark_building_CV_TRAIN,
          test))))
confusionMatrix(test$BUILDINGID,results_validation$building)



# floor GENERAL

benchmark_FLOOR_CV_TRAIN <- ranger(formula = FLOOR~.,
                             data = str_train$SAMP2 %>%
                               select(-BUILDINGID,LONGITUDE,LATITUDE))
saveRDS(object =benchmark_building_CV_TRAIN, 
        file = "Models/benchmark_FLOOR_CV_TRAIN.rds")
results_CV_TRAIN$FLOOR_GENERAL <-predictions(predict(benchmark_FLOOR_CV_TRAIN,
                                                    str_train$SAMP1))
confusionMatrix(str_train$SAMP1$FLOOR,results_CV_TRAIN$FLOOR_GENERAL)

results_validation$FLOOR_GENERAL <- predictions(
  predict(benchmark_FLOOR_CV_TRAIN, test))
confusionMatrix(test$FLOOR,results_validation$FLOOR_GENERAL)


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

# floor building 2
data_b2_F_CV <- str_train$SAMP2 %>% filter(BUILDINGID == 2) %>%
  select(contains("WAP"),FLOOR)
test_b2_F_CV <- str_train$SAMP1 %>% filter(BUILDINGID == 2)
validation_b2 <- test %>% filter(BUILDINGID == 2)
validation_b2$FLOOR <- factor(validation_b2$FLOOR)
benchmark_FLOOR_B2_CV_TRAIN <- ranger(FLOOR ~ .,
                                      data = data_b2_F_CV)
saveRDS(object =benchmark_FLOOR_B2_CV_TRAIN, 
        file = "Models/benchmark_FLOOR_B2_CV_TRAIN.rds")
results_CV_TRAIN_B2_FLOOR <- predictions(predict(benchmark_FLOOR_B2_CV_TRAIN,
                                                 data = test_b2_F_CV))
confusionMatrix(factor(results_CV_TRAIN_B2_FLOOR),
                factor(test_b2_F_CV$FLOOR))

validation_building_2_floor <- predict(benchmark_FLOOR_B2_CV_TRAIN,
                                       data = validation_b2)

confusionMatrix(validation_b2$FLOOR,
                factor(validation_building_2_floor$predictions))


# longitude general

benchmark_LONGITUDE_CV_TRAIN <- ranger(formula = LONGITUDE~.,
                                   data = str_train$SAMP2 %>%
                                     select(-BUILDINGID,FLOOR,LATITUDE))
saveRDS(object =benchmark_building_CV_TRAIN, 
        file = "Models/benchmark_LONGITUDE_CV_TRAIN.rds")
results_CV_TRAIN$LONGITUDE_GENERAL <-
  predictions(predict(benchmark_LONGITUDE_CV_TRAIN,str_train$SAMP1))
postResample(str_train$SAMP1$LONGITUDE,results_CV_TRAIN$LONGITUDE_GENERAL)

results_validation$LONGITUDE_GENERAL <- predictions(
  predict(benchmark_LONGITUDE_CV_TRAIN, test))
postResample(test$LONGITUDE,results_validation$LONGITUDE_GENERAL)

# LATITUDE general

benchmark_LATITUDE_CV_TRAIN <- ranger(formula = LATITUDE~.,
                                       data = str_train$SAMP2 %>%
                                         select(-BUILDINGID,FLOOR,LATITUDE))
saveRDS(object =benchmark_building_CV_TRAIN, 
        file = "Models/benchmark_LATITUDE_CV_TRAIN.rds")
results_CV_TRAIN$LATITUDE_GENERAL <-
  predictions(predict(benchmark_LATITUDE_CV_TRAIN,str_train$SAMP1))
postResample(str_train$SAMP1$LATITUDE,results_CV_TRAIN$LATITUDE_GENERAL)

results_validation$LATITUDE_GENERAL <- predictions(
  predict(benchmark_LATITUDE_CV_TRAIN, test))
postResample(test$LATITUDE,results_validation$LATITUDE_GENERAL)

# predicting building -----------------------------------------------------
#predicting Building
{# building
data_building <- model_building %>% select(contains("WAP"),BUILDINGID) 
data_building$BUILDINGID <- as.factor(data_building$BUILDINGID)
ranger_model_building <- ranger(BUILDINGID ~ ., data = data_building)
pred.ranger.building <- predict(ranger_model_building, data = test) 

table(test$BUILDINGID, pred.ranger.building$predictions)}

# predicting floor B0 -----------------------------------------------------

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

# predicting floor B1 SIMPLE, train and test----------------------------------


# floor building 1

data_building_1_floor <- train %>% filter(BUILDINGID == 1) %>%
  select(contains("WAP"),FLOOR)
# we remove zero var variables
ZerobarB1 <- nearZeroVar(data_building_1_floor,saveMetrics = TRUE)
data_building_1_floor <- data_building_1_floor[,!ZerobarB1$zeroVar]

data_building_1_floor[data_building_1_floor == 100] <- NA
temp <- data_building_1_floor %>% select(a,FLOOR) %>% unique() %>% 
  select(-WAP119) %>% 
  filter(WAP088 > -89 | WAP089 > -89 | WAP148 > -89 | WAP149 > -89)
table(temp)
# we preprocess the test dataframe
test_building1WAP<- test %>% filter(BUILDINGID == 1) %>%
  select(starts_with("WAP"))
test_building1floor <- (test%>% filter(BUILDINGID == 1) %>%
                          select(FLOOR))
train %>% filter(BUILDINGID == 1) %>% select(FLOOR) 

graph <- combine(
  test %>% filter(BUILDINGID == 1) %>% select(LONGITUDE,LATITUDE,FLOOR),
  train %>% filter(BUILDINGID == 1) %>% select(LONGITUDE,LATITUDE,FLOOR),
  names = c("train","test"))

plot_ly(graph %>% filter(FLOOR == 3),
        x = ~LONGITUDE, y = ~LATITUDE, color = ~source,
        colors = c("#E69F00", "#56B4E9"), size = 0.01) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude")))

ggplot(data = graph, aes(x = LONGITUDE,y = LATITUDE, color = source)) + 
  geom_point() + 
  facet_grid(FLOOR ~ .)

# WE BIN THE INTESITY SIGNALS
# intesidad <- function(x){
#   return(findInterval(x,vec = c(-90,-80,-70,-60,-30,0)))
# }
# data_building_1_floor_binned <- as_tibble(apply(data_building_1_floor %>%
#                                   select(contains("WAP")),
#                                 2,
#                                 intesidad))
# las columnes con 100 y menos de -90 son iguales ya que no conectan a internet
# data_building_1_floor_binned[data_building_1_floor_binned==6]<-0
# data_building_1_floor_binned$FLOOR <- data_building_1_floor$FLOOR
# we remove the 0 variance columns

# ZerobarB1_BINNED <- nearZeroVar(data_building_1_floor_binned,
#                                 saveMetrics = TRUE)
# data_building_1_floor_binned <- data_building_1_floor_binned[
#   ,!ZerobarB1_BINNED$zeroVar]

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
#   Prediction   0  1  2  3
#             0 24  1  5  0
#             1  1 93 46  3
#             2  0  1 78  8
#             3  0  0  4 43
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
# # modelo sin PCA con binning
# 
# ranger_model_building_1_floor_bin <- ranger(FLOOR ~ .,
#                                         data = data_building_1_floor_binned)
# pred.ranger.building_1_floor_bin <- predict(ranger_model_building_1_floor_bin,
#                                         data = test %>% 
#                                           filter(BUILDINGID == 1))
# confusionMatrix(as.factor(test_building1floor$FLOOR),
#                 factor(pred.ranger.building_1_floor_bin$predictions))
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

# model B1 FLOOR, PCA -----------------------------------------------------

# Modelo PCA
{
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
}
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


# MODEL B1 FLOOR RANDOM RESAMPLING ----------------------------------------

out_B1 <- stratified(indt = fulldata %>% filter(BUILDINGID == 1),
                     group = "FLOOR", size =.15, bothSets = TRUE)
names(out_B1) <- c("TEST01","TRAIN01")
TRAIN_SAMP_01 <- as_tibble(out_B1$TRAIN01 %>% select(starts_with("WAP"),FLOOR))
test_out_SAMP01 <- as_tibble(out_B1$TEST01)

ZerobarB1_SAMP <- nearZeroVar(TRAIN_SAMP_01,saveMetrics = TRUE)
TRAIN_SAMP_01 <- TRAIN_SAMP_01[,!ZerobarB1_SAMP$zeroVar]

ranger_model_building_1_floor <- ranger(FLOOR ~ .,
                                        data = TRAIN_SAMP_01)
PRED_SAMP01 <- predict(ranger_model_building_1_floor,
                                        data = test_out_SAMP01)
confusionMatrix(factor(test_out_SAMP01$FLOOR),
                factor(PRED_SAMP01$predictions))

# predicting floor B2 -----------------------------------------------------


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


 erroranalysisF1B1 <- test  %>% filter(BUILDINGID == 1) # %>% 
#   mutate(predictions = pred.ranger.building_1_floor$predictions)

erroranalysisF1B1$Predictions <- pred.ranger.building_1_floor$predictions
erroranalysisF1B1$misplaced <- erroranalysisF1B1$FLOOR != 
  erroranalysisF1B1$Predictions
# sum(erroranalysisF1B1$misplaced)
errors_to_plot <- erroranalysisF1B1 %>% filter(misplaced == TRUE)
errors_to_plot %>% group_by(USERID,PHONEID) %>% summarise(n())
errors_to_plot %>% group_by(FLOOR,Predictions) %>% summarise(n())

errorsb1_n46<- errors_to_plot %>% filter(FLOOR == 1 & Predictions == 2)

errorsb1_n46_WAP <- errorsb1_n46 %>% select(starts_with("WAP"))
errorsb1_n46_WAP <- apply(errorsb1_n46_WAP, 1:2, function(x){
  ifelse(test = x==100,return(NA),return(x))
})

Zerobarerror <- nearZeroVar(errorsb1_n46_WAP,saveMetrics = TRUE)
errorsb1_n46_WAP_VARIANCE <- as_tibble(errorsb1_n46_WAP[,!Zerobarerror$zeroVar])
a <- names(errorsb1_n46_WAP_VARIANCE[,-70 < 
                                     apply(errorsb1_n46_WAP_VARIANCE, 2,
                                           function(x){mean(x, na.rm = TRUE)})])

for (i in a) {
  print(table(errorsb1_n46_WAP_VARIANCE[,a]>-80))
}


summary(errors_to_plot)
ggplot(errors_to_plot, aes(x=LATITUDE,y=LONGITUDE)) +
  geom_point()
ggplot(errors_to_plot, aes(x=LATITUDE))+
  geom_histogram(binwidth = 1,aes(fill = LONGITUDE))
ggplot(errors_to_plot, aes(x=LONGITUDE))+
  geom_histogram(binwidth = 1,aes(fill = LONGITUDE))

create_report(errors_to_plot %>% filter(FLOOR == 1 & Predictions == 2) %>% 
                select(-FLOOR,-BUILDINGID,-SPACEID,-RELATIVEPOSITION,
                       -USERID, - Predictions))

ggplot(data=train %>% filter(BUILDINGID==1 & (FLOOR == 2)) %>% select(LONGITUDE,
                                                                      LATITUDE),
       aes(x=LATITUDE,y = LONGITUDE)) + 
  geom_point()


# PREDICTING LOCATION -----------------------------------------------------


# location B1 -------------------------------------------------------------

location_b1_train <- train %>% filter(BUILDINGID==1) %>% 
  select(starts_with("WAP"),LATITUDE,LONGITUDE)

ZerobarB1_lon_train <- nearZeroVar(location_b1_train,saveMetrics = TRUE)
location_b1_train <- location_b1_train[,!ZerobarB1_lon_train$zeroVar]

location_b1_test <- test %>% filter(BUILDINGID==1) %>% 
  select(starts_with("WAP"),LATITUDE,LONGITUDE)

r_b1_latitude <- ranger(formula = LATITUDE~.,
       data = location_b1_train %>% select(-LONGITUDE),verbose = TRUE,
       num.trees = 1000)
prediction_b1_latitude <- predict(r_b1_latitude,location_b1_test)

TRAIN_SAMP_02 <- as_tibble(out_B1$TRAIN01 %>% select(starts_with("WAP"),
                                                     LATITUDE,LONGITUDE))
test_out_SAMP02 <- as_tibble(out_B1$TEST01)


r_b1_sam_latitude <- ranger(formula = LATITUDE~.,
                        data = TRAIN_SAMP_02 %>% select(-LONGITUDE),
                        verbose = TRUE)
                        # num.trees = 5000)
prediction_b1_sam_latitude <- predict(r_b1_sam_latitude,test_out_SAMP02)
postResample(prediction_b1_sam_latitude$predictions,test_out_SAMP02$LATITUDE)


r_b1_sam_longitude <- ranger(formula = LONGITUDE~.,
                            data = TRAIN_SAMP_02 %>% select(-LATITUDE),
                            verbose = TRUE)

prediction_b1_sam_longitude <- predict(r_b1_sam_longitude,test_out_SAMP02)
postResample(prediction_b1_sam_longitude$predictions,test_out_SAMP02$LONGITUDE)


min <- TRAIN_SAMP_02$LATITUDE %>% min() %>% trunc() 
max <- TRAIN_SAMP_02$LATITUDE %>% max() %>% ceiling()

test_out_SAMP02$BINS <- cut(x = test_out_SAMP02$LATITUDE,
                           breaks = seq(min,max),
                           labels= seq(1,151))

TRAIN_SAMP_02$BINS <- cut(x = TRAIN_SAMP_02$LATITUDE,
                          breaks = seq(min,max),
                          labels= seq(1,151))

r_b1_sam_latitude_bins <- ranger(formula = BINS~.,
                            data = TRAIN_SAMP_02 %>% select(-LATITUDE),
                            verbose = TRUE)
prediction_b1_sam_lat_bin <- predict(r_b1_sam_latitude_bins,test_out_SAMP02)
confusionMatrix(prediction_b1_sam_lat_bin$predictions,test_out_SAMP02$BINS)
