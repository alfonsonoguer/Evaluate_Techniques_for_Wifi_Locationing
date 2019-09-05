data_KNN_train <- full_data_dummy %>% filter(source == "training") %>%
  select(starts_with("WAP"),contains("dummy"),LONGITUDE,LATITUDE,FLOOR)
data_KNN_validation <- full_data_dummy %>% filter(source == "validation") %>%
  select(starts_with("WAP"),contains("dummy"),LONGITUDE,LATITUDE)

coordinates <- c("LONGITUDE","LATITUDE")

validation_clean <- full_data_dummy %>% filter(source == "validation")
knn_LONG <- knn.reg(train = data_KNN_train %>% select(starts_with("WAP"),
                                                      contains("dummy")),
                    test = data_KNN_validation%>% select(-LATITUDE,-LONGITUDE),
                    y = data_KNN_train$LONGITUDE,
                    k = 1)

knn_LAT <- knn.reg(train = data_KNN_train %>% select(starts_with("WAP"),
                                                     contains("dummy")),
                    test = data_KNN_validation%>% select(-LATITUDE,-LONGITUDE),
                    y = data_KNN_train$LATITUDE,
                    k = 1)
knn_results <- tibble(LONGITUDE = data_KNN_validation$LONGITUDE,
                      pred_long = knn_LONG$pred,
                      LATITUDE = data_KNN_validation$LATITUDE,
                      pred_lat = knn_LAT$pred)
knn_results <- bind_cols(knn_results, FLOOR = validation_clean$FLOOR,
                         dummyB0 = validation_clean$dummyB0,
                         dummyB1 = validation_clean$dummyB1)
knn_results <- knn_results %>% mutate(error_long = abs(LONGITUDE - pred_long),
                                      error_lat = abs(LATITUDE - pred_lat))

 postResample(knn_results$LONGITUDE,knn_results$pred_long)
# RMSE   Rsquared        MAE 
# 12.1566646  0.9898634  7.6929228 
 postResample(knn_results$LATITUDE,knn_results$pred_lat)
# RMSE   Rsquared        MAE 
# 11.9745374  0.9713469  7.2903662 


big_error0 <- knn_results %>% filter(error_lat > 20 & error_long > 20)

big_error1 <- big_error0 %>% select(pred_long ,pred_lat ,error_long,error_lat)
big_error2 <- big_error1 %>% rename(LONGITUDE = pred_long, LATITUDE = pred_lat)

worst_observations <- semi_join(full_data %>% filter(source == "training"),
                                 big_error2, by = c("LONGITUDE","LATITUDE")) %>% 
  unite("location",BUILDINGID,FLOOR,SPACEID,RELATIVEPOSITION,remove = FALSE)

x <- inner_join(full_data %>% filter(source == "training"),
                distinct(big_error2 %>% select(LONGITUDE,LATITUDE)),
                by = c("LONGITUDE","LATITUDE"))



bad <- semi_join(full_data %>% filter(source == "training"),
                                 big_error, by = c("LONGITUDE","LATITUDE"))
good <- anti_join(full_data %>% filter(source == "training"),
                                 big_error, by = c("LONGITUDE","LATITUDE"))

sort(table(worst_observations$highestWAP),decreasing = TRUE)
sort(table(worst_observations$USERID),decreasing = TRUE)
sort(table(worst_observations$PHONEID),decreasing = TRUE)
sort(table(worst_observations$location),decreasing = TRUE)


ggplot(data = worst_observations %>% group_by(highestWAP) %>% 
                     summarise(count = n()) %>%  arrange(desc(count))) + 
  geom_col(aes(x = reorder(highestWAP,-count), y = count, fill= highestWAP)) + 
  labs(title = "count of WAPS whith the higest error")

waps_to_remove <- worst_observations %>% group_by(highestWAP) %>% 
  summarise(count = n()) %>%  arrange(desc(count))


apply(worst_observations %>% select(starts_with("WAP")), 1, max)

ggplot(data = head(worst_observations %>% group_by(highestWAP) %>% 
         summarise(count = n()) %>%  arrange(desc(count)),n=10)) + 
  geom_col(aes(x = reorder(highestWAP,-count), y = count, fill= highestWAP)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  labs(title = "count of the 10 WAPS whith the higest error")  +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = worst_observations %>% group_by(location) %>% 
         summarise(count = n())) + 
  geom_col(aes(x = reorder(location,-count), y = count, fill= location)) + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) + 
  labs(title = "count of locations whith the higest error")  

ggplot(data =head(worst_observations %>% group_by(location) %>% 
         summarise(count = n()) %>% arrange(desc(count)),n=10)) + 
  geom_col(aes(x = reorder(location,-count), y = count, fill= location)) + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) + 
  labs(title = "count of the 10 locations whith the higest error") +
  theme(axis.text.x = element_text(angle = 45))
  
to_plot_errors <- as_tibble(combine(bad,good))

to_plot_errors <- to_plot_errors %>% rename(pred_quality = source.1)
ggplot() + 
  geom_point(data = to_plot_errors %>% filter(pred_quality == 'good'),
              aes(x = LONGITUDE,y = LATITUDE),
              color = "skyblue") +
  geom_point(data = to_plot_errors %>% filter(pred_quality == 'bad'),
              aes(x = LONGITUDE,y = LATITUDE),
              color = "red") +
  geom_jitter(data = big_error0 ,
             aes(x = LONGITUDE,y = LATITUDE),
             color = "darkgreen")
  
