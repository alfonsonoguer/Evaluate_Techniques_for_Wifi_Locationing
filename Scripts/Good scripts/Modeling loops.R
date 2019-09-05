
# comentarios -------------------------------------------------------------

# eliminar atributos que solo mejoran levemente el resultado, 
# eliminar filas de 100 no afecta de forma perceptiblemente el resultado.
# ruido mejora levemente el resultado.

# predicciones por edifico son entre entre 0.5 a 1 m mejores.


# PREDICTION LOOPS --------------------------------------------------------
str_train <- stratified(train,
                        group =  c("BUILDINGID", "FLOOR"),size =.25,
                        bothSets = TRUE)
training <- as.data.frame(str_train$SAMP2)
testing <- as.data.frame(str_train$SAMP1)
# dataframes
lista_resample_train    <- list()
lista_resample_train_edificio    <- list()
resultados_train <- list()
resultados_train_edificio <- list()
resultados_test_edificio <- list()
resultados_test  <- list()
lista_resample_test    <- list()
lista_modelos    <- list()
lista_modelos_edificio   <- list()
nombres <- c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")
equation <- c("BUILDINGID~.","FLOOR~.","LONGITUDE~.","LATITUDE~.")
for (z in 1:4){
  i = nombres[z]
  TRAIN <- as.data.frame(str_train$SAMP2 %>% select(starts_with("WAP"),i))
  lista_modelos[[i]] <- ranger(formula = as.formula(equation[z]),
                               TRAIN)
}

for (z in 2:4){
  i = nombres[z]
  for (j in 0:2) {
    TRAIN <- str_train$SAMP2 %>%
      filter(BUILDINGID == j) %>% 
      select(starts_with("WAP"),i)
    lista_modelos_edificio[[paste(i,j,sep = "_")]] <- 
      ranger(formula = as.formula(equation[z]),  TRAIN)  
  }
}

for (z in 2:4){
  i = nombres[z]
  for (j in 0:2) {
    resultados_train_edificio[[paste(i,j,sep = "_")]]<-
      predict(lista_modelos_edificio[[paste(i,j,sep = "_")]],
              testing %>% filter(BUILDINGID == j))
    testing_int <- testing %>% filter(BUILDINGID == j)
    lista_resample_train_edificio[[paste(i,j,sep = "_")]] <- 
      postResample(
        pred = resultados_train_edificio[[paste(i,j,sep = "_")]]$predictions,
        obs = testing_int[,i]) 
    
    validation_int <- as.data.frame(test %>% filter(BUILDINGID == j))
    resultados_test_edificio[[paste(i,j,sep = "_")]]<-
      predict(lista_modelos_edificio[[paste(i,j,sep = "_")]],
              validation_int)
    lista_resample_train_edificio[[paste(i,j,sep = "_")]] <- 
      postResample(
        pred = resultados_test_edificio[[paste(i,j,sep = "_")]]$predictions,
        obs = validation_int[,i]) 
  }
}

for (i in c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")){
  resultados_train[[i]]<-predict(lista_modelos[[i]], str_train$SAMP1)
  lista_resample_train[[i]] <- 
    postResample(pred = resultados_train[[i]]$predictions,
                 obs = testing[,i])
  resultados_test[[i]]<-predict(lista_modelos[[i]], test)
  lista_resample_test[[i]] <- 
    postResample(pred = resultados_test[[i]]$predictions,
                 obs = as.data.frame(test)[,i])
}


# PREDICTION LOOPS, removing 0 variance --------------------------------
lista_resample_train2    <- list()
lista_resample_train_edificio2    <- list()
resultados_train2 <- list()
resultados_train_edificio2 <- list()
resultados_test_edificio2 <- list()
resultados_test2  <- list()
lista_resample_test2    <- list()
lista_resample_edificio_test2    <- list()
lista_modelos2    <- list()
lista_modelos_edificio2   <- list()

# for (z in 1:4){
#   i = nombres[z]
#   TRAIN <- as.data.frame(str_train$SAMP2 %>% select(starts_with("WAP"),i))
#   ZerobarB1 <- nearZeroVar(TRAIN,saveMetrics = TRUE)
#   TRAIN <- TRAIN[,!ZerobarB1$zeroVar]
#   lista_modelos2[[i]] <- ranger(formula = as.formula(equation[z]),
#                                 TRAIN)
# }
# saveRDS(lista_modelos,file = "Models/lista_modelos_general.rsd")
# for (z in 2:4){
#   i = nombres[z]
#   for (j in 0:2) {
#     TRAIN <- str_train$SAMP2 %>%
#       filter(BUILDINGID == j) %>% 
#       select(starts_with("WAP"),i)
#     ZerobarB1 <- nearZeroVar(TRAIN,saveMetrics = TRUE)
#     TRAIN <- TRAIN[,!ZerobarB1$zeroVar]
#     lista_modelos_edificio2[[paste(i,j,sep = "_")]] <- 
#       ranger(formula = as.formula(equation[z]),  TRAIN)  
#   }
# }
# saveRDS(lista_modelos_edificio2,file = "Models/listamodelosporedificio.rsd")

for (i in c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")){
  resultados_train2[[i]]<-predict(lista_modelos2[[i]], str_train$SAMP1)
  lista_resample_train2[[i]] <- 
    postResample(pred = resultados_train2[[i]]$predictions,
                 obs = testing[,i])
  resultados_test2[[i]]<-predict(lista_modelos2[[i]], test)
  lista_resample_test2[[i]] <- 
    postResample(pred = resultados_test2[[i]]$predictions,
                 obs = as.data.frame(test)[,i])
}
for (z in 2:4){
  i = nombres[z]
  for (j in 0:2) {
    resultados_train_edificio2[[paste(i,j,sep = "_")]]<-
      predict(lista_modelos_edificio2[[paste(i,j,sep = "_")]],
              testing %>% filter(BUILDINGID == j))
    testing_int <- testing %>% filter(BUILDINGID == j)
    lista_resample_train_edificio2[[paste(i,j,sep = "_")]] <- 
      postResample(
        pred = resultados_train_edificio2[[paste(i,j,sep = "_")]]$predictions,
        obs = testing_int[,i]) 
    
    validation_int <- as.data.frame(test %>% filter(BUILDINGID == j))
    resultados_test_edificio2[[paste(i,j,sep = "_")]]<-
      predict(lista_modelos_edificio2[[paste(i,j,sep = "_")]],
              validation_int)
    lista_resample_edificio_test2[[paste(i,j,sep = "_")]] <- 
      postResample(
        pred = resultados_test_edificio2[[paste(i,j,sep = "_")]]$predictions,
        obs = validation_int[,i]) 
  }
}
#comented useless PREDICTION LOOPS, removing useless rows, and columns -----
# lista_resample_train3    <- list()
# lista_resample_train_edificio3    <- list()
# resultados_train3 <- list()
# resultados_train_edificio3 <- list()
# resultados_test_edificio3 <- list()
# resultados_test3  <- list()
# lista_resample_test3    <- list()
# lista_modelos3    <- list()
# lista_modelos_edificio3   <- list()
# 
# interestingrows <- function(dataframe)
# {
#   apply(dataframe,1,
#         function(vector){
#           ifelse(1 < length(unique(vector)),return(TRUE),return(FALSE))
#         }
#   )
# }
# 
# 
# 
# str_train$SAMP2 <- str_train$SAMP2[interestingrows(str_train$SAMP2 %>% 
#                                                      select(starts_with("WAP"))),]
# 
# for (z in 1:4){
#   i = nombres[z]
#   TRAIN <- as.data.frame(str_train$SAMP2 %>% select(starts_with("WAP"),i))
#   TRAIN <- TRAIN[interestingrows(TRAIN %>% 
#                                    select(starts_with("WAP"))),]
#   ZerobarB1 <- nearZeroVar(TRAIN,saveMetrics = TRUE)
#   TRAIN <- TRAIN[,!ZerobarB1$zeroVar]
#   lista_modelos3[[i]] <- ranger(formula = as.formula(equation[z]),
#                                 TRAIN)
# }
# 
# for (z in 2:4){
#   i = nombres[z]
#   for (j in 0:2) {
#     TRAIN <- str_train$SAMP2 %>%
#       filter(BUILDINGID == j) %>% 
#       select(starts_with("WAP"),i)
#     TRAIN <- TRAIN[interestingrows(TRAIN %>% 
#                                      select(starts_with("WAP"))),]
#     ZerobarB1 <- nearZeroVar(TRAIN,saveMetrics = TRUE)
#     TRAIN <- TRAIN[,!ZerobarB1$zeroVar]
#     lista_modelos_edificio3[[paste(i,j,sep = "_")]] <- 
#       ranger(formula = as.formula(equation[z]),  TRAIN)  
#   }
# }
# 
# for (z in 2:4){
#   i = nombres[z]
#   for (j in 0:2) {
#     resultados_train_edificio3[[paste(i,j,sep = "_")]]<-
#       predict(lista_modelos_edificio3[[paste(i,j,sep = "_")]],
#               testing %>% filter(BUILDINGID == j))
#     testing_int <- testing %>% filter(BUILDINGID == j)
#     lista_resample_train_edificio3[[paste(i,j,sep = "_")]] <- 
#       postResample(
#         pred = resultados_train_edificio3[[paste(i,j,sep = "_")]]$predictions,
#         obs = testing_int[,i]) 
#     
#     validation_int <- as.data.frame(test %>% filter(BUILDINGID == j))
#     resultados_test_edificio3[[paste(i,j,sep = "_")]]<-
#       predict(lista_modelos_edificio3[[paste(i,j,sep = "_")]],
#               validation_int)
#     lista_resample_train_edificio3[[paste(i,j,sep = "_")]] <- 
#       postResample(
#         pred = resultados_test_edificio3[[paste(i,j,sep = "_")]]$predictions,
#         obs = validation_int[,i]) 
#   }
# }
# 
# for (i in c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")){
#   resultados_train3[[i]]<-predict(lista_modelos3[[i]], str_train$SAMP1)
#   lista_resample_train3[[i]] <- 
#     postResample(pred = resultados_train3[[i]]$predictions,
#                  obs = testing[,i])
#   resultados_test3[[i]]<-predict(lista_modelos3[[i]], test)
#   lista_resample_test3[[i]] <- 
#     postResample(pred = resultados_test3[[i]]$predictions,
#                  obs = as.data.frame(test)[,i])
# }


# knn long lat b2 ---------------------------------------------------------

knn_LONG <- knn.reg(train = data_model_knn[,names_train],
                    test = validation_int2b[,names_train],
                    y = data_model_knn$LONGITUDE,
                    k = 1)

knn_LAT <- knn.reg(train = data_model_knn[,names_train],
                   test = validation_int2b[,names_train],
                   y = data_model_knn$LATITUDE,
                   k = 1)
