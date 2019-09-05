#Neural Network

m <- data_train_dummy %>% select(starts_with("WAP"),LONGITUDE)

head(m)
library(neuralnet)

tic()
nn <- neuralnet(LONGITUDE ~ .,
                data=m,
                hidden=c(150), threshold=0.01)
nn$result.matrix
plot(nn)
toc()
names(data_train_dummy)
a <- predict(nn,data_validation_dummy)
predict(nn,data_validation_dummy)
postResample(predict(nn,data_validation_dummy),data_validation_dummy$LONGITUDE)
