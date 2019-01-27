dataset <- read.csv("C:/Users/aakar/Desktop/IS/EOD-WMT.csv")
dataset$Date <- as.Date(dataset$Date)
set.seed(7)
rferesult1 <- dataset[,2:5]


#normalizing data
attach(rferesult1)

open_minimum <- min(Open)
open_maximum <- max(Open)
rferesult1 <- within(rferesult1, Open <- (Open - open_minimum)/(open_maximum - open_minimum))

high_minimum <- min(High)
high_maximum <- max(High)
rferesult1 <- within(rferesult1, High <- (High - high_minimum)/(high_maximum - high_minimum))

low_minimum <- min(Low)
low_maximum <- max(Low)
rferesult1 <- within(rferesult1, Low <- (Low - low_minimum)/(low_maximum - low_minimum))

close_minimum <- min(Close)
close_maximum <- max(Close)
rferesult1 <- within(rferesult1, Close <- (Close - close_minimum)/(close_maximum - close_minimum))

rferesult <- rferesult1



#dividing dataset into training and testing randomly

indices <- sample(1:nrow(rferesult),size = 0.2 * nrow(rferesult))
actual_data <- dataset$Close[indices]
traindata <- rferesult[-indices,]
testdata <- rferesult[indices,]

#building recurrent neural network model

library(RSNNS)
fit1 <- elman(traindata[,1:3], traindata[,4], size = 50, maxit = 1000, learnFuncParams = c(0.01))
pred1 <- predict(fit1,testdata[,1:3])
pred1<-as.data.frame(pred1)
result <- pred1[1:349,]

compare<-cbind(testdata[,4],pred1$V1)
colnames(compare)<-c("actual","predicted")
head(compare)

ta#denormalizing predicted values
dataframe1 <- as.data.frame(compare)
dataframe1<-within(dataframe1,dataframe1$actual <- (dataframe1$actual * (close_maximum - close_minimum)) + close_minimum)
dataframe1<-within(dataframe1,dataframe1$predicted <- (dataframe1$predicted * (close_maximum - close_minimum)) + close_minimum)
head(dataframe1)

library(Metrics)
rmse(testdata[,4], pred1$V1)


