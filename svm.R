library("e1071")
walmart<-read.csv(file="C:/Users/aakarsh/Downloads/IS/EOD-WMT.csv",header = TRUE,sep=",")
walmart<-walmart[,c(2,3,4,5)]
attach(walmart)
close_minimum <- min(Close)
close_maximum <- max(Close)
walmart<- within(walmart, Close <- (Close - close_minimum)/(close_maximum - close_minimum))
open_minimum <- min(Open)
open_maximum <- max(Open)
walmart<- within(walmart, Open <- (Open - open_minimum)/(open_maximum - open_minimum))
high_minimum <- min(High)
high_maximum <- max(High)
walmart<- within(walmart,  High<- (High - high_minimum)/(high_maximum - high_minimum))
low_minimum <- min(Low)
low_maximum <- max(Low)
walmart<- within(walmart, Low <- (Low - low_minimum)/(low_maximum - low_minimum))

indices <- sample(1:nrow(walmart),size = 0.2 * nrow(walmart))
traindata <- walmart[-indices,]
testdata <- walmart[indices,]
plot(testdata$Close)

svm_model <- svm(traindata$Close ~., data=traindata)
model <- svm(traindata$Close~.,data=traindata,kernel='radial',gamma=0.3334)
preds <- predict(model,testdata[,c(1,2,3)])
points(preds, col = "blue")
