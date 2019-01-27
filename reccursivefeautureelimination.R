set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
walmart<-read.csv(file="C:/Users/aakarsh/Downloads/IS/EOD-WMT.csv",header = TRUE,sep=",")
# define the control using a random forest selection function
walmart$Date<-as.Date(as.character(walmart$Date),formate="%y%m%d")
control <- rfeControl(functions=rfFuncs, method="cv")
results <- rfe(walmart[,-5], walmart[,5], rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))