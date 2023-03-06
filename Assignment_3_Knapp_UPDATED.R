library(neuralnet)
library(caret) 
library(dplyr)
library(ROSE)

set.seed(12345)

combine_data <- read.csv("NFL.csv")
str(combine_data)

combine_data <- select(combine_data,-c(Year, Player, School, Drafted..tm.rnd.yr., Player_Type, Position_Type, Position))

combine_data <- na.omit(combine_data)

combine_data$Drafted <- factor(combine_data$Drafted, levels = c("Yes", "No"),
                               labels = c("Drafted", "Undrafted"))
combine_data$Bench_Press_Reps<- as.numeric(combine_data$Bench_Press_Reps)

combine_data$Age<- as.numeric(combine_data$Age)

str(combine_data)

train_sample <- sample(1497, 1000)

str(train_sample)

combine_train <- combine_data[train_sample, ]

data_balanced_over <- ovun.sample(Drafted ~ ., data = combine_train, method = "over",N = 1576)$data
table(data_balanced_over$Drafted)

data_balanced_over[1:10] <- scale(data_balanced_over[1:10])

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

combine_data_norm <- as.data.frame(lapply(data_balanced_over[1:10], normalize))

summary(combine_data_norm$Age)

combine_data_norm$Drafted <- data_balanced_over$Drafted

indexes=createDataPartition(data_balanced_over$Drafted, p=.60, list = F)
indexes

train = data_balanced_over[indexes, ]
test = data_balanced_over[-indexes, ] 

xtest = test[, -11]
ytest = test[, 11]

str(train)
str(test)

nnet=neuralnet(Drafted~., train, hidden = 3, linear.output = FALSE)

plot(nnet) 

ypred = neuralnet::compute(nnet, xtest) 
DraftPred = ypred$net.result
print(DraftPred)

DraftPred=data.frame("Drafted"=ifelse(max.col(DraftPred[ ,1:2])==1, "Drafted", "Undrafted"))

DraftPred

cm=confusionMatrix(as.factor(ytest), (as.factor(DraftPred$Drafted)))

print(cm) 