library(Amelia)
library(pscl)
library(caret)
library(ROCR)
library(dplyr)
library(ROSE)

set.seed(1234)

combine_data <- read.csv('NFL.csv')

combine_data<- select(combine_data,-c(Year, Player, School, Drafted..tm.rnd.yr., Player_Type, Position_Type, Position))

combine_data <- na.omit(combine_data)

combine_data$Drafted <- factor(combine_data$Drafted, levels = c("Yes", "No"),
                               labels = 0:1)
combine_data$Bench_Press_Reps<- as.numeric(combine_data$Bench_Press_Reps)

combine_data$Age<- as.numeric(combine_data$Age)

str(combine_data)

train_sample <- sample(1497, 1000)

str(train_sample)

combine_train <- combine_data[train_sample, ]

data_balanced_over <- ovun.sample(Drafted ~ ., data = combine_train, method = "over",N = 1576)$data
table(data_balanced_over$Drafted)

shuffle_oversample <- sample(1:nrow(data_balanced_over))
the_combine_data <- data_balanced_over[shuffle_oversample, ]

str(the_combine_data)

sapply(the_combine_data,function(x) sum(is.na(x)))

sapply(the_combine_data, function(x) length(unique(x)))

missmap(the_combine_data, main = "Missing values vs observed")

train_over <- the_combine_data[1:1000,]
test_over <- the_combine_data[1001:1576,]

model <- glm(Drafted ~.,family=binomial(link='logit'),data=train_over)
summary(model)

anova(model, test="Chisq")

pR2(model)

fitted.results <- predict(model,newdata=subset(test_over,select=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)),type='response')
fitted.results <-ifelse(fitted.results>0.5,1,0)
fitted.results = as.factor(fitted.results)

str(fitted.results)

misClasificError <- mean(fitted.results != test_over$Drafted)

confusionMatrix(fitted.results, test_over$Drafted)

print(paste('Accuracy',1-misClasificError))

p <- predict(model, newdata=subset(test_over,select=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), type="response")
pr <- prediction(p, test_over$Drafted)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
