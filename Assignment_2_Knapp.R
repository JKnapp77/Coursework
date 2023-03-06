combine_data <- read.csv("NFL.csv", stringsAsFactors = FALSE)
str(combine_data)

library(dplyr)

combine_data<- select(combine_data,-c(Year, Player, School, Drafted..tm.rnd.yr., Player_Type, Position_Type, Position))

combine_data <- na.omit(combine_data)

combine_data$Drafted <- factor(combine_data$Drafted, levels = c("Yes", "No"),
                               labels = c("Drafted", "Undrafted"))
combine_data$Bench_Press_Reps<- as.numeric(combine_data$Bench_Press_Reps)
str(combine_data)

table(combine_data$Drafted)
round(prop.table(table(combine_data$Drafted)) * 100, digits = 1)

shuffle_index <- sample(1:nrow(combine_data))

combine_data <- combine_data[shuffle_index, ]
head(combine_data)

combine_data$Drafted

set.seed(1234)

train_sample <- sample(1497, 1000)

str(train_sample)

combine_train <- combine_data[train_sample, ]
combine_test  <- combine_data[-train_sample, ]   

prop.table(table(combine_train$Drafted))   
prop.table(table(combine_test$Drafted)) 

library(rpart)
library(rpart.plot)
library(ROSE)
library(caret)

treeimb <- rpart(Drafted ~ ., data = combine_train)
pred.treeimb <- predict(treeimb, newdata = combine_test)

accuracy.meas(combine_test$Drafted, pred.treeimb[,2])

roc.curve(combine_test$Drafted, pred.treeimb[,2], plotit = F)

data_balanced_over <- ovun.sample(Drafted ~ ., data = combine_train, method = "over",N = 1576)$data
table(data_balanced_over$Drafted)

data_balanced_under <- ovun.sample(Drafted ~ ., data = combine_train, method = "under", N = 424, seed = 1)$data
table(data_balanced_under$Drafted)

data_balanced_both <- ovun.sample(Drafted ~ ., data = combine_train, method = "both", p=0.5, N=1497, seed = 1)$data
table(data_balanced_both$Drafted)

data.rose <- ROSE(Drafted ~ ., data = combine_train, seed = 1)$data
table(data.rose$Drafted)

tree.rose <- rpart(Drafted ~ ., data = data.rose)
tree.over <- rpart(Drafted ~ ., data = data_balanced_over)
tree.under <- rpart(Drafted ~ ., data = data_balanced_under)
tree.both <- rpart(Drafted ~ ., data = data_balanced_both)

pred.tree.rose <- predict(tree.rose, newdata = combine_test)
pred.tree.over <- predict(tree.over, newdata = combine_test)
pred.tree.under <- predict(tree.under, newdata = combine_test)
pred.tree.both <- predict(tree.both, newdata = combine_test)

roc.curve(combine_test$Drafted, pred.tree.rose[,2])

roc.curve(combine_test$Drafted, pred.tree.over[,2])

roc.curve(combine_test$Drafted, pred.tree.under[,2])

roc.curve(combine_test$Drafted, pred.tree.both[,2])

fit <- rpart(Drafted ~ Age+Height+Weight+Sprint_40yd+Vertical_Jump+Bench_Press_Reps+Broad_Jump+Agility_3cone+Shuttle, data = data_balanced_over, method = 'class')
rpart.plot(fit)

predict_unseen <-predict(fit, data_balanced_over, type = 'class')

table_mat <- table(data_balanced_over$Drafted, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_balanced_over, type = 'class')
  table_mat <- table(data_balanced_over$Drafted, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 250,
                         minbucket = 25,
                         maxdepth = 20,
                         cp = 0)

tune_fit <- rpart(Drafted ~ Age+Height+Weight+Sprint_40yd+Vertical_Jump+Bench_Press_Reps+Broad_Jump+Agility_3cone+Shuttle, data = data_balanced_over, method = 'class', control = control)

rpart.plot(tune_fit)

accuracy_tune(tune_fit)

confusionMatrix(data_balanced_over$Drafted, predict_unseen)
