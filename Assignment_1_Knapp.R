combine_data <- read.csv("NFL.csv", stringsAsFactors = FALSE)
str(combine_data)

combine_data <- combine_data[-1]
combine_data <- combine_data[-1]
combine_data <- combine_data[-1]
combine_data <- combine_data[-1]
combine_data <- combine_data[-9]
combine_data <- combine_data[-10]
combine_data <- combine_data[-10]
combine_data <- combine_data[-10]

clean_combine_data <- na.omit(combine_data)

table(clean_combine_data$Drafted)
round(prop.table(table(clean_combine_data$Drafted)) * 100, digits = 1)

clean_combine_data$Drafted <- factor(clean_combine_data$Drafted, levels = c("Yes", "No"),
                         labels = c("Drafted", "Undrafted"))
clean_combine_data$Bench_Press_Reps<- as.numeric(clean_combine_data$Bench_Press_Reps)
str(clean_combine_data)

summary(clean_combine_data[c("Sprint_40yd", "Vertical_Jump", "Broad_Jump")])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}
normalize(c(210, 420, 630, 840, 1050))

clean_combine_data_u <- as.data.frame(lapply(clean_combine_data[1:9], normalize))
summary(clean_combine_data_u$Height)

set.seed(1234)

library(tidymodels)
split_strat  <- initial_split(clean_combine_data, prop = 0.7, 
                              strata = "Drafted")
train_strat  <- training(split_strat)
test_strat   <- testing(split_strat)
train_strat_u <- train_strat[-10]
test_strat_u <- test_strat[-10]

str(test_strat)
str(train_strat)

table(train_strat$Drafted)

prop.table(table(train_strat$Drafted))

library(rpart)
treeimb <- rpart(Drafted ~ ., data = train_strat)
pred.treeimb <- predict(treeimb, newdata = test_strat)

accuracy.meas(test_strat$Drafted, pred.treeimb[,2])

roc.curve(test_strat$Drafted, pred.treeimb[,2], plotit = F)

data_balanced_over <- ovun.sample(Drafted ~ ., data = train_strat, method = "over",N = 1660)$data
table(data_balanced_over$Drafted)

data_balanced_under <- ovun.sample(Drafted ~ ., data = train_strat, method = "under", N = 762, seed = 1)$data
table(data_balanced_under$Drafted)

data_balanced_both <- ovun.sample(Drafted ~ ., data = train_strat, method = "both", p=0.5, N=1211, seed = 1)$data
table(data_balanced_both$Drafted)

library(ROSE)

data.rose <- ROSE(Drafted ~ ., data = train_strat, seed = 1)$data
table(data.rose$Drafted)

tree.rose <- rpart(Drafted ~ ., data = data.rose)
tree.over <- rpart(Drafted ~ ., data = data_balanced_over)
tree.under <- rpart(Drafted ~ ., data = data_balanced_under)
tree.both <- rpart(Drafted ~ ., data = data_balanced_both)

pred.tree.rose <- predict(tree.rose, newdata = test_strat)
pred.tree.over <- predict(tree.over, newdata = test_strat)
pred.tree.under <- predict(tree.under, newdata = test_strat)
pred.tree.both <- predict(tree.both, newdata = test_strat)

roc.curve(test_strat$Drafted, pred.tree.rose[,2])

roc.curve(test_strat$Drafted, pred.tree.over[,2])

roc.curve(test_strat$Drafted, pred.tree.under[,2])

roc.curve(test_strat$Drafted, pred.tree.both[,2])

ROSE.holdout <- ROSE.eval(Drafted ~ ., data = train_strat, learner = rpart, method.assess = "holdout", extr.pred = function(obj)obj[,2], seed = 1)
ROSE.holdout

clean_combine_train_labels <- train_strat[, 10]
clean_combine_test_labels <- data_balanced_over[, 10]

library(class)
clean_combine_data_test_pred <- knn(train = train_strat_u, test = data_balanced_over[-10],
                      cl = clean_combine_train_labels, k=41)

library(gmodels)
CrossTable(x = clean_combine_test_labels, y = clean_combine_data_test_pred,
           prop.chisq=FALSE)

library(caret)
confusionMatrix(clean_combine_test_labels,clean_combine_data_test_pred)