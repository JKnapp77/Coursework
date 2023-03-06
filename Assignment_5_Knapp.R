library(dplyr)
library(ROSE)
library(car)
library(MASS)

set.seed(12345)

combine_data <- read.csv('NFL.csv')

str(combine_data)

combine_data <- dplyr::select(combine_data,-c(Year, Player, School, Drafted..tm.rnd.yr., Player_Type, Position_Type, Position, Drafted))

combine_data <- na.omit(combine_data)

combine_data$Bench_Press_Reps<- as.numeric(combine_data$Bench_Press_Reps)

combine_data$Age<- as.numeric(combine_data$Age)

str(combine_data)

combine_model <- lm(Sprint_40yd~., data=combine_data)
summary(combine_model)

combine_model1 <- lm(Sprint_40yd~ Age + Height + Weight + Vertical_Jump + Bench_Press_Reps + Broad_Jump + Agility_3cone + BMI, data=combine_data)
summary(combine_model1)

combine_model2 <- lm(Sprint_40yd~ Height + Weight + Vertical_Jump + Bench_Press_Reps + Broad_Jump + Agility_3cone + BMI, data=combine_data)
summary(combine_model2)

combine_model3 <- lm(Sprint_40yd~ Weight + Vertical_Jump + Bench_Press_Reps + Broad_Jump + Agility_3cone + BMI, data=combine_data)
summary(combine_model3)

combine_model4 <- lm(Sprint_40yd~ Weight + Vertical_Jump + Bench_Press_Reps + Broad_Jump + Agility_3cone, data=combine_data)
summary(combine_model4)

fitted(combine_model2)

residuals(combine_model2)

par(mfrow = c(2, 2))
plot(combine_model2)

durbinWatsonTest(combine_model2)

cutoff <- 4/(nrow(combine_data) - length(combine_model2$coefficients) - 1)
plot(combine_model2, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

outlierTest(combine_model2)

crPlots(combine_model2)

ncvTest(combine_model2)
spreadLevelPlot(combine_model2)

qqPlot(combine_model2, labels = FALSE, simulate = TRUE, main = "Q-Q Plot")

cor(combine_data)
vif(combine_model2)
sqrt(vif(combine_model2)) > 2

stepAIC(combine_model, direction = "backward")

fit = lm(formula = Sprint_40yd ~ Age + Height + Weight + Vertical_Jump + 
     Bench_Press_Reps + Broad_Jump + Agility_3cone + BMI, data = combine_data)
summary(fit)


