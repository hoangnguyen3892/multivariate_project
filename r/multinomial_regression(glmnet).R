library(glmnet)   # multinomial regression

train <- read.csv('train_processed_R.csv', header = T, stringsAsFactors = F, na.strings = "", strip.white=TRUE)


# Variables
OutcomeType <- factor(train$OutcomeType)

HasName <- factor(train$HasName)
train$TimeofDay <- factor(train$TimeofDay)
train$WeekDay <- factor(train$WeekDay)
Sex <- factor(train$Sex)
IsIntact <- factor(train$IsIntact)
Age <- train$Age
#IsMixBreed <- factor(train$IsMixBreed)
train$MixorMultipleorSimple <- factor(train$MixorMultipleorSimple)
Group <- factor(train$Group)
Size <- train$Size
IsMixColor <- factor(train$IsMixColor)


# Set reference level

train$TimeofDay <- relevel(train$TimeofDay, ref = "morning")
train$WeekDay <- relevel(train$WeekDay, ref = "Monday")
train$MixorMultipleorSimple <- relevel(train$MixorMultipleorSimple, ref = "Simple")

TimeofDay <- factor(train$TimeofDay)
WeekDay <- factor(train$WeekDay)
MixorMultipleorSimple <- factor(train$MixorMultipleorSimple)

x = cbind(HasName,TimeofDay,WeekDay,Sex,IsIntact,Age,MixorMultipleorSimple,Group,Size,IsMixColor)
#x = as.matrix(HasName,TimeofDay,WeekDay,Sex,IsIntact,Age,MixorMultipleorSimple,Group,Size,IsMixColor)
y = as.factor(OutcomeType)


cvfit = cv.glmnet(x, y, family="multinomial",type.multinomial = "grouped")
plot(cvfit)
title("Multinomial Family",line=2.5)

best_lambda <- cvfit$lambda.min
best_lambda
coef(cvfit)

y_pred = predict(cvfit,newx=x[1:15499,])
Accuracy(y_pred, y)

model1 = vglm(OutcomeType ~ factor(HasName)+factor(train$TimeofDay) + factor(train$WeekDay)
              +Sex+factor(IsIntact)+Age+factor(train$MixorMultipleorSimple)
              +Group+Size+factor(IsMixColor), family = multinomial(refLevel = 1), data = train)
summary(model2)