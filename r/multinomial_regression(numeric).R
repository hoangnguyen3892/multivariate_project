library(nnet)   # multinomial regression
library(VGAM)   # multinomial regression

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

model2 = vglm(OutcomeType ~ factor(HasName)+factor(train$TimeofDay) + factor(train$WeekDay)
                            +Sex+factor(IsIntact)+Age+factor(train$MixorMultipleorSimple)
                            +Group+Size+factor(IsMixColor), family = multinomial(refLevel = 1), data = train)
summary(model2)


# Just for interest, calculating t-stats
model1 = multinom(OutcomeType ~ factor(HasName)+factor(train$TimeofDay) + factor(train$WeekDay)
                  +Sex+factor(IsIntact)+Age+factor(train$MixorMultipleorSimple)
                  +Group+Size+factor(IsMixColor))
summary(model1)
t <- summary(model1)$coefficients/summary(model1)$standard.errors
t

# odd ratios
real <- exp(coef(model2))
real

# R squared
model = vglm(OutcomeType ~ 1, family = multinomial(refLevel = 1), data = train)
lrtest(model2, model)
LL0 <- VGAM::logLik(model)
LLf1 <- VGAM::logLik(model2)
as.vector(1 - (LLf1/LL0))

# Confident interval
confint(model2)

# Predict
p <- predict(model, newdata = test, "probs")

# Use MixorMultipleorSimple/ not use IsMixBreed


#lrtest(model1, model)
#LLf <- VGAM::logLik(model1)
#LL0 <- VGAM::logLik(model)
#as.vector(1 - (LLf/LL0))



# # Use IsMixBreed/ not use MixorMultipleorSimple
model3 = multinom(OutcomeType ~ factor(HasName)+factor(train$TimeofDay) + factor(train$WeekDay)
+Sex+factor(IsIntact)+Age+factor(train$MixorMultipleorSimple)
+Group+Size+factor(IsMixColor))
summary(model3)
# 1 - pchisq(30145.06, 30345.06)
# 
# model4 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
#               +Group+Size+IsMixColor, family = multinomial(refLevel = 1), data = train)
# summary(model4)
# 
# model5 = vglm(OutcomeType ~ 1, family = multinomial(refLevel = 1), data = train)
# lrtest(model5)
# LLf <- VGAM::logLik(model4)
# LL0 <- VGAM::logLik(model5)
# as.vector(1 - (LLf/LL0))
# 
# 
# model6 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
#               +Group+Size, family = multinomial(refLevel = 1), data = train)
# summary(model6)
# LLf <- VGAM::logLik(model6)
# as.vector(1 - (LLf/LL0))
# 
# 
# model7 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
#               +Group, family = multinomial(refLevel = 1), data = train)
# summary(model7)
# LLf <- VGAM::logLik(model7)
# as.vector(1 - (LLf/LL0))
# 
# model8 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed, family = multinomial(refLevel = 1), data = train)
# summary(model8)
# LLf <- VGAM::logLik(model8)
# as.vector(1 - (LLf/LL0))
# 
# library(mnlogit)
# model = mnlogit(OutcomeType ~ HasName | TimeofDay | WeekDay|Sex|IsIntact|Age|MixorMultipleorSimple
#                 |IsMixBreed|Group|Size|IsMixColor, data= train,choiceVar = "alt")
# summary(model)
