library(ggplot2)  # visualize
library(ggthemes) 
library(dplyr) # data manipulation
library(nnet)   # multinomial regression
library(VGAM)   # multinomial regression
library(randomForest) # classification algorithm

train <- read.csv('train_processed_R.csv', header = T, stringsAsFactors = F, na.strings = "", strip.white=TRUE)


# Variables
train$OutcomeType <- as.factor(train$OutcomeType)

train$HasName <- as.factor(train$HasName)
train$TimeofDay <- as.factor(train$TimeofDay)
train$WeekDay <- as.factor(train$WeekDay)
train$Sex <- as.factor(train$Sex)
train$IsIntact <- as.factor(train$IsIntact)
train$Age <- as.factor(train$Age)
train$IsMixBreed <- as.factor(train$IsMixBreed)
train$MixorMultipleorSimple <- as.factor(train$MixorMultipleorSimple)
train$Group <- as.factor(train$Group)
train$Size <- as.factor(train$Size)
train$IsMixColor <- as.factor(train$IsMixColor)


# Build model
set.seed(731)

rf_mod <- randomForest(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+MixorMultipleorSimple
                       +IsMixBreed+Group+Size+IsMixColor, 
                       data = train, 
                       ntree = 400, 
                       importance = TRUE)

# Model error
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)

# Rank importance
importance    <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Visualize
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'lavender', fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_few()





# Build model
set.seed(731)

rf_mod <- randomForest(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
                       +Group+Size+IsMixColor, 
                       data = train, 
                       ntree = 600, 
                       importance = TRUE)

# Model error
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)

# Rank importance
importance    <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Visualize
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'lavender', fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_few()

