library(lubridate)  # dates
library(ggplot2)  # visualize
library(ggthemes) 
library(dplyr) # data manipulation
library(nnet)   # multinomial regression
library(VGAM)   # multinomial regression
library(randomForest) # classification algorithm
library(xgboost)    # tree learning algorithm


library(formattable)
library(pander)
#library(VIM)
#library(mice)   # missing value

# Read data
train <- read.csv('train.csv', header = T, stringsAsFactors = F, na.strings = "", strip.white=TRUE)
dog_breeds <- read.csv("dog_breeds.csv", header = T, stringsAsFactors = FALSE, strip.white=TRUE)
#unique(train$SexuponOutcome)

###########################################################################
#                          Data preprocessing                             #
###########################################################################

################################################################################################################
# Data of dogs
train = subset(train, train$AnimalType !="Cat")

# TRACKING
nrow(train)   # 15595


#------------------------------------------------------------------------------------------------------
# Objective: Non-metric variable: HasName (0: has name, 1: no name)
#----------------------Name----------------------
# Replace blank names with "Nameless"
#train$Name <- ifelse(is.na(train$Name)==TRUE), 'Nameless', train$Name)

train$Name[is.na(train$Name)] <- "Nameless"


# Make a name/no name variable
#train$HasName[train$Name == 'Nameless'] <- 0
#train$HasName[train$Name != 'Nameless'] <- 1
train$HasName = ifelse(train$Name == 'Nameless', 0, 1)

#------------------------------------------------------------------------------------------------------
# Objective:  Non metric variable: TimeofDay (morning, midday, lateday and night)
#             Non metric variable: WeekDay (Monday, Tuesday, ..., Sunday)
#----------------------DateTime----------------------
# Extract time variables from date (uses the "lubridate" package)
train$Hour    <- hour(train$DateTime)
train$Weekday <- wday(train$DateTime)
train$Month   <- month(train$DateTime)
train$Year    <- year(train$DateTime)

train$TimeofDay <-  ifelse(train$Hour >= 5 & train$Hour < 11, 'morning',
                    ifelse(train$Hour >= 11 & train$Hour < 16, 'midday',
                    ifelse(train$Hour >= 15 & train$Hour < 20, 'lateday', 'night')))
train$TimeofDay <- factor(train$TimeofDay, levels = c('morning', 'midday','lateday', 'night'))

# Get the date value
train$Date = sapply(train$DateTime, function(x) strsplit(x, split = ' ')[[1]][1])
train$WeekDay = weekdays(as.Date(train$Date))
train$WeekDay <- factor(train$WeekDay, levels = c("Monday", "Tuesday","Wednesday", "Thursday",
                                                    "Friday", "Saturday", "Sunday"))

#------------------------------------------------------------------------------------------------------
# Objective: Non metric dependent variable Outcome (Return_to_owner, Adoption, Transfer, Euthanasia, Died)
#----------------------OutcomeType----------------------
unique(train$OutcomeType, incomparables = FALSE)
train$OutcomeType <- factor(train$OutcomeType, levels = c('Adoption', 'Transfer','Return_to_owner', 
                                                    "Euthanasia", "Died"))
#------------------------------------------------------------------------------------------------------
# Objective: Delete this unused variable
#----------------------OutcomeSubType----------------------
#unique(train$OutcomeSubtype, incomparables = FALSE)
# This information is not really helpful --> Delete
train$OutcomeSubtype <- NULL

#------------------------------------------------------------------------------------------------------
# Objective:  Non metric variable: IsIntact (0: no, 1: yes)
#             Non metric variable: Sex (Male, Female)
#----------------------SexuponOutcome----------------------
unique(train$SexuponOutcome, incomparables = FALSE)
#train$sex = sapply(train$SexuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])
train$IsIntact =    ifelse(train$SexuponOutcome == "Neutered Male" | train$SexuponOutcome == "Spayed Female", 0,
                    ifelse(train$SexuponOutcome == "Intact Male" | train$SexuponOutcome == "Intact Female", 1, NA))
train$Sex =   ifelse(train$SexuponOutcome == "Neutered Male" | train$SexuponOutcome == "Intact Male", 'Male',
              ifelse(train$SexuponOutcome == "Spayed Female" | train$SexuponOutcome == "Intact Female", "Female", 
                    NA))

# TRACKING
#nrow(train)   # 15595
#summary(train$Sex == "Male")
#summary(train$IsIntact == 0)

#------------------------------------------------------------------------------------------------------
# Objective:  Metric variable: AgeinYears (age of animal)
#             Non metric variable: Age (Juvenile, Adult, Old)
#----------------------AgeuponOutcome----------------------
#train$UnitofTime = sapply(train$AgeuponOutcome, function(x) strsplit(x, split = ' ') return c(1, "month")
# Get the time value
train$TimeValue = sapply(train$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])
                        
# Get unit of time
train$UnitofTime = sapply(train$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])
                           
# Convert UnitOfTime in factor and TimeValue in numeric
train$UnitofTime = as.factor(train$UnitofTime)
train$TimeValue = as.numeric(train$TimeValue)

# Convert TimeValue in years using the appropriate divider based on UnitofTime
Divider <- ifelse(train$UnitofTime == 'day' | train$UnitofTime == 'days', 365, 
              ifelse(train$UnitofTime == 'week' | train$UnitofTime == 'weeks', 52, 
              ifelse(train$UnitofTime == 'month' | train$UnitofTime == 'months', 12, 
              ifelse(train$UnitofTime == 'year' | train$UnitofTime == 'years', 1, NA))))
train$AgeinYears = train$TimeValue / Divider
train$AgeinYears = ifelse(train$AgeinYears < 0.5, 0,
                  ifelse(train$AgeinYears >= 0.5 & train$AgeinYears < 1, 1, train$AgeinYears))

train$Age_alpha = ifelse(train$AgeinYears < 2, "Juvenile",
                   ifelse(train$AgeinYears >= 2 & train$AgeinYears <= 6, "Adult", "Old"))
train$Age_alpha <- factor(train$Age_alpha, levels = c('Juvenile', 'Adult','Old'))

train$Age = ifelse(train$Age_alpha == "Juvenile", 0,
                         ifelse(train$Age_alpha == "Adult", 1, 2))
summary(train$Age == 2)
#summary(train$IsIntact == 1)

#------------------------------------------------------------------------------------------------------
# Objective:  Non metric variable: IsMixBreed (0: no, 1: yes)
#             Non metric variable: MixorMultipleorSimple (Mix, Multiple, Simple)
#             Non metric variable: Group (Sporting, NonSporting, Hound, Working, 
#                                   Terrier, Toy, Herding, Pitbull)
#             Non metric variable: Size (S, M, L and G)
# Temperament Non metric variable: IsIntelligent (0: no, 1: yes)
#             Non metric variable: IsAggressive (0: no, 1: yes)
#----------------------Breed----------------------
# Website
# http://www.akc.org/dog-breeds/
# http://www.dogbreedslist.info/Tags-L/#.WB9OU-F96T_
# http://dogs.petbreeds.com/
# http://list25.com/25-most-dangerous-dog-breeds/
# https://pethelpful.com/dogs/100-smartest-dog-breeds

# Keep dominant breed
train$SimpleBreed <- sapply(train$Breed, function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))
# unique(train$SimpleBreed)

###### Statistics of Breeds
# unique(train$Breed, incomparables = FALSE)
# frequency_table <- table(train$SimpleBreed)
# sort(frequency_table, decreasing = TRUE)

# Mix or Simple breed
train$IsMixBreed <- ifelse(grepl('Mix', train$Breed), 1, 0)

# Mix/Multiple or Simple breed
train$MixorMultipleorSimple <- ifelse(grepl('Mix', train$Breed), "Mix",
                                ifelse(grepl('/', train$Breed), "Multiple", "Simple"))


# Merge train data with dog_breeds based on SimpleBreed
train <- merge(train, dog_breeds, by = ("SimpleBreed"), all.x = TRUE)


train$Size_alpha <- factor(train$Size_alpha, levels = c('S', 'M','L', 'G'))
train$Size = ifelse(train$Size_alpha == 'S', 0,
            ifelse(train$Size_alpha == 'M', 1,
            ifelse(train$Size_alpha == 'L', 2, 3)))
# TRACKING
nrow(train)     #15595

#------------------------------------------------------------------------------------------------------
# Objective: Non metric variable: IsMixColor (0: no, 1: yes)
#----------------------Color----------------------
# unique(train$Color, incomparables = FALSE)
train$IsMixColor <- ifelse(grepl("/", train$Color), 1, 0)


################################################################################################################
#----------------------Reordering the columns in data frame----------------------
train <- train[c("OutcomeType", "HasName", "TimeofDay","WeekDay", "Sex", "IsIntact",
                 "AgeinYears", "Age", "IsMixBreed","MixorMultipleorSimple", "Group", "Size", "IsMixColor")]

#----------------------Check missing values----------------------
sum(is.na(train$HasName))
sum(is.na(train$TimeofDay))
sum(is.na(train$WeekDay))
sum(is.na(train$Sex))
sum(is.na(train$IsIntact))
sum(is.na(train$AgeinYears))
sum(is.na(train$Age))
sum(is.na(train$IsMixBreed))
sum(is.na(train$MixorMultipleorSimple))
sum(is.na(train$Group))
sum(is.na(train$Size))
sum(is.na(train$IsMixColor))

sum(!complete.cases(train)) 

#----------------------Delete missing values----------------------
train <- na.omit(train)
write.csv(train, file = "train_processed_R.csv")

#----------------------Check variables----------------------
unique(train$OutcomeType)
unique(train$HasName)
unique(train$TimeofDay)
unique(train$WeekDay)
unique(train$Sex)
unique(train$IsIntact)
unique(train$AgeinYears)
unique(train$Age)
unique(train$IsMixBreed)
unique(train$MixorMultipleorSimple)
unique(train$Group)
unique(train$Size)
unique(train$IsIntelligent)
unique(train$IsAggressive)
unique(train$IsMixColor)

# #----------------------Impute missing value----------------------
# #methods(mice)
# 
# #replacecolumn_numeric = subset(train, select = c(AgeinYears))
# #summary(replacecolumn_numeric)
# 
# #replacecolumn_factor = subset(train, select = c(Group, Size))
# #replacecolumn_Binaryfactor = subset(train, select = c(Sex,Neutered, Intelligent, Aggressive))
# 
# #imputed_Data1 <- mice(replacecolumn_numeric, m=5, maxit = 50, method = 'pmm', seed = 500)
# #summary(imputed_Data1)
# 
# 
# #imputed_Data2 <- mice(replacecolumn_factor, m=5, maxit = 50, method = 'polyreg', seed = 500)
# #summary(imputed_Data2)
# #train <- complete(imputed_Data2)
# 
# 
# # TRACKING
# nrow(train)     # 15378
# 
# ###########################################################################
# #                          Multinomial regression                         #
# ###########################################################################
# # Variables
# train$OutcomeType <- as.factor(train$OutcomeType)
# 
# train$HasName <- as.factor(train$HasName)
# train$TimeofDay <- as.factor(train$TimeofDay)
# train$WeekDay <- as.factor(train$WeekDay)
# train$Sex <- as.factor(train$Sex)
# train$IsIntact <- as.factor(train$IsIntact)
# train$Age <- as.factor(train$Age)
# train$IsMixBreed <- as.factor(train$IsMixBreed)
# train$MixorMultipleorSimple <- as.factor(train$MixorMultipleorSimple)
# train$Group <- as.factor(train$Group)
# train$Size <- as.factor(train$Size)
# train$IsMixColor <- as.factor(train$IsMixColor)
# 
# 
# 
# # Regression
# 
# # Use MixorMultipleorSimple/ not use IsMixBreed
# model1 = multinom(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+MixorMultipleorSimple
#              +Group+Size+IsMixColor)
# summary(model1)
# 
# model2 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+MixorMultipleorSimple
#               +Group+Size+IsMixColor, family = multinomial(refLevel = 1), data = train)
# summary(model2)
# 
# #model3 = vglm(Outcome ~ 1, family = multinomial(refLevel = 1), train)
# #LLf <- VGAM::logLik(model2)
# #LL0 <- VGAM::logLik(model3)
# #as.vector(1-(LLf/LL0))
# #lrtest(model2)
# 
# # Use IsMixBreed/ not use MixorMultipleorSimple
# model3 = multinom(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
#                   +Group+Size+IsMixColor)
# summary(model3)
# 
# model4 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
#               +Group+Size+IsMixColor, family = multinomial(refLevel = 1), data = train)
# summary(model4)
# 
# 
# 
# 
# 
# library(mlogit)
# H <- mlogit.data(train, shape="wide", choice="OutcomeType",varying = NULL)
# model3 <- mlogit(formula = Outcome ~ 0 | HasName+TimeofDay+WeekDay+Sex+Neutered+AgeinYears
#                  +MixorMultipleorSimple+Group+Size+IsIntelligent+IsAggressive+IsMixColor, refLevel=0, data=train)
# summary(model3)
# 
# ###########################################################################
# #                         Random forest                                   #
# ###########################################################################
# #factorVars <- c('OutcomeType','HasName','TimeofDay','WeekDay','Sex','IsIntact','IsMixBreed',
# #                'MixorMultipleorSimple','Group','Size','IsIntelligent','IsAgressive','IsMixColor')
# 
# #train[factorVars] <- lapply(train[factorVars], function(x) as.factor(x))
# 
# # Build model
# set.seed(731)
# 
# rf_mod <- randomForest(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+MixorMultipleorSimple
#                        +Group+Size+IsMixColor, 
#                        data = train, 
#                        ntree = 600, 
#                        importance = TRUE)
# 
# # Model error
# plot(rf_mod, ylim=c(0,1))
# legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)
# 
# # Rank importance
# importance    <- importance(rf_mod)
# varImportance <- data.frame(Variables = row.names(importance), 
#                             Importance = round(importance[ ,'MeanDecreaseGini'],2))
# rankImportance <- varImportance %>%
#   mutate(Rank = paste0('#',dense_rank(desc(Importance))))
# 
# # Visualize
# ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
#                            y = Importance)) +
#   geom_bar(stat='identity', colour = 'black') +
#   geom_text(aes(x = Variables, y = 0.5, label = Rank),
#             hjust=0, vjust=0.55, size = 4, colour = 'lavender', fontface = 'bold') +
#   labs(x = 'Variables', title = 'Relative Variable Importance') +
#   coord_flip() + 
#   theme_few()
# 
# ###########################################################################
# #                              XGBoost                                    #
# ###########################################################################
# y_train <- as.numeric(as.factor(train$OutcomeType)) - 1
# labels_train <- data.frame(train$OutcomeType, y_train)
# 
# 
# xgb_train <- xgb.DMatrix(model.matrix(~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+MixorMultipleorSimple
#                                       +Group+Size+IsMixColor, data = train), label=y_train, missing=NA)
# xgb_model <- xgboost(xgb_train, y_train, nrounds=50, objective='multi:softprob',
#                      num_class=5, eval_metric='mlogloss', max_depth = 9, 
#                      early.stop.round=TRUE)
# 
# #max_depth
# feature_important = xgb.importance(model = xgb_model, feature_names = colnames(train))
# head(feature_important, n = 20) %>%
#   formattable(align = 'l')
# importance_matrix <- xgb.importance(model = xgb_train)
# print(importance_matrix)
# xgb.plot.importance(importance_matrix = importance_matrix)
