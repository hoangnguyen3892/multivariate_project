library(dplyr)
# Read data and fill blank space with NA
train <- read.csv('train.csv', header = T, stringsAsFactors = F, na.strings="")
test <- read.csv('test.csv', header = T, stringsAsFactors = F, na.strings="")

#----------------------AnimalID----------------------
# Rename the ID column so train & test match
names(test)[1] <- 'AnimalID'
test$AnimalID <- as.character(test$AnimalID)
train <- bind_rows(train, test)

# Save new data
write.csv(train,file="newtrain.csv")
