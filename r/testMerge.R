train_new <- read.csv('train.csv', header = T, stringsAsFactors = F, na.strings = "", strip.white=TRUE)
dog_breeds_new <- read.csv("dog_breeds.csv", header = T, stringsAsFactors = FALSE, strip.white=TRUE)



train_new$SimpleBreed <- sapply(train_new$Breed, function(x) gsub('Mix', '', strsplit(x, split = '/')[[1]][1]))
train_new <- train_new[1:200,]
a <- merge(train_new, dog_breeds_new, by=("SimpleBreed"), all.x = TRUE)
