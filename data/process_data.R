## read data
train <- read.csv("original/training.csv")

## remove Weight variable - not allowed as classifying variable
train$Weight <- NULL

## create new CSV
write.csv(train, file="processed/processed_train.csv", row.names=F)
