## read data
train <- read.csv("../../data/original/training.csv")
test <- read.csv("../../data/original/test.csv")

## examine data
head(train, 1)
str(train)
summary(train)

head(test, 1)
