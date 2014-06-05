Submission 5 - RF Bagging
========================================================

```r
train <- read.csv("../../data/processed/processed_train.csv")
# remove EventId, not allowed for classifying
train$EventId <- NULL
test <- read.csv("../../data/original/test.csv")
```


```r
set.seed(12345)
training.indices <- sample(nrow(train), size = 0.8 * nrow(train))
training <- train[training.indices, ]
validate <- train[-training.indices, ]
```


```r
bagging <- function(formula, data, test, m = 5, ntree = 500, mtry = NULL, trace = T) {
    outcome.label <- outcomeLabel(formula)
    mtry <- ifelse(is.null(mtry), floor(sqrt(ncol(data))), mtry)
    C <- matrix(nrow = nrow(test), ncol = m)
    n <- nrow(data)
    for (i in 1:m) {
        t <- data[sample(n, n, replace = T), ]
        t[, outcome.label] <- droplevels(t[, outcome.label])  # in case any outcomes are not sampled
        fit <- randomForest(formula, data = t, ntree = ntree, do.trace = trace)
        C[, i] <- as.character(predict(fit, test))
    }
    apply(C, 1, bagPrediction)
}

bagPrediction <- function(sample) {
    max.classes <- maxClasses(sample)
    ifelse(length(max.classes) == 1, max.classes, sample(max.classes, 1))
}

maxVoteCount <- function(sample) {
    sum(sample == names(which.max(table(sample))))
}

maxClasses <- function(sample) {
    names(which(table(sample) == maxVoteCount(sample)))
}

outcomeLabel <- function(formula) {
    toString(formula[[2]])
}
```

Run a single RF with the training and validation data for baseline

```r
require(randomForest)
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
rf <- randomForest(Label ~ ., data = training)
```


```r
baseline.prediction <- predict(rf, validate)
```


```r
sum(baseline.prediction == validate$Label)/nrow(validate)
```

```
## [1] 0.8399
```

Bagging RF with defaults (m=5)

```r
bagging.prediction <- bagging(Label ~ ., training, validate, trace = F)
```


```r
sum(bagging.prediction == validate$Label)/nrow(validate)
```

```
## [1] 0.8385
```






