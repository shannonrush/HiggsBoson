Submission 7 - Forward Feature Selection of Linear Models
========================================================

```r
train <- read.csv("../../data/processed/processed_train.csv")
# remove EventId, not allowed for classifying
train$EventId <- NULL
test <- read.csv("../../data/original/test.csv")
```


```r
set.seed(123)
training.indices <- sample(nrow(train), size = 0.8 * nrow(train))
training <- train[training.indices, ]
validate <- train[-training.indices, ]
```

Uses a forward seeking technique to find the optimum model fit to glm.  
Does not include any treatment of undefined -999.0 quantities.  
Uses only one 80/20 split to validate.  

```r
LinearResult <- function(f, formula.string) {
    formula <- as.formula(paste(formula.string, f))
    glm <- glm(formula, data = training, family = binomial)
    # a vector of probabilities that prediction is 's'
    probs <- predict.glm(glm, newdata = validate, type = "response")
    prediction <- sapply(probs, function(x) ifelse(x < 0.5, "b", "s"))
    length(which(prediction == validate$Label))
}
```



```r
features <- names(training)[1:30]
complete.results <- data.frame(formula = character(), correct = integer())
formula.string <- "Label ~"
while (length(features) > 0) {
    results <- sapply(features, LinearResult, formula.string)
    winner.index <- which.max(results)
    winner <- features[winner.index]
    winning.formula <- paste(formula.string, winner)
    new.result <- data.frame(formula = winning.formula, correct = max(results))
    complete.results <- rbind(complete.results, new.result)
    # add winner to formula and remove from features
    features <- features[-winner.index]
    formula.string <- paste(winning.formula, " + ")
}
```


```r
complete.results$formula <- as.character(complete.results$formula)
winning.result <- which.max(complete.results$correct)
best.formula <- as.formula(complete.results[winning.result, "formula"])
best.formula
```

```
## Label ~ DER_mass_transverse_met_lep + PRI_tau_pt + DER_deltar_tau_lep + 
##     DER_pt_h + DER_mass_vis + DER_pt_tot + DER_mass_jet_jet + 
##     PRI_jet_subleading_pt + PRI_jet_all_pt + DER_mass_MMC + PRI_jet_num + 
##     DER_pt_ratio_lep_tau + PRI_lep_pt + PRI_met + PRI_lep_eta + 
##     PRI_tau_eta + PRI_met_phi + DER_sum_pt + DER_deltaeta_jet_jet + 
##     PRI_jet_subleading_phi + DER_lep_eta_centrality + PRI_jet_subleading_eta + 
##     DER_prodeta_jet_jet + PRI_lep_phi
```


```r
glm <- glm(best.formula, data = train, family = binomial)
# a vector of probabilities that prediction is 's'
probs <- predict.glm(glm, newdata = test, type = "response")
probs.df <- data.frame(EventId = test$EventId, prob = probs)
# order probs.df by decreasing probability of s to get the RankOrder
require(plyr)
```

```
## Loading required package: plyr
```

```r
ordered.pred <- arrange(probs.df, desc(prob))
class <- sapply(ordered.pred$prob, function(x) ifelse(x < 0.5, "b", "s"))
pred.df <- data.frame(EventId = ordered.pred$EventId, RankOrder = 550000:1, 
    Class = class)
write.csv(pred.df, file = "submission7.csv", row.names = F, quote = F)
```





