library(randomForest)
library(plyr)

boostedRF <- function(x.names, y.name, train, test, m=5, verbose=T, ntree=500, mtry=NULL) {
    mtry <- ifelse(is.null(mtry), floor(sqrt(ncol(train))), mtry)
    K <- nlevels(train[,y.name])
    C <- matrix(nrow=nrow(test), ncol=m)
    # this is just for Higgs Boson, could be generalized 
    # for now it's just a vector of s probabilities averaged over
    # all forests
    probs <- numeric(nrow(test)) 
    n <- nrow(train)
    w <- rep(1/n,n) # initialize weights
    A <- numeric(m)
    t <- train
    for (i in 1:m) {
        print(paste("FOREST",i))
        fit <- randomForest(t[,x.names], t[,y.name], do.trace=verbose, ntree=ntree, mtry=mtry)
        pred <- as.data.frame(predict(fit, test, type="prob"))
        print("test prediction made")
        C[,i] <- apply(pred, 1, function(p) ifelse(p["s"] > p["b"], "s", "b"))
        probs <- (probs+pred$s)/i
        rm(pred)
        print("test prediction removed")
        h <- predict(fit, train) # predict classes on training 
        print("train prediction made")
        rm(fit) # free up memory
        print("fit removed")
        levels(h)<-levels(t[,y.name]) # rebalancing levels
        misses <- as.numeric(h!=t[,y.name])
        rm(h)
        print("train prediction removed")
        err <- error(w, misses)
        A[i] <- alpha(err, K) # one weighted error value for each forest
        w <- weights(w, A[i], misses)
        print("weights recalculated")
        t <- train[sample(n, n, replace=T, prob=w), ]
        t[,y.name]<-droplevels(t[,y.name]) # in case any outcomes are not sampled
        print("resampling complete")
    }
    finalPrediction(C, A, probs)
}

error <- function(w, misses) {
    sum(w * misses)
}

alpha <- function(err, K) {
    if (err>0.5) {
        1
    } else if (err<=0) {
        20
    } else {
        log((1-err)/err)+log(K-1)
    }
}

weights <- function(w, a, misses) {
    tmp_w <- w * exp(a*misses)
    renorm(tmp_w)
}

renorm <- function(tmp_w) {
    tmp_w/(sum(tmp_w))
}

finalPrediction <- function(C, A, probs) {
    class <- apply(C, 1, function(sample) samplePrediction(sample, A))
    event.id <- test$EventId
    tmp.df <- data.frame(Class=class, EventId=event.id, probs=probs)
    ordered.df <- arrange(tmp.df, probs)
    data.frame(EventId=ordered.df$EventId, RankOrder=1:550000, Class=ordered.df$Class)
}

samplePrediction <- function(sample, A) {
    # sample is a row of probablities, one set of outcome levels for each forest
    # it returns a train frame with an averaged probability for each outcome
    votes.table <- votesTable(sample, A)
    prediction(votes.table)
}

votesTable <- function(sample, A) {
    classes <- levels(train[,y.name])
    sapply(classes, function(k) weightedClassVote(k, sample, A))
}

weightedClassVote <- function(k, sample, A) {
    # k is a class
    # sample is a row of probablities, one set of outcome levels for each forest
    # A is the weighted errors
    sum(A * (sample==k))
}

prediction <- function(votes.table) {
    max.votes <- maxVotes(votes.table)
    ifelse(length(max.votes)==1, max.votes, sample(max.votes,1)) # random if tie
}

maxVotes <- function(votes.table) {
    names(which(votes.table==max(votes.table)))
}