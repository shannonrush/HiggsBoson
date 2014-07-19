library(plyr)

PrepPrediction <- function(pred, test) {
    data.frame(EventId=test$EventId, b=pred[,"b"], s=pred[,"s"]) 
}

WriteSubmission <- function(pred.df, submission.num) {
    ordered.pred <- arrange(pred.df, s)
    Class <- apply(ordered.pred, 1, function(p) ifelse(p["s"] > p["b"], "s", "b"))
    final.prediction <- data.frame(EventId=ordered.pred$EventId,RankOrder=1:550000,Class)
    filename <- paste0("submission",submission.num,".csv")
    write.csv(final.prediction, file=filename, row.names=F, quote=F)
}