MeanDiff <- function(pred) {
    mean(abs(pred$b-pred$s))
}

Weights <- function(pred) {
    1-abs(1-(pred$s/.5))
}

PredClass <- function(pred) {
    apply(pred, 1, function(x) ifelse(x["b"] > x["s"], "b", "s"))
}
