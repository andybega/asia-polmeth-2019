mbrier <- function(f, o, ordered = TRUE) {
  prediction = f
  outcome = o
  
  stopifnot(length(prediction)==length(outcome))
  stopifnot(all(outcome %in% c(0L, 1L, NA)))
  stopifnot(all.equal(sum(prediction), sum(outcome), 1L) | any(is.na(prediction)))
  
  if (ordered) {
    pairs <- NULL
    for (i in 1:(length(prediction)-1)) {
      p_i <- sapply(split(prediction, 1:length(prediction) > i), sum)
      o_i <- sapply(split(outcome, 1:length(outcome) > i), max)
      pairs <- c(pairs, sum((p_i - o_i)^2))
    }
    brier <- mean(pairs)
  } else {
    brier <- sum((prediction - outcome)^2)
  }
  
  return(brier)
}