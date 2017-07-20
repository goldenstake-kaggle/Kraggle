#to output progress of for loop by iteration. input the starting time and maximum number of iterations
progressIndicator <- function(startTime = startTime, max = numeric()) {
  currentTime <- Sys.time()
  averageIterationTime <- difftime(currentTime, startTime, units = "hours")/i
  timeRemaingEstimate <- averageIterationTime * (max - i)
  cat("\r", 
      i, "of", max, paste0("(", round(i/max*100,2), "%)"), 
      "|", 
      "Estimated Hours Remaining:", round(timeRemaingEstimate,2))
}

#standard deviation of standard deviations for submatrices by how many times quartered
sdsd_square <- function(imgLayer, qtimes) {
  if (2^qtimes > dim(imgLayer)[2]) {
    return(NA)
  } else {
    split <- dim(imgLayer)[2]/(2^qtimes)
    a <- imgLayer %>%
      split(rep(1:(2^qtimes), each = split)) %>%
      lapply(split, rep(1:(2^qtimes), each = split^2)) %>%
      lapply(lapply, sd) %>%
      unlist() %>%
      sd()
    return(a)
  }
}

## needs work for all zero rows and dataframe issuesy
#Kaggle scoring function (F beta). Input 17 column predictor matrix and 17 column true value matrix
require(MLmetrics)
f2score <- function(y_pred, y_true) {
  fbs <- numeric()
  for (i in 1:nrow(y_true)) fbs[i] = FBeta_Score(y_pred = y_pred[i,], y_true = trueValues[i,], positive = "1", beta = 2)
  fbs[which(is.na(fbs))] <- 0   # if none labelled correctly get NaN so set to 0
  f2score <- mean(fbs)
  cat('F2 Score:', f2score)
}



# Feature Key
#1) sum(cImg)
#2) sum(cImg^2)
#3) sum(cImg^3)
#4) sum(cImg^4)
#14) sd(rd)

#9) quantile(rd, 0.1)
#10) quantile(rd, 0.9)
#11) median(rd)
#12) max(rd)
#13) min(rd)

#15) sd sd rd_split4 (2^2)
#16) sd sd rd_split16 (2^4)
#17) sd sd rd_split64 (2^6)
#18) sd sd rd_split256 (2^8)

#sumabdif lag 1,2,3,4,5
#sumabdif dif 1,2,3
#2 directions

#repeat for 4 * 18 = 72 predictors

