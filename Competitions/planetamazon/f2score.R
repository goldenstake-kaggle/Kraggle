# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#system("ls ../input")

# Any results you write to the current directory are saved as output.

# F2 score (FBeta) in R
#  based on anokas python example 
require(MLmetrics)

y = rbind(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0),
          c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0),
          c(1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1),
          c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0),
          c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0))

# Note that with f2 score, your predictions need to be binary
# If you have probabilities, you will need to do some form of thresholding beforehand
p = rbind(c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0),
          c(1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
          c(1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1),
          c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0),
          c(0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0))
          
fbs <- rep(0,length(y[,1]))
for(i in 1:length(y[,1])) fbs[i] =FBeta_Score(y_pred = p[i,], y_true = y[i,], positive = "1", beta = 2)
fbs[which(is.na(fbs))] <- 0   # if none labelled correctly get NaN so set to 0
f2score <- mean(fbs)
cat('F2 Score:', f2score)

#  this way gives same score as python for average='micro'
f2score <- FBeta_Score(y_pred = p, y_true = y, positive = "1", beta = 2)
cat('F2 Score:', f2score)
