library(EBImage)
library(caret)
library(dplyr)

source('dependents.R')

set.seed(619719)

## make sure Kaggle data files are downloaded and unzipped into working directory (same directory where this file lives)

# load and parse outcomes ----
# source('trainingOutcomes.R')

# load data and create features ----
# source('createTrainingFeatures.R')

train <- readRDS('train_featurePopulated.RDS')
outcomes <- readRDS('outcomes.RDS')
cleanedData <- merge(train, outcomes)

# weather ----
weatherLabels <- c("clear","cloudy","partly_cloudy","haze")
weatherOutcomes <- cleanedData[,weatherLabels]
allZeroRows <- which(apply(weatherOutcomes,1,sum) == 0)
cleanedData <- cleanedData[-allZeroRows,]

cleanedData$weather_class <- weatherLabels[apply(cleanedData[,weatherLabels], 1, function(x) which(x == 1))]

# train ----
labels <- names(cleanedData)[168:length(cleanedData)]
predictors <- cleanedData[3:166]
for (label in labels) {
  print(paste(label, Sys.time()))
  fullSet <- cbind(predictors, cleanedData[label])
  fullSet[,label] <- as.factor(fullSet[,label])
  trainIndex <- createDataPartition(fullSet[,label], p = 0.8, list = F, times = 1)
  trainingSet <- fullSet[trainIndex,]
  testSet <- fullSet[-trainIndex,]
  # yt <- as.formula(paste0(label, " ~ ."))
  # rf <- train(data = trainingSet, yt, method = "rf", preProcess = c("zv","nzv","corr"))
  # rf <- train(trainingSet[,1:(ncol(trainingSet)-1)], trainingSet[,ncol(trainingSet  )], method = "nnet", preProcess = "range", trace = FALSE)
  rf <- train(trainingSet[,1:(ncol(trainingSet)-1)], trainingSet[,ncol(trainingSet  )], method = "knn", preProcess = c("center","scale"), tuneLength = 1)
  trainName <- paste("nn1", label, sep = "_")
  saveRDS(rf, paste0(trainName, ".RDS"))
  testSet[trainName] <- predict(rf, testSet)
  saveRDS(testSet, paste0(trainName, "-test", ".RDS"))
}

# # train (unique1: using some labels as predictors)----
# predictorLabels <- c(
#   "artisinal_mine",
#   "blooming",
#   "blow_down",
#   "conventional_mine",
#   "selective_logging",
#   "slash_burn"
#   )
# outcomeLabels <- c(
#   "agriculture",
#   "bare_ground",
#   "clear",
#   "cloudy",
#   "cultivation",
#   "habitation",
#   "haze",
#   "partly_cloudy",
#   "primary",
#   "road",
#   "water",
#   "weather_class"
# )
# predictors <- cbind(cleanedData[3:166], cleanedData[predictorLabels])
# for (label in outcomeLabels) {
#   print(paste(label, Sys.time()))
#   fullSet <- cbind(predictors, cleanedData[label])
#   fullSet[,label] <- as.factor(fullSet[,label])
#   trainIndex <- createDataPartition(fullSet[,label], p = 0.8, list = F, times = 1)
#   trainingSet <- fullSet[trainIndex,]
#   testSet <- fullSet[-trainIndex,]
#   # yt <- as.formula(paste0(label, " ~ ."))
#   # rf <- train(data = trainingSet, yt, method = "rf", preProcess = c("zv","nzv","corr"))
#   rf <- train(trainingSet[,1:(ncol(trainingSet)-1)], trainingSet[,ncol(trainingSet  )], method = "nnet", preProcess = "range", trace = FALSE)
#   trainName <- paste("xnn2", label, sep = "_")
#   saveRDS(rf, paste0(trainName, ".RDS"))
#   testSet[trainName] <- predict(rf, testSet)
#   saveRDS(testSet, paste0(trainName, "-test", ".RDS"))
# }

