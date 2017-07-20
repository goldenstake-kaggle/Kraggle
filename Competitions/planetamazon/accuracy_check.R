library(caret)
set.seed(619719)

fileList <- list.files(pattern = "-test.RDS", full.names = T, recursive = T, ignore.case = T)
accuracyTable <- data.frame(files = fileList, stringsAsFactors = F)
overallAccuracies <- data.frame()

for (i in 1:nrow(accuracyTable)) {
  testedSet <- readRDS(accuracyTable[i,"files"])
  nCols <- ncol(testedSet)
  testedSet <- testedSet[,(nCols-1):nCols]
  cm <- confusionMatrix(testedSet[,2],testedSet[,1])
  table <- data.frame(t(cm$overall))
  if (i == 1) {
    overallAccuracies <- table
  } else {
    overallAccuracies <- rbind(overallAccuracies, table)
  }
}

accuracyTable <- cbind(accuracyTable, overallAccuracies)
View(accuracyTable)
