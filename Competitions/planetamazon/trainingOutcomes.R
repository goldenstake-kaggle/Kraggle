library(EBImage)
library(caret)
library(dplyr)

source('dependents.R')

## make sure Kaggle data files are downloaded and unzipped into working directory (same directory where this file lives)

# load and parse outcomes ----
outcomes <- read.csv("train_v2.csv", stringsAsFactors = F)


uniqueTags <- outcomes$tags %>%
  as.character() %>%
  paste(collapse = ' ') %>%
  strsplit(split = ' ') %>%
  unlist() %>%
  unique() %>%
  sort()

startTime <- Sys.time()
for (i in 1:nrow(outcomes)) {
  for (var in uniqueTags) {
    outcomes[i,var] <- ifelse(var %in% unlist(strsplit(outcomes[i,"tags"], split = ' ')), 1, 0)
  }
  progressIndicator(startTime, max = nrow(outcomes))
}
saveRDS(outcomes, "outcomes.RDS")