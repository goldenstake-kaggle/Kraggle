library(EBImage)
library(caret)
library(dplyr)

source('dependents.R')

finalSet <- readRDS('test_featurePopulated.RDS')

# # test (no weather aggregation) ----
# trainPath <- "rf1/"
# tagList <- c( "agriculture",
#               "artisinal_mine",
#               "bare_ground",
#               "blooming",
#               "blow_down",
#               "clear",
#               "cloudy",
#               "conventional_mine",
#               "cultivation",
#               "habitation",
#               "haze",
#               "partly_cloudy",
#               "primary",
#               "road",
#               "selective_logging",
#               "slash_burn",       
#               "water" )
# 
# for (tag in tagList) {
#   print(tag)
#   print(Sys.time())
#   trainFile <- list.files(trainPath, pattern = paste0("rf1_",tag), full.names = T, recursive = F, ignore.case = T)
#   trainObject <- readRDS(trainFile)
#   finalSet[tag] <- predict(trainObject, finalSet)
# }
# 
# tagify <- finalSet[c("image_name",tagList)]
# for (name in names(tagify)[-1]) {
#   tagify[name] <- sapply(tagify[name], function(x) ifelse(x == 1, name, NA))
# }
# tagify$tags <- apply(tagify[,-1], 1, paste, collapse = " ") %>% 
#   gsub(pattern = "NA ", replacement = "") %>%
#   gsub(pattern = " NA", replacement = "")
# 
# submission <- tagify[c("image_name","tags")]
# write.csv(submission, "submission1-rf1.csv", row.names = F)


# test (with weather aggregation) ----
# this uses the "rfmain" folder. Makes sure the algoirthm of choice is in that folder, tagged with "rfmain_"

trainPath <- "knn1/"
tagList <- c( "agriculture",
              "artisinal_mine",
              "bare_ground",
              "blooming",
              "blow_down",
              "conventional_mine",
              "cultivation",
              "habitation",
              "primary",
              "road",
              "selective_logging",
              "slash_burn",       
              "water" )

for (tag in c(tagList,"weather_class")) {
  print(tag)
  print(Sys.time())
  trainFile <- list.files(trainPath, pattern = paste0("x_",tag), full.names = T, recursive = F, ignore.case = T)
  trainObject <- readRDS(trainFile)
  finalSet[tag] <- predict(trainObject, finalSet)
}

tagify <- finalSet[c("image_name",tagList,"weather_class")]
for (name in tagList) {
  tagify[name] <- sapply(tagify[name], function(x) ifelse(x == 1, name, NA))
}
tagify$tags <- apply(tagify[,-1], 1, paste, collapse = " ") %>% 
  gsub(pattern = "NA ", replacement = "") %>%
  gsub(pattern = " NA", replacement = "")

submission <- tagify[c("image_name","tags")]
write.csv(submission, "submission8-knn.csv", row.names = F)