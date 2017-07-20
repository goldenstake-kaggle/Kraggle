library(EBImage)
library(caret)
library(dplyr)

source('dependents.R')

finalSet <- readRDS('test_featurePopulated.RDS')
tagList <- c( "agriculture",
              "artisinal_mine",
              "bare_ground",
              "blooming",
              "blow_down",
              "clear",
              "cloudy",
              "conventional_mine",
              "cultivation",
              "habitation",
              "haze",
              "partly_cloudy",
              "primary",
              "road",
              "selective_logging",
              "slash_burn",
              "water" 
              )
modelList <- c( "rfmain",
                "nn1",
                "knn1")

# predict all ----
for (model in modelList) {
  print(model)
  for (tag in tagList) {
    print(tag)
    print(Sys.time())
    model_tag <- paste0(model,"_",tag)
    trainPath <- paste0(model,"/")
    trainFile <- list.files(trainPath, pattern = model_tag, full.names = T, recursive = F, ignore.case = T)
    trainObject <- readRDS(trainFile)
    finalSet[model_tag] <- predict(trainObject, finalSet)
    finalSet[model_tag] <- as.numeric(finalSet[[model_tag]])
  }  
}

# aggregate ----
for (tag in tagList) {
  cols <- paste0(modelList,"_",tag)
  finalSet[tag] <- apply(finalSet[cols],1,sum)
}

# tagify ----
tagify <- finalSet[c("image_name",tagList,"weather_class")]
for (name in tagList) {
  tagify[name] <- sapply(tagify[name], function(x) ifelse(x == 1, name, NA))
}
tagify$tags <- apply(tagify[,-1], 1, paste, collapse = " ") %>% 
  gsub(pattern = "NA ", replacement = "") %>%
  gsub(pattern = " NA", replacement = "")

submission <- tagify[c("image_name","tags")]
write.csv(submission, "submission8-knn.csv", row.names = F)