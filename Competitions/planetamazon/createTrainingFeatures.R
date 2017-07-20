library(EBImage)
library(caret)
library(dplyr)

source('dependents.R')

imgPath <- "train-tif-v2/"
pathList <- list.files(imgPath, pattern = ".tif", full.names = T, recursive = F, ignore.case = T)
picList <- list.files(imgPath, pattern = ".tif", full.names = F, recursive = F, ignore.case = T) %>%
  gsub(pattern = ".tif", replacement = "")
train <- data.frame(stringsAsFactors = FALSE,
                    file_path = pathList, 
                    image_name = picList)

startTime <- Sys.time()
for (i in 1:nrow(train)) {
  img <- readImage(train[i, "file_path"])
  for (chnl in 1:4) {
    cImg <- img[,,chnl]
    train[i,paste0("sum-",chnl)] <- sum(cImg)
    train[i,paste0("sum^2-",chnl)] <- sum(cImg^2)
    train[i,paste0("sum^3-",chnl)] <- sum(cImg^3)
    train[i,paste0("sum^4-",chnl)] <- sum(cImg^4)
    train[i,paste0("sd-",chnl)] <- sd(cImg)
    train[i,paste0("min-",chnl)] <- min(cImg)
    train[i,paste0("max-",chnl)] <- max(cImg)
    train[i,paste0("med-",chnl)] <- median(cImg)
    train[i,paste0("q10-",chnl)] <- quantile(cImg, 0.1)
    train[i,paste0("q20-",chnl)] <- quantile(cImg, 0.2)
    train[i,paste0("q30-",chnl)] <- quantile(cImg, 0.3)
    train[i,paste0("q40-",chnl)] <- quantile(cImg, 0.4)
    train[i,paste0("q60-",chnl)] <- quantile(cImg, 0.6)
    train[i,paste0("q70-",chnl)] <- quantile(cImg, 0.7)
    train[i,paste0("q80-",chnl)] <- quantile(cImg, 0.8)
    train[i,paste0("q90-",chnl)] <- quantile(cImg, 0.9)
    train[i,paste0("sdsd1-",chnl)] <- sdsd_square(cImg, 1)
    train[i,paste0("sdsd2-",chnl)] <- sdsd_square(cImg, 2)
    train[i,paste0("sdsd3-",chnl)] <- sdsd_square(cImg, 3)
    train[i,paste0("sdsd4-",chnl)] <- sdsd_square(cImg, 4)
    train[i,paste0("sdsd5-",chnl)] <- sdsd_square(cImg, 5)
    train[i,paste0("sdsd6-",chnl)] <- sdsd_square(cImg, 6)
    train[i,paste0("sdsd7-",chnl)] <- sdsd_square(cImg, 7)
    train[i,paste0("difsdsd.d1-",chnl)] <- sd(apply(apply(cImg, 1, diff, differences = 1), 1, sd))
    train[i,paste0("difsdmean.d1-",chnl)] <- mean(apply(apply(cImg, 1, diff, differences = 1), 1, sd))
    train[i,paste0("difsdmax.d1-",chnl)] <- max(apply(apply(cImg, 1, diff, differences = 1), 1, sd))
    train[i,paste0("difsdmin.d1-",chnl)] <- min(apply(apply(cImg, 1, diff, differences = 1), 1, sd))
    train[i,paste0("difsdsd.d2-",chnl)] <- sd(apply(apply(cImg, 1, diff, differences = 2), 1, sd))
    train[i,paste0("difsdmean.d2-",chnl)] <- mean(apply(apply(cImg, 1, diff, differences = 2), 1, sd))
    train[i,paste0("difsdmax.d2-",chnl)] <- max(apply(apply(cImg, 1, diff, differences = 2), 1, sd))
    train[i,paste0("difsdmin.d2-",chnl)] <- min(apply(apply(cImg, 1, diff, differences = 2), 1, sd))
    train[i,paste0("difsdsd.d3-",chnl)] <- sd(apply(apply(cImg, 1, diff, differences = 3), 1, sd))
    train[i,paste0("difsdmean.d3-",chnl)] <- mean(apply(apply(cImg, 1, diff, differences = 3), 1, sd))
    train[i,paste0("difsdmax.d3-",chnl)] <- max(apply(apply(cImg, 1, diff, differences = 3), 1, sd))
    train[i,paste0("difsdmin.d3-",chnl)] <- min(apply(apply(cImg, 1, diff, differences = 3), 1, sd))
    train[i,paste0("difabsumsum.d1-",chnl)] <- sum(apply(abs(apply(cImg, 1, diff, differences = 1)), 1, sum))
    train[i,paste0("difabsumsd.d1-",chnl)] <- sd(apply(abs(apply(cImg, 1, diff, differences = 1)), 1, sum))
    train[i,paste0("difabsumsum.d2-",chnl)] <- sum(apply(abs(apply(cImg, 1, diff, differences = 2)), 1, sum))
    train[i,paste0("difabsumsd.d2-",chnl)] <- sd(apply(abs(apply(cImg, 1, diff, differences = 2)), 1, sum))
    train[i,paste0("difabsumsum.d3-",chnl)] <- sum(apply(abs(apply(cImg, 1, diff, differences = 3)), 1, sum))
    train[i,paste0("difabsumsd.3-",chnl)] <- sd(apply(abs(apply(cImg, 1, diff, differences = 3)), 1, sum))
  }
  progressIndicator(startTime, max = nrow(train))
  if (i %% 100 == 0) {
    saveRDS(train, "train_featurePopulated.RDS")
  }
}
saveRDS(train, "train_featurePopulated.RDS")