library(EBImage)
library(caret)
library(dplyr)

source('dependents.R')

imgPath <- "test-tif-v2/"
pathList <- list.files(imgPath, pattern = ".tif", full.names = T, recursive = F, ignore.case = T)
picList <- list.files(imgPath, pattern = ".tif", full.names = F, recursive = F, ignore.case = T) %>%
  gsub(pattern = ".tif", replacement = "")
test <- data.frame(stringsAsFactors = FALSE,
                    file_path = pathList, 
                    image_name = picList)

startTime <- Sys.time()
for (i in 1:nrow(test)) {
  img <- readImage(test[i, "file_path"])
  for (chnl in 1:4) {
    cImg <- img[,,chnl]
    test[i,paste0("sum-",chnl)] <- sum(cImg)
    test[i,paste0("sum^2-",chnl)] <- sum(cImg^2)
    test[i,paste0("sum^3-",chnl)] <- sum(cImg^3)
    test[i,paste0("sum^4-",chnl)] <- sum(cImg^4)
    test[i,paste0("sd-",chnl)] <- sd(cImg)
    test[i,paste0("min-",chnl)] <- min(cImg)
    test[i,paste0("max-",chnl)] <- max(cImg)
    test[i,paste0("med-",chnl)] <- median(cImg)
    test[i,paste0("q10-",chnl)] <- quantile(cImg, 0.1)
    test[i,paste0("q20-",chnl)] <- quantile(cImg, 0.2)
    test[i,paste0("q30-",chnl)] <- quantile(cImg, 0.3)
    test[i,paste0("q40-",chnl)] <- quantile(cImg, 0.4)
    test[i,paste0("q60-",chnl)] <- quantile(cImg, 0.6)
    test[i,paste0("q70-",chnl)] <- quantile(cImg, 0.7)
    test[i,paste0("q80-",chnl)] <- quantile(cImg, 0.8)
    test[i,paste0("q90-",chnl)] <- quantile(cImg, 0.9)
    test[i,paste0("sdsd1-",chnl)] <- sdsd_square(cImg, 1)
    test[i,paste0("sdsd2-",chnl)] <- sdsd_square(cImg, 2)
    test[i,paste0("sdsd3-",chnl)] <- sdsd_square(cImg, 3)
    test[i,paste0("sdsd4-",chnl)] <- sdsd_square(cImg, 4)
    test[i,paste0("sdsd5-",chnl)] <- sdsd_square(cImg, 5)
    test[i,paste0("sdsd6-",chnl)] <- sdsd_square(cImg, 6)
    test[i,paste0("sdsd7-",chnl)] <- sdsd_square(cImg, 7)
    test[i,paste0("difsdsd.d1-",chnl)] <- sd(apply(apply(cImg, 1, diff, differences = 1), 1, sd))
    test[i,paste0("difsdmean.d1-",chnl)] <- mean(apply(apply(cImg, 1, diff, differences = 1), 1, sd))
    test[i,paste0("difsdmax.d1-",chnl)] <- max(apply(apply(cImg, 1, diff, differences = 1), 1, sd))
    test[i,paste0("difsdmin.d1-",chnl)] <- min(apply(apply(cImg, 1, diff, differences = 1), 1, sd))
    test[i,paste0("difsdsd.d2-",chnl)] <- sd(apply(apply(cImg, 1, diff, differences = 2), 1, sd))
    test[i,paste0("difsdmean.d2-",chnl)] <- mean(apply(apply(cImg, 1, diff, differences = 2), 1, sd))
    test[i,paste0("difsdmax.d2-",chnl)] <- max(apply(apply(cImg, 1, diff, differences = 2), 1, sd))
    test[i,paste0("difsdmin.d2-",chnl)] <- min(apply(apply(cImg, 1, diff, differences = 2), 1, sd))
    test[i,paste0("difsdsd.d3-",chnl)] <- sd(apply(apply(cImg, 1, diff, differences = 3), 1, sd))
    test[i,paste0("difsdmean.d3-",chnl)] <- mean(apply(apply(cImg, 1, diff, differences = 3), 1, sd))
    test[i,paste0("difsdmax.d3-",chnl)] <- max(apply(apply(cImg, 1, diff, differences = 3), 1, sd))
    test[i,paste0("difsdmin.d3-",chnl)] <- min(apply(apply(cImg, 1, diff, differences = 3), 1, sd))
    test[i,paste0("difabsumsum.d1-",chnl)] <- sum(apply(abs(apply(cImg, 1, diff, differences = 1)), 1, sum))
    test[i,paste0("difabsumsd.d1-",chnl)] <- sd(apply(abs(apply(cImg, 1, diff, differences = 1)), 1, sum))
    test[i,paste0("difabsumsum.d2-",chnl)] <- sum(apply(abs(apply(cImg, 1, diff, differences = 2)), 1, sum))
    test[i,paste0("difabsumsd.d2-",chnl)] <- sd(apply(abs(apply(cImg, 1, diff, differences = 2)), 1, sum))
    test[i,paste0("difabsumsum.d3-",chnl)] <- sum(apply(abs(apply(cImg, 1, diff, differences = 3)), 1, sum))
    test[i,paste0("difabsumsd.3-",chnl)] <- sd(apply(abs(apply(cImg, 1, diff, differences = 3)), 1, sum))
  }
  progressIndicator(startTime, max = nrow(test))
  if (i %% 100 == 0) {
    saveRDS(test, "test_featurePopulated.RDS")
  }
}
saveRDS(test, "test_featurePopulated.RDS")