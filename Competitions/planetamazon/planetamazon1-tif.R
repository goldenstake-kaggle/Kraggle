setwd("X:/Phi/Documents/Kraggle/Competitions/planetamazon")

library(EBImage)
library(magrittr)

tags <- read.csv("train.csv/train.csv")
imgPath <- "train-tif/"
pathlist <- list.files(imgPath, pattern = ".tif", full.names = T, recursive = F, ignore.case = T)
piclist <- list.files(imgPath, pattern = ".tif", full.names = F, recursive = F, ignore.case = T) %>%
  gsub(pattern = ".tif", replacement = "")

#training set
train <- data.frame(file_path = pathlist, 
                    image_name = piclist,
                    R_apl = NA,
                    G_apl = NA,
                    B_apl = NA,
                    IR_apl = NA,
                    R_sd = NA,
                    G_sd = NA,
                    B_sd = NA,
                    IR_sd = NA,
                    stringsAsFactors = FALSE) %>%
  merge(tags)

#calculate APL of each RGB channel for each image
progress <- winProgressBar(title = "Calculating APLs", label = "Progress", min = 0, max = nrow(train), width = 500)

for (i in 1:nrow(train)) {
  if (is.null(tryCatch(readImage(train[i, "file_path"]), error=function(e) NULL))) {
    next
  } else {
    img <- readImage(train[i, "file_path"])
    train[i, "R_apl"] <- mean(img[,,1])
    train[i, "R_sd"] <- sd(img[,,1])
    train[i, "G_apl"] <- mean(img[,,2])
    train[i, "G_sd"] <- sd(img[,,2])
    train[i, "B_apl"] <- mean(img[,,3])
    train[i, "B_sd"] <- sd(img[,,3])
    train[i, "IR_apl"] <- mean(img[,,4])
    train[i, "IR_sd"] <- sd(img[,,4])
    setWinProgressBar(progress, i, label = paste(i,"/",nrow(train),"-",round(i/nrow(train)*100, 0),"% done"))
  }

}

close(progress)

write.csv(train, "train_apl-tif.csv")


