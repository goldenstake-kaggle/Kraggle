library(EBImage)

samp <- 10
filepath <- '/Users/paul/kraggle/train'
piclist<- sample((list.files(filepath,
                             pattern=".jpg",
                             full.names=TRUE,
                             recursive = T)),
                 ((samp/100)*length(list.files(filepath,
                                               pattern=".jpg",
                                               full.names=TRUE,
                                               recursive = T)))
                 )
df = data.frame("Image_path" = character(), "image" = numeric(), stringsAsFactors=FALSE)
for (i in 1:length(piclist)) {
    img_path = piclist[i]
    img = readImage(img_path)
    img = resize(img, w=64, h=36)
    img = channel(img, 'gray')@.Data
    img = as.vector(img)
    vec <- c(img)
    df <- rbind(df, vec)
}
