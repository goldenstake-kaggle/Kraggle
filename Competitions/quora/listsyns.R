## synonym list by Phi
## This function uses wordnet (Princeton) to list all of the synonyms of all words in a sentence, by part of speach
## Note: it removes punction and numbers, taking only letters of the alphabet
## Choices for pos are: "NOUN", "VERB", "ADVERB" ,"ADJECTIVE"

# Sample Usage:
# for (i in 1:nrow(trainingSet)) {
#   q1 <- trainingSet[i, "question1"]
#   q2 <- trainingSet[i, "question2"]
# 
#   a <- listsyns(q1, pos = "NOUN")
#   b <- listsyns(q2, pos = "NOUN")
#   trainingSet[i, "noun synonym overlap"] <- length(intersect(a,b))/length(union(a,b))
# }  

library(wordnet)
library(magrittr)

listsyns <- function(x, pos = "NOUN") {
  x <- gsub(x, pattern = "[^[:alpha:] ]", replacement = "") %>%
    strsplit(x, split = " ") %>%
    unlist()
  
  b <- list()
  
  for (word in x) {
    b[[word]] <- synonyms(word, pos = pos)
  }
  
  b <- unlist(b, use.names = F)
  return(b)
  rm(x, b)
}