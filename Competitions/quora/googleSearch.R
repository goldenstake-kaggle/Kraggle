#################################
## Phi's Google Search Scraper ##
#################################

## Input string, output character vector of search result descriptions

library(rvest)

googleSearch <- function(a) {

a <- gsub(pattern = " ",replacement = "%20", a) 

url <- paste0("https://www.google.com/search?q=",a)

session <- html_session(url)
searchResult <- html_text(html_nodes(session, css = ".st"))
searchResult <- gsub(patter = "[^[:alpha:] ]", replacement = "", searchResult)

return(searchResult)
rm(a, url, session, searchResult)

}

# testsearch <- "What is the step by step guide to invest in share market in india?"
# googleSearch(testsearch)