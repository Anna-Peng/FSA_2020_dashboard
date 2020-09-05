
#### LIBRARIES ####
library(shinydashboard)
library(DBI)
library(RSQLite)
library(DBI)
options(shiny.sanitize.errors = FALSE)
library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(lubridate)
library(magrittr)
library(stringr)
library(visNetwork)
library(DT)
library(tm)
library(wordcloud)
library(memoise)
library(RColorBrewer)
library(readr)
library(colorspace)



#################################  FILE LOCATIONS #################################
IN_DB <- "../../../data/processed/foobar.db"
IN_MD_START <- "desc_start.md"
IN_MD_TYPOLOGY <- "desc_typology.md"
IN_MD_PLATFORMS <- "desc_platforms.md"
IN_MD_RESTAURANTS <- "desc_restaurants.md"
IN_MD_ECOSYSTEM <- "desc_ecosystem.md"
IN_MD_SOCIAL <- "desc_social.md"
IN_ECO_NODES <- "ecosys_nodes.txt"
IN_ECO_EDGES <- 'ecosys_edges.txt'
IN_ECO_SPECS <- 'ecosys_specifications.txt'
IN_FOOD_DICT <- "FoodDictionary_editable.csv"
IN_OMITTED_WORDS <- "OmittedWords_editable.csv"



#################################  FUNCTIONS #################################


generateHref <- function(website){
  cleanname = website %>% str_remove("http.?://") %>% str_remove("/$")
  link <- str_detect(website, "^http") %>% ifelse(website, sprintf("http://%s", website))
  href <- sprintf('<a href="%s" target="_blank">%s</a>',link, cleanname)
  href
}
buttonInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
defineColor <- function(sel, defaultcols){
  highlight = "#20c1ed"
  sapply(1:length(sel), function(i){
    if(is.na(sel[[i]])){
      NA
    } else if(sel[[i]] == "default"){
      defaultcols[[i]]
    } else if(sel[[i]] == "deselected"){
      clr <- hex2RGB(defaultcols[[i]])
      clr <- as(clr, "HLS")
      clr@coords[,"L"] <- min(c(clr@coords[,"L"] * 1.5, 1))
      clr <- as(clr, "RGB")
      hex(clr)
    } else{
      highlight
    }
  })
}


################################# MAIN NLP VECTORISATION FUNCTION #################################
################################# NLP MATRIX #################################

getMatrix  <- function(text, rm_words){
  text <- iconv(text, from = "UTF-8", to = "ASCII", " ")
  text <- sapply(text, function(x) strsplit(x, split = " "))
  
  myCorpus <-  VCorpus(VectorSource(text))
  toSpace <- content_transformer(function (x , pattern) sub(pattern, "\\1", x))
  myCorpus <-  tm_map(myCorpus, content_transformer(tolower))
  myCorpus <-  tm_map(myCorpus, removePunctuation)
  myCorpus <-  tm_map(myCorpus, removeNumbers)
  myCorpus <-  tm_map(myCorpus, removeWords, rm_words)
  myCorpus <-  tm_map(myCorpus, removeWords,c(stopwords("SMART")))
  myCorpus <- tm_map(myCorpus, toSpace, "(.*)s$" )
  myDTM <- TermDocumentMatrix(myCorpus, control=list(stemming=FALSE))
  m <-  as.matrix(myDTM)
  sort(rowSums(m), decreasing = TRUE)
}

##################### MAKE WORDCLOUDS WITH HIGHLIGHTED COLOR FROM SIMILAR WORDS #####################
getTermMatrix <-  memoise(function(text, food_dict, rm_words) {
  # get similar words and word count vectorizer
  words.freq <- getMatrix(text, rm_words)
  # make all words black except for the cluster center
  words.freq.col <- rep("#333333", length = nrow(words.freq))
  words.freq.col[names(words.freq) %in% food_dict] <- "#0f97bd"
  # the size of the words will be the frequency from the NY Times headlines
  return(list(words.freq, words.freq.col))
})
