## Capstone: Coursera Data Science
## Predicting the next world
## Seher



################################################################

library(shiny); library(stringr); library(tm);library(markdown); library(stylo);library(shinythemes)
# setting the working directory
#setwd("C:/Users/s.fazlioglu/Dropbox/Coursera/Course10_capstones")
# Loading bigram, trigram and quadgram frequencies words matrix frequencies


bg <- readRDS("bigram.RData"); tg <- readRDS("trigram.RData"); qd <- readRDS("quadgram.RData")

names(bg)[names(bg) == 'word1'] <- 'w1'; names(bg)[names(bg) == 'word2'] <- 'w2';
names(tg)[names(tg) == 'word1'] <- 'w1'; names(tg)[names(tg) == 'word2'] <- 'w2'; names(tg)[names(tg) == 'word3'] <- 'w3';
names(qd)[names(qd) == 'word1'] <- 'w1'; names(qd)[names(qd) == 'word2'] <- 'w2'; names(qd)[names(qd) == 'word3'] <- 'w3'
names(qd)[names(qd) == 'word4'] <- 'w4';
message <- "" ## cleaning message


dataCleaner<-function(text){
    
    cleanText <- tolower(text)
    cleanText <- removePunctuation(cleanText)
    cleanText <- removeNumbers(cleanText)
    cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
    cleanText <- stripWhitespace(cleanText)
    
    return(cleanText)
}

cleanInput <- function(text){
    
    textInput <- dataCleaner(text)
    textInput <- txt.to.words.ext(textInput, 
                                  language="English.all", 
                                  preserve.case = TRUE)
    
    return(textInput)
}


nextWordPrediction <- function(wordCount,textInput){
    
    if (wordCount>=3) {
        textInput <- textInput[(wordCount-2):wordCount] 
        
    }
    
    else if(wordCount==2) {
        textInput <- c(NA,textInput)   
    }
    
    else {
        textInput <- c(NA,NA,textInput)
    }
    
    
    ### 1 ###
    wordPrediction <- as.character(qd[qd$w1==textInput[1] & 
                                                  qd$w2==textInput[2] & 
                                                  qd$w3==textInput[3],][1,]$w4)
    
    if(is.na(wordPrediction)) {
        wordPrediction <- as.character(tg[tg$w1==textInput[2] & 
                                                       tg$w2==textInput[3],][1,]$w3)
    
        if(is.na(wordPrediction)) {
            wordPrediction <- as.character(bg[bg$w1==textInput[3],][1,]$w2)
        }
        if(is.na(wordPrediction)) {
            wordPrediction <- as.character(tg[tg$w1==textInput[3],][1,]$w2)
        }
        if(is.na(wordPrediction)) {
            wordPrediction <- as.character(head("it",1))
        }
    }
    
    
    print(wordPrediction)
    
}




