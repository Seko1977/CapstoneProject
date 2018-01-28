###########################
# This file creates RDatas to be used in NextWorkPredict file.

# Input: HC corpora dataset
# Output: 3 data files containing most frequently used, 2 words, 3 words and 4 words.

# HC Corpora data set is  broken into words and sentences, and to turn it into n-grams. 
# These are all called tokenization because we are breaking up the text into units of meaning, called 
# tokens.

# In Natural Language Processing (NLP), n-gram is a contiguous sequence of n items from a given 
#sequence of text or speech. Unigrams are single words. Bigrams are two words combinations. 
# Trigrams are three-word combinations and etc.

#The tokenizer method is allowed in R using the package RWeka. The following function is used to 
# extract 1-grams, 2-grams, 3-grams and 4-grams from the text Corpus using RWeka. those are saved as 
# biagram, triagram and quadgram RData to predict the next word.

#########################################

# loading some packages
library(RWekajars)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(qdap)
library(NLP)
library(tm)
library(SnowballC)
library(slam)
library(RWeka)
library(rJava)
library(wordcloud)
library(stringr)
library(DT)
library(stringi)
library(googleVis)
library(ggplot2)


# setting the working directory
setwd("C:/Users/s.fazlioglu/Dropbox/Coursera/Course10_capstones")

###################################################
#The data is from HC Corpora with access to 4 languages, but only English will be used. 
#The dataset has three files: 1) en_US.blogs.txt, 2) en_US.news.txt 3) en_US.twitter.txt.
#The data was loaded from Coursera Link to local machine and will be read from local disk.

# Please check Milestone_report_v3 file for more detailed info on the data if necessary. In this file
# we skip some analysis to speed up the process.


# downloading the data 

path <- file.path("./final" , "en_US")
files<-list.files(path, recursive=TRUE)
# Lets make a file connection of the twitter data set
con <- file("./final/en_US/en_US.twitter.txt", "r") 
twitter<-readLines(con, skipNul = TRUE)
# Close the connection handle when you are done
close(con)
# Lets make a file connection of the blog data set
con <- file("./final/en_US/en_US.blogs.txt", "r") 
blogs<-readLines(con, skipNul = TRUE)
# Close the connection handle when you are done
close(con)
# Lets make a file connection of the news data set
con <- file("./final/en_US/en_US.news.txt", "r") 
news<-readLines(con, skipNul = TRUE)
# Close the connection handle when you are done
close(con)


# Sample the data
set.seed(5000)
sample.size=0.01
data.sample <- c(sample(twitter, length(twitter) * sample.size),
                 sample(blogs, length(blogs) * sample.size),
                 sample(news, length(news) * sample.size))


# Create corpus and clean the data
corpus <- VCorpus(VectorSource(data.sample))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, tolower)
#corpus <- tm_map(corpus, removeWords, stopwords("en"))
# Removing Profanity Words
profanityWords = readLines('./profanity_words.txt')
corpus <- tm_map(corpus,removeWords, profanityWords)

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)


# Dropping some files
rm(blogs,news,twitter)
rm(data.sample)

#############################
# Tokenization
#############################

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

getNGramFreqs <- function(tdm){
    # Helper function to tabulate frequency
    freqs <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
    NGramFreqs <- data.frame(word=names(freqs), frequency=freqs)
    return(NGramFreqs)
}


# Tokenizer function to get bigrams
bigramTDM_sparse <- TermDocumentMatrix(corpus, control=list(tokenize=BigramTokenizer))
bigramTDM <- removeSparseTerms(bigramTDM_sparse, 0.999)
rm(bigramTDM_sparse)
bigram <- getNGramFreqs(bigramTDM)

names(bigram) <- c("words","freq")
head(bigram)
bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram, 
                    one = sapply(str2,"[[",1),   
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)

## saving files 
write.csv(bigram[bigram$freq > 1,],"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")


# Tokenizer function to get trigrams
trigramTDM_sparse <- TermDocumentMatrix(corpus, control=list(tokenize=TrigramTokenizer))
trigramTDM <- removeSparseTerms(trigramTDM_sparse, 0.9999)
rm(trigramTDM_sparse)
trigram <- getNGramFreqs(trigramTDM)

names(trigram) <- c("words","freq")
head(trigram)
##################### 
trigram$words <- as.character(trigram$words)
str3 <- strsplit(trigram$words,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))
# trigram$words <- NULL
trigram <- data.frame(word1 = trigram$one,word2 = trigram$two, 
                      word3 = trigram$three, freq = trigram$freq,stringsAsFactors=FALSE)
# saving files
write.csv(trigram[trigram$freq > 1,],"trigram.csv",row.names=F)
trigram <- read.csv("trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"trigram.RData")


# Tokenizer function to get quadgrams
quadgramTDM_sparse <- TermDocumentMatrix(corpus, control=list(tokenize=QuadgramTokenizer))
quadgramTDM <- removeSparseTerms(quadgramTDM_sparse, 0.9999)
rm(quadgramTDM_sparse)
quadgram <- getNGramFreqs(quadgramTDM)

names(quadgram) <- c("words","freq")
quadgram$words <- as.character(quadgram$words)

str4 <- strsplit(quadgram$words,split=" ")
quadgram <- transform(quadgram,
                      one = sapply(str4,"[[",1),
                      two = sapply(str4,"[[",2),
                      three = sapply(str4,"[[",3), 
                      four = sapply(str4,"[[",4))
# quadgram$words <- NULL
quadgram <- data.frame(word1 = quadgram$one,
                       word2 = quadgram$two, 
                       word3 = quadgram$three, 
                       word4 = quadgram$four, 
                       freq = quadgram$freq, stringsAsFactors=FALSE)
# saving files
write.csv(quadgram[quadgram$freq > 1,],"quadgram.csv",row.names=F)
quadgram <- read.csv("quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"quadgram.RData")



