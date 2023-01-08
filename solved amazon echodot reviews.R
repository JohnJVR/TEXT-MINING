library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)



# Amazon Reviews for Echo Dot 3rd Generation #############################
amaz <- "https://www.amazon.in/Echo-Dot-3rd-Gen-improved/product-reviews/B07PFFMP9P/ref=cm_cr_arp_d_viewpnt_rgt?ie=UTF8&reviewerType=all_reviews&filterByStar=critical&pageNumber=1"
echo_review <- NULL # All the Echo product review will store in echo_review
for (i in 1:50){
    echo <- read_html(as.character(paste(amaz,i,sep="="))) # reading as a character
    ed <- echo %>% # pipe connection to speed up the process
    html_nodes(".review-text") %>% # only review data will be retrived from the web page
    html_text() # reading html nodes in to text data
    echo_review <- c(echo_review,ed) # storing in playstation_reviews
}

write.table(echo_review,"echodot.txt",row.names = FALSE)# storing in to echodot.txt
getwd() # E:/excelr/excelr/data science/assignment/text minning/solved
View(echo_review)


# Sentimental Analysis
setwd("E:/excelr/excelr/data science/assignment/text minning/solved")
txt <- read.delim(file.choose())
View(txt)
str(txt)
class(txt)

# Corpus - Collection od documents or reviews
library(tm)
txtcorpus <- txt[-1,]
head(txtcorpus)
class(txtcorpus)

echodot <- Corpus(VectorSource(txtcorpus))
class(echodot)
inspect(echodot[1:10]) # Reading 1st 10 reviews

 # Data cleansing
echodot1 <- tm_map(echodot,tolower)
inspect(echodot1[1:10])

echodot1 <- tm_map(echodot1,removePunctuation)
inspect(echodot1[1:10])

echodot1 <- tm_map(echodot1, removeNumbers)
inspect(echodot1[1:10])

echodot1 <- tm_map(echodot1, removeWords, stopwords('english'))
inspect(echodot1[1:10])

echodot1 <- tm_map(echodot1,stripWhitespace)
inspect(echodot1[1:10])

removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
echodot1 <- tm_map(echodot1, content_transformer(removeURL))
inspect(echodot1[1:10])

#Term Document Matrix
# converting unstructured data to structured format using TDM
tdm <- TermDocumentMatrix(echodot1) # 344 terms are there in the matrix
tdm
dtm <- t(tdm)

tdm <- as.matrix(tdm)
tdm[2:20, 2:10] # subset of tdm
tdm[90:100, 1:20]

View(tdm)

w <- rowSums(tdm) 
w

w_sub <- subset(w, w >= 50) # terms that are use more than 50 times
w_sub
barplot(w_sub, las=2, col = rainbow(25))

# Term Echo repeats most documents
echodot1 <- tm_map(echodot1, removeWords, c('echo','amazon')) # removing product name and amazon website name
echodot1 <- tm_map(echodot1, stripWhitespace)
inspect(echodot1[1:10])

tdm <- TermDocumentMatrix(echodot1)
tdm


tdm <- as.matrix(tdm)
tdm[2:20, 2:20]

tdm <- as.matrix(tdm)
tdm[90:100, 1:20]
# Word cloud
library(wordcloud)
w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
w_sub1
#set.seed(234)
wordcloud(words = names(w_sub), freq = w_sub)
wordcloud(words = names(w_sub1), freq = w_sub1)

wordcloud(words = names(w_sub), freq = w_sub, random.order = F, colors = rainbow(25), scale=c(5,2), rot.per = 0.4)
wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(25), scale=c(5,2), rot.per = 0.4)


w_small <- subset(w, w >= 10) 
w_small

barplot(w_small, las=2, col = rainbow(30))

library(wordcloud2)

w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.5, shape='circle')

wordcloud2(w1, size=0.5, shape = 'triangle')

# Google, understand, playing, recognition occurs most of the time in the reviews.

# Sentiment Analysis for IMDB_Reviews.

library(syuzhet)
library(topicmodels)

rowTotals <- apply(dtm, 1, sum)
dtm.new   <- dtm[rowTotals > 0, ]
lda <- LDA(dtm.new, 10)
term <- terms(lda, 20)
term
tops <- terms(lda)
tb <- table(names(tops), unlist(tops))
tb <- as.data.frame.matrix(tb)
View(tb)
cls <- hclust(dist(tb), method = 'ward.D2') #ward is absolute distance
par(family = "HiraKakuProN-W3")
plot(cls)

# Sentimental Analysis
txt = read.delim2(file.choose())
x <- iconv(txt[-1,], "UTF-8")

s <- get_nrc_sentiment(x)
s

x[10]
get_nrc_sentiment('supported')

# Bar plot for Amazon echodot reviews

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'amazon echodot3 reviews')

wordcloud2(w1, size=0.5, shape='circle')

wordcloud2(w1, size=0.5, shape = 'triangle')

# According to the reviews customer who bought the product is happy about the product
