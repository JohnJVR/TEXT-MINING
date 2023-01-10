library(rvest)
library(XML)
library(magrittr)

# Joker Movie Review ##

IMDB_reviews_joker <- NULL

aurl <- "https://www.imdb.com/title/tt7286456/reviews?ref_=tt_urv"  # read html as the character
for (i in 1:10)
length(IMDB_reviews_joker)
write.table(IMDB_reviews_joker, "joker2.txt",row.names = FALSE) # Saying rows as false
getwd()
setwd("E:/excelr/excelr/data science/assignment/text minning")

# Reading text as a data

jokertxt <- read.delim(file.choose())

View(jokertxt)
str(jokertxt)
class(jokertxt)
dim(jokertxt)
#x <- as.character(jokertxt)
#str(x)
library(tm)
jokercorpus <- jokertxt[-1,]
View(jokercorpus)
jc <- Corpus(VectorSource(jokercorpus)) # Combination of documents
inspect(jc[2:10]) # reading first 10 reviews

# Data cleansing
jc1 <- tm_map(jc, tolower)
inspect(jc1[2:10])

jc1 <- tm_map(jc1, removePunctuation)
inspect(jc1[2:10])

jc1 <- tm_map(jc1, removeNumbers)
inspect(jc1[2:10])

jc1 <- tm_map(jc1, removeWords, stopwords('english'))
inspect(jc1[2:10])

jc1 <- tm_map(jc1, stripWhitespace)
inspect(jc1[2:10])

removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
jc1 <- tm_map(jc1, content_transformer(removeURL))
inspect(jc1[2:10])

jc1 <- tm_map(jc1, removeWords, c('movie','movies','film','films'))
jc1 <- tm_map(jc1, stripWhitespace)
inspect(jc1[2:10])

# Term Document matrix
# converting unstructured data to structured format using TDM
tdm <- TermDocumentMatrix(jc1) # 856 terms are there in the matrix
tdm
dtm <- t(tdm) # transpose of tdm->dtm(document term matrix)

tdm <- as.matrix(tdm)
tdm[2:20, 2:10] # subset of tdm
tdm[90:100, 1:20]

View(tdm)

w <- rowSums(tdm) 
w

w_sub <- subset(w, w >= 50) # terms that are use more than 50 times
w_sub

barplot(w_sub, las=2, col = rainbow(25))

# Term joker repeats most documents
jc1 <- tm_map(jc1, removeWords, 'joker') # removing movie name
jc1 <- tm_map(jc1, stripWhitespace)

tdm <- TermDocumentMatrix(jc1)
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
wordcloud(words = names(w_sub1), freq = w_sub, random.order = F, colors = rainbow(25), scale=c(5,2), rot.per = 0.4)


w_small <- subset(w, w >= 10) # Reducing term count from 25 to 10
w_small

barplot(w_small, las=2, col = rainbow(30))

library(wordcloud2)

w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.8, shape='circle')

wordcloud2(w1, size=0.8, shape = 'triangle')

# Character, best, performance terms occuring most of the times.


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

txt = readLines(file.choose())
x <- iconv(txt, "UTF-8")

s <- get_nrc_sentiment(x)
s

x[2]
get_nrc_sentiment('masterpiece')

# Bar plot for joker IMDB reviews

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'joker reviews')

wordcloud2(w1, size=0.8, shape='circle')

wordcloud2(w1, size=0.8, shape = 'triangle')

# Most of the viewers like the movie and likes actors performance