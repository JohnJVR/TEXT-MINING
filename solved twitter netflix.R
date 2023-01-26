library("twitteR")
library("ROAuth") # R aunthenticating
cred <- OAuthFactory$new(consumerKey='43hHJkZa4snEM8vbikYur2C0V', # Consumer Key (API Key)
                         consumerSecret='T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata") # saving twitter authentication

getwd() # "E:/excelr/excelr/data science/assignment/text minning/solve"

load("twitter authentication.Rdata")

library(base64enc)

library(httpuv)

setup_twitter_oauth("43hHJkZa4snEM8vbikYur2C0V", # Consumer Key (API Key)
                    "T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM", #Consumer Secret (API Secret)
                    "529590041-AJNiB9OMFcaZuCqzoN99fH4pyfUN8uJ9EXDBwY5B",  # Access Token
                    "frz56qjS1TXT1jmBcIWEmXGFLXQu1X3gUswyvMEdCMOfj")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('netflix', n = 2500, includeRts = TRUE) # n is number of tweets depends on preson interest

TweetsDF <- twListToDF(Tweets) # Converting twitter list to data frame
View(TweetsDF)
write.csv(TweetsDF, "netflix.csv", row.names = FALSE)
getwd()

# Reading data
txt <- read.csv(file.choose())

str(txt)
View(txt)
class(txt)
dim(txt)

txt <- as.data.frame(txt)

#x <- as.character(txt)
x <- as.character(txt$text)
View(x)
length(x)

# Corpus
#install.packages("tm")
library(tm) 
# cleaning and cleansing is possible in tm package

x <- Corpus(VectorSource(x)) # corpus-combination of all dacuments is called corpus

inspect(x[1])
inspect(x[300])

#Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

inspect(x1[300])
x1 <- tm_map(x1, removeNumbers)
inspect(x1[300])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# Remove URL's from corpus
 removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
 x1 <- tm_map(x1, content_transformer(removeURL))
 inspect(x1[1])

#striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm)

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]

tdm[90:100, 1:20]
# View(tdm)

inspect(x[3])


# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 25)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term mcdonalds repeats in all most all documents
x1 <- tm_map(x1, removeWords, 'can')
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm # sparcity is empty

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

tdm <- as.matrix(tdm)
tdm[90:100, 1:20]

## Word cloud
#install.packages("wordcloud")
library(wordcloud)
w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
w_sub1
#set.seed(222)
wordcloud(words = names(w_sub), freq = w_sub)
wordcloud(words = names(w_sub1), freq = w_sub1)

wordcloud(words = names(w_sub), freq = w_sub, random.order = F, colors = rainbow(50), scale=c(5,2), rot.per = 0.4)

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))

library(wordcloud2)

w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=1, shape='circle')

wordcloud2(w1, size=1, shape = 'triangle')

# Viewers start liking netflix

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

# Barplot of Analysis

txt = read.csv(file.choose())

txt <- as.data.frame(txt)

x <- as.character(txt$text)
x

x1 <- iconv(x, "UTF-8")

s <- get_nrc_sentiment(x1)
s

x[25]
get_nrc_sentiment('good')

# Bar plot forTwitter's Netflix review

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Netflix Analysis')

wordcloud2(w1, size=1, shape='circle')

wordcloud2(w1, size=1, shape = 'triangle')

# As per analysis viewers likes netflix applocation to watch their favourite shows
