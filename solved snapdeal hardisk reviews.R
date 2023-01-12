library(rvest)
library(XML)
library(magrittr)


# Snapdeal reviews #############################
surl_1 <- "https://www.snapdeal.com/product/wd-elements-25-inch-1/131655018/reviews?page="
surl_2 <- "2&sortBy=HELPFUL"
snapdeal_reviews <- NULL
for (i in 1:50){
  surl <- read_html(as.character(paste(surl_1,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"hardisk.txt",row.names = FALSE)
getwd() # E:/excelr/excelr/data science/assignment/text minning/solved

# Sentimental Analysis
setwd("E:/excelr/excelr/data science/assignment/text minning/solved")
snap <- read.delim(file.choose())
View(snap)
str(snap)
class(snap)

library(tm)
snapcorpus <- snap[-1,]
head(snapcorpus)
class(snapcorpus)

disk <- Corpus(VectorSource(snapcorpus))
class(disk)
inspect(disk[1:10])

# Data Cleansing
disk1 <- tm_map(disk,tolower)
inspect(disk1[1:10])

disk1 <- tm_map(disk1, removePunctuation)
inspect(disk1[1:10])

disk1 <- tm_map(disk1,removeNumbers)
inspect(disk1[1:10])

disk1 <- tm_map(disk1,removeWords,stopwords('english'))
inspect(disk1[1:10])

disk1 <- tm_map(disk1,stripWhitespace)
inspect(disk1[1:10])

#Term Document Matrix
# converting unstructured data to structured format using TDM
tdm <- TermDocumentMatrix(disk1) 
tdm
dtm <- t(tdm)

tdm <- as.matrix(tdm)
tdm[2:20, 2:10] # subset of tdm

View(tdm)

w <- rowSums(tdm) 
w

w_sub <- subset(w, w >= 25) # terms that are use more than 50 times
w_sub
barplot(w_sub, las=2, col = rainbow(25))

library(wordcloud)
w_sub <- sort(rowSums(tdm), decreasing = TRUE)
w_sub
#set.seed(234)
wordcloud(words = names(w_sub), freq = w_sub)
wordcloud(words = names(w_sub), freq = w_sub, random.order = F, colors = rainbow(25), scale=c(5,2), rot.per = 0.4)


w_small <- subset(w, w >= 10) # Reducing term count from 25 to 10
w_small

barplot(w_small, las=2, col = rainbow(30))

library(wordcloud2)

w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.5, shape='circle')

wordcloud2(w1, size=0.5, shape = 'triangle')

# Product, good, excellent, working so customers who are bought are more happy than regreting of buying.

# Sentiment Analysis for Snapdeal Hardisk reviews

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
get_nrc_sentiment('Excellent')

# Bar plot for Snapdeal Hardisk reviews

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Snapdeal Hardisk reviews')

# According to the reviews customer who bought the product is happy and positive about the product
