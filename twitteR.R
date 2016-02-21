# install.packages(c("devtools", "rjson", "bit64", "httr"))
# library(devtools)
# install_github("twitteR", username="geoffjentry")


library(twitteR)

api_key <- "t2XUFZp5m48HVBk9ercMYQ"
api_secret <- "JyTtExZ2AZorsU2DD9nNmiTSbyiEgLh2kFzi5VMk"
access_token <- "1862882166-noF6LWbXTqO62fS4kaSfXhfYWptX8hXGlYLDQqu"
access_token_secret <- "Sic16jEl1SJS1gzwVTaNFzJeeOMU5dXyF4qBWOvDJys"



setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

searchTwitter("iphone")

#開始進行資料擷取
rdmTweets <- userTimeline("rdatamining", n=200)
# save(rdmTweets, file="rdmTweets.RData")
load("rdmTweets.RData")
length(rdmTweets)

#將tweets轉換為data frame
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))


#語料庫
library(tm)
myCorpus <- Corpus(VectorSource(df$text))

#資料處理：字母小寫化、移除標點符號、數字、網址


# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(function(u)tolower(u)))
# remove punctuation
myCorpus <- tm_map(myCorpus, content_transformer(function(u)removePunctuation(u)))
#  remove numbers
myCorpus <- tm_map(myCorpus, content_transformer(function(u)removeNumbers(u)))
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(function(u)removeURL(u)))
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords("english"), "available", "via")
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, content_transformer(function(u)removeWords(u,myStopwords)))

# Strip extra whitespace
myCorpus <- tm_map(myCorpus, content_transformer(function(u)stripWhitespace(u)))

#class(myCorpus[[1]]) character => PlainTextDocument,TextDocument
# myCorpus <- Corpus(VectorSource(myCorpus))

# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
content(myCorpus[[12]])

stemCompletion_mod <- function(x,dict=dictCorpus) {
  stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" "))
}

myCorpus <- tm_map(myCorpus, content_transformer(function(u)stemCompletion_mod(u, dict=myCorpusCopy)))
content(myCorpus[[12]])


# count frequency of "mining"
miningCases <- tm_map(myCorpus, content_transformer(function(u)grep(" mining ",u)))
miningCases <-  which(sapply(miningCases, function(u)length(content(u))==1))
length(miningCases)

# count frequency of "miners"
minerCases <- tm_map(myCorpus, content_transformer(function(u)grep(" miner ",u)))
minerCases <- which(sapply(minerCases, function(u)length(content(u))==1))
length(minerCases)

# replace "miners" with "mining"
myCorpus <- tm_map(myCorpus, content_transformer(function(u)gsub(" miner "," mining ", u)))


# Building a Term-Document Matrix
myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(1,Inf)))


# look at the frst six terms starting with “r” and tweets numbered 101 to 110.
idx <- which(dimnames(myTdm)$Terms == "r")
inspect(myTdm[idx+(0:5),101:110])


# look at the popular words and the association between words
findFreqTerms(myTdm, lowfreq=10)


#amp是什麼？ 查看是哪一篇文章出現了amp：原來是....
# http://blog.xuite.net/com.110/110/52331451-文字、表情、特殊符號表大全
ampCases <- tm_map(myCorpus, content_transformer(function(u)grep(" amp ",u)))
which(sapply(ampCases, function(u)length(content(u)))==1)
content(myCorpus[[11]])
rdmTweets[[11]]

#remove "amp"
myCorpus <- tm_map(myCorpus, content_transformer(function(u)removeWords(u, "amp")))

#Re-Building a Term-Document Matrix and check popular words
myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(1,Inf)))
findFreqTerms(myTdm, lowfreq=10)


#visuallization
termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
barplot(sort(termFrequency), las=2, col=rainbow(length(termFrequency)))


#Association
findAssocs(myTdm, 'r' , 0.25)
findAssocs(myTdm, 'mining' , 0.25)


#文字雲
library(wordcloud)
m <- as.matrix(myTdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)

#移除差距太大的詞，可讓視覺化的數據更有意義
m.withoutR <- m[-which(rownames(m)=="r"),]
wordFreq <- sort(rowSums(m.withoutR), decreasing=TRUE)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)


# Clustering Words (字的群集)
# remove sparse terms
myTdm2 <- removeSparseTerms(myTdm, sparse=0.95)
m2 <- as.matrix(myTdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D")
plot(fit)


# cut tree into 10 clusters
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))



# transpose the matrix to cluster tweets (文件集群)
dtm <- t(m2)
# k-means clustering of tweets
# 淺顯易懂理論 http://www.dotblogs.com.tw/dragon229/archive/2013/02/04/89919.aspx
# Gaming source http://etrex.blogspot.tw/2008/05/k-mean-clustering.html
k <- 8
kmeansResult <- kmeans(dtm, k)
# cluster centers
round(kmeansResult$centers, digits=3)

# result of cluster
kmeansResult$cluster
which(kmeansResult$cluster==1)
dtm[which(kmeansResult$cluster==1)[4],]
which(dtm[which(kmeansResult$cluster==1)[4],]!=0)


for (i in sort(unique(kmeansResult$cluster))) {
  cat(paste("<<Cluster ", i, ">>： \n", sep=""))
  doc.index <- which(kmeansResult$cluster==i)
  print(paste0(substr(sapply(rdmTweets[doc.index],function(u)u$text),1,60), "..."))
  cat("\n")
}
