# Tokenization-----------------------------------------

# Tokenization example (1) 
# Why do you want to learn about text mining? 
option1 <- "What is text mining?"
option2 <- "I'm not sure."
option3 <- "I like learning interesting skills."
option4 <- "Text mining is a trend in the future."


  
# 去除句點與問號
option1 <- gsub("[.]|[?]","",option1)
option2 <- gsub("[.]|[?]","",option2)
option3 <- gsub("[.]|[?]","",option3)
option4 <- gsub("[.]|[?]","",option4)


# 用空格將個別句子(文件)記號化
token1 <- unlist(strsplit(option1, " "))
token2 <- unlist(strsplit(option2, " "))
token3 <- unlist(strsplit(option3, " "))
token4 <- unlist(strsplit(option4, " "))


# Tokenization example (2)
# 自然語言處理的其中一個重要環節就是中文斷詞的處理，
# 比起英文斷詞，中文斷詞在先天上就比較難處理，比如電
# 腦要怎麼知道「全台大停電」要斷詞成「全台 / 大 / 停電」
# 呢？如果是英文「Power outage all over Taiwan」，就可以
# 直接用空白斷成「Power / outage / all / over / Taiwan」 
# (截錄至： http://blog.fukuball.com/)
library(jiebaR)
mixseg = worker()
segment("你品嚐了夜的巴黎，你踏過下雪的北京", mixseg)
segment("江州市长江大桥参加了长江大桥的通车仪式", mixseg)

# 詞性標注對照 https://gist.github.com/luw2007/6016931
tagseg = worker('tag')
segment("你品嚐了夜的巴黎，你踏過下雪的北京", tagseg)
segment("江州市长江大桥参加了长江大桥的通车仪式", tagseg)

# Stemming example with R
# 在R中可以使用SnowballC這個套件來處理Stemming， 此
# 套件在R中調用C語言，其使用的是Porter‘s word stemming 
# algorithm。目前支援英、法、德、俄及西班牙等15種語言
# 的詞幹提取。
#Stemmming
library(SnowballC)
stem1 <- wordStem(token1, language = "english")
stem2 <- wordStem(token2, language = "english")
stem3 <- wordStem(token3, language = "english")
stem4 <- wordStem(token4, language = "english")


#中文詞庫擴充
# 「全台大停電」要斷詞成「全 / 台大 / 停電」 ，還是
# 「全台 / 大 / 停電」 ，依需求而定。以電腦邏輯，若程式
# 自動判斷為前者可能性高，這時則需人工擴充詞庫。
# •透過edit_dict函數，可編輯結巴R的詞庫，建議使用notepad++編輯
edit_dict()
mixseg = worker()
segment("全台大停電", mixseg)



# Practice (1)、(2)
# Processing and Tokenization
Pr1 <- "Text mining, also referred to as text data mining, roughly equivalent to text analytics, refers to the process of deriving high-quality information from text. High-quality information is typically derived through the devising of patterns and trends through means such as statistical pattern learning."
Pr1 <- gsub("[.]|[?]|[,]","",Pr1)
unlist(strsplit(Pr1, " "))

example <- "文字探勘不同於資料探勘的地方，則在於它的原始輸入資料，都是沒有特定結構的純文字，這些文字的內容，都是用人類的自然語言所寫成的，所以，無法直接套用資料探勘的演算法，來計算出些什麼有意義的東西。"
segment(example, mixseg)

# Stemming
wordStem(unlist(strsplit(Pr1, " ")), language = "english")

# 中文詞性
tagger = worker("tag")
tagger <= example



# ---Google Play案例時間----
# （注意：上面使用過的變數待會還會使用）


library(tm)
# 詞袋模型(Bag of Words) 與詞頻計算
options <- c(option1,option2,option3,option4)
myCorpus <- Corpus(VectorSource(options))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(function(u)tolower(u)))
# remove punctuation
myCorpus <- tm_map(myCorpus, content_transformer(function(u)removePunctuation(u)))
# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument)

# Building a Term-Document Matrix
# 稀疏矩陣（Sparse matrix），是其元素大部分為零的矩陣
myTdm <- DocumentTermMatrix(myCorpus)
dtm_corpus <- as.matrix(myTdm)

# 每個欄的總合剛好就是整個語料庫的word count
sort(colSums(dtm_corpus))

####-----------
# TF-IDF的主要思想是：如果某個詞或短語在一篇文章中出現的頻率
# TF高，並且在其他文章中很少出現，則認為此詞或者短語具有
# 很好的類別區分能力，適合用來分類。
mixseg <- worker()
# 斷詞後文本向量 <- lapply(未斷詞文本向量, function(u)segment(u, mixseg))
options.seged <- lapply(options, function(u)segment(u, mixseg))

# 取得IDF並存於工作目錄下
get_idf(options.seged, path = "idf.txt")

tokenEngine <- worker("keywords", idf = "idf.txt")
# 以文件1為例，計算TFIDF，其中log為natural logarithms
vector_keywords(segment(option1, mixseg), tokenEngine)

# 驗算
# tf <- table(options.seged[[1]])
# 
# idf.mining <- log(length(options.seged)/length(grep("mining",options.seged)))
# tfidf.mining <- tf["mining"]*idf.mining
# 
# idf.text <- log(length(options.seged)/length(grep("text",options.seged)))
# tfidf.text <- tf["text"]*idf.text

#Associated 
cor(dtm_corpus)

# 文字雲1
library(wordcloud)
wordCount <- colSums(dtm_corpus)
words <- names(wordCount)
wordcloud(words,wordCount)

# 文字雲2-1
# install.packages("devtools")
# devtools::install_github('adymimos/rWordCloud')
library(rWordCloud)
content <- c('test test1 test2 hi ho hurray hi hurray ho ho ho ho','ho hi uh ho','test')
label <- c('a1','a2','a3')
d3TextCloud(content = content, label = label)

# 文字雲2-2
text <- c('d3','wordcloud','impressive','experiment','htmlwidgets','myfirstwidget')
size <- c(25,20,13,9,6,1)
df <- data.frame(text,size)
d3Cloud(text = text, size = size)

# 文字雲3
# devtools::install_github("jbkunst/d3wordcloud")
library(d3wordcloud)
words <- c("I", "love", "this", "package", "but", "I", "don't", "like", "use", "wordclouds")
freqs <- sample(seq(length(words)))
d3wordcloud(words, freqs)

# Practice(3)
doc_1 <- "資料處理與分析是當代IT顯學，在大數據當道的時代，讓我們來探討搜尋引擎與文字探勘的應用"
doc_2 <- "文字探勘，也被稱為文本挖掘、文字採礦、智慧型文字分析、文字資料探勘或文字知識發現，一般而言，指的是從非結構化的文字中，萃取出有用的重要資訊或知識"
doc_3 <- "資料探勘(Data Mining)與文字探勘(Text Mining)關係緊密，相較於前者顯著的結構化，後者長短不一、沒有規律，且尚有現今生活中隨口說出來的"
doc_4 <- "與 Data Mining 不同之處，在於 Text Mining 是針對文字進行分析，且文字多 ... TF-IDF 是一種用於資訊檢索與文字探勘的常用加權技術，為一種統計"
doc_5 <- "許多重要的資訊，其重要性不容小覷，使得文字探勘技術成為近年重要的研. 究領域之一。文字探勘（Text Mining）是從半結構化或非結構化的文件當中，. 發掘出文件中"


corpus <- list(doc_1,doc_2,doc_3,doc_4,doc_5)
mixseg = worker()
corpus <- lapply(corpus, function(u)segment(u, mixseg))
tmp <- unique(unlist(corpus))

dtm_corpus <- matrix(0,nrow=length(corpus),ncol=length(tmp),dimnames=list(NULL,tmp))
for(i in 1:length(corpus)){
  count_tmp <- table(match(corpus[[i]],tmp))
  dtm_corpus[i,as.numeric(names(count_tmp))] <- count_tmp
}


sort(colSums(dtm_corpus))

#TF-IDF
tf <- colSums(dtm_corpus)
idf <- log(nrow(dtm_corpus)/colSums(dtm_corpus>0))
tfidf <- tf*idf



#Clustering words
library(tm)
tdm_corpus <- t(dtm_corpus)
tdm_corpus <- tdm_corpus[nchar(rownames(tdm_corpus))!=1,]
tdm_corpus <- as.TermDocumentMatrix(tdm_corpus, weight = weightTf)
tdm_corpus <- removeSparseTerms(tdm_corpus, sparse=0.7)

dist_tdm_corpus <- dist(as.matrix(tdm_corpus))
fit <- hclust(dist_tdm_corpus, method="ward.D")
plot(fit)

###########
