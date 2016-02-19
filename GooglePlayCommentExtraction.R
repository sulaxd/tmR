library(XML)
library(RCurl)

# Sys.setlocale(category='LC_ALL', locale='C')
url <- "https://play.google.com/store/apps/details?id=com.facebook.katana&hl=zh-TW"

# https的website會有SSL的認證問題，加上參數ssl.verifypeer=False即可
# windows常有中文亂碼問題，不過有時在資料解析過後就會正常
data <- getURL(url, ssl.verifypeer = FALSE)
data <- htmlParse(data, encoding = "UTF-8")

# apply系列函數可參考 http://rightthewaygeek.blogspot.tw/2013/08/r-1-apply_28.html
data <- xpathSApply(data,"//div[@class='review-body']",xmlValue)
data <- gsub("完整評論","",data)
data <- gsub(" ","",data)





#---------------------------------------------
library(RJSONIO)

result <- c()
for(i in 1:10){
  print(i)
  data <- postForm("https://play.google.com/store/getreviews?authuser=0",
                   reviewType = "0", 
                   pageNum = i,
                   id = "jp.naver.line.android",
                   reviewSortOrder = "4",
                   xhr = "1",
                   token = "ow4xLEVhiBH_o5lzyRTQ5UxOuJE:1439703309614",
                   hl = "en"
                   ,.opts = list(ssl.verifypeer = FALSE),.encoding="UTF-8")
  
  
  data <- fromJSON(substr(data,7,nchar(data)), encoding="UTF-8")
  data <- htmlParse(data[[1]][[3]],encoding="utf8")
  data <- xpathSApply(data,"//div[@class='review-body']")
  data <- lapply(data,function(u)xmlValue(u,trim=T))
  data <- gsub("完整評論","",data)
  result <- c(result, data)
}


library(jiebaR)
mixseg = worker()
resultSegmentCorpus <- sapply(result, segment, mixseg)
resultSegmentWords <- do.call(c,resultSegmentCorpus)
sort(table(resultSegmentWords))


# 稀疏矩陣（Sparse matrix），是其元素大部分為零的矩陣
dtm_corpus <- matrix(0,nrow=length(resultSegmentCorpus),
                     ncol=length(resultSegmentWords),
                     dimnames=list(NULL,resultSegmentWords))
for(i in 1:length(resultSegmentCorpus)){
  count_tmp <- table(match(resultSegmentCorpus[[i]],resultSegmentWords))
  dtm_corpus[i,as.numeric(names(count_tmp))] <- count_tmp
}



# k-means clustering
# 淺顯易懂理論 http://www.dotblogs.com.tw/dragon229/archive/2013/02/04/89919.aspx
# Gaming source http://etrex.blogspot.tw/2008/05/k-mean-clustering.html
k <- 8
kmeansResult <- kmeans(dtm_corpus, k)
# cluster centers
round(kmeansResult$centers, digits=3)

# result of cluster
kmeansResult$cluster
which(kmeansResult$cluster==1)
dtm_corpus[which(kmeansResult$cluster==1)[4],]
which(dtm_corpus[which(kmeansResult$cluster==1)[4],]!=0)


for (i in sort(unique(kmeansResult$cluster))) {
  cat(paste("<<Cluster ", i, ">>： \n", sep=""))
  doc.index <- which(kmeansResult$cluster==i)
  print(result[head(doc.index)])
  cat("\n")
}
