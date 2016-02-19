library(stringr)

Keyword <- "大數據"
# Keyword <- iconv(Keyword,"UTF-8","UTF-8")
Keyword <- iconv(Keyword,"big5","UTF-8")
Keyword <- URLencode(Keyword)
URL <- sprintf("http://zh.wikipedia.org/w/api.php?format=xmlfm&action=query&uselang=zh-tw&titles=%s&prop=revisions&rvprop=content&redirects",Keyword)

Content <- readLines(URL, warn = F)
Content <- iconv(Content,"UTF-8","UTF-8")

temp <- str_extract(Content,"\\[\\[.*\\]\\]")
temp <- temp[!is.na(temp)]
temp <- unlist(regmatches(temp, gregexpr("[[?<\\[].*?[?\\]]]", temp, perl=T)))
Dele <- grep(":|=",temp)

temp <- temp[-Dele]


# if(length(Dele)!=0){
#   temp <- temp[-grep(":|=",temp)]
# }

temp <- gsub("\\[|\\]","",temp)
temp <- gsub("\\|.*","",temp)
temp <- unique(temp)


#-------------------參考---------------------

#先安裝套件
# install.packages("igraph")
# install.packages("stringr")

#建立Function
WikiLinks <- function(Keyword){
  require(igraph)
  require(stringr)
  
  #big5處理
  
  if(is.na(iconv(Keyword,"big5","UTF-8"))){
    Keyword <- iconv(Keyword,"UTF-8","UTF-8")
  }else{
    Keyword <- iconv(Keyword,"big5","UTF-8")
  }
  
  while(length(Keyword)!=0){
    Keyword <- URLencode(Keyword)
    URL <- sprintf("http://zh.wikipedia.org/w/api.php?format=xmlfm&action=query&uselang=zh-tw&titles=%s&prop=revisions&rvprop=content&redirects",Keyword)
    Content <- readLines(URL,warn=F)
    Content <- iconv(Content,"UTF-8","UTF-8")
    
    temp <- str_extract(Content,"\\[\\[.*\\]\\]")
    temp <- temp[!is.na(temp)]
    temp <- unlist(regmatches(temp, gregexpr("[[?<\\[].*?[?\\]]]", temp, perl=T)))
    Dele <- grep(":|=",temp)
    if(length(Dele)!=0){
      temp <- temp[-grep(":|=",temp)]
    }
    temp <- gsub("\\[|\\]","",temp)
    temp <- gsub("\\|.*","",temp)
    temp <- unique(temp)
    
    K <- c()
    for(i in 1:length(Content))
      K <- paste(K, Content[i])
    Content <- K;rm(K)
    
    temp <- temp[order(sapply(str_locate_all(Content,temp),length),decreasing = T)]
    
    if(length(temp)>10){
      temp <- temp[1:10]
    }
    
    # 視覺化  
    Keyword <- iconv(URLdecode(Keyword),"UTF-8","UTF-8")
    edgelist <- cbind(rep(Keyword,length(temp)),temp)
    g <- graph.edgelist(edgelist,directed=F)
    V(g)$size <- rep(50,length(V(g)))
    plot(g, layout=layout.kamada.kawai)
    
    print(temp)
    cat("想連結到哪個關鍵字呢？(結束請按0)")
    tmp <- readLines(n=1)
    while (as.integer(tmp) < 0 | as.integer(tmp) > 10) {
      cat(paste0("請輸入", 1, "到", length(temp), "(結束請按0)"))
      tmp <- readLines(n=1)
    }
    
    Keyword <- temp[as.integer(tmp)]
    
  }
}

#執行Function-- 參數為關鍵字字串
WikiLinks("MERS")
