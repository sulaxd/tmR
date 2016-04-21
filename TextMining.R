#' ---
#' title: "文字資料探勘實作"
#' author: "Andrew Tang"
#' output:
#'   html_document:
#'     highlight: tango
#'     number_sections: yes
#'     theme: united
#'     toc: yes
#' ---
#' 
## ----R語言快速入門, child='GettingStarted.Rmd'---------------------------

#' 
#' # R語言快速入門
#' 
## ---- include=FALSE, , message=FALSE-------------------------------------
library(ngram)
help_console <- function(topic, format=c("text", "html", "latex", "Rd"),
                         lines=NULL, before=NULL, after=NULL) {  
  format=match.arg(format)
  if (!is.character(topic)) topic <- deparse(substitute(topic))
  helpfile = utils:::.getHelpFile(help(topic))

  hs <- capture.output(switch(format, 
                              text=tools:::Rd2txt(helpfile),
                              html=tools:::Rd2HTML(helpfile),
                              latex=tools:::Rd2latex(helpfile),
                              Rd=tools:::prepare_Rd(helpfile)
                              )
                      )
  hs <- gsub("Process","ngram{ngram}", hs) # 只針對ngram
  hs <- hs[-grep("R.css", hs)]
  hs <- gsub("<(\\/|)code>","`",hs)
  if(!is.null(lines)) hs <- hs[lines]
  hs <- c(before, hs, after)
  cat(hs, sep="\n")
  invisible(hs)
}


#' 
#' ## 套件安裝及載入
#' 
## ---- eval=FALSE---------------------------------------------------------
## install.packages('ngram')
## library(ngram)

#' ![安裝套件](IMG/installPackages.png)
#' 
#' ## 說明文件
#' 
#' Calling help opens a page with general information on the first line such as the name of the package where
#' is the documented function. Then comes a title followed by sections which give detailed information.
#' 
#' * Description: 簡略的函數資訊
#' * Usage: 函數的名稱及其所有的參數名稱和可能對應的預設值(參數)。
#' * Arguments: 每個參數名稱的詳細用法
#' * Details: 詳細的函數資訊
#' * Value: 函數回傳值的物件型別
#' * See Also: 與此函數相關的其他說明頁面
#' * Examples: 可直接執行的範例程式碼
#' 
#' For beginners, it is good to look at the section **Examples**. Generally, it
#' is useful to read carefully the section Arguments. Other sections may be
#' encountered, such as Note, References or Author(s).
#' 
#' Source: [R for Beginners](https://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf)
#' 
#' ---
#' 
## ---- eval=FALSE---------------------------------------------------------
## ?ngram
## help(ngram)

#' 
#' ---
#' 
## ---- echo=FALSE, results='asis'-----------------------------------------
help_console(ngram, "html")

#' 
#' ---
#' 
#' ## 運算
#' 
#' * character：字元，用 `''` 包起來，例：`'test'`
#' * numeric：實數
#' * integer：整數值(2L)
#' * complex：複數值(3i+1)
#' * logical：邏輯值(True/False)
#' * 註解符號： #
#' 
## ------------------------------------------------------------------------
1 + 1
5 * 5
2 ^ 8
paste('text', 'mining', sep=' ')
99 > 10
100 == 100

#' 
#' ## 變數與向量
#' * R 在給予變數值時是利用「<-」 (快捷鍵: Alt+「-」)
#' * 變數命名上，大小寫有區別
#' * NA 表示遺缺值，有物件但是裡面沒東西
#' * NULL 物件不存在
#' * 利用 c(...) 建立向量，切記向量元素必須具相同資料型別
#' * x[i]：回傳向量元素所有資訊
#' * x[[i]]：只回傳向量元素的值
## ------------------------------------------------------------------------
(x <- 1)
(y <- 2)
(z <- x + y)
(x <- c(1, 2, 3))
(x <- c("Eric","Jack","Tom"))
(x <- c(Eric=12, Jack=14, Tom=17))
x[1]
x[[1]]
x["Eric"]
x[["Eric"]]

#' 
#' ## 常見函數
#' 
#' * length：計算向量中的元素個數。
#' * sum：將向量所有元素加總。
#' * sort：將向量元素排列，產生排序過的向量。
#' * rank：回傳各向量元素的排序值。
## ------------------------------------------------------------------------
c(3, 4, 2) + c(3, 4, 2)
c(3, 4, 2) * c(3, 4, 2)
(x <- c(3, 4, 2))
length(x)
sum(x)
sort(x)
rank(x)
order(x)


#' 
#' ## 串列
#' 
#' 串列(List)跟向量很相似，但最大的不同在於串列可以包含不同資料型別的資料。
#' 
## ------------------------------------------------------------------------
(x <- list(a = 1, b = TRUE, c = "test", d = c(1, 2, 3)))
x[1]
x[[1]]
x$a
length(x)
names(x)
x[[4]][1]

#' 
#' ## 資料框架
#' 資料框架(Data frame)類似資料表，經常使用於各種資料集，例如：匯入csv、xlsx或讀取資料庫資料等
#' 取值方式 [row, col]
#' 
## ------------------------------------------------------------------------
(name <- c("Eric","Jack","Tom"))
(age <- c("28", "26", "34"))
(gender <- c("Male","Male","Female"))
(data <- data.frame(name, age, gender, stringsAsFactors = F))
data[1,]
data[1, 1]


#' 
#' ## 條件執行及迴圈
#' 如果5大於0，就輸出YES字串，反之輸出NO字串
## ------------------------------------------------------------------------
if (5 > 0) {
   print('YES')
 } else {
   print('NO')
 }


#' 
#' 先預設y為0，x從1到10開始迴圈，每次都將x與y之和覆寫變數y
#' 
## ------------------------------------------------------------------------
y <- 0
for (x in 1:10) {
   y <- x + y
   print(y)
}

#' 
#' ## 字串處理
## ------------------------------------------------------------------------
(文件集 <- c("Is that apple pie I smell?",
         "Julie never missed a ball, a promenade, or a play.",
         "Did the cat get your tongue at the table?"))

# 將文件集中的字串"A"以字串"B"取而代之
gsub("a", "A", 文件集)

# 將字串"TATAT"以"A"為切割點拆開
strsplit("TATAT","A")

# 列出文件集中哪幾個文件包含字串"A"
grep("apple", 文件集)

# 計算字串長度
nchar(文件集)

# 字串"CD"位於字串"ABCDE"的第幾到幾字元 => [3, 4]
library(stringr)
str_locate("ABCDE", "CD")

# 取出字串"ABCDE"的第3到4個字元
substr("ABCDE", 3, 4)


#' 
#' ## 正則表達式(regular expression)
#' * [Aa]     :: A 或 a
#' * [^1-9]   :: not 1:9
#' * [1-9]    :: 1:9
#' * [a-z]    :: a b c ... z
#' * [A-Z]    :: A B C ... Z
#' * [a-zA-Z] :: 所有英文字母
#' * [[:space:]] :: 空白或換行
#' * [[:punct:]] :: 標點符號
#' * {x , y} :: 至少出現x次，至多Y次
#' * * :: {0, } 0到無限次
#' * ? :: {0,1} 0到1次
#' * . :: 任意字元
## ------------------------------------------------------------------------
# 將兩個以上的空白，取代為1個空白
(x <- "Is that   apple  pie I   smell?")
gsub(" {2, }", " ", x) 

# 消除標點符號
gsub("[[:punct:]]", "", "T,E!X$T")

# 括弧的用法
(x <- c("company","companies"))
grep("company|companies",x)
grep("compan(y|ies)",x)

#' 
#' 
#' ## 資料IO (Input & Output)
#' 設定工作目錄(Working Directory)
## ----eval=FALSE----------------------------------------------------------
## getwd()
## setwd("C:/")

#' ![以UI設定工作目錄](IMG/WorkingDirectory.png)
#' 
#' ***
#' 讀取csv檔
## ----eval=FALSE----------------------------------------------------------
## read.csv("CSV_input.csv")
## write.csv(tmp, "CSV_output.csv", row.names=F)

#' 

#' 
#' ## install the necessary packages.
## ----eval=FALSE----------------------------------------------------------
## install.packages("ngram")
## install.packages("jiebaR")
## install.packages("SnowballC")
## install.packages("tm")
## install.packages("qdap")
## install.packages("wordcloud")
## install.packages("devtools")
## devtools::install_github("jbkunst/d3wordcloud")
## devtools::install_github('adymimos/rWordCloud')

#' 
#' # 文字資料探勘簡介
#' 
#' 資料探勘(Data Mining)與文字探勘(Text Mining)關係緊密，相較於前者顯著的結構化，後者長短不一、沒有規律，且尚有現今生活中隨口說出來的字彙，或是從社群網站、BBS衍生出的用語，隨著使用者每天更新及增減而無法事先定義。 (Source: [Text Mining技術淺談/李慶堂](http://www.cc.ntu.edu.tw/chinese/epaper/0031/20141220_3101.html))
#' 
#' * 關鍵的差別：
#'     + 傳統：結構性資料
#'     + 文字：通常以非(半)結構化方式儲存
#' 
#' * 非結構化資料無法套用在傳統資料探勘的演算法計算
#' * 整合不同資訊領域的技術：
#'   資料探勘、資訊萃取、統計學、自然語言處理、機器學習、情感分析、自動分類、語意分析、…
#' 
#' ## 文字探勘流程
#' ![文字探勘流程](IMG/flowOfTextMining.png)
#' 
#' # 文件記號化(Tokenization)
#' 
#' ## 文件記號化簡介
#' Token記號是指在語言分析上有意義的要素，例如：字或詞組，故tokenization是將文章斷成有意義的單元，稱之為記號化。Tokenize可以是斷句、斷字、或是斷詞組，所以端視token是那種層級的要素。
#' 
#' ## Tokenization example for English
#' 
#' Assign the strings to objects that from wiki.apple 1 to 4
## ------------------------------------------------------------------------
wiki.apple1 <- "Apple Inc. is an American multinational technology company headquartered in Cupertino, California, that designs, develops, and sells consumer electronics, computer software, and online services. Its hardware products include the iPhone smartphone, the iPad tablet computer, the Mac personal computer, the iPod portable media player, and the Apple Watch smartwatch. Apple's consumer software includes the OS X and iOS operating systems, the iTunes media player, the Safari web browser, and the iLife and iWork creativity and productivity suites. Its online services include the iTunes Store, the iOS App Store and Mac App Store, and iCloud."

wiki.apple2 <- "Apple was founded by Steve Jobs, Steve Wozniak, and Ronald Wayne on April 1, 1976, to develop and sell personal computers.[5] It was incorporated as Apple Computer, Inc. on January 3, 1977, and was renamed as Apple Inc. on January 9, 2007, to reflect its shifted focus toward consumer electronics. Apple (NASDAQ: AAPL) joined the Dow Jones Industrial Average on March 19, 2015.[6]"

wiki.apple3 <- "Apple is the world's largest information technology company by revenue, the world's largest technology company by total assets,[7] and the world's second-largest mobile phone manufacturer.[8] On November 25, 2014, in addition to being the largest publicly traded corporation in the world by market capitalization, Apple became the first U.S. company to be valued at over US$700 billion.[9] The company employs 115,000 permanent full-time employees as of July 2015[4] and maintains 453 retail stores in sixteen countries as of March 2015;[1] it operates the online Apple Store and iTunes Store, the latter of which is the world's largest music retailer."

wiki.apple4 <- "Apple's worldwide annual revenue totaled $233 billion for the fiscal year ending in September 2015.[3] The company enjoys a high level of brand loyalty and, according to the 2014 edition of the Interbrand Best Global Brands report, is the world's most valuable brand with a valuation of $118.9 billion.[10] By the end of 2014, the corporation continued to receive significant criticism regarding the labor practices of its contractors and its environmental and business practices, including the origins of source materials."


#' ---
#' 
#' Remove the punctuation and numbers.
## ------------------------------------------------------------------------
wiki.apple1 <- gsub("[[:punct:]]","",wiki.apple1)
wiki.apple2 <- gsub("[[:punct:]]","",wiki.apple2)
wiki.apple3 <- gsub("[[:punct:]]","",wiki.apple3)
wiki.apple4 <- gsub("[[:punct:]]","",wiki.apple4)

wiki.apple1 <- gsub("[0-9]","",wiki.apple1)
wiki.apple2 <- gsub("[0-9]","",wiki.apple2)
wiki.apple3 <- gsub("[0-9]","",wiki.apple3)
wiki.apple4 <- gsub("[0-9]","",wiki.apple4)

#' ---
#' 
## ------------------------------------------------------------------------
wiki.apple1
wiki.apple2
wiki.apple3
wiki.apple4

#' ---
#' 
#' It's not good idea that did every same action 4 times. <br>
#' Put it in a ***Vector***. <br>
## ------------------------------------------------------------------------
wiki.apple.vec <- c(wiki.apple1,
                    wiki.apple2,
                    wiki.apple3,
                    wiki.apple4)


## ---- echo=FALSE---------------------------------------------------------
print(wiki.apple.vec)

#' 
#' ---
#' 
#' Remove the surplus white-space and convert strings to lowercase.
## ------------------------------------------------------------------------
wiki.apple.vec <- gsub(" {2,}"," ",wiki.apple.vec)
wiki.apple.vec <- tolower(wiki.apple.vec)

#' ---
#' 
#' Split the strings with white-space.
#' Notice: After strsplit, the class(類型) of output is a list.
## ------------------------------------------------------------------------
wiki.apple.list <- strsplit(wiki.apple.vec, " ")

## ---- echo=FALSE---------------------------------------------------------
wiki.apple.list

#' ---
#' 
#' ## ngram
#' Tokenization where n-grams are extracted is also useful. N-grams are sequences of words. So a 2-gram would be two words together.
## ------------------------------------------------------------------------
library(ngram)
ng <- ngram(wiki.apple1, n=2)
get.ngrams(ng)

#' ---
#' 
#' ## Tokenization example for Chinese
#' 自然語言處理的其中一個重要環節就是中文斷詞的處理，比起英文斷詞，中文斷詞在先天上就比較難處理，比如電 <br>
#' 腦要怎麼知道「全台大停電」要斷詞成「全台 / 大 / 停電」呢？如果是英文「Power outage all over Taiwan」，<br>
#' 就可以直接用空白斷成「Power / outage / all / over / Taiwan」(截錄至： http://blog.fukuball.com/)
#' 
#' ---
#' 
#' 先將字串資料賦值至new.zhTW與news.zhCN
## ------------------------------------------------------------------------
news.zhTW <- "美國最新飲食指南首度訂出健康成人咖啡攝取量，建議有喝咖啡習慣的人可喝黑咖啡，或在咖啡中添加低脂牛奶，但不建議額外添加糖或奶精。至於平日沒有喝咖啡習慣的人，是否需要現在開始喝咖啡？衛福部國健署昨天表示「不建議」。"

news.zhCN <- "凤凰科技讯 北京时间2月19日消息，据《今日美国》网络版报道，苹果刚刚推出了新的以旧换新计划，以便于部分用户使用他们的老款iPhone置换新机型。而且，苹果目前接受的以旧换新手机包括Android手机、Windows Phone手机以及iPhone。"

#' ---
#' 
#' 透過結巴R的套件，以默認的斷詞引擎進行斷詞。
## ------------------------------------------------------------------------
library(jiebaR)
mixseg = worker()
segment(news.zhTW, mixseg)
segment(news.zhCN, mixseg)

#' ---
#' 
#' 以標注詞性的斷詞引擎進行斷詞。<br>
#' 詞性標注對照 https://gist.github.com/luw2007/6016931
## ------------------------------------------------------------------------
tagseg = worker('tag')
segment(news.zhTW, tagseg)
segment(news.zhCN, tagseg)

#' ---
#' 
#' # 文字資料處理
#' ## Stemming and Stem Completion example with R
#' + 使用SnowballC套件來處理Stemming
#' + 使用tm套件處理Completion
#' 
## ------------------------------------------------------------------------
# library(SnowballC)
library(tm)
# test <- wordStem(wiki.apple.list[[4]], language = "english")
test <- stemDocument(wiki.apple.list[[4]], language = "english")
(test <- stemCompletion(test,dictionary=Corpus(VectorSource(wiki.apple.list))))

#' ---
#' 
#' 
## ------------------------------------------------------------------------
# 以迴圈的方式將list中的每個字串向量都stemming及completion
wiki.apple.list.dict <- wiki.apple.list
wiki.apple.list <- lapply(wiki.apple.list, stemDocument, language = "english")
wiki.apple.list <- lapply(wiki.apple.list, function(u)
stemCompletion(u,dictionary=Corpus(VectorSource(wiki.apple.list.dict)), type="shortest"))

#' ---
#' 
#' ### Stem & Completion issue
#' 
#' For stemCompletion(), the default type of completion is "prevalent", which takes the most frequent match as completion (use grep). Although "mining" is much more frequent than "miners" in my text, it still completed "mine" to "miners".
#' 
#' Because "mining" didn't grep by "mine".
## ------------------------------------------------------------------------
(a <- c("mining", "miners", "mining"))
(b <- stemDocument(a))
(d <- stemCompletion(b, dictionary=a))

#' 
#' ### Stem & Completion issue solution
## ---- message=FALSE------------------------------------------------------
library(qdap)
# list to retain and indentifier keys
retain <- c("mining")
(replace <- paste(seq_len(length(retain)), "SPECIAL_WORD", sep="_"))

# sub the words you want to retain with identifier keys
(a <- mgsub(pattern=retain, replacement=replace, a))

# Stem it
(b <- stemDocument(a))

# reverse -> sub the identifier keys with the words you want to retain
(a <- mgsub(pattern=replace, replacement=retain, a))

#' 
#' 
#' ## 中文詞庫擴充
#' 「全台大停電」要斷詞成「全 / 台大 / 停電」 ，還是「全台 / 大 / 停電」，依需求而定。 <br>
#' 以電腦邏輯，若程式自動判斷為前者可能性高，這時則需人工擴充詞庫。
#' 
#' + 透過edit_dict函數，可編輯結巴R的**使用者自定義詞庫**
#' + 透過USERPATH可取得**使用者自定義詞庫**的路徑
#' + 修改完成後，重新載入斷詞引擎
## ------------------------------------------------------------------------
# edit_dict()
USERPATH
# dict.fix <- read.csv(USERPATH, header = F, stringsAsFactors = F)
# write.table(data.frame(c(dict.fix[,1], "全台大")), row.names = F, col.names = F, quote = F, file = USERPATH)
mixseg = worker()
segment("全台大停電", mixseg)

#' ---
#' 
## ----案例實作 - Google Play APP評論擷取, child="GooglePlayScraping.Rmd"----

#' 
#' # 案例實作 - Google Play APP評論擷取
#' 
#' ## 前言
#' 
#' 在iOS和Android手機App市集中，App的評價和評論對App在市集的排序有很大的影響；對於App開發者而言，透過評論確實可掌握使用者的需求，並在產生抱怨前能快速反應避免危機。然而，**每日多達上百篇的評論，透過人力逐篇查看，不止耗費時間，更無法整合性的瞭解使用者的需求與問題。**
#' 
#' ## 關於擷取網頁必要知識
#' 
#' + 在HTTP 1.1的版本中定義了八種 Method (方法)，如下所示：
#'     + OPTIONS
#'     + **GET**
#'     + HEAD
#'     + **POST**
#'     + PUT
#'     + DELETE
#'     + TRACE
#'     + CONNECT
#' 
#' + GET與POST流程：
#' ![Flow of GET & POST](IMG/GET_POST_flow.png)
#' 
#' + GET(明信片)與POST(信)的差別：
#' ![Example of GET & POST](IMG/GET_POST_EX.png)
#' 
#' + 開發人員工具
#'     + 建議使用Chrome
#'     + 滑鼠右鍵 -> 檢查
#' 
#' + XPath
#' ![開發人員工具介面及XPath尋找](IMG/GET_POST_xpath.png)
#' 
#' + GET
#'     + 若為GET則只需網址及XPath即可擷取資料
#'     
#' + POST
#' ![以開發人員工具找到POST的表單格式](IMG/POST_scrapingFlow.png)
#' 
#' + 剛好Play商店GET跟POST都可以練習
#' 
#' ## 擷取Play商店評論：GET
#' 
## ---- message=FALSE------------------------------------------------------
library(XML)
library(RCurl)

# 若windows有中文亂碼問題，可以試試run下面註解的code
# Sys.setlocale(category='LC_ALL', locale='C')
url <- "https://play.google.com/store/apps/details?id=com.facebook.katana&hl=zh-TW"

# https的website會有SSL的認證問題，加上參數ssl.verifypeer=False即可
# windows常有中文亂碼問題，不過有時在資料解析過後就會正常
data <- getURL(url, ssl.verifypeer = FALSE)
data <- htmlParse(data, encoding = "UTF-8")

# apply系列函數可參考 http://rightthewaygeek.blogspot.tw/2013/08/r-1-apply_28.html
(data <- xpathSApply(data,"//div[@class='review-body with-review-wrapper']",xmlValue))
(data <- gsub("完整評論| ","",data))

#' 
#' ## 擷取Play商店評論：POST
#' 
#' + 測試：只抓第六頁
#' 
## ------------------------------------------------------------------------
library(RJSONIO)

  data <- postForm("https://play.google.com/store/getreviews?authuser=0",
                   reviewType = "0", 
                   pageNum = 6,
                   id = "jp.naver.line.android",
                   reviewSortOrder = "4",
                   xhr = "1",
                   token = "ow4xLEVhiBH_o5lzyRTQ5UxOuJE:1439703309614",
                   hl = "zh-TW"
                   ,.opts = list(ssl.verifypeer = FALSE),.encoding="UTF-8")
  
  
  data <- fromJSON(substr(data,7,nchar(data)), encoding="UTF-8")
  data <- htmlParse(data[[1]][[3]],encoding="utf8")
  data <- xpathSApply(data,"//div[@class='review-body with-review-wrapper']", xmlValue)
  (data <- gsub("完整評論| ","",data))


#' 
#' 
#' + 透過迴圈擷取多頁，以下為1至3頁
#' + 擷取所有頁面的方法：
#'     + 透過XPath找到總頁數
#'     + 找出當pageNum超出總頁數時的規則，並設為停止訊號
#' 
## ------------------------------------------------------------------------
library(RJSONIO)

result <- c()
for(i in 1:3){
  print(i)
  data <- postForm("https://play.google.com/store/getreviews?authuser=0",
                   reviewType = "0", 
                   pageNum = i,
                   id = "jp.naver.line.android",
                   reviewSortOrder = "4",
                   xhr = "1",
                   token = "ow4xLEVhiBH_o5lzyRTQ5UxOuJE:1439703309614",
                   hl = "zh-TW"
                   ,.opts = list(ssl.verifypeer = FALSE),.encoding="UTF-8")
  
  
  data <- fromJSON(substr(data,7,nchar(data)), encoding="UTF-8")
  data <- htmlParse(data[[1]][[3]],encoding="utf8")
  data <- xpathSApply(data,"//div[@class='review-body with-review-wrapper']", xmlValue)
  (data <- gsub("完整評論| ","",data))
  result <- c(result, data)
}

print(result)


#' 
#' ---
#' 
#' ## 補充內容
#' 
## ------------------------------------------------------------------------
library(jiebaR)
mixseg = worker()
resultSegmentCorpus <- sapply(result, segment, mixseg)
resultSegmentWords <- do.call(c,resultSegmentCorpus)
tail(sort(table(resultSegmentWords)),50)

#' 
## ------------------------------------------------------------------------
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

for (i in sort(unique(kmeansResult$cluster))) {
  cat(paste("<<Cluster ", i, ">>： \n", sep=""))
  doc.index <- which(kmeansResult$cluster==i)
  print(result[head(doc.index)])
  cat("\n")
}

#' 
#' 
#' 
#' 

#' 
#' 
#' # 詞袋模型(Bag of Words) 與詞頻計算
#' 詞袋模型（bag of words model）是資訊檢索領域中，文件表示法的一種。其將文件中出現的詞彙，想像是放在袋子裡零散而獨立的物件，如此一個袋子代表一篇文件。詞袋模型的重點，不在於這個想像中的袋子，而在於其對待袋子中的詞彙方式，亦即每個詞彙都是獨立的單位，不考慮其相依性。
#' 
#' ![詞袋模型-想像](IMG/bagOfWords.png)
#' 
#' 文件中的詞彙代表空間中的一個維度，而維度與維度之間是獨立的，由文件向量形成的文件詞項矩陣(Document-term matrix)，便於後續的向量計算。
#' 
## ---- echo=FALSE---------------------------------------------------------
wiki.apple.vec

#' 
#' ## 詞頻計算（tm套件應用）
## ------------------------------------------------------------------------
library(tm)
(myCorpus <- Corpus(VectorSource(wiki.apple.vec)))

# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, content_transformer(function(u)removeWords(u,stopwords("english"))))
lapply(myCorpus, content)

# Strip extra whitespace
myCorpus <- tm_map(myCorpus, content_transformer(function(u)stripWhitespace(u)))
lapply(myCorpus, content)

# stemming & completion
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)

# completion (bug patch)
stemCompletion_mod <- function(x,dict=dictCorpus) {
  stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" "))
}

myCorpus <- tm_map(myCorpus, content_transformer(function(u)stemCompletion_mod(u, dict=myCorpusCopy)))

# Building a Term-Document Matrix
# 稀疏矩陣（Sparse matrix），是其元素大部分為零的矩陣
# 稀疏度指的是稀疏矩陣中，零所佔的比例
(myTdm <- DocumentTermMatrix(myCorpus))
(dtm_corpus <- as.matrix(myTdm))

# 每個欄的總合剛好就是整個語料庫的word count
sort(colSums(dtm_corpus))

#' ---
#' 
#' ## TF-IDF
#' TF-IDF的主要思想是：如果某個詞或短語在一篇文章中出現的頻率TF高，並且在其他文章中很少出現， <br>
#' 則認為此詞或者短語具有很好的類別區分能力，適合用來分類。
#' 
#' Term Frequency–Inverse Document Frequency
#' TF-IDF用於評估一個字詞於一篇文件中的重要程度
#' 字詞的重要性隨著它在文件中出現的次數成正比增加，但同時會隨著它在文件集中出現的頻率成反比下降
#' TF-IDF的主要思想是：如果某個詞或短語在*一篇*文章中出現的頻率TF高，並且在其他文章中很少出現，則認為此詞或者短語具有很好的類別區分能力，適合用來分類。
#' 
#' IDF的計算方式：
#' Log( 總文件數／包含該字詞之文件數 )
#' 
#' ## TF-IDF：例子from維基
#' - 假如一篇文件的總詞語數是100個，而詞語「蘋果」出現了3次，那麼「蘋果」一詞在該文件中的詞頻(TF)就是3/100=0.03。
#' - 計算文件頻率 (DF) 的方法是測定有多少份文件出現過「蘋果」一詞，然後除以文件集裡包含的文件總數。
#' - 如果「蘋果」一詞在100份文件出現過，而文件總數是10000份的話，其逆向文件頻率(IDF)就是log10(10000/100)=2。
#' - TF-IDF的分數為TF乘IDF
#'   + 0.03 * 4=0.06
#'   + 對於該文件來說，蘋果一詞的TF-IDF分數為0.06
#' 
#' 
#' *透過結巴R套件即可以做到TF-IDF的計算*
## ------------------------------------------------------------------------
mixseg <- worker()

# 取得IDF並存於工作目錄下
get_idf(wiki.apple.list, path = "idf.txt")
tokenEngine <- worker("keywords", idf = "idf.txt")
# 以文件1為例，計算TFIDF，其中log為natural logarithms
for(i in 1:length(wiki.apple.list)){
  print(vector_keywords(wiki.apple.list[[i]], tokenEngine))
}

#' 
#' 
#' *TF-IDF驗算*
## ------------------------------------------------------------------------
tf <- table(wiki.apple.list[[3]])

idf.case.1 <- log(length(wiki.apple.list)/length(grep("largest",wiki.apple.list)))
tfidf.case.1 <- tf["largest"]*idf.case.1

idf.case.2 <- log(length(wiki.apple.list)/length(grep("world",wiki.apple.list)))
tfidf.case.2 <- tf["world"]*idf.case.2

#' ---
#' 
## ----案例實作 - 知識地圖, child="WikiLinks.Rmd"--------------------------

#' 
#' # 案例實作 - 知識地圖
#' 
#' ## 前言
#' 
#' 概念是透過User輸入的關鍵字於維基百科中尋找吻合頁面，並以輻射狀視覺化列出頁面中較重要的6-10個關鍵字連結(Wiki內部連結)及代表其主題的圖片，技術上不難卻是有趣的概念，因為維基頁面中的關鍵字連結就代表著有知識在裡面！
#' 
#' ![Wiki頁面](IMG/wiki.png)
#' 
#' ## 實作
#' 
## ------------------------------------------------------------------------
library(stringr)

Keyword <- "大數據"

# 若為windows可能需run下面註解的code
# Keyword <- iconv(Keyword,"big5","UTF-8")
Keyword <- URLencode(Keyword)
URL <- sprintf("https://zh.wikipedia.org/w/api.php?format=xmlfm&action=query&uselang=zh-tw&titles=%s&prop=revisions&rvprop=content&redirects",Keyword)

(Content <- readLines(URL, warn = F))
# 若為windows可能需run下面註解的code
# Content <- iconv(Content,"UTF-8","UTF-8")

(temp <- str_extract(Content,"\\[\\[.*\\]\\]"))
(temp <- temp[!is.na(temp)])
(temp <- unlist(regmatches(temp, gregexpr("[[?<\\[].*?[?\\]]]", temp, perl=T))))
(Dele <- grep(":|=",temp))

(temp <- temp[-Dele])


# if(length(Dele)!=0){
#   temp <- temp[-grep(":|=",temp)]
# }

(temp <- gsub("\\[|\\]","",temp))
(temp <- gsub("\\|.*","",temp))
(temp <- unique(temp))


#' 
#' 
#' ## 補充內容(僅供參考)
#' 
## ---- eval=FALSE---------------------------------------------------------
## #先安裝套件
## # install.packages("igraph")
## # install.packages("stringr")
## 
## #建立Function
## WikiLinks <- function(Keyword){
##   require(igraph)
##   require(stringr)
## 
##   #big5處理
## 
##   if(is.na(iconv(Keyword,"big5","UTF-8"))){
##     Keyword <- iconv(Keyword,"UTF-8","UTF-8")
##   }else{
##     Keyword <- iconv(Keyword,"big5","UTF-8")
##   }
## 
##   while(length(Keyword)!=0){
##     Keyword <- URLencode(Keyword)
##     URL <- sprintf("http://zh.wikipedia.org/w/api.php?format=xmlfm&action=query&uselang=zh-tw&titles=%s&prop=revisions&rvprop=content&redirects",Keyword)
##     Content <- readLines(URL,warn=F)
##     Content <- iconv(Content,"UTF-8","UTF-8")
## 
##     temp <- str_extract(Content,"\\[\\[.*\\]\\]")
##     temp <- temp[!is.na(temp)]
##     temp <- unlist(regmatches(temp, gregexpr("[[?<\\[].*?[?\\]]]", temp, perl=T)))
##     Dele <- grep(":|=",temp)
##     if(length(Dele)!=0){
##       temp <- temp[-grep(":|=",temp)]
##     }
##     temp <- gsub("\\[|\\]","",temp)
##     temp <- gsub("\\|.*","",temp)
##     temp <- unique(temp)
## 
##     K <- c()
##     for(i in 1:length(Content))
##       K <- paste(K, Content[i])
##     Content <- K;rm(K)
## 
##     temp <- temp[order(sapply(str_locate_all(Content,temp),length),decreasing = T)]
## 
##     if(length(temp)>10){
##       temp <- temp[1:10]
##     }
## 
##     # 視覺化
##     Keyword <- iconv(URLdecode(Keyword),"UTF-8","UTF-8")
##     edgelist <- cbind(rep(Keyword,length(temp)),temp)
##     g <- graph.edgelist(edgelist,directed=F)
##     V(g)$size <- rep(50,length(V(g)))
##     plot(g, layout=layout.kamada.kawai)
## 
##     print(temp)
##     cat("想連結到哪個關鍵字呢？(結束請按0)")
##     tmp <- readLines(n=1)
##     while (as.integer(tmp) < 0 | as.integer(tmp) > 10) {
##       cat(paste0("請輸入", 1, "到", length(temp), "(結束請按0)"))
##       tmp <- readLines(n=1)
##     }
## 
##     Keyword <- temp[as.integer(tmp)]
## 
##   }
## }
## 
## #執行Function-- 參數為關鍵字字串
## WikiLinks("MERS")

#' 

#' 
#' # 字詞關聯與文字雲
#' 
#' 字詞關聯意味著每個字詞之間的關聯性，通常是計算詞與詞的相關係數(Correlation Coefficient)，介於-1與+1之間。
#' 
#' 概念是出現Text的文章中也同時出現Mining的情況越多，相關係數越高；表示他們的關聯性越強。
#' 
#' + 透過tm套件的findAssocs函數，可以查詢相關性較高的詞
## ------------------------------------------------------------------------
#Associated 
cor(dtm_corpus)[1:10, 1:30]
findAssocs(myTdm, 'steve' , 0.25)
findAssocs(myTdm, 'apple' , 0.25)

#' ---
#' 
#' # 文字雲
#' 標籤雲或文字雲是關鍵詞的視覺化描述，用於彙總用戶生成的標籤或一個網站的文字內容。標籤一般是獨立的辭彙，常常按字母順序排列，其重要程度又能通過改變字體大小或顏色來表現
#' 一般的文字雲較單純，綜合文章的詞頻取前n個排名，依詞頻高低顯示不同大小。若講究多篇文章中，較具代表性的文字雲，則使用TF-IDF概念依重要程度製圖。
#' 
#' *以下主要介紹各種文字雲套件的使用方式*
## ------------------------------------------------------------------------
# 文字雲1
library(wordcloud)
freqs <- colSums(dtm_corpus)
words <- names(freqs)
wordcloud(words,freqs, min.freq = 1)

# 文字雲2
library(rWordCloud)
wiki.apple.wordcount <- table(unlist(wiki.apple.list))
d3Cloud(text = names(wiki.apple.wordcount), size = as.numeric(wiki.apple.wordcount))

# # 文字雲3
# library(d3wordcloud)
# d3wordcloud(names(wiki.apple.wordcount), as.numeric(wiki.apple.wordcount))

#' ---
#' 
## ----案例實作 - 文字雲以台灣股市新聞為例, child="Stock.Rmd"--------------

#' 
#' # 案例實作 - 文字雲以台灣股市新聞為例
#' 
#' 新聞來源：中時電子報(http://money.chinatimes.com/news/news-overview-table.aspx)
#' 
## ------------------------------------------------------------------------
load("NewsContent.RData")
colnames(NewsContent)
library(jiebaR)

NewsContent[1,]

mixseg = worker()

Seg <- function(NEWs){
  tmp <- segment(NEWs, mixseg)
  tmp <- gsub("[0-9]","",tmp)
  tmp <- tmp[nchar(tmp)>1]
  return(tmp)
}

result <- unlist(sapply(NewsContent$Content,Seg))
tail(sort(table(result)), 50)

# Mac加上
par(family="STKaiti")
library(wordcloud)
wordcloud(names(table(result)),table(result), max.words=40)


#詞頻小於300的才顯示
wordcloud(names(table(result)[table(result)<300]),table(result)[table(result)<300], 
          max.words=40,scale=c(2,0.002))


#' 

#' 
#' 
#' # 詞項與文件之集群分析
#' 
#' * 「物以類聚」性質越近的越聚在一起
#' * 將每個觀察值的變數視為m維空間中的座標點
#' * 計算點與點的距離，越近則同質性越高
#' * 一般以歐氏距離(Euclidean distance)測量m維空間的距離
#' 
#' ![集群分析概念圖](IMG/clusterDot.png)
#' 
#' + 集群分析－階層式(Hierarchical) ：
#'     + 凝聚法(Agglomerative)
#'         + 開始時每一個體為一群，然後最近的兩個體合成一群，一次結合使群組越變越少，最後所有個體結合成一群。
#'         + 最近鄰法 (Nearest Neighborhood) - single
#'         + 最遠鄰法 (Furthest Neighborhood) - complete
#'         + 中心法 (Centroid Method)
#'         + 華德最小變異數法 (Ward' s Minimum Variance Method)
#'     + 分離法(Divisive)
#'         + 開始所有個體為一群，然後分成兩群、三群，直到每個體為一群。此法不常用。
#' + 集群分析－分割法(Partitioning)：
#'     + K平均數集群法(K-means clustering)
#' 
#' ---
#' 
#' + 階層式(Hierarchical)分群應用於文字資料
#' + 將**文字**分群
## ------------------------------------------------------------------------
(tdm_corpus <- t(dtm_corpus))
(tdm_corpus <- as.TermDocumentMatrix(tdm_corpus, weight = weightTf))
(tdm_corpus <- removeSparseTerms(tdm_corpus, sparse=0.7))

dist_tdm_corpus <- dist(as.matrix(tdm_corpus))
fit <- hclust(dist_tdm_corpus, method="ward.D")
plot(fit)


#' 
#' 
## ----案例實作 - 集群分析以推特為例, child="Twitter.Rmd"------------------

#' 
## ---- eval=FALSE---------------------------------------------------------
## # install.packages(c("devtools", "rjson", "bit64", "httr"))
## # library(devtools)
## # install_github("twitteR", username="geoffjentry")
## 
## library(twitteR)
## 
## #開始進行資料擷取
## load("rdmTweets.RData")
## length(rdmTweets)
## 
## #將tweets轉換為data frame
## df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
## 
## 
## #語料庫
## library(tm)
## myCorpus <- Corpus(VectorSource(df$text))
## 
## #資料處理：字母小寫化、移除標點符號、數字、網址
## 
## 
## # convert to lower case
## myCorpus <- tm_map(myCorpus, content_transformer(function(u)tolower(u)))
## # remove punctuation
## myCorpus <- tm_map(myCorpus, content_transformer(function(u)removePunctuation(u)))
## #  remove numbers
## myCorpus <- tm_map(myCorpus, content_transformer(function(u)removeNumbers(u)))
## # remove URLs
## removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
## myCorpus <- tm_map(myCorpus, content_transformer(function(u)removeURL(u)))
## # add two extra stop words: "available" and "via"
## myStopwords <- c(stopwords("english"), "available", "via")
## # remove stopwords from corpus
## myCorpus <- tm_map(myCorpus, content_transformer(function(u)removeWords(u,myStopwords)))
## 
## # Strip extra whitespace
## myCorpus <- tm_map(myCorpus, content_transformer(function(u)stripWhitespace(u)))
## 
## #class(myCorpus[[1]]) character => PlainTextDocument,TextDocument
## # myCorpus <- Corpus(VectorSource(myCorpus))
## 
## # keep a copy of corpus to use later as a dictionary for stem completion
## myCorpusCopy <- myCorpus
## # stem words
## myCorpus <- tm_map(myCorpus, stemDocument)
## content(myCorpus[[12]])
## 
## stemCompletion_mod <- function(x,dict=dictCorpus) {
##   stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" "))
## }
## 
## myCorpus <- tm_map(myCorpus, content_transformer(function(u)stemCompletion_mod(u, dict=myCorpusCopy)))
## content(myCorpus[[12]])
## 
## 
## # count frequency of "mining"
## miningCases <- tm_map(myCorpus, content_transformer(function(u)grep(" mining ",u)))
## miningCases <-  which(sapply(miningCases, function(u)length(content(u))==1))
## length(miningCases)
## 
## # count frequency of "miners"
## minerCases <- tm_map(myCorpus, content_transformer(function(u)grep(" miner ",u)))
## minerCases <- which(sapply(minerCases, function(u)length(content(u))==1))
## length(minerCases)
## 
## # replace "miners" with "mining"
## myCorpus <- tm_map(myCorpus, content_transformer(function(u)gsub(" miner "," mining ", u)))
## 
## 
## # Building a Term-Document Matrix
## myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(1,Inf)))
## 
## 
## # look at the frst six terms starting with “r” and tweets numbered 101 to 110.
## idx <- which(dimnames(myTdm)$Terms == "r")
## inspect(myTdm[idx+(0:5),101:110])
## 
## 
## # look at the popular words and the association between words
## findFreqTerms(myTdm, lowfreq=10)
## 
## 
## #amp是什麼？ 查看是哪一篇文章出現了amp：原來是....
## # http://blog.xuite.net/com.110/110/52331451-文字、表情、特殊符號表大全
## ampCases <- tm_map(myCorpus, content_transformer(function(u)grep(" amp ",u)))
## which(sapply(ampCases, function(u)length(content(u)))==1)
## content(myCorpus[[11]])
## rdmTweets[[11]]
## 
## #remove "amp"
## myCorpus <- tm_map(myCorpus, content_transformer(function(u)removeWords(u, "amp")))
## 
## #Re-Building a Term-Document Matrix and check popular words
## myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(1,Inf)))
## findFreqTerms(myTdm, lowfreq=10)
## 
## 
## #visuallization
## termFrequency <- rowSums(as.matrix(myTdm))
## termFrequency <- subset(termFrequency, termFrequency>=10)
## barplot(sort(termFrequency), las=2, col=rainbow(length(termFrequency)))
## 
## 
## #Association
## findAssocs(myTdm, 'r' , 0.25)
## findAssocs(myTdm, 'mining' , 0.25)
## 
## 
## #文字雲
## library(wordcloud)
## m <- as.matrix(myTdm)
## wordFreq <- sort(rowSums(m), decreasing=TRUE)
## grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
## wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)
## 
## #移除差距太大的詞，可讓視覺化的數據更有意義
## m.withoutR <- m[-which(rownames(m)=="r"),]
## wordFreq <- sort(rowSums(m.withoutR), decreasing=TRUE)
## grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
## wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)
## 
## 
## # Clustering Words (字的群集)
## # remove sparse terms
## myTdm2 <- removeSparseTerms(myTdm, sparse=0.95)
## m2 <- as.matrix(myTdm2)
## # cluster terms
## distMatrix <- dist(scale(m2))
## fit <- hclust(distMatrix, method="ward.D")
## plot(fit)
## 
## 
## # cut tree into 10 clusters
## rect.hclust(fit, k=10)
## (groups <- cutree(fit, k=10))
## 
## 
## 
## # transpose the matrix to cluster tweets (文件集群)
## dtm <- t(m2)
## # k-means clustering of tweets
## # 淺顯易懂理論 http://www.dotblogs.com.tw/dragon229/archive/2013/02/04/89919.aspx
## # Gaming source http://etrex.blogspot.tw/2008/05/k-mean-clustering.html
## k <- 8
## kmeansResult <- kmeans(dtm, k)
## # cluster centers
## round(kmeansResult$centers, digits=3)
## 
## # result of cluster
## kmeansResult$cluster
## which(kmeansResult$cluster==1)
## dtm[which(kmeansResult$cluster==1)[4],]
## which(dtm[which(kmeansResult$cluster==1)[4],]!=0)
## 
## 
## for (i in sort(unique(kmeansResult$cluster))) {
##   cat(paste("<<Cluster ", i, ">>： \n", sep=""))
##   doc.index <- which(kmeansResult$cluster==i)
##   print(paste0(substr(sapply(rdmTweets[doc.index],function(u)u$text),1,60), "..."))
##   cat("\n")
## }
## 

#' 

#' 
#' # 參考文獻 
#' 
#' + 從搜尋引擎到文字探勘 (2014) 王建興
#' + http://zh.wikipedia.org/wiki/词干提取
#' + http://cran.r-project.org/web/packages/SnowballC/index.html
#' + http://jackycode.github.io/blog/2014/06/18/text-mining1/
#' + http://www.vikparuchuri.com/blog/natural-language-processing-tutorial/
#' + http://blog.fukuball.com/ru-he-shi-yong-jieba-jie-ba-zhong-wen-fen-ci-cheng-shi/
#' + http://zh.wikipedia.org/wiki/TF-IDF
#' + http://stats.stackexchange.com/questions/78321/term-frequency-inverse-document-frequency-tf-idf-weighting
#' + https://sensblogs.wordpress.com/2011/08/23/quick-reviews-on-object-recognition-and-bag-of-words-bow-models-by-fei-fei-li/
#' + http://zh.wikipedia.org/wiki/标签云
#' + http://www.mcu.edu.tw/department/management/stat/ch_web/etea/SPSS/Applied_Multivariate_Data_Analysis_ch10.pdf
#' + http://terms.naer.edu.tw/detail/1311901/
#' + http://belleaya.pixnet.net/blog/post/31468581-%5B教學%5D-%5B統計%5D-cluster-analysis-集群分析(1)
#' + http://thinktostart.com/category/r-tutorials/
#' + R and Data Mining: Examples and Case Studies
#' + http://joe11051105.gitbooks.io/r_basic/content/index.html
#' + https://www.ptt.cc/bbs/R_Language/M.1364646786.A.452.html
#' + http://blog.toright.com/posts/1203/淺談-http-method：表單中的-get-與-post-有什麼差別？.html
#' 
#' 
#' 
#' 
