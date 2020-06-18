#定義主要人物之人名
c1 <- c("韋小寶","小桂子","小寶","小白龍","爵爺","韋香主","康熙","小玄子","玄燁","行癡",
        "順治","徐天川","徐三哥","洪安通","洪教主","康親王","傑書","陳近南","陳永華","舵主",
        "沐劍屏","小郡主","方怡","雙兒","蘇荃","建寧公主","曾柔","阿珂","九難","長平公主",
        "吳六奇","大力將軍","雪中鐵丐","鐵丐","海內奇男子","海大富","多隆","張康年","趙齊賢","蕊初",
        "沐劍聲","吳立身","劉一舟","毛東珠","瘦頭陀","胖頭陀", "陸高軒","茅十八","韋春花","楊溢之",
        "晦聰","澄觀","胡逸之","歸辛樹","歸鍾","何惕守","鰲拜","索額圖","鄭克爽","馮錫范",
        "呂留良","黃宗羲","顧炎武","莊允城","吳應熊","葛爾丹","陳圓圓","李自成","吳三桂","平西王",
        "桑結","施琅","柳大洪")
k <- length(c1)

#將人名替代為特殊的英文字串
c3 <- c(paste("CA0",1:9, sep=""), paste("CA",10:k, sep=""))
c2 <-paste(c3,"~",sep="")   

num.hei <- 50   #定義回數  共50回
y <- matrix(0, nrow=k, ncol=num.hei)  
rownames(y)<- c1
colnames(y)<-paste(1:num.hei,"回")
str(y)

for (j in 1:num.hei){
  #' file.use <- paste(paste("/cloud/project/R-Intro/text/test", j, sep=""),".txt",sep="")
  #' path to folder
  file.use <- paste0("/cloud/project/R-Intro/text/test", j, ".txt")
  #' 指定編碼為 Big-5
  x <- scan(file=file.use, what=character(), fileEncoding="Big-5")
  for (i in 1:k) {
  #' 把中文人名替換為英文代號 + ~
    x <- gsub(c1[i], c2[i], x, fixed=TRUE)
  }
  
  #' 以符號 ~ 分段方便計數
  x1 <- strsplit(x, "~")
  
  for (i in 1:k) {
  #' 用迴圈從人名 1 到人名向量的長度 k, 使用 length() 函數計算人物出現次數後,
  #' 匯回矩陣 i 人名, j 回數
    y[i,j] <- length(grep(c3[i],x1[[1]]))
  }
}
#1 人物與回數矩陣
y

#彙整相同人物使用不同的名稱
copy.y <- y  #先做一份拷貝，再進行匯整整理
copy.y[1,]<-copy.y[1,]+copy.y[2,]+copy.y[3,]+copy.y[4,]+copy.y[5,]+copy.y[6,] #彙整韋小寶 						  
copy.y[7,] <- copy.y[7,]+copy.y[8,] + copy.y[9,]   #彙整康熙

copy.y[11,] <- copy.y[11,]+copy.y[10,] #彙整順治
copy.y[12,] <- copy.y[12,]+copy.y[13,] #彙整徐天川
copy.y[14,] <- copy.y[14,]+copy.y[15,] #彙整洪安通
copy.y[16,] <- copy.y[16,]+copy.y[17,] #彙整康親王
copy.y[19,] <- copy.y[18,]+copy.y[19,] + copy.y[20,]  #彙整陳近南
copy.y[21,] <- copy.y[21,]+copy.y[22,] #彙整沐劍屏
copy.y[29,] <- copy.y[29,]+copy.y[30,] 	#彙整長平公主
copy.y[69,] <- copy.y[69,]+copy.y[70,] 	#彙整吳三桂
copy.y[31,] <- copy.y[31,]+copy.y[32,] + copy.y[33,]+copy.y[34,] + copy.y[35,]
#彙整吳六奇							
y22<-copy.y[-c(2:6,8,9,10,13,15,17,18,20,22,30,32:35, 70),] 		
y22    #形成了統一人名後的完整矩陣

#2  人物出現總次數列表
y.row.sum <- apply(y22,1,sum)
#前十名人物與對應的的長條途
y.row.sum.first.10 <- sort(y.row.sum,decreasing = TRUE)[1:10]
y.row.sum.first.10
barplot(y.row.sum.first.10,names.arg=names(y.row.sum.first.10),log="y",
        xlab="人名", ylab="log(次數)",main="前十名人物出現log次數統計表")

#3  人物有出現在哪一回矩陣
y2 <- 1*(y22 & TRUE)
y2.times<- apply(y2,1, sum)
y2.times
y2.times.10<- sort(y2.times, decreasing = TRUE)[1:10]
#出現再50回中次數最多的前十名人物及長條圖
y2.times.10
barplot(y2.times.10,names.arg=names(y2.times.10),
        xlab="人名", ylab="次數",main="前十名人物出現次數統計表")

#人物相關係數矩陣
cor(t(y22))

install.packages("tm")
install.packages("wordcloud")
library(tm)
library(wordcloud)
wordcloud(names(y.row.sum),y.row.sum,scale=c(4,1),
          rot.per=0.1,min.freq=30,colors= rainbow(20))  #文字雲描素人物出現次數

wordcloud(names(y.row.sum),sqrt(y.row.sum),scale=c(4,0.5),
          rot.per=0.1,min.freq=5,colors= rainbow(20))  #文字雲描素人物出現次數取平方根

#七夫人的相關分析
wife.vec<-c1 <- c("沐劍屏","方怡","雙兒","蘇荃","建寧公主","曾柔","阿珂")
#七夫人的總出現次數 與 長條圖
apply(y22[wife.vec, ],1,sum)
barplot(apply(y22[wife.vec, ],1,sum),main="七夫人出現的次數比較圖")
#以出現次數多寡繪製的條圖
barplot(sort(apply(y22[wife.vec, ],1,sum),decreasing=TRUE),main="七夫人出現的次數比較圖")

#七夫人與小寶的相關係數
cor(t(y22[c("韋小寶",wife.vec), ]))
wordcloud(wife.vec,apply(y22[wife.vec, ],1,sum),random.order=FALSE,rot.per=.5,
          colors= rainbow(7))  #七夫人文字雲描素人物出現次數

