setwd("/cloud/project/R-Intro/survey")
dir()
file.rename(list.files(pattern = ".csv"), 
            paste0("group", 1:7, ".csv"))

file <- paste0("group", 1:7, ".csv")

data <- NULL
for (i in 1:length(file)) {
  data <- rbind(data, read.csv(file[i],header = TRUE, 
                      stringsAsFactors = TRUE, 
                      fileEncoding = "BIG5"))
}

data[, 1] <- as.numeric(gsub(data[, 1], pattern = "零", replacement = "0"))
data <- data[order(data[, 1]), ]
data$A1 <- factor(data$A1, levels = 1:8,
               labels = c("Apple", "Hㄒㄈ", "Samsung", "Sony", "LG", "ASUS", "小米", "其他"))
data$B1 <- factor(data$B1, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
data$B2 <- factor(data$B2, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
data$B3 <- factor(data$B3, levels = 1:6,
               labels = c("中華電信", "台灣大哥大", "遠傳", "亞太", "台灣之星", "其他"))
data$B4 <- factor(data$B4, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
data$B5 <- factor(data$B5, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
data$B6 <- factor(data$B6, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
data$C2 <- factor(data$C2, levels = 1:2,
               labels = c("男性", "女性"))
data$C3 <- factor(data$C3, levels = 1:5,
               labels = c("台北市", "新北市", "基隆市", "桃園市", "其他"))
data$C4 <- factor(data$C4, levels = 1:9,
               labels = c("軍公教", "金融業", "科技業", "服務業", "製造業", "廣告業", "學生", "家管", "其他"))
colnames(data) <- c("ID", "手機品牌", "購買年", "購買月", "手機價格", "月租費", paste0("B", 1:6), "出生年", "出生月", "性別", "目前居住城市", "目前職業")
rownames(data) <- NULL
View(data)
data <- as.data.frame(data)

#' write.csv(data, file = "Merged.csv", row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")

index <- which(complete.cases(data) != TRUE)
#' [1] 501 507 621 710 756

data[index, c("手機品牌", "手機價格", "月租費", "目前居住城市")]
#' 缺失的資料不影響手機品牌與手機價格比較

x <- data

#' Plot
library(RColorBrewer)
myPalette <- brewer.pal(8, "Set3") 

par(mai = c(.4, .4, .4, .4))
layout(matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE))

##' 手機價格
summary(x$手機價格)
hist(x$手機價格, breaks = seq(0, 55000, 5000), main = "手機價格與頻度分布圖", 
     ylim = c(0, 200), xlim = c(0, 60000), xlab = "", col = myPalette[7], 
     ylab = "人數")

##' 月租費
summary(x$月租費)
hist(x$月租費, breaks = seq(0, 3000, 500), main = "月租費與頻度分布圖", 
     col = myPalette[8], ylim = c(0, 500), xlab = "NTD", ylab = "人數")

##' 手機價格對月租費的相關性
cor(x$月租費, x$手機價格)
plot(x$手機價格, x$月租費, col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=1.3, 
     xlab = "手機價格", ylab = "月租費", main = "手機價格對月租費的相關性", 
     sub = paste("相關係數 =", cor(x$月租費, x$手機價格)))

par(mai = c(.4, .4, .4, .4))
layout(matrix(c(1, 2), ncol = 2, byrow = TRUE))

##' 手機品牌佔比
nrow(x) == sum(table(x$手機品牌))
pie(table(x[, "手機品牌", drop = FALSE])/nrow(x), 
    col = myPalette, border = "white", main = "手機品牌佔比")

##' 問卷人口分布
table(x$目前居住城市)
barplot(table(x$目前居住城市), col = myPalette, border = "white", 
        main = "問卷人口分布", ylim = c(0, 400))
