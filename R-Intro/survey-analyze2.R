setwd("/cloud/project/R-Intro/survey")
dir()
file.rename(list.files(pattern = ".csv"), 
            paste0("group", 1:7, ".csv"))

file <- paste0("group", 1:7, ".csv")

x <- NULL
for (i in 1:length(file)) {
  x <- rbind(x, read.csv(file[i],header = TRUE, 
                         stringsAsFactors = TRUE, 
                         fileEncoding = "BIG5"))
}

x[, 1] <- as.numeric(gsub(x[, 1], pattern = "零", replacement = "0"))

x <- x[order(x[, 1]), ]

rownames(x) <- NULL

x$A1 <- factor(x$A1, levels = 1:8,
               labels = c("Apple", "Hㄒㄈ", "Samsung", "Sony", "LG", "ASUS", "小米", "其他"))
x$B1 <- factor(x$B1, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
x$B2 <- factor(x$B2, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
x$B3 <- factor(x$B3, levels = 1:6,
               labels = c("中華電信", "台灣大哥大", "遠傳", "亞太", "台灣之星", "其他"))
x$B4 <- factor(x$B4, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
x$B5 <- factor(x$B5, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
x$B6 <- factor(x$B6, levels = 1:5,
               labels = c("非常滿意", "滿意", "無意見", "不滿意", "非常不滿意"))
x$C2 <- factor(x$C2, levels = 1:2,
               labels = c("男性", "女性"))
x$C3 <- factor(x$C3, levels = 1:5,
               labels = c("台北市", "新北市", "基隆市", "桃園市", "其他"))
x$C4 <- factor(x$C4, levels = 1:9,
               labels = c("軍公教", "金融業", "科技業", "服務業", "製造業", "廣告業", "學生", "家管", "其他"))
colnames(x) <- c("ID", "手機品牌", "購買年", "購買月", "手機價格", "月租費", paste0("B", 1:6), "出生年", "出生月", "性別", "目前居住城市", "目前職業")
View(x)

write.csv(x, file = "Merged.csv", row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")

index <- which(complete.cases(x) != TRUE)
#' [1] 501 507 621 710 756

data[index, c("手機品牌", "手機價格", "月租費", "目前居住城市")]
#' 缺失的資料不影響手機品牌與手機價格比較

---
library(car)

#' 將所有的資料建立因子與標籤，並作一個簡單的結果測試。
#' 抽取300至400分問卷進行後續整理
#' 將資料依照性質整理、剔除無效問卷加上因子標籤，新建立手機壽命(以月為單位)變數。
#' 對於某因子進行壽命或者手機價格月租費用彙整統計。並加以說明結果。
#' 至少完成三個列聯交乘表。並加以說明結果。
#' 至少繪製四張統計圖。並加以說明結果。
#' 對於手機壽命以某一因子為分類進行常態分配檢測(Shapiro.test QQline等)。並加以說明結果。
#' 最後完成一個小組繳交一個R程式檔以及一個完整的WORD報告檔在期末報告作業   資料夾內，此成績為單獨小組計分。


x[c(index), ]
x.m <- x[x$性別 == "男性", ]
x.f <- x[x$性別 == "女性", ]
sample.x <- x.m[sample(1:nrow(x.m), 200), ]
sample.x <- rbind(sample.x, x.f[sample(1:nrow(x.f), 200), ])
table(sample.x$性別)
View(sample.x)
index <- which(complete.cases(sample.x) != TRUE)
sample.x[index, ]


#' Plot


##' 手機價格
#' summary(sample.x$手機價格)
hist(sample.x$手機價格, breaks = seq(0, 55000, 5000), main = "手機價格與頻度分布圖", 
     ylim = c(0, 100), xlim = c(0, 60000), xlab = "", col = myPalette[7], 
     ylab = "人數")

#' QQ plot
qqnorm(sample.x$手機價格, main = "手機價格 QQ 圖")
qqline(sample.x$手機價格)

#' Shapiro text
x.price.shapiro.test <- shapiro.test(sample.x$手機價格)
stopifnot(x.price.shapiro.test$p.value < .05) #' Alpha = .05
#' H_0: 手機價格 = N
#' H_a: 手機價格 != N
#' because p-value < .05 = alpha, then reject H_0

##' 月租費
summary(sample.x$月租費)
hist(sample.x$月租費, breaks = seq(0, 3000, 500), main = "月租費與頻度分布圖", 
     col = myPalette[8], ylim = c(0, 300), xlab = "NTD", ylab = "人數")

#' QQ plot
qqnorm(sample.x$月租費, main = "月租費 QQ 圖")
qqline(sample.x$月租費)

#' Shapiro text
x.monthly.cost.shapiro.test <- shapiro.test(sample.x$月租費)
stopifnot(x.monthly.cost.shapiro.test$p.value < .05) #' Alpha = .05
#' H_0: 月租費 = N
#' H_a: 月租費 != N
#' because p-value < .05 = alpha, then reject H_0

##' 手機價格對月租費的相關性

cor(sample.x$月租費, sample.x$手機價格)
plot(sample.x$月租費 ~ sample.x$手機價格, col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=1.3, 
     xlab = "手機價格", ylab = "月租費", main = "手機價格對月租費的相關性")
model <- lm(sample.x$月租費 ~ sample.x$手機價格)
model.value <- fitted(model)
lines(sample.x$手機價格, model.value, col = myPalette[5])
text(x = 40000, y = 2750, paste("相關係數 =", cor(sample.x$月租費, sample.x$手機價格)))
Anova(model)

##' 統計至107年12月, 用月份換算手機使用壽命
##' 參考老師的換算方法(107-購買年)*12 + (12-購買月) 換算已使用手機的時間(單位:月份)
conv <- (107-sample.x$購買年)*12 + (12-sample.x$購買月) #' 公式
sample.x <- cbind(sample.x, "手機的壽命(單位:月份)"= conv)

plot(sample.x[, "性別"], sample.x$`手機的壽命(單位:月份)`, xlab = "", 
     ylab = "已使用手機的時間 (單位:月份)", main = "以性別劃分的手機壽命分析", sub = "統計至107年年尾", 
     col = myPalette) #' 繪製盒鬚圖 x 軸必須是因子 (factor)

##' 二維列聯表 (Two Way Table)
#' 性別 vs 職業
library(knitr)

kable(with(table(目前職業, 性別), data = sample.x), format = "html", 
      caption = "目前職業 vs 性別")
kable(with(table(目前居住城市, 性別), data = sample.x), format = "markdown", 
      caption = "目前居住城市 vs 性別")
kable(with(table(手機品牌, 性別), data = sample.x), format = "markdown", 
      caption = "手機品牌 vs 性別")
