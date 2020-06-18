#' 1. 下載軟體包MASS 並使用其中的數據框Cars93 在1993 年銷售部93 汽車資料。
#' 將其中的汽車類別變數Type 轉換成為table 變數並使用mfcol=c(1,2) 繪圖參數設
#' 定在單張頁面中並排繪製ㄧ張直條圖(barplot) 與另ㄧ張圓餅圖(pie)。
#' 2. 下載軟體包MASS 並使用其中的數據框Cars93 在1993 年銷售部93 汽車資料。
#' 使用兩個耗油量數值變數MPG.city 與MPG.highway 繪製散佈圖，並加上迴歸線
#' 與加註標題。
library(MASS)
library(RColorBrewer)
View(Cars93)
coul <- brewer.pal(8, "Set2")

#' MPG.city
#'   City MPG (miles per US gallon by EPA rating).
#' MPG.highway
#'   Highway MPG.


#' Q1:
Cars93.type <- table(Cars93[, "Type"])
par(mfcol=c(1, 2))
barplot(Cars93.type, col = coul)
pie(Cars93.type, col = coul)


#' Q2:
Cars.lm <- Cars93[c("MPG.city", "MPG.highway")]
MPG.lm <- lm(MPG.city ~ MPG.highway, data = Cars.lm)
MPG.value <- fitted(MPG.lm)
par(mfcol=c(1, 1))
plot(Cars.lm$MPG.highway, Cars.lm$MPG.city, main = "高速公路與城市油耗散佈圖", 
     xlab = "Highway MPG", ylab = "City MPG")
lines(Cars.lm$MPG.highway, MPG.value, col = coul[1])
