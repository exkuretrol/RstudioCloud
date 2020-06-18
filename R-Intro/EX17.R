#' EX17.R
#' 1. 使用 histogram()函數繪製 crabs 數據框的 FL 變數的直方圖；
#' 並使用 sex 因子變數作為條件變數再繪製條狀直方圖，並解說你的到的結果。
#' 2. 使用 qqnorm() 與 qqline() 函數繪製 crabs 數據框的 FL 變數的 QQ 圖，
#' 並解說你所得到的結果；再使用 shapiro.test() 檢測 crabs$FL 變數是否符合常態分配。

library(lattice)
library(MASS)

##' FL = frontal lobe size (mm). 

#' Q1
stopifnot(nrow(crabs) > 30)
histogram(crabs$FL, xlab = "frontal lobe size", ylab = "mm")
histogram(~ FL | sex, data = crabs, xlab = "FL difference between male and female", ylab = "mm")

##' 大樣本, 未知母體標準差 -> 中央極限定理 

#' Q2
qqnorm(crabs$FL, main = "QQ for crabs")
qqline(crabs$FL)
test <- shapiro.test(crabs$FL)
test$method

##' 如果QQ圖的點越接近qqline產生的線, 表示數據越接近常態分配
##' 
##' > test
##' 
##' Shapiro-Wilk normality test
##' 
##' data:  crabs$FL
##' W = 0.99037, p-value = 0.2023
##' 
##' H_0: FL == Normal.dist
##' H_1: FL != Normal.dist
##' significant level alpha = 0.05
##' p-value = 0.2023 > alpha = 0.05, failed to reject H_0
