#' 四、實作題：（如果題目有描述不週詳時，請自行假設條件。）
#' 
#' 如果我們要產生如下2 個繪圖的佈局，應該如何使用layout() 函數來達成?
#'   參考：使用3 圖寬度比為1:4:1
#'           行3 圖高度比為5:1:5。

myplot <- function(n) {
  for (i in 1:n) {
    plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    text(1, 1, labels = paste(i), cex = 3)
  }
}

par(mfcol = c(1, 2), mar = c(0, 0, 0, 0))


mat <- matrix(c(1, 1, 0,
                0, 0, 0,
                0, 2, 2), byrow = TRUE, ncol = 3)
layout(mat, widths = c(1, 4, 1), heights = c(5, 1, 5), respect = TRUE)
myplot(2)
