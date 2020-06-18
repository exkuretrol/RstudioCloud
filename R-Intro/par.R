##' Source: [R par 多圖合併 | 龍崗山上的倉鼠](https://kanchengzxdfgcv.blogspot.com/2017/09/r-par.html)

myplot <- function(n) {
  for (i in 1:n) {
    plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    text(1, 1, labels = paste(i), cex = 3)
  }
}
par(mfcol = c(3, 2), mar = c(.5, .5, .5, .5))
myplot(6)

par(mai = c(.1, .1, .1, .1))
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
myplot(3)
