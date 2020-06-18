#' 1. 以rnorm(100,mean=60,sd=12) 產生100 筆平均數為60 標準差為12 的常態分配
#'   隨機數向量x，並計算出x 的平均數、中位數、眾數、變異數、標準差、全距、
#'   最大值、最小值、第一四分位數、第三四分位數等各項統計 。
#' 註：rnorm( ) 函數的第一個參數是表示產生100 筆資料。
#' 2. 參考實例ch16-44 建立上題x 的直方圖並加上密度圖。
#' 3. 使用summary( ) 函數以了解前題的向量x 的各項總結統計並繪製其箱型圖。
#' 4. 以rchisq(100, df=8) 產生100 筆自由度為8 卡方分配的隨機向量y。
#' 
#' 5. 檢討上述題目所產生的x 與y 兩向量間的線性相關係數。

#' Q1:
my.stat <- function (x) {
  x.stat <- numeric(10)
  names(x.stat) <- c("平均數", "中位數", "眾數", "變異數", "標準差", "全距", "最大值", 
                     "最小值", "第一四分位數", "第三四分位數")
  x.stat[1] <- mean(x)
  x.stat[2] <- median(x)
  x.stat[3] <- ifelse(which.max(table(x)) == 1, "NA", names(which.max(table(x))))
  x.stat[4] <- var(x)
  x.stat[5] <- sd(x)
  x.stat[6] <- max(x) - min(x)
  x.stat[7] <- max(x)
  x.stat[8] <- min(x)
  x.stat[9] <- quantile(x, .25)
  x.stat[10] <- quantile(x, .75)
  x.stat
}
x <- rnorm(100, mean = 60, sd = 12)
x.stat <- my.stat(x)

#' Q2:
##' Method #1
hist(x, freq = FALSE)
dens.x <- density(x)
lines(dens.x)

##' Method #2
##' Source: [Quick-R: Density Plots](https://www.statmethods.net/graphs/density.html)
h <- hist(x, ylim = c(0, 35), col = rgb(1, 0, 0, .5))
x.xfit <- seq(min(x), max(x), length = 40)
x.yfit <- dnorm(x.xfit, mean(x), sd(x))
x.yfit <- x.yfit * diff(h$mids[1:2]*length(x))
lines(x.xfit, x.yfit, col = rgb(1, 0, 0, .9), lwd = 2)

#' Q3:
summary(x)
boxplot(x)

#' Q4:
y <- rchisq(100, df = 8)
y.stat <- my.stat(y)
#' dens.y <- density(y)

#' Q5:
cor(x, y)

##' Source: [R par 多圖合併 | 龍崗山上的倉鼠](https://kanchengzxdfgcv.blogspot.com/2017/09/r-par.html)
par(
  mfrow=c(1,2),
  mar=c(4,4,4,4)
)

h <- hist(x, main = "Normal Distrubution \n N = 100, sd = 12", 
          col = rgb(1, 0, 0, .5))
x.xfit <- seq(min(x), max(x), length = 40)
x.yfit <- dnorm(x.xfit, mean(x), sd(x))
x.yfit <- x.yfit * diff(h$mids[1:2]*length(x))
lines(x.xfit, x.yfit, col = rgb(1, 0, 0, .9), lwd = 2)

g <- hist(y, main = "Chi-square Distribution \n N = 100, d.f. = 8",
          col = rgb(0, 0, 1, .5))
y.xfit <- seq(min(y), max(y), length = 40)
y.yfit <- dnorm(y.xfit, mean(y), sd(y))
y.yfit <- y.yfit * diff(g$mids[1:2] * length(y)) 
lines(y.xfit, y.yfit, col = rgb(0, 0, 1, .9), lwd = 2)
legend("topright", legend = c("x", "y"), fill = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)), cex = .8)
