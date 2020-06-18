#' Question: P(1)=0.35, P(4)=0.45, P(6)=P(9)=0.1 
#' 繳交一個Word檔案, 包含四種方法的R程式, 執行1000次的summary、table
#' 以及繪製條圖次數分配等結果。

#' 離散接受拒絕法
#' P(1)=0.35, P(4)=0.45, P(6)=P(9)=0.1
simtimes <- 1000
X <- c(1, 4, 6, 9)
PR <- c(0.35, 0.45, 0.1, 0.1)
AR <- NULL
AR.f <- function (X = X<-c(1, 4, 6, 9), PR = c(0.1, 0.2, 0.3, 0.4)) {
  AR<-PR/max(PR)
  U<-runif(2)
  I<-ceiling(length(PR)*U[1])
  if (U[2]<AR[I]) return(X[I]) else return(999) #999 代表改次結果為拒絕
}
for(i in 1:simtimes) AR<-c(AR,AR.f(X, PR));#AR1
AR.fac <- factor(AR, levels = c(X, 999), labels = c(X, "拒絕"))

summary(AR[AR != 999])
table(AR.fac)

#' 快速查表法
#' P(1)=0.35, P(4)=0.45, P(6)=P(9)=0.1
Xandpr <- c(rep(1, 7), rep(4, 9), rep(6, 2), rep(9, 2))
lookup <- function (X=c(3,5,5,5,11,11,11,11,rep(19,12))) {
  X[ceiling(length(X)*runif(1))]
}
lup1 <- NULL
for (i in 1:simtimes) lup1<-c(lup1,lookup(Xandpr))
lup1
summary(lup1)
table(lup1)

#'  alias法
#' 首先經過過程計算出:
#'  Step_0:
#'    | X(i)  | 1   | 4   | 6  | 9  |
#'    |-------|-----|-----|----|----|
#'    | f(X)  | .35 | .45 | .1 | .1 |
#'    | Y(i)  | 1   | 4   | 6  | 9  |
#'    | fA(i) | 1   | 1   | 1  | 1  |
#'  bar{p} = .25
#'  
#'  Step_1:
#'    | X(i)  | 1   | 4   | 6   | 9   |
#'    |-------|-----|-----|-----|-----|
#'    | f(X)  | .35 | .15 | .25 | .25 |
#'    | Y(i)  | 1   | 4   | 4   | 4   |
#'    | fA(i) | 1   | 1   | .4  | .4  |
#'    
#'  Step_2:
#'    | X(i)  | 1   | 4   | 6   | 9   |
#'    |-------|-----|-----|-----|-----|
#'    | f(X)  | .25 | .25 | .25 | .25 |
#'    | Y(i)  | 1   | 1   | 4   | 4   |
#'    | fA(i) | 1   | .6  | .4  | .4  |
#'    
#' P(1) = 0.35, P(4)=0.45, P(6)=P(9)=0.1
#' bar{P} = .25
#' X(i) = 1, 4, 6, 9
#' Y(i) = 1, 1, 4, 4
#' fA(i) = 1, .6, .4, .4
X <- c(1, 4, 6, 9)
Y <- c(1, 1, 4, 4)
P <- c(1, .6, .4, .4)

my.alias <- function (V1=X, V2=Y, PP=P) {
  idx<-ceiling(length(X)*runif(1))
  if (runif(1)<PP[idx]) return(V1[idx]) else return(V2[idx])
}
als1 <- NULL
for(i in 1:simtimes) als1 <- c(als1, my.alias(X, Y, P))
als1
summary(als1)
table(als1)

#' 作圖
#' 
library(RColorBrewer)
coul <- brewer.pal(5, "Set2")
barplot(table(AR), main = "使用接受拒絕法執行1000次模擬", xlab = "X(i)",
        ylab = "次數", sub = "P(1)=0.35, P(4)=0.45, P(6)=P(9)=0.1", col = coul)
barplot(table(lup1), main = "使用快速查表法執行1000次模擬", xlab = "X(i)",
        ylab = "次數", sub = "P(1)=0.35, P(4)=0.45, P(6)=P(9)=0.1", col = coul)
barplot(table(als1), main = "使用alias法執行1000次模擬", xlab = "X(i)",
        ylab = "次數", sub = "P(1)=0.35, P(4)=0.45, P(6)=P(9)=0.1", col = coul)

