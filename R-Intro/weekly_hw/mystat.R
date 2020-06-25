install.packages("TSA")
library(TSA)

score <- sample(80:100, size = 50, replace = TRUE)
stat <- numeric(12)
names(stat) <- c("平均數",
                 "中位數",
                 "眾數",
                 "全距中值",
                 "半四分位數距",
                 "變異數",
                 "標準差",
                 "全距",
                 "四分位距",
                 "變異係數",
                 "偏性係數",
                 "峰態係數")
stat[1] <- mean(score)
stat[2] <- median(score)
stat[3] <- as.numeric(names(table(score))[which.max(table(score))])
stat[4] <- mean(c(max(score), min(score)))
stat[5] <- (summary(score)[5] - summary(score)[2])/2
stat[6] <- var(score)
stat[7] <- sd(score)
stat[8] <- max(score)-min(score)
stat[9] <- IQR(score)
stat[10] <- var(score)/mean(score)
stat[11] <- skewness(score)
stat[12] <- kurtosis(score)
my.stat <- function(x) {
  semi <- temp <- (summary(x)[5]-summary(x)[2])/2
  attr(semi, "names") <- NULL
  stat <- c(mean(x),
            median(x),
            as.numeric(names(table(x))[which.max(table(x))]),
            mean(c(max(x), min(x))),
            semi,
            var(x),
            sd(x),
            max(x)-min(x),
            IQR(x),
            var(x)/mean(x),
            skewness(x),
            kurtosis(x))
  names(stat) <- c("平均數",
                   "中位數",
                   "眾數",
                   "全距中值",
                   "半四分位數距",
                   "變異數",
                   "標準差",
                   "全距",
                   "四分位距",
                   "變異係數",
                   "偏性係數",
                   "峰態係數")
  return(stat)
}
