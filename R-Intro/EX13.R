#' 實作題：（如果題目有描述不週詳時，請自行假設條件。）
#' 1. 請重新設計實例ch13_1.R，請自行設定未來6 天動物出現次數，同時執行下列
#' 運算。
#'   a. 列出各動物最大出現次數。
#'   b. 列出各動物最小出現次數。
#'   c. 列出各動物平均出現次數。
#' 2. 請重新設計實例ch13_1.R，請自行設定未來6 天動物出現次數，同時請設定各
#' 動物有一天出現次數是NA，同時執行下列運算。
#'   a. 列出各動物最大出現次數。
#'   b. 列出各動物最小出現次數。
#'   c. 列出各動物平均出現次數。
#' 3. 請參考實例ch13_5.R，請用tapply( ) 函數，執行計算美國4 大區下列運算。
#'   a. 人口數各是多少。
#'   c. 面積是各是多少。
#'   b. 收入平均是多少。

#' Q1:
an_info <- matrix(sample(0:10, 18, replace = TRUE), ncol = 3)
colnames(an_info) <- c("Lion", "Leopard", "Rabbit")
rownames(an_info) <- paste("day", 1:6)
#'   a. 列出各動物最大出現次數。
apply(an_info, 2, max)
#'   b. 列出各動物最小出現次數
apply(an_info, 2, min)
#'   c. 列出各動物平均出現次數。
apply(an_info, 2, mean)

#' Q2:
an_info <- matrix(sample(1:10, 18, replace = TRUE), ncol = 3)
an_info[1, 1] <- NA
an_info[3, 2] <- NA
an_info[5, 3] <- NA
colnames(an_info) <- c("Lion", "Leopard", "Rabbit")
rownames(an_info) <- paste("day", 1:6)
#'   a. 列出各動物最大出現次數。
apply(an_info, 2, max, na.rm = TRUE)
#'   b. 列出各動物最小出現次數
apply(an_info, 2, min, na.rm = TRUE)
#'   c. 列出各動物平均出現次數。
apply(an_info, 2, mean, na.rm = TRUE)

#' Q3:
region <- state.region
df <- data.frame(state.x77, region)
colnames(df)
df <- df[, -c(3:7)]
#'   a. 人口數各是多少。
tapply(df[, 1], df[, 4], sum)
#'   c. 面積是各是多少。
tapply(df[, 3], df[, 4], sum)
#'   b. 收入平均是多少。
tapply(df[, 2], state.region, mean)
