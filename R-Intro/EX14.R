#' 1. 請設計程式，此程式會要求輸入姓名，然後請回應"Welcome"和所輸入的姓名。
#' 2. 重新輸入上一個程式，但將輸出轉至exer14_2.txt。
#' 3. 請參考實例ch14_18.R，但將資料改成有10筆，讀取後執行下列動作。
#'      a. 求總計。
#'      b. 求平均。
#'      c. 求最大值。
#'      d. 求最小值。
#' 4. 參考前一實例，將執行結果寫入exer14_3.txt。

#' Q1
welcome <- function() {
  cat("請輸入姓名: ")
  name <- scan(what = character())
  cat("Welcome, ", name, ".", sep = "")
}

#' Q2
welcome <- function() {
  cat("請輸入姓名: ")
  name <- scan(what = character())
  cat("Welcome, ", name, ".", sep = "", file = "exer14_2.txt")
}

#' Q3
write(1:10, file = "exer14_3.txt", ncolumns = 10)
file <- scan(file = "exer14_3.txt")
sum(file)
mean(file)
max(file)
min(file)

#' Q4
cat("總計: ", sum(file), "\n",
    "均值: ", mean(file), "\n",
    "最大值: ", max(file), "\n",
    "最小值: ", min(file), sep = "", file = "exer14_4.txt"
)
