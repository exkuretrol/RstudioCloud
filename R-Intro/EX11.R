library(dplyr)
dummy <- c(0.2387, 1, 0.5)
#' Q1. 重新設計實例ch11_11.R，使用3 點參數觀念，如果不輸入第2 個參數，將產生
#' 帶1 位小數的百分比。

addPercent <- function(x, ...){  
  percent <- round(x * 100, digits = 1)
  paste(percent, "%", ...)
}
addPercent(0.00065, sep="")

#' Q2. 重新設計實例ch11_17.R，設計通用函數，使用3 點參數觀念，如果輸入是數
#' 值，預設是求平均值，如果輸入是字元，則將字元改成大寫，預設函數觀念則不
#' 變。

generic_function_practice <- function (x, ...){
  UseMethod("generic_function_practice")
}

generic_function_practice.default <- function (x, ...) {
  print("本函數目前不能處理此物件")
}

generic_function_practice.numeric <- function (x, ...) {
  mean(x)
}

generic_function_practice.character <- function (x, ...) {
  toupper(x)
}

#' Q3. 設計一個計算電費的通用函數，每度電費100 元，如果輸入是非數值向量，則輸
#' 出" 輸入錯誤，請輸入數值向量"。

electricityFee_calc <- function(x, ...) {
  UseMethod("electricityFee_calc")
}

electricityFee_calc.default <- function (x, ...) {
  print("輸入錯誤，請輸入數值向量")
}

electricityFee_calc.numeric <- function (x, ...) {
  return(paste0("電費: ", x * 100, "元"))
}
