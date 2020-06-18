x <- c(-1, 0, 1, 2, -4)
#' 1. 不得使用R 內建的函數，請設計下列函數。
#'   a. mymax( ) 求最大值
#'   b. mymin( ) 求最小值
#'   c. myave( ) 求平均值
#'   d. mysort( ) 執行排序
#' 如果輸入是非數值向量，則輸出" 輸入錯誤，請輸入數值向量"。
#' a. mymax( ) 求最大值
mymax <- function (x) {
  if (is.numeric(x) == FALSE) print("輸入錯誤，請輸入數值向量")
  else {
    temp <- -Inf
    for (i in x) {
      if (i > temp) {temp <- i}
    }
    return(temp)
  }
}

#' b. mymin( ) 求最小值
mymin <- function (x) {
  if (is.numeric(x) == FALSE) print("輸入錯誤，請輸入數值向量")
  else {
    temp <- Inf
    for (i in x) {
      if (i < temp) {temp <- i}
    }
    return(temp)
  }
}

#' c. myave( ) 求平均值
mymean <- function(x) {
  if (is.numeric(x) == FALSE) print("輸入錯誤，請輸入數值向量")
  else {
    sum <- 0
    count <- 0
    for (i in x) {
      count <- count + 1
      sum <- sum + i
    }
    mean <- sum/count
    return(mean)
  }
}

#' d. mysort( ) 執行排序
#' {9、4、1、6、7、3、8、2、5}
#' x <- c(9, 4, 1, 6, 7, 3, 8, 2, 5)
#' 參考資料: QuickSort 
#' <http://alrightchiu.github.io/SecondRound/comparison-sort-quick-sortkuai-su-pai-xu-fa.html>
mysort <- function(x) {
  if (is.numeric(x) == FALSE) print("輸入錯誤，請輸入數值向量")
  else {
    ori <- x
    counter <- 0
    for (i in x) counter <- counter + 1
    
    swap <- function(x, i, j) {
      if (length(x)<2) print("數列長度需大於或等於2")
      temp <- x[i]
      x[i] <- x[j]
      x[j] <- temp
      x
    }
    #' array x = {9、4、1、6、7、3、8、2、5}
    #' x <- c(9, 4, 1, 6, 7, 3, 8, 2, 5)
    partition <- function(x, front, end) {
      pivot <- x[end]
      i <- front - 1
      for (j in front:(end-1)) {
        if (x[j]>pivot) {
        } else {
          i <- i + 1
          x <- swap(x, i, j)
        }
      }
      x <- swap(x, i+1, end)
      return(x)
    }
    index <- function(x, front, end) {
      pivot <- x[end]
      i <- front - 1
      for (j in front:(end-1)) {
        if (x[j]>pivot) {
        } else {
          i <- i + 1
          x <- swap(x, i, j)
        }
      }
      x <- swap(x, i+1, end)
      return(i)
    }
    QuickSort <- function(x, front, end) {
      if (front < end) {
        pivot <- index(x, front, end) + 1
        x <- partition(x, front, end)
        x <- QuickSort(x, front, pivot - 1)
        QuickSort(x, pivot + 1, end)
      } else x
    }
    list("Original: "= ori,"Sorted: "= QuickSort(x, 1, counter))
  }
}

#' 2. 請設計一個計算電價的程式，收費規則如下：
#'   a. 每度100 元
#'   b. 超過300 度打8 折，"> 300"
#'   c. 超過100 度但小於等於300 度打9 折，"> 100" 和"< = 300"
#'   d. 政府機構上述計算完再打7 折。
#'   e. 清寒證明上述計算完再打5 折。
#' 請至少輸入考量所有狀況的12 筆資料做測試。
electricityFee_calc <- function(x, gov = FALSE, poor = FALSE) {
  if (x>300) x <- x*100*.8
  else if (x<=300 & x>100) x <- x*100*.9
  else x <- x*100
  if (gov == TRUE) x <- x*.7
  if (poor == TRUE) x <- x*.5
  x
}

#' 3. 重新設計實例ch12_17.R，計算系統內建數據集state.region（6-9 節有介紹此數
#' 據集），每一區各有多少個州。

region <- names(table(state.region))

countState <- function(x) {
  x <- as.character(x)
  label <- unique(x)
  for (i in 1:length(label)) {
    counter <- 0
    for (j in 1:length(x)) {
      if (x[j] == label[i]) counter <- counter + 1
    }
    print(paste(label[i], counter))
  }
}

#' 4. 使用state.x77 數據集，配合state.region 數據集，計算美國4 大區：
#'   a. 人口數各是多少。
#'   b. 收入平均是多少。
#'   c. 面積是各是多少。
df <- cbind(state.x77, Region = as.character(state.region))
df <- df[, -(3:7)]
region <- unique(df[,4])

#'  a. 人口數各是多少。
for (i in 1:length(region)) {
  popsum <- 0
  for (j in 1:nrow(df)) {
    if(region[i] == df[j, 4]) popsum <- popsum + as.numeric(df[j, 1])
  }
  print(paste(region[i], popsum))
}

#'  b. 收入平均是多少。
for (i in 1:length(region)) {
  incomemean <- 0
  for (j in 1:nrow(df)) {
    if(region[i] == df[j, 4]) incomemean <- incomemean + as.numeric(df[j, 2])
  }
  print(paste(region[i], incomemean/50))
}

#'  c. 面積是各是多少。
for (i in 1:length(region)) {
  areasum <- 0
  for (j in 1:nrow(df)) {
    if(region[i] == df[j, 4]) areasum <- areasum + as.numeric(df[j, 3])
  }
  print(paste(region[i], areasum))
}
