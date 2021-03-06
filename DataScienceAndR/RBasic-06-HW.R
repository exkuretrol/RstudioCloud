#' 這裡我們使用CO2這個資料集請同學做練習，
data(CO2)

#' 請問CO2 有多少列(row)？
answer1 <- nrow(CO2) #請將NULL替換成你的程式碼

#' 請問CO2 有多少欄(column)？
answer2 <- ncol(CO2) #請將NULL替換成你的程式碼

#' 請問CO2 的各欄的名稱為何？
answer3 <- colnames(CO2) #請將NULL替換成你的程式碼

#' 請問uptake這欄的平均值為多少？
answer4 <- mean(CO2$uptake) #請將NULL替換成你的程式碼

#' CO2 共有很多很多列（answer1），
#' 請從CO2 中挑出一些列，滿足以下的條件：
#' 這些列的uptake值，超過全部CO2"平均"的uptake值
#' （`answer4`）
#' 
#'   你可以先取出uptake的向量、接著拿該向量和平均值做比較、把結果的logical vector丟到`[]`的第一個參數中。
answer5 <- CO2[CO2$uptake > mean(CO2$uptake), ] #請將NULL替換成你的程式碼

#' 請問Type有多少種類別？
#' ps. answer6 應該是一個整數
answer6 <- length(table(CO2$Type)) #請將NULL替換成你的程式碼

#' 請問當類別為Quebec時，uptake的平均值為多少？
answer7 <- mean(CO2$uptake[CO2$Type == "Quebec"]) #請將NULL替換成你的程式碼

#' 請問當類別為Mississippi時，uptake的平均值為多少？
answer8 <- mean(CO2$uptake[CO2$Type == "Mississippi"]) #請將NULL替換成你的程式碼

#' 我們可以利用`model.matrix`來建立一個矩陣。
#' 舉例來說：`model.matrix(~ Type + Treatment + conc, CO2)`可以建立一個基於Type、Treatment和conc的矩陣。

X <- model.matrix(~ Type + Treatment + conc, CO2)

#' 請取出uptake的值放入y 之中
y <- CO2$uptake #請將NULL替換成你的程式碼

#' 請利用<https://en.wikipedia.org/wiki/Ordinary_least_squares#Estimation>的公式，運用迴歸的演算法，
#' 找出beta.hat讓 X %*% beta.hat 很靠近 y
beta.hat <- solve(t(X) %*% X, t(X) %*% y) #請將NULL替換成你的程式碼

#' 請計算X %*% beta.hat 和 y 的correlation（提示：用函數`cor`）
answer11 <-  cor(X %*% beta.hat, y)#請將NULL替換成你的程式碼

#' answer11 的平方，就是迴歸分析時常提到的：R-squared。
#' 很多分析師會用這個數據來判斷這個模型是否完善。
#' 在R 中，跑迴歸分析時，可以利用`lm`這個函數：
g <- lm(uptake ~ Type + Treatment + conc, CO2)

#' g 這個物件就會包含我們剛剛算過的答案
#' g$coef就會是beta.hat
#' g$fitted.value就會是X %*% beta.hat
#' summary(g)則會顯示各個參數的t 檢定，以及整個模型的R-squared。
g.s <- summary(g)
#' mode(g.s)顯示它是一個list。
#' 請找出一個名字，answer12，讓g.s[[answer12]]就是R-squared，
#' 你可以參考help(summary.lm)裡面的說明。
answer12 <- "r.squared" #請將NULL替換成你的程式碼
