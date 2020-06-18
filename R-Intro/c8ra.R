#' 參考三顆骰子18拉遊戲改寫成四顆骰子的18啦，得到理論機率分配與，隨機實驗2000次的結果。

c8.ra <- function(x1=2, x2=5, x3=3, x4=4) {
  y <- sort(c(x1, x2, x3, x4))
  if (y[1] == y[4]) result <- 13 #' 四顆骰子點數相同, 豹子
  else if (y[1] == y[3] | y[2] == y[4]) result <- 1 #' 三顆骰子點數相同, 重骰
  else if (y[1] == y[2]) result <- y[3] + y[4] #' 一二顆骰子點數相同, 結果為三四顆骰子點數總和
  else if (y[2] == y[3]) result <- y[1] + y[4] #' 二三顆骰子點數相同, 結果為一四顆骰子點數總和
  else if (y[3] == y[4]) result <- y[1] + y[2] #' 三四顆骰子點數相同, 結果為一二顆骰子點數總和
  else result <- 2 #' 沒有任何一顆骰子點數相同, 重骰
  result
}

result <- numeric(6^4)
i = 0
for (D1 in 1:6) {
  for (D2 in 1:6) {
    for (D3 in 1:6) {
      for (D4 in 1:6) {
        i <- i + 1
        result[i] <- c8.ra(D1, D2, D3, D4)
      }
    }
  }
}
factor.result <- factor(result, levels = 1:13, labels = c("三骰同", 
                                                          "無骰同", 
                                                          "3點",
                                                          "4點",
                                                          "5點",
                                                          "6點",
                                                          "7點",
                                                          "8點",
                                                          "9點",
                                                          "10點",
                                                          "11點",
                                                          "12點",
                                                          "豹子"))
table(factor.result)
plot(factor.result, main = "西吧ra結果的理論次數分配")

simulation.times <- 2000
sim.result <- NULL
for (i in 1:simulation.times) {
  four.dices <- sample(1:6, 4, replace = TRUE)
  sim.result[i] <- c8.ra(four.dices[1], four.dices[2], four.dices[3], four.dices[4])
}
factor.sim.result <- factor(sim.result, levels = 1:13, labels = c("三骰同", 
                                                                  "無骰同", 
                                                                  "3點",
                                                                  "4點",
                                                                  "5點",
                                                                  "6點",
                                                                  "7點",
                                                                  "8點",
                                                                  "9點",
                                                                  "10點",
                                                                  "11點",
                                                                  "12點",
                                                                  "豹子"))
table(factor.sim.result)
plot(factor.sim.result, main = paste0("西吧ra結果的", simulation.times, "次模擬次數分配"))
