CountLine <- function(balls=c(1:10)) {
  length(unique(balls))
}

balls <- 10
times <- 10000
result <- numeric(times)
for (i in 1:times) {
  result[i] <- CountLine(sample(1:balls, balls, replace = TRUE))
}

result <- factor(result, levels = 1:10, labels = paste0("中 ", 1:10, " 格"))
table(result)
# plot(result)
sum(table(result)/times) == 1
