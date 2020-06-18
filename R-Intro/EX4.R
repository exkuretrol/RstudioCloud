library(dplyr)
# Q1
fname <- letters[1:10]
fblood <- sample(c("A", "B", "AB", "O"), 10, replace = TRUE)
names(fblood) <- fname
fage <- sample(18:65, 10)

ans_1_d-A <- sort(fage)
ans_1_d-B <- sort(fage, decreasing = TRUE)

# Q2
pname <- sample(fname, 5)
score.A <- c(sample(1:5, 15, replace = TRUE))
score.B <- c(sample(1:5, 15, replace = TRUE))
score.C <- c(sample(1:5, 15, replace = TRUE))
score.D <- c(sample(1:5, 15, replace = TRUE))
score.E <- c(sample(1:5, 15, replace = TRUE))

score.A.sum <- sum(score.A*(3:1)); score.A.mean <- score.A.sum/5
score.B.sum <- sum(score.B*(3:1)); score.B.mean <- score.B.sum/5
score.C.sum <- sum(score.C*(3:1)); score.C.mean <- score.C.sum/5
score.D.sum <- sum(score.D*(3:1)); score.D.mean <- score.D.sum/5
score.E.sum <- sum(score.E*(3:1)); score.E.mean <- score.E.sum/5

score.A.num <- sum(score.A)
score.B.num <- sum(score.B)
score.C.num <- sum(score.C)
score.D.num <- sum(score.D)
score.E.num <- sum(score.E)

game1 <- c(sum(score.A[1:3]*3:1),
           sum(score.B[1:3]*3:1),
           sum(score.C[1:3]*3:1),
           sum(score.D[1:3]*3:1),
           sum(score.E[1:3]*3:1))
game2 <- c(sum(score.A[4:6]*3:1),
           sum(score.B[4:6]*3:1),
           sum(score.C[4:6]*3:1),
           sum(score.D[4:6]*3:1),
           sum(score.E[4:6]*3:1))
game3 <- c(sum(score.A[7:9]*3:1),
           sum(score.B[7:9]*3:1),
           sum(score.C[7:9]*3:1),
           sum(score.D[7:9]*3:1),
           sum(score.E[7:9]*3:1))
game4 <- c(sum(score.A[10:12]*3:1),
           sum(score.B[10:12]*3:1),
           sum(score.C[10:12]*3:1),
           sum(score.D[10:12]*3:1),
           sum(score.E[10:12]*3:1))
game5 <- c(sum(score.A[13:15]*3:1),
           sum(score.B[13:15]*3:1),
           sum(score.C[13:15]*3:1),
           sum(score.D[13:15]*3:1),
           sum(score.E[13:15]*3:1))
c(pname[which.max(game1)],
  pname[which.max(game2)],
  pname[which.max(game3)],
  pname[which.max(game4)],
  pname[which.max(game5)])

# Q3
month.date <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
names(month.date) <- month.abb
month.date[month.date==31] %>% names()

# Q4
islands %>% sort %>% .[c(30, 35)]

# Q5
islands %>% sort %>% head(15)
islands %>% sort %>% tail(15)

# Q6
islands %>% sort %>% .[1:length(islands) %% 2 == 1]
islands %>% sort %>% .[1:length(islands) %% 2 == 0]
