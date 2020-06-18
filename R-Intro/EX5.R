# Q1
ans_1_a <- matrix(1:30, nrow = 5, ncol = 6)
ans_1_b <- matrix(1:30, nrow = 5, ncol = 6, byrow = T)
ans_1_c <- str(ans_1_a, ans_1_b)

# Q2
x1 <- c(10, 12, 14)
x2 <- c(7, 14, 5)
x3 <- c(15, 3, 19)

ans_2_a <- rbind(x1, x2, x3)
ans_2_b <- cbind(x1, x2, x3)
ans_2_c <- ans_2_a[1:2, ]
ans_2_d <- ans_2_a[1:2, 2:3]
ans_2_e <- ans_2_b[, 2:3]
ans_2_f <- ans_2_b[2:2, 2:3]
ans_2_g <- ans_2_a[-1, ]
ans_2_h <- ans_2_b[, -2]

# Q3
score.A <- c(sample(1:5, 6, replace = TRUE))
score.B <- c(sample(1:5, 6, replace = TRUE))
score.C <- c(sample(1:5, 6, replace = TRUE))
score.D <- c(sample(1:5, 6, replace = TRUE))
score.E <- c(sample(1:5, 6, replace = TRUE))
score <- rbind(score.A, score.B, score.C, score.D, score.E)

# Q4
fname <- letters[1:10]
pname <- sample(fname, 5)
dimnames(score) <- list(c(pname), c(paste0(1:6, "th")))

# Q5
rowSums(score)

# Q6
rowMeans(score)

# Q7
grade <- 0:100 %>% sample(20)
dim(grade) <- c(5, 2, 2)
dimnames(grade) <- list(c(paste0("ID-0", 1:5)),
                          c("R", "Math"),
                          c("class-A", "class-B"))
