library(dplyr)
# Q1
fname <- letters[1:10]
fblood <- sample(c("A", "B", "AB", "O"), 10, replace = TRUE)
names(fblood) <- fname
ans_Q1 <- as.factor(fblood)

# Q2
ans_Q2 <- factor(ans_Q1, levels = c("A", "AB", "B", "O"))

# Q3
score <- sample(56:91, 20)
score.grade <- vector()
for (i in 1:length(score)) {
  if (score[i]>=90) {score.grade[i] <- "A"}
  else if (score[i]>=80) {score.grade[i] <- "B"}
  else if (score[i]>=70) {score.grade[i] <- "C"}
  else if (score[i]>=90) {score.grade[i] <- "D"}
  else {score.grade[i] <- "F"}
}
score.grade.ordered <- factor(score.grade, 
                              levels = c("F", "D", "C", "B", "A"), 
                              ordered = TRUE)
ans_Q3_a <- which(score.grade.ordered >= "B")
ans_Q3_b <- which(score.grade.ordered == "F")
ans_Q3_c <- table(score.grade.ordered)        
