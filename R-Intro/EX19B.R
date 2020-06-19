library(MASS)

par(mai = c(.5, .5, .5, .5))
mat <- matrix(c(0, 1, 1, 1, 
                2, 3, 3, 3, 
                2, 3, 3, 3, 
                2, 3, 3, 3), ncol = 4, byrow = TRUE)
layout(mat = mat)

hist(crabs$CL, main = "螃蟹的前額葉長度直方圖 (mm)")
boxplot(crabs$FL, main = "螃蟹的甲殼長度箱型圖 (mm)")
with(plot(FL ~ CL, main = "螃蟹的前額葉 vs 甲殼長度散佈圖"), data = crabs)
