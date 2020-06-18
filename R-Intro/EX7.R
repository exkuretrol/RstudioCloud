library(dplyr)
#! Q1:
name <- LETTERS[1:5]
gender <- sample(c("F", "M"), replace = TRUE, size = 5)
height <- sample(155:175, replace = TRUE, size = 5)

A1 <- cbind(name, gender, height) %>% data.frame(., stringsAsFactors = FALSE)

A2 <- data.frame(name = c(LETTERS[6:8]),
                 gender = sample(c("F", "M"), replace = TRUE, size = 3),
                 height = sample(155:175, replace = TRUE, size = 3), 
                 stringsAsFactors = FALSE)
ans_1_c <- rbind(A1, A2)
ans_1_c$height <- as.numeric(ans_1_c$height)

ans_1_dwhich(ans_1_c$height>=170)

#! Q2:
weight <- sample(50:80, replace = TRUE, size = 8)
age <- sample(18:60, replace = TRUE, size = 8)
ans_2 <- cbind(ans_1_c, cbind(weight, age))
# with dplyr package
ans_2_a <- ans_2 %>% filter(.$gender == "F")
ans_2_b <- ans_2 %>% filter(.$gender == "M" & .$weight > 70)
# without dplyr package
ans_2_a <- ans_2[ans_2$gender == "F", ]
ans_2_b <- ans_2[ans_2$gender == "M" & ans_2$weight > 70, ]

