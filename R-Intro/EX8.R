#! Q1:
season <- c("春", "夏", "秋", "冬")
plant <- c("梅", "蘭", "竹", "菊")
other <- rep(c("紅中", "發財", "白板"), 4)
wan <- paste(1:9, "萬") %>% rep(., 4)
string <- paste(1:9, "條") %>% rep(., 4)
cookie <- paste(1:9, "餅") %>% rep(., 4)
ans_1 <- list("季節" = season, "草" = plant, "字" = other, 
              "萬"= wan, "條"= string, "餅" = cookie)

#! Q2:
year <- c(1996:2000) %>% as.character()
city <- state.name %>% sample(., 5, replace = TRUE)
friend <- LETTERS[1:5]
A <- list("year" = year, "city" = city, "friend" = friend)

A[[3]] %>% .[2]
A[3] %>% unlist() %>% .[2]
A$friend[2]
A[[3]][2]

#! Q3
A <- list("year" = year, "city" = city, "friend" = friend)

#! Q4
A[[1]] %>% str
A[1] %>% str
A$year %>% str

#! Q5
A[-c(1)]

#! Q6
A$city

#! Q7
A$eat_breakfast_or_not <- c(sample(c("Y", "N"), size = 3, replace = TRUE))

#! Q8
B <- c(today = "Fri", Hour = "2", Min = "57") %>% as.list()

#! Q9
ans_9 <- c(A, B)
