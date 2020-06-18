library(dplyr)

#' Q1
name <- c("Chen", "Chia", "Wei")
ans_1_a <- paste(name, collapse = " ")
ans_1_b <- paste(paste(name[2:3], collapse = " "), name[1], collapse = " ")
strsplit(ans_1_a, split = " ") %>% unlist() %>% paste

#' Q2
namelist <- c("Kim Jong-un",
              "Donald John Trump",
              "Tsai Ing wen",
              "Winnie the Pooh",
              "Xi Jin ping")
sort(namelist)
sort(namelist, decreasing = TRUE)

#' Q3
state.name[grep("South", state.name)]

#' Q4
state.name[grep("M", state.name)] %>% gsub("M", "m", .)

#' Q5
state.name[-grep(" ", state.name)]

#' Q6
state.name[grep(pattern = "A|M", state.name)]
