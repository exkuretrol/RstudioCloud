#' Q0 Birthday
thatday <- "1996-12-30 22:00" %>% as.POSIXct()
difftime(time1 = Sys.Date(),time2 = thatday, units = "days")
difftime(time1 = Sys.Date(),time2 = thatday, units = "weeks") 
difftime(time1 = Sys.Date(),time2 = thatday)/(365.25/12)
difftime(time1 = Sys.Date(),time2 = thatday)/365.25


#' Q1
#' 準備環境
setwd("/cloud/project/R-Intro")
dir.create("sheets")
#' [中華民國統計資訊網（一般民眾）]
#' (https://www1.stat.gov.tw/ct.asp?xItem=15409&CtNode=4693&mp=3)
#' * 	出生數、出生率、死亡數、死亡率 		內政部戶政司、統計處

download.file('https://www.moi.gov.tw/files/site_stuff/321/1/month/m1-02.ods', 
              destfile = "sheet/data.ods", 
              method = "wget", 
              extra = "-r -p --random-wait")

#' install.packages("readODS")
#' install.packages("stringr")
library(readODS)
library(dplyr)
library(stringr)
library(ggplot2)
df <- read_ods(path = "./sheet/data.ods", skip = 2)
df %>% colnames() %>% head
year.birth <- df[grep("年", df[, 1]), 1:2]
our.df <- year.birth %>% rename(., "year" = colnames(year.birth)[1], 
                      "births" = colnames(year.birth)[2]) %>% 
              mutate(year =  as.numeric(str_extract(year, "[0-9]+$"))) %>%
              #' drop 2020
              na.omit() %>% .[1:nrow(.)-1,] %>% .[.[[1]]>2019-30,] %>% arrange(., year)

qplot(data = our.df, x = year, y = births)

#' Q2
#' install.packages("tidyquant")
library(tidyquant)

FANG_max_by_qtr <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = apply.quarterly, 
               FUN        = max, 
               col_rename = "max.close") %>%
  mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
  select(-date)
FANG_max_by_qtr

#' Q3
#' [台灣地區主要水庫蓄水量報告表](https://fhy.wra.gov.tw/ReservoirPage_2011/StorageCapacity.aspx)
#' 2015-01~2016-12
石門水庫 <- c(232.95, 228.07, 221.89, 220.26, 218.78, 232.70, 234.49, 243.66, 243.67, 243.29, 243.48, 239.46,
              236.90, 244.73, 242.56, 244.25, 244.06, 242.76, 244.11, 243.40, 241.39, 243.82, 244.64, 243.79)
翡翠水庫 <- c(167.16, 164.65, 163.29, 166.25, 164.78, 162.47, 159.23, 159.88, 161.81, 165.79, 164.38, 166.05, 
              167.13, 166.67, 165.38, 165.43, 163.94, 161.03, 163.22, 160.29, 156.42, 165.29, 163.82, 167.22)
寶山第二水庫 <- c(137.64, 133.51, 130.26, 129.64, 132.92, 145.37, 148.57, 149.63, 149.09, 149.49, 149.76, 145.73, 
                  144.56, 149.59, 149.73, 149.71, 149.75, 149.73, 149.74, 148.84, 147.69, 149.05, 149.76, 149.43)

ts(cbind(石門水庫, 翡翠水庫, 寶山第二水庫), start = c(2015, 1), end = c(2016, 12), frequency = 12)

#' Q4

dailyspend <- rnorm(30, 200, sd=15)
ts(dailyspend, frequency = 30)
