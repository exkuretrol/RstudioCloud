#' 4. 請計算 iris 物件花瓣以及花萼平均的length / width。
#' 5. 請將 islands 物件一面積大小分成 6 等份。
#' 6&7. 重新設計實例ch15_42.R，合併符合人口數少於 200 萬人的州和月收入少於 3800 美元的州。合併的四種情形。
#' 8. 使用mtcars資料，獲得 HP 對應(A) 手自排 (B) 汽缸數 (C) 手自排*汽缸數 的總結資料(包含min、Q1、median、mean、Q3 and max)及平均數
#' 

#' Q4
head(iris)
cp.iris <- within(iris, Petal.Ratio <- Petal.Length / Petal.Width)
cp.iris <- within(cp.iris, Sepal.Ratio <- Sepal.Length / Sepal.Width)
apply(cp.iris[, 6:7], 2, mean)

#' Q5
cut(islands, 6)


#' Q6 + Q7
cp.state.x77 <- as.data.frame(state.x77[, 1:2])
cp.state.x77$State <- rownames(cp.state.x77)
rownames(cp.state.x77) <- NULL

pop.lt2000 <- cp.state.x77[cp.state.x77$Population < 2000, c("State", "Population")]
inc.lt3800 <- cp.state.x77[cp.state.x77$Income < 3800, c("State", "Income")]

#' 聯集
merge(pop.lt2000, inc.lt3800, all = TRUE)

#' 交集
merge(pop.lt2000, inc.lt3800, all = FALSE)

#' (Pop | Income)
merge(pop.lt2000, inc.lt3800, all.x = TRUE)

#' (Income | Pop)
merge(pop.lt2000, inc.lt3800, all.y = TRUE)

#' Q8
#' A data frame with 32 observations on 11 (numeric) variables.

#' [, 1]	mpg	Miles/(US) gallon
#' [, 2]	cyl	Number of cylinders
#' [, 3]	disp	Displacement (cu.in.)
#' [, 4]	hp	Gross horsepower
#' [, 5]	drat	Rear axle ratio
#' [, 6]	wt	Weight (1000 lbs)
#' [, 7]	qsec	1/4 mile time
#' [, 8]	vs	Engine (0 = V-shaped, 1 = straight)
#' [, 9]	am	Transmission (0 = automatic, 1 = manual)
#' [,10]	gear	Number of forward gears
#' [,11]	carb	Number of carburetors

cp.mtcars <- mtcars[, c("hp", "am", "cyl")]
cp.mtcars <- within(cp.mtcars, am <- factor(am, levels = 0:1, 
                                            labels = c("自排",
                                                       "手排")))
#' Summary()
#' 馬力對應自排/手排
hpam <- with(cp.mtcars, tapply(hp, am, summary))

#' 馬力對應汽缸數
hpcyl <- with(cp.mtcars, tapply(hp, cyl, summary))

#' 馬力對應手自排和汽缸數
hpamcyl <- aggregate(hp ~ am + cyl, cp.mtcars, summary)

#' 馬力對應自排/手排
hpam <- with(cp.mtcars, tapply(hp, am, mean))

#' mean()
#' 馬力對應汽缸數
hpcyl <- with(cp.mtcars, tapply(hp, cyl, mean))

#' 馬力對應手自排和汽缸數
hpamcyl <- aggregate(hp ~ am + cyl, cp.mtcars, mean)
