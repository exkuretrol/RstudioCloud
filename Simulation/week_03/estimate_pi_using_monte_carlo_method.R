library(dplyr)
library(ggplot2)
#! week_06 estimate pi using monte carlo method
#! 1. Draw a square, then inscribe a quadrant within it
#! 2. Uniformly scatter a given number of points over the square
#! 3. Count the number of points inside the quadrant, i.e. having a distance from the origin of less than 1
#! 4. The ratio of the inside-count and the total-sample-count is an estimate of the ratio of the two areas, pi/4. Multiply the result by 4 to estimate pi.
#! Ref: [Monte_Carlo_method](https://en.wikipedia.org/wiki/Monte_Carlo_method)
#! Ref: [R Programming Tutorial – How to Compute PI using Monte Carlo in R?](https://helloacm.com/r-programming-tutorial-how-to-compute-pi-using-monte-carlo-in-r/)

# use system time as seed
Sys.time() %>% set.seed

# define number of simulation
num_sims <- 100000

# 2. Uniformly scatter a given number of points over the square
x <- runif(num_sims)
y <- runif(num_sims)

# 3. Count the number of points inside the quadrant
d <- sqrt(x^2+y^2)
inside_the_quadrant <- which(d <= 1)

# 4. The ratio of the inside-count and the total-sample-count is an 
# estimate of the ratio of the two areas, pi/4. Multiply the result 
# by 4 to estimate pi.
pi <- inside_the_quadrant %>% length/num_sims*4
df <- as.data.frame(cbind(x, y, d))


qplot <- ggplot(df, aes(x, y, color=d>1)) + geom_point(size=0.5) +
            coord_fixed() + 
            labs(title = "Estimate π using monte carlo method",
                 subtitle = paste0(c("n = 100000; ", "estimated π = ", pi), collapse = ""),
                 color = "distance from the origin > 1")
qplot

ggsave('qplot.png', width = 5, height = 5)


