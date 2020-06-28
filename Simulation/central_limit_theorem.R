library(TSA)
my.stat <- function(x) {
  semi <- temp <- (summary(x)[5]-summary(x)[2])/2
  attr(semi, "names") <- NULL
  stat <- c(mean(x),
            median(x),
            as.numeric(names(table(x))[which.max(table(x))]),
            mean(c(max(x), min(x))),
            semi,
            var(x),
            sd(x),
            max(x)-min(x),
            IQR(x),
            var(x)/mean(x),
            skewness(x),
            kurtosis(x))
  names(stat) <- c("mean",
                   "median",
                   "mode",
                   "midrange",
                   "semi",
                   "variance",
                   "sd",
                   "R",
                   "IQR",
                   "CV",
                   "skewness",
                   "kurtosis")
  return(stat)
}
my.stat(1:10)

#' default.par <- par()

#' Unifiorm Distribution U(0, 1)
Umat <- matrix(runif(36*1000), nrow = 1000)
n <- c(1, 2*2, 3*3, 4*4, 5*5, 6*6)
CLT.U <- matrix(numeric(1000 * length(n)), nrow = 1000)
for (i in 2:6){
  CLT.U[, i] <- apply(Umat[, 1:n[i]], 1, mean)
}
par(mfrow = c(2, 3))
for (i in 1:6){
  hist(CLT.U[, i], xlim = c(0, 1), xlab = "",
       main = paste0("U(0, 1) sample sizes = ", n[i]))
}
#' par(default.par)
resultU <- apply(CLT.U, 2, my.stat)
CLT.U[, 1] <- Umat[, 1]
colnames(resultU) <- paste("n =", n)
View(resultU)

#' Exponential Distribution E(1)
Emat <- matrix(rexp(36 * 1000), nrow = 1000)
n <- c(1, 2*2, 3*3, 4*4, 5*5, 6*6)
CLT.E <- matrix(numeric(1000 * length(n)), nrow = 1000)
for (i in 2:6){
  CLT.E[, i] <- apply(Emat[, 1:n[i]], 1, mean)
}
CLT.E[, 1] <- Emat[, 1]
#' View(resultE)
for (i in 1:6){
  hist(CLT.E[, i], xlim = c(0, 5), xlab = "", 
       main = paste0("E(1) sample sizes = ", n[i]))
}
resultE <- apply(CLT.E, 2, my.stat)
colnames(resultE) <- paste("n =", n)
View(resultE)

#' Normal Distribution N(0, 2)
n <- c(1, 2*2, 3*3, 4*4, 5*5, 6*6)
sd <- 2
nosim <- 1000
CLT.N <- sapply(n , function(n) {
  x <- rnorm(nosim * n, sd=sd)
  apply(matrix(x, nosim), 1, mean)
})
par(mfrow = c(2, 3))
for (i in 1:length(n)){
  hist(CLT.N[, i], xlim = c(-6, 6), xlab = "", 
       main = paste0("N(0, 2) sample sizes = ", n[i]))
}
resultN <- apply(CLT.N, 2, my.stat)
colnames(resultN) <- paste("n =", n)
View(resultN)

#' Binomial Distribution B(n, 0.5) 
n <- c(1, 2*2, 3*3, 4*4, 5*5, 6*6)
p <- .5
nosim <- 1000
CLT.Bi <- sapply(n , function(n) {
  x <- sample(0:1, nosim * n, replace=TRUE, prob=c(p, 1-p))
  apply(matrix(x, nosim), 1, mean)
})
par(mfrow = c(2, 3))
for (i in 1:length(n)) {
  hist(CLT.Bi[, i], xlim = c(0, 1), xlab = "",
       main = paste0("B(", n[i], ", 2) sample sizes = ", n[i]))
}
resultBi <- apply(CLT.Bi, 2, my.stat)
colnames(resultBi) <- paste("n =", n)
View(resultBi)

#' Ref: https://github.com/resendedaniel/math/tree/master/17-central-limit-theorem