# 程式語言_week05

# 梯型積分的應用，使用函數 f(z)=exp(-z^2/2)/sqrt(2*pi) ,
#  A. 從a=-3 積分至 b=3;
#  B. 從a=-2 積分至 b=2;
#  C. 從a=-1 積分至 b=1;
# 梯形面積為 (上底+下底)*高 /2
# 使用10000塊切割
# 下底位置使用lowerx; 計算出來的函數值為 lowerf
# 上底位置使用upperx; 計算出來的函數值為 upperf
# 高為deltax = (b-a)/ k

trapezoidal_rule <- function(a, b, n) {
  deltax <- (b-a)/n
  lowerx <- seq(a, b-deltax, by = deltax)
  upperx <- seq(a+deltax, b, by = deltax)
  lowery <- my_function(lowerx)
  uppery <- my_function(upperx)
  area <- sum((uppery+lowery)*deltax /2)
  return(area)
}

my_function <- function(x) {
  # PDF of standard normal distribution
  y <- exp(-(1/2)*x^2)/sqrt(2*pi)
  return(y)
}
