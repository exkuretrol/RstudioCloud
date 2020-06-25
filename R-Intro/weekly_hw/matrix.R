a <- matrix(c(2, 2, 1,
                 1, 3, 2,
                -2, 1, 2), nrow = 3, byrow = TRUE)
det(a)

a %*% solve(a)
solve(a) %*% a


b <- c(1, 2, 3) 
# x = inv_a * b
x <- solve(a) %*% c(1, 2, 3) 

# a * x = b
all.equal(a %*% x %>% as.vector(), b)
