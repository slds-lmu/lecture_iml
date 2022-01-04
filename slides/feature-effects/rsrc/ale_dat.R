# DATA -------------------------------------------------------------------------

set.seed(1)
n = 5000
x <- runif(n, min = 0, max = 1)
x1 <- x + rnorm(n, 0, 0.05)
x2 <- x + rnorm(n, 0, 0.05)
y = x1 + x2^2 + rnorm(n, 0, 0.1)
DAT = data.frame(y, x1, x2)

test.ind = sample(1:n, size = 0.5*n)
test = DAT[test.ind, ]
DAT = DAT[-test.ind, ]