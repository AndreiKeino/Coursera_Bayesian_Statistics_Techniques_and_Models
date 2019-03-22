set.seed(5)
m <- 100000

y <- rbeta(m, 5, 3)
y1 <- y / (1 - y)
ans5 <- mean(y1)
ans5

y1 <- y1 > 1
ans6 <- mean(y1)
ans6

ns <- rnorm(m, 0, 1)
ans7 <- quantile(ns, 0.3)
ans7

ans8 <- sqrt(5.2 / 5000) 
ans8

