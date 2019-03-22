Q <- matrix(c(0, 1, 0.3, 0.7), nrow =2, byrow=T)
m <- 4

power <- function(q, p) 
{
    r = q
    for (i in seq(1, p - 1))
    {
        #print(i)
        r = r %*% q
    }
    r
}

q <- power(Q, 3)
ans3 <- q[1, 2]
ans3


q <- power(Q, 500)
ans4 = q[1, ]
ans4


ans5 <- q[1, 2]
ans5
