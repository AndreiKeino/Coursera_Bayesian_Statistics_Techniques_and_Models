Q <- matrix(c(0, 1, 0.3, 0.7), nrow =2, byrow=T)
m <- 4

power <- function(q, p) 
{
    r = q
    for (i in 1:p)
    {
        #print(i)
        r = r %*% q
    }
    r
}

q <- power(Q, 2)
q

ans3 <- q[1, 2]
ans3
