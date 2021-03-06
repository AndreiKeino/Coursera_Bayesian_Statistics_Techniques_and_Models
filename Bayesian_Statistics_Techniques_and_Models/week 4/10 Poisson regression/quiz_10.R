if(FALSE)
{
# Q_1
ans_1 <- exp(1.5 + 0.8 * (- 0.3) + 1.2 * 1.0)
ans_1

# Q_2
library("COUNT")
data("badhealth")
?badhealth
head(badhealth)
any(is.na(badhealth))

hist(badhealth$numvisit, breaks=20)

plot(jitter(log(numvisit)) ~ jitter(age), data=badhealth, subset=badh==0, xlab="age", ylab="log(visits)")
points(jitter(log(numvisit)) ~ jitter(age), data=badhealth, subset=badh==1, col="red")

library("rjags")
mod_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age", "b_intx")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim, ask=F)


gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

dic = dic.samples(mod, n.iter=1e3)

mod_string2 = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] 
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
} "

params2 = c("int", "b_badh", "b_age")

mod2 = jags.model(textConnection(mod_string2), data=data_jags, n.chains=3)
update(mod2, 1e3)

mod_sim2 = coda.samples(model=mod,
                       variable.names=params2,
                       n.iter=5e3)
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2))

## convergence diagnostics
plot(mod_sim2, ask=F)


gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
autocorr.plot(mod_sim2)
effectiveSize(mod_sim2)

dic2 = dic.samples(mod2, n.iter=1e3)

ans_2 <- dic - dic2 
# dic is DIC for the old model, dic2 is DIC for the new model
ans_2

# Q 4

ans_4 <- ppois(21, 30)
ans_4
}


# Q 5

dat = read.csv(
    file="C:/! Coursera/Bayesian statistics/Bayesian_Statistics_Techniques_and_Models/week 4/10 Poisson regression/callers.csv", header=TRUE)
pairs(dat)
boxplot(calls/days_active~isgroup2,data=dat, main="Calls by group", xlab="isgroup2", 
        ylab="(number of calls) / days_active")

# Q 7

mod_string = " model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois( days_active[i] * lam[i] )
        log(lam[i]) = b0 + b_age*age[i] + b_group*isgroup2[i]
    } 
    b0 ~ dnorm(0.0, 1.0/1e2)
    b_age ~ dnorm(0.0, 1.0/1e2)
    b_group ~ dnorm(0.0, 1.0/1e2)
} "

data_jags = as.list(dat)

params = c("b0", "b_age", "b_group")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

head(mod_csim)

head(mod_csim[, 3])

b_group <- mod_csim[, 3]
head(b_group)

ans_7 <- mean(b_group > 0)
ans_7


