library("car")  # load the 'car' package
data("Anscombe")  # load the data set

library("rjags")

mod_string <- " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data_jags = as.list(Anscombe)

set.seed(5)

params1 = c("b", "sig")

inits1 = function() {
    inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod_string2 <- " model {
    for (i in 1:length(education)) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
}

b0 ~ dnorm(0.0, 1.0/1.0e6)
for (i in 1:2) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
## Initial guess of variance based on overall
## variance of education variable. Uses low prior
## effective sample size. Technically, this is not
## a true 'prior', but it is not very informative.
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

inits2 = function() {
    inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod_string3 <- " model {
    for (i in 1:length(education)) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
}

b0 ~ dnorm(0.0, 1.0/1.0e6)
for (i in 1:3) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
## Initial guess of variance based on overall
## variance of education variable. Uses low prior
## effective sample size. Technically, this is not
## a true 'prior', but it is not very informative.
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "



mod1 = jags.model(textConnection(mod_string), data=data_jags, inits=inits1, n.chains=3)
mod2 = jags.model(textConnection(mod_string2), data=data_jags, inits=inits2, n.chains=3)
mod3 = jags.model(textConnection(mod_string3), data=data_jags, inits=inits1, n.chains=3)

update(mod1, 1000) # burn-in
update(mod2, 1000) # burn-in
update(mod3, 1000) # burn-in

iter <- 1e4

dic.samples(mod1, n.iter=iter)
dic.samples(mod2, n.iter=iter)
dic.samples(mod2, n.iter=iter)

dic.samples(mod1, n.iter=iter)

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains
incom_coef <- mod1_csim[ , 1]
ans_5 <- mean(incom_coef > 0)
ans_5

summary(mod1_sim)




