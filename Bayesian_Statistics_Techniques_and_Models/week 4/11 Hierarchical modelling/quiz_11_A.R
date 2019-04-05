fname <- "C:/! Coursera/Bayesian statistics/Bayesian_Statistics_Techniques_and_Models/week 4/11 Hierarchical modelling/pctgrowth.csv"

dat <- read.csv(file=fname, header=TRUE)

head(dat)

library("rjags")
par(ask=F)

mod_string = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(theta[grp[i]], sigma)
}

for (j in 1:max(grp)) {
theta[j] ~ dnorm(mu, tau)
}

mu ~ dnorm(0, 1e6)
tau ~ dgamma(1/2, 1 * 3/2)
sigma ~ dgamma(2/2, 2 * 1/2)

sig = 1 / sqrt(sigma)

} "

set.seed(5)

data_jags = as.list(dat)

params = c("theta", "mu", "sig")

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

## compute DIC
dic = dic.samples(mod, n.iter=1e3)

# Q 4

pm_params = colMeans(mod_csim)
means_theta <- pm_params[3:7]

means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)

plot(means_anova)
points(means_theta, col="red") ## where means_theta are the posterior point estimates for the industry means.




