# Define a vector of lambda values
lambdas <- c(2, 4, 6)

# Generate a random number from a Poisson distribution for each lambda value
random_values <- rpois(length(lambdas), lambda = lambdas)

# Print the generated values
print(random_values)

X ~ Pois(2*15)
ppois(21, 30)
lemda <- 1.5-(0.3*0.8)+(1.0*1.2)
lam = exp(lemda)
intv <- rpois(n=1, lam)


data("badhealth")
any(is.na("badhealth"))
hist(badhealth$numvisit, breaks=20)
min(badhealth$numvisit)
sum(badhealth$numvisit==0)

mod_string = " model {
for (i in 1:length(numvisit)) {
numvisit[i] ~ dpois(lam[i])
log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
}
int ~ dnorm(0.0, 1.0/1e6)
b_badh ~ dnorm(0.0, 1.0/1e4)
b_age ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)

mod_csim = as.mcmc(do.call(rbind, mod_sim))

plot(density(mod_csim[,"b_badh"]))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic1 = dic.samples(mod, n.iter=1e3)

dic
dic1

y3 = rpois(n=1e5, lambda=2*15)
plot(table(factor(y3, levels=0:18))/1e5, pch=2, 
     ylab="posterior prob.", xlab="visits")
mean(y3<21)
