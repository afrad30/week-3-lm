library("rjags")
library("car")
data("Anscombe")
#pairs(Anscombe)

mod_string = " model {
    
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]}
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

#data_jags <- as.list(Anscombe)

data0_jags <- list(
   y = data_jags$education,
   income = data_jags$income,
   young = data_jags$young,
   urban = data_jags$urban,
   n = nrow(Anscombe)
 )

set.seed(72)

params0= c("b", "sig")

inits0=function(){
  inits=list("b"=rnorm(3, 0, 100), prec=rgamma(1,1.0,1.0))
}

mod0=jags.model(textConnection(mod_string), data=data0_jags, inits = inits0, n.chains = 3)

update(mod0, 1000)


mod0_sim = coda.samples(model = mod0, variable.names = params0, n.iter = 100000)

mod0_csim = as.mcmc(do.call(rbind, mod0_sim))

#plot(mod0_csim)

#convergens


gelman.diag(mod0_sim)# understood, variance between chain and within chain

gelman.plot(mod0_sim)

autocorr.diag(mod0_sim)

effectiveSize(mod0_sim)

summary(mod0_csim)



# residuals from frequentist
lmod0= lm(education ~ income+young+urban, data=Anscombe)
plot(lmod0)

plot(resid(lmod0))
plot(predict(lmod0), resid(lmod0))

qqnorm(resid(lmod0))

#residuals from mcmc


dic.samples(mod0, n.iter = 100000)


#posterior mean

X = cbind(data0_jags$income, data0_jags$young, data0_jags$urban)
head(X)

pm_params0 = colMeans(mod0_csim)
yhat = drop(X %*% pm_params0[1:3])
mean(yhat)
resid1 = data0_jags$y - yhat
plot(resid1)
plot(yhat, resid1)
qqnorm(resid1)

sd(resid1)
par(mfrow=c(2,1))

(rownames(dat)[order(resid1, decreasing = TRUE)])
head(rownames(dat)[order(resid1, decreasing = TRUE)])


summary(mod0_sim)

variable_names <- colnames(as.matrix(mod0_sim[[1]]))
print(variable_names)

# Extracting income coefficient samples correctly
income_coeff_samples <- as.matrix(mod0_sim)[, "b[1]"]

# Calculate the posterior probability that the income coefficient is positive
post_prob_income_positive <- mean(income_coeff_samples > 0)

# Print the result, rounded to two decimal places
print(round(post_prob_income_positive, 3))

