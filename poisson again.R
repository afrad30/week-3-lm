model_string <- "
model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois(days_active[i] * lam[i])
        log(lam[i]) = b0 + b1*age[i] + b2*isgroup2[i]
    }
    
    b0 ~ dnorm(0, 1.0/100)
    b1 ~ dnorm(0, 1.0/100)
    b2 ~ dnorm(0, 1.0/100)
}
"
library("rjags")
library("coda")

# Assuming data_frame_name is your dataframe
data_jags=as.list(poisoon_callers)

initial_values <- function(){
  list("b0" = rnorm(1, 0, 10), "b1" = rnorm(1, 0, 10), "b2"= rnorm(1, 0, 10))
}

# Compile the model
model <- jags.model(textConnection(model_string), data = data_jags, inits = initial_values, n.chains = 3)

# Burn-in
update(model, 1000)

# MCMC Sampling
samples <- coda.samples(model, variable.names = c("b0", "b1", "b2"), n.iter = 5000)

mod_csim = as.mcmc(do.call(rbind, samples))

summary(samples)

b2_samples <- as.matrix(samples)[, "b2"]
prob_b2_gt_0 <- mean(b2_samples > 0)
round(prob_b2_gt_0, 2)


