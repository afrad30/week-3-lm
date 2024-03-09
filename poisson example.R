library("rjags")
library("coda")

mod_stringx = "model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois(lam[i])
        log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1e6)
    
    for (i in 1:2){
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
        }
}"

set.seed(170)

# Correcting typo in dataframe name
data_jagsx = list(calls=poisoon_callers$calls, age=poisoon_callers$age, isgroup2=poisoon_callers$isgroup2)

paramsx = c("b0", "b")

initsx = function() {
  list("b" = rnorm(, 0.0, 100.0), "b0" = rnorm(1, 0.0, 100.0))
}

modx = jags.model(textConnection(mod_stringx), data=data_jagsx, inits=initsx, n.chains=3)

update(modx, 1000) # burn-in
