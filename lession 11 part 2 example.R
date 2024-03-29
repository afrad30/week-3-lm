library("MASS")
data("OME")

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)

mod_glm = glm(Correct/Trials ~ ID + Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")

X = model.matrix(mod_glm)[,-1]# -1 removes the column of 1s for the intercept

data= as.data.frame(X)

max_id <- max(data$ID, na.rm = TRUE)
data$ID[is.na(data$ID)] <- max_id + 1

# Step 2: Compress IDs into a range of 1 to 63
unique_ids <- unique(data$ID)
id_mapping <- match(unique_ids, unique_ids)
data$ID <- id_mapping[match(data$ID, unique_ids)]

data

library("rjags")
library("coda")

mod_string3 = " model {
	
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = a[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	  for (j in 1:63) {
    a[j] ~ dnorm(mu, prec_a)
  }
	
	mu ~ dnorm(0, 100)
  prec_a ~ dgamma(1/2, 1/2)
  tau = sqrt( 1.0 / prec_a )
  
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

data_jags3 = as.list(data)
data_jags3$y = dat$Correct
data_jags3$n = dat$Trials

params3 = c("a","b", "mu", "tau")

mod3 = jags.model(textConnection(mod_string3), data=data_jags3, n.chains=3)

update(mod3, 1e3)

mod1_sim3 = coda.samples(model=mod3,
                         variable.names = params3,
                         n.iter=5e3)
summary(mod1_sim3)

mod1_csim3 = as.mcmc(do.call(rbind, mod1_sim3))

summary(mod1_csim3)

r= raftery.diag(mod1_csim3)

## convergence diagnostics
plot(mod1_sim3, ask=TRUE)

gelman.diag(mod1_sim3)
autocorr.diag(mod1_sim3)
autocorr.plot(mod1_sim3)
effectiveSize(mod1_sim3)

## calculate DIC
dic1 = dic.samples(mod3, n.iter=1e3)
dic1

summary(mod1_csim3)

par(mfrow=c(3,2))
densplot(mod1_csim[,1:6], xlim=c(-3.0, 3.0))

colnames(X) # variable names

(pm_coef = colMeans(mod2_csim))

pm_Xb = pm_coef["int"] + X[,c(1,4,6)] %*% pm_coef[1:3]

phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)



