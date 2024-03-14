library(readr)
datadata <- read.csv("C:/Users/afrad/Downloads/example for hm.csv", header = TRUE)

#plot(df$y ~ df$grp, col = df$grp,
#     main = "Scatter Plot of y by grp",
#     xlab = "Group", ylab = "y")

#model <- lm(y ~ grp, data = df)
#residuals <- resid(model)
# plot(residuals)


library("rjags")

jags_model= "model {
      
      for (i in 1:length(y)){
      
      y[i] ~ dnorm(theta[grp[i]], Sig)
      
      }
      
      for(j in 1:max(grp)){
      theta[j] ~ dnorm(mu,Tao)
      }
      
      sig ~ dgamma(2/2, 2.1/2)
      mu ~ dnorm(0,1e6)
      tao ~ dgamma(1/2, 1.3/2)
      Sig=1/sig
      Tao=1/tao
}"

set.seed(72)

jag_data= as.list(datadata)


prams=c("theta", "mu","sig", "tao")

jag_model= jags.model(textConnection(jags_model), data = jag_data, n.chains = 3)

update(jag_model, 1e3)

Mod_sim = coda.samples(model=jag_model,
                       variable.names=prams,
                       n.iter=5e3)
Mod_csim = as.mcmc(do.call(rbind, Mod_sim))

summary(Mod_sim)

#residuals from non bayesian

means_anova = tapply(datadata$y, INDEX=datadata$grp, FUN=mean)

plot(means_anova)

pm_params = colMeans(Mod_csim)
pm_params
boxplot(pm_params)
points(pm_params[4:8], col="red")


plot(y~grp, data = datadata)


#Residuals















