library("datasets")
data("ChickWeight")

any(is.na(ChickWeight))


library(tidyverse)
library(purrr)

ckw = as_tibble(ChickWeight)
boxplot(Time ~ Chick, data=ckw)

#to see faulty data

faulty_ckw<- ckw %>%
  group_by(Chick) %>%
  summarise(NumberOfObservations = n()) %>% 
  filter(NumberOfObservations<12)

print(faulty_ckw)

#again, now keep completed data

chicks_to_remove <- ckw %>%
  group_by(Chick) %>%
  filter(n() < 12) %>%
  summarise() %>%
  pull(Chick) # This pulls out the Chick IDs to remove

# Now, filter these out from the main dataset
filtered_dataset <- ckw %>%
  filter(!(Chick %in% chicks_to_remove))

# Check the result
print(head(filtered_dataset))
utils::View(filtered_dataset)
boxplot(Time ~ Chick, data=filtered_dataset)

diet11 = filtered_dataset[filtered_dataset$Diet==1,]
diet22 = filtered_dataset[filtered_dataset$Diet==2,]
diet33 = filtered_dataset[filtered_dataset$Diet==3,]
diet44 = filtered_dataset[filtered_dataset$Diet==4,]

mean(diet11$weight)
mean(diet22$weight)
mean(diet33$weight)
mean(diet44$weight)

# Realtionship between weight and diet(4) grp over some period of time(21) of 50 chick
#given on doc
require(graphics)
coplot(weight ~ Time | Chick, data = filtered_dataset,
       type = "b", show.given = FALSE)



#primary inference
boxplot(weight ~ Diet, data=filtered_dataset)

lmod=lm(weight ~ Diet, data=filtered_dataset)
summary(lmod)
anova(lmod)

#jags model

library("rjags")
library("coda")

mod3_string = " model {
    for( i in 1:length(y)) {
        y[i] ~ dnorm(mu[Diet[i]], prec)
    }
    
    for (j in 1:4){
            mu[j] ~ dnorm(0.0, 1.0/1.0e6)
            }
    
    prec ~ dgamma(3/2.0, 3*1.0/2.0)
    sig = sqrt(1.0 / prec)
} "

str(filtered_dataset)

data3_jags = list(y=filtered_dataset$weight, Diet=filtered_dataset$Diet)

params3 = c("mu", "sig")

mod3 = jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1e3)

mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=5e3)
summary(mod3_sim)

mod3_csim = as.mcmc(do.call(rbind, mod3_sim))

summary(mod3_csim)

#plot(mod3_sim, ask=TRUE)

## convergence diagnostics
gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
effectiveSize(mod3_sim)
raftery.diag(mod3_sim)

dic2 = dic.samples(mod3, n.iter=1e3)
dic2








