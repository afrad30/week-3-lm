library(utils)


url <- "https://d3c33hcgiwev3.cloudfront.net/_25ad8f0154906d5d83efc640647beb4b_cookies.dat?Expires=1710028800&Signature=HfKqQSYa4gxWZWOEwyDtA-wf~WkqiscD2hry5gA0ElrCzSwtX~4f6R6tBLWDa7wF~xVsP8MI2xMiiT~sVc47G-e9XLMmeeDy9kwMuDkYwYtU4F5CnENwdB2-yf8Ta3OAQT1KcQt4Sbkj34Az7~ibdYjp6dKxQPB9vH5RRCHx~~0_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"

dat <- read.table(url, header = TRUE, sep = " ")

table(dat$location)

hist(dat$chips)

boxplot(chips ~ location, data=dat)



set.seed(112)
n_sim = 500
alpha_pri = rexp(n_sim, rate=1.0/2.0)
beta_pri = rexp(n_sim, rate=5.0)

mu_pri = alpha_pri/beta_pri
sig_pri = sqrt(alpha_pri/beta_pri^2)

summary(mu_pri)

summary(sig_pri)

lam_pri = rgamma(n=n_sim, shape=alpha_pri, rate=beta_pri)
summary(lam_pri)

#alternative (lam_pri = rgamma(n=5, shape=alpha_pri[1:5], rate=beta_pri[1:5]))

y_pri = rpois(n=150, lambda=rep(lam_pri, each=30))