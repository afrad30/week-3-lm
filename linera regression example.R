library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

plot(education ~ income+young+urban, data=Anscombe)

hist(Anscombe$income)


lmod=lm(education ~ income + young + urban, data=Anscombe)

plot(lmod)




summary(lmod)
