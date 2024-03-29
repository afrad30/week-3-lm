data("warpbreaks")
?warpbreaks
head(warpbreaks)

table(warpbreaks$wool, warpbreaks$tension)

boxplot(breaks ~ wool + tension, data=warpbreaks)
