library(digest)
library(purrr)
library(tidyverse)

digest(1234)

x= c(1,2,3,4)

map(x, ~ . + 1)
map(x, ~ digest(.))

#map always return a list

booking <- read_csv("Tidyverse/bookings.csv")
properties <- read_csv("Tidyverse/properties.csv")
d <- booking %>% full_join(properties)