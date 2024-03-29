install.packages("tidyverse")

library(tidyverse)

booking <- read_csv("Tidyverse/bookings.csv")
booking
head(booking)
tail(booking)
subset <- select(booking, room_nights, review_score) #can add more column
subset2 <- select(booking, -room_nights)
subset
subset2



subset3 <- select (booking, room_nights)
subset3
subset4 <- filter(subset3, room_nights<6)
subset4
head(subset4, 2)

# Pipe operator

function(x,y) # same as
x %>% function(y)

c(1,2,3) %>% sum()
c(1,2,3, NA) %>% sum(na.rm=TRUE)
booking %>% head(2)

head(
  select(
    filter(
      booking, price_per_night<80
      ),
    price_per_night , room_nights
  ),
  2
)

booking %>% filter(price_per_night <80) %>% select(price_per_night , room_nights) %>% head(2)



booking %>% ggplot(aes(x=price_per_night, y=review_score)) + geom_point()



booking %>% 
  filter(room_nights <=7, status=="stayed") %>% 
  select(price_per_night, review_score) %>% 
  ggplot(aes(price_per_night, review_score)) +
  geom_point()

subset6= booking %>% 
  mutate(total_price = price_per_night + room_nights)
#if column name does not exist it will create, or it will replace existing column
subset6

booking %>% 
  mutate(property_id=as.integer(property_id))

subset7=booking %>% 
  mutate(total_price = price_per_night + room_nights) %>% 
  summarise(
    n=n(),
    n_stayed= sum(status=="stayed"),
    mean=mean(total_price)
  )
subset7

subset8= booking %>% 
  group_by(for_business) %>% 
  summarise(
    n=n(),
    review_score= mean(review_score, na.rm=TRUE)
  )
 
subset9= booking %>%  
  filter(for_business=="TRUE")
subset9

booking <- read_csv("Tidyverse/bookings.csv")
properties <- read_csv("Tidyverse/properties.csv")
d <- booking %>% full_join(properties)

d %>%  drop_na() # remove all the row with missing value
d %>% drop_na(review_score) # remove missing value only on review score column 
properties
booking

#reshaping data

day_order <- c("sun", "mon", "tue","wed", "thu", "fri", "sat")


checkin_out <- d %>% 
  count(destination , checkin_day) %>% 
  mutate(checkin_day = factor(checkin_day, levels=day_order))

p<- checkin_out %>% spread(checkin_day, n)

rev <- p %>% gather(day,n, sun,mon,tue,wed,thu,fri,sat)




# Create a vector of numbers
x <- c(1, 2, 3, 4, 5)

# Use if_else() to check if each element of x is greater than 3
result <- if_else(x > 3, "greater", "less or equal")

# Print the result
print(result)

library(dplyr)

# Create a sample dataframe
df <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(6, 7, 8, 9, 10)
)

# Apply if_else() to create a new column based on a condition
df <- df %>%
  mutate(new_column = if_else(x > 3, "greater", "less or equal"))

# Print the modified dataframe
print(df)



review_by_group <- d %>% 
  group_by(property_id, for_business) %>% #just group it for further use does not change dataset or make subset
  summarise(review_score=mean(review_score, na.rm = TRUE))#calculates the mean of the review_score variable for each group
review_by_group

wide_review <- review_by_group %>% 
  mutate(for_business=if_else(for_business, "business", "tourist")) %>% 
  spread(for_business, review_score)

wide_review %>% 
  ggplot(aes(business, tourist)) + geom_point()

review_by_group  
#applyfunction cheat sheet


filter(booking, property_id==1001)











