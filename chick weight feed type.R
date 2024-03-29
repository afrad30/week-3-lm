library("datasets")
data("ChickWeight")
#?ChickWeight

any(is.na(ChickWeight))            # for any missing value
dataset=ChickWeight
boxplot(Time ~ Chick, data=dataset)


dataset <- dataset[-c(195, 196), ] # REMOVING INCOMPLETE DATA
row.names(dataset) <- NULL         # REINDEXING

# STEP 1. ORDER FACTOR TO NUMETIC
chick_numeric <- as.numeric(as.character(dataset$Chick)) # ordered factor to numeric first 

# Step 2: Compress Chick into a range of 1 to 48
unique_ids <- unique(chick_numeric)
id_mapping <- match(unique_ids, unique_ids)
chick_numeric <- id_mapping[match(chick_numeric, unique_ids)]

# numeric to order factor
dataset$Chick=chick_numeric
dataset$Chick <- factor(dataset$Chick, levels = unique(dataset$Chick), ordered = TRUE)# again numeric to ordered factor

summary(dataset)
boxplot(weight ~ Chick, data=dataset)
boxplot(weight ~ Time, data=dataset)
boxplot(weight ~ Diet, data=dataset)



chick_numeric <- as.numeric(as.character(dataset$Chick))
chick_1_data <- subset(chick_numeric, Chick == 1)
print(chick_1_data)

