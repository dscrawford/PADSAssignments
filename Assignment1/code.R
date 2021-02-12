df <- read.csv('housing.csv')
View(df)

require(corrplot)
require(ggplot2)

# Percentage of null values
sapply(df, function(x) sum(is.na(x))) / nrow(df)
# Remove unwanted null values
df <- na.omit(df[-ncol(df)])

M <- cor(df)
corrplot(M, method="circle")

# Geographical map

# Histogram
hist(df$housing_median_age, breaks = 100, main='Histogram of housing_median_age')
hist(df$total_rooms, breaks=100, main='Histogram of total_rooms')

ggplot(df, aes(x = length(df))) +
  geom_histogram(fill = "white", colour = "black")


plot(housing_median_age, median_income, data=df)

lm.fit(df$median_income~df$housing_median_age+df$total_rooms, data=df)