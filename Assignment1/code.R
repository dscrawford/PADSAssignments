install.packages(c('mltools'))

df <- read.csv('housing.csv')
View(df)

require(corrplot)
require(ggplot2)

# Percentage of null values
sapply(df, function(x) sum(is.na(x))) / nrow(df)

# Remove unwanted null values
df <- na.omit(df)

# Display histograms of each variable
lapply(colnames(df[-which(colnames(df) == 'ocean_proximity')]), 
       FUN=function(x) hist(as.vector(df[[x]]),
                            main=paste('Histogram of ', x),
                            xlab=x,
                            breaks=100)
       )

# One hot encode ocean
library(mltools)
library(data.table)

df$ocean_proximity
# Create correlation plot
M <- cor(df)
corrplot(M, method="circle")

# Geographical map
# put code here



plot(housing_median_age, median_income, data=df)

lm.fit(df$median_income~df$housing_median_age+df$total_rooms, data=df)