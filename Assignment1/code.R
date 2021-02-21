install.packages(c('caret'))


df <- read.csv('housing.csv')
View(df)

require(corrplot)
require(ggplot2)
library(caret)

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
dummy <- dummyVars(" ~ .", data=df)
df <- data.frame(predict(dummy, newdata=df))

# Create correlation plot
data = df[, -10]
M <- cor(data)
corplot <- corrplot(M, method="circle", tl.cex=1)
cormat <- as.data.frame(corrplot(M,method = "number"))
row.names(cormat)[abs(cormat$median_house_value) > 0.50]

#Plots to check linearity of each var
plot(df$longitude, df$median_house_value)
abline(lm(df$median_house_value ~ df$longitude), lwd=3, col="red")

plot(df$latitude, df$median_house_value)
abline(lm(df$median_house_value ~ df$latitude), lwd=3, col="red")

plot(df$housing_median_age, df$median_house_value)
abline(lm(df$median_house_value ~ df$housing_median_age), lwd=3, col="red")

plot(df$total_rooms, df$median_house_value)
abline(lm(df$median_house_value ~ df$total_rooms), lwd=3, col="red")

plot(df$total_bedrooms, df$median_house_value)
abline(lm(df$median_house_value ~ df$total_bedrooms), lwd=3, col="red")

plot(df$population, df$median_house_value)
abline(lm(df$median_house_value ~ df$population), lwd=3, col="red")

plot(df$households, df$median_house_value)
abline(lm(df$median_house_value ~ df$households), lwd=3, col="red")

plot(df$median_income, df$median_house_value)
abline(lm(df$median_house_value ~ df$median_income), lwd=3, col="red")

#pairs(df, col = "dodgerblue") #Takes too long for me to load

# Geographical map
# put code here

#Predicting the median_house_value

# According to the correlation matrix and table, we can see that median_income
# is the only highly correlated variable with the median_house_value. We try our
# our first model with single linear regression.
plot(df$median_income, df$median_house_value)
abline(lm(df$median_house_value ~ df$median_income), lwd=3, col="red")

fit1 <- lm(df$median_house_value ~ df$median_income, df)
summary(fit1)
confint(fit1, level = 0.90)

plot(df$median_income, df$median_house_value)
abline(fit1, lwd=3, col="red")


#From this model, we see that the residual range is about 100k, meaning there are 
#some errors in our model. The T-val is high and the P-val is low, showing that this
#is indeed a significant predictor, which belongs in the model. The adjusted R^2 is .4738.
#We believe this is a good base value and we can check for improvement with the addition 
#of more variables. 

# Now we can add other lower correlated variables that we feel may help the model improve.
# We can first add latitude and longitude. 
plot(df$longitude, df$median_house_value)
abline(lm(df$median_house_value ~ df$longitude), lwd=3, col="red")

plot(df$latitude, df$median_house_value)
abline(lm(df$median_house_value ~ df$latitude), lwd=3, col="red")

fit2 <- lm(df$median_house_value ~ df$median_income + df$longitude + df$latitude, df)
summary(fit2)
confint(fit2, level = 0.90)

plot(df$median_income + df$longitude + df$latitude, df$median_house_value)
abline(lm(df$median_house_valu~ I(df$median_income + df$longitude + df$latitude)) , lwd=3, col="red")

#From this model, we see that the residual range is also about 100k. The 
#latitude/longitude both have small Tvalues unlike the median_income. However the
#pvals for latitude/longitude are small and all 3 vars have 3 star significance. 
#The Adjusted r^2 is higher than fit1 by .1103 with .5841. 

# Now we can try adding housing_median_age as the predictor.
plot(df$housing_median_age, df$median_house_value)
abline(lm(df$median_house_value ~ df$housing_median_age), lwd=3, col="red")

fit3 <- lm(df$median_house_value ~ df$median_income + df$longitude + df$latitude + df$housing_median_age, df)
summary(fit3)
confint(fit3, level = 0.90)

plot(df$median_income + df$longitude + df$latitude + df$housing_median_age, df$median_house_value)
abline(lm(df$median_house_value ~ I(df$median_income + df$longitude + df$latitude + df$housing_median_age)) , lwd=3, col="red")

#In this model, the residual range is also around 100k. The t values are positive and high for median_income
#and housing_median_age. The longitude/latitude are still negative and low, but all the pvals are small
#and the variables all have 3 star significance codes. The adjusted r^2 is .594. 

#Now we can try adding total_rooms to the model.
plot(df$total_rooms, df$median_house_value)
abline(lm(df$median_house_value ~ df$total_rooms), lwd=3, col="red")

fit4 <- lm(df$median_house_value ~ df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms, df)
summary(fit4)
confint(fit4, level = 0.90)

plot(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms, df$median_house_value)
abline(lm(df$median_house_value~ I(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$medianf_house_value)) , lwd=3, col="red")

#Info of model 4

# Next model (total_bedrooms)
fit5 <- lm(df$median_house_value ~ df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms + df$total_bedrooms, df)
summary(fit5)
confint(fit5, level = 0.90)

plot(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms + df$total_bedrooms, df$median_house_value)
abline(lm(df$median_house_value ~ I(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$medianf_house_value + df$total_bedrooms)) , lwd=3, col="red")

#Info of model 5

# Next model (population)
fit6 <- lm(df$median_house_value ~ df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms + df$total_bedrooms + df$population, df)
summary(fit6)
confint(fit6, level = 0.90)

plot(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms + df$total_bedrooms + df$population, df$median_house_value)
abline(lm(df$median_house_value ~ I(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$medianf_house_value + df$total_bedrooms + df$population)) , lwd=3, col="red")

#Info of model 6

# Next model (households)
fit7 <- lm(df$median_house_value ~ df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms + df$total_bedrooms + df$population + df$households, df)
summary(fit7)
confint(fit7, level = 0.90)

plot(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms + df$total_bedrooms + df$population + df$households, df$median_house_value)
abline(lm(df$median_house_value ~ I(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$medianf_house_value + df$total_bedrooms + df$population + df$households)) , lwd=3, col="red")

#Info of model 7

# Next model (ocean proximity)
fit8 <- lm(df$median_house_value ~ df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms + df$total_bedrooms + df$population + df$households 
           + df$ocean_proximity.1H.OCEAN + df$ocean_proximityINLAND + df$ocean_proximityISLAND + df$ocean_proximityNEAR.BAY + df$ocean_proximityNEAR.OCEAN, df)
summary(fit8)
confint(fit8, level = 0.90)

plot(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$total_rooms + df$total_bedrooms + df$population + df$households + df$ocean_proximity.1H.OCEAN + df$ocean_proximityINLAND + df$ocean_proximityISLAND + df$ocean_proximityNEAR.BAY + df$ocean_proximityNEAR.OCEAN, df$median_house_value)
abline(lm(df$median_house_value ~ I(df$median_income + df$longitude + df$latitude + df$housing_median_age + df$medianf_house_value + df$total_bedrooms + df$population + df$households + df$ocean_proximity.1H.OCEAN + df$ocean_proximityINLAND + df$ocean_proximityISLAND + df$ocean_proximityNEAR.BAY + df$ocean_proximityNEAR.OCEAN)) , lwd=3, col="red")

#info of model 8