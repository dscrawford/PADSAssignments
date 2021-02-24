# install.packages(c('caret'))

df <- read.csv('housing.csv')
View(df)

require(corrplot)
require(ggplot2)
library(scales)
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
M <- cor(data)
corplot <- corrplot(M, method="circle", tl.cex=1)
cormat <- as.data.frame(corrplot(M,method = "number"))
row.names(cormat)[abs(cormat$median_house_value) > 0.15]

#Plots to check linearity of each var
plot_line <- function (x) {
  plot(as.vector(df[[x]]),
       as.vector(df$median_house_value),
       main=paste('Plot of median_house_value vs ', x),
       xlab=x,
       ylab='median_house_value')
  abline(lm(df$median_house_value ~ df[[x]]), lwd=3, col="red")
}

lapply(colnames(df[-which(colnames(df) == 'median_house_value')]), 
       FUN=plot_line
) 

# Train/test split
sample <- sample.int(n = nrow(df), size = floor(0.75 * nrow(data)), replace=F)
train <- data[sample, ]
test <- data[-sample, ]

# Starting fits based on information
# First first based on median income
lm.fit1 = lm(median_house_value~median_income, data=train)
summary(lm.fit1)

# Geographical map
mhv_map = ggplot(train, aes(x = longitude, y = latitude, color = median_house_value)) +
  geom_point(aes(size = population), alpha = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Median House Value wrt Population Map") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Spectral", labels = comma) +
  labs(color = "Median House Value (in $USD)", size = "Population")

mi_map = ggplot(train, aes(x = longitude, y = latitude, color = median_income)) +
  geom_point(aes(size = population), alpha = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Median Income wrt Population Map") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Spectral", labels = comma) +
  labs(color = "Median Income (in tens of thousands of $USD)", size = "Population")

hs_map = ggplot(train, aes(x = longitude, y = latitude, color = households)) +
  geom_point(aes(size = population), alpha = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Total Number of Households wrt Population Map") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Spectral", labels = comma) +
  labs(color = "Total Number of Households", size = "Population")

mhv_map
mi_map
hs_map

# According to the correlation matrix and table, we can see that median_income
# is the only highly correlated variable with the median_house_value. We try our
# our first model with single linear regression.
plot(df$median_income, df$median_house_value)
abline(lm(train$median_house_value ~ train$median_income), lwd=3, col="red")

fit1 <- lm(train$median_house_value ~ train$median_income, train)
summary(fit1)
confint(fit1, level = 0.90)

# Next model (ocean proximity)
fit.all <- lm(median_house_value ~ ., train)
summary(fit.all)
confint(fit.all, level = 0.90)

fit2 <- lm(median_house_value ~ (.)^2, train)
summary(fit2)



