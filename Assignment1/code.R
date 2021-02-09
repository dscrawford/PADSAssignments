df <- read.csv('housing.csv')
View(df)

# Percentage of null values
sapply(df, function(x) sum(is.na(x))) / nrow(df)

# Remove unwanted rows
df <- na.omit(df[-ncol(df)])

require(corrplot)
M <- cor(df)
corrplot(M, method="circle")


