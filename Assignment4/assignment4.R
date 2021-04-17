require(graphics)
require(jpeg)
require(RCurl)

url <- "https://external-content.duckduckgo.com/iu/?u=http%3A%2F%2Fcdn.wccftech.com%2Fwp-content%2Fuploads%2F2017%2F01%2FZelda-Breath-of-the-Wild-screenshots6.jpg&f=1&nofb=1"
readImage <- readJPEG(getURLContent(url, binary=TRUE))

# KMeans
dm <- dim(readImage)
rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

plot(y ~ x, data=rgbImage, main="Base picture",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")

k <- 10
clusteredImage <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], centers = k)
clusterColour <- rgb(clusteredImage$centers[clusteredImage$cluster, ])

plot(y ~ x, data=rgbImage, main="Picture with 10 kmeans centers",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")

# PCA
r <- rgbImage$r.value
g <- rgbImage$g.value
b <- rgbImage$b.value

r.pca <- prcomp(r, center = FALSE)
g.pca <- prcomp(g, center = FALSE)
b.pca <- prcomp(b, center = FALSE0)