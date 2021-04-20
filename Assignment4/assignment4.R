require(graphics)
require(jpeg)
require(RCurl)

url <- "https://external-content.duckduckgo.com/iu/?u=http%3A%2F%2Fcdn.wccftech.com%2Fwp-content%2Fuploads%2F2017%2F01%2FZelda-Breath-of-the-Wild-screenshots6.jpg&f=1&nofb=1"
readImage <- readJPEG(getURLContent(url, binary=TRUE))
download.file(url, "photo.jpg", mode="wb")

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

write_kmeans_picture <- function(k) {
    clusteredImage <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], centers = k)
    clusterColour <- clusteredImage$centers[clusteredImage$cluster, ]
    
    dim(clusterColour) <- dm
  
    
    writeJPEG(clusterColour, paste("photo_kmeans_", k, ".jpg", sep = ""))
}

sapply(c(3, 5, 10, 20, 30), write_kmeans_picture)

# PCA #
ncol(readImage)
nrow(readImage)

#Extracts individual color matricies
r <- readImage[,,1]
g <- readImage[,,2]
b <- readImage[,,3]

#PCA is performed on each color value matrix
r.pca <- prcomp(r, center = FALSE)
g.pca <- prcomp(g, center = FALSE)
b.pca <- prcomp(b, center = FALSE)

#Collects pca objects into a list
rgb.pca <- list(r.pca, g.pca, b.pca)

#Compress the image
for (i in seq.int(3, round(nrow(readImage) - 10), length.out = 5)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  
  writeJPEG(pca.img, paste('photo_pca_compressed_', round(i,0), '_components.jpg', sep = ''))
}


# Display the file sizes of each of the methods performed above.
print_file_size <- function(file_name) {
  print(paste(file_name, " size (in KB): ", as.double(file.info(file_name)["size"]) / 1024))
}

file_names <- c("photo.png", 
  sapply(X = c(3, 5, 10, 20, 30),
         FUN= function(i) paste('photo_kmeans_', i, '.jpg', sep = '')), 
  sapply(X = seq.int(3, round(nrow(readImage) - 10), length.out = 5),
         FUN= function(i) paste('photo_pca_compressed_', round(i,0), '_components.jpg', sep = ''))
  )

for (file_name in file_names) {
  print_file_size(file_name)
}
