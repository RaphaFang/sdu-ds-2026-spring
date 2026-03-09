#### Section With Name #### 
# ================================================================================================================================================
# 6.5
# ================================================================================================================================================
vec_1 = c(1, -2, 3, -4, 5)
mean(vec_1)
max(vec_1)
min(vec_1)

mean(abs(vec_1))

# ========================================================================
vec_1[1:2]

n_vec_1 = c(vec_1[1:2], 42, vec_1[3:length(vec_1)])
n_vec_1

# ========================================================================
vec_2 = c(6, -7, 8, -9, 10)

sum(vec_1, vec_2)

# ========================================================================
r_vec = rnorm(100)
mean(r_vec)

l = length(r_vec)
r_vec[(l-5):l]

# have to keep the parameter

# ================================================================================================================================================
# 6.6
# ================================================================================================================================================
row1 = c(1, 2)
row2 = c(3, 4)

A = rbind(row1, row2)
A
# ========================================================================
B = A * -1  

null_matrix = A + B
null_matrix

# ========================================================================
multiplier_matrix = matrix(2, nrow = 2, ncol = 2) # it's vertically
multiplier_matrix

doubled_A = A * multiplier_matrix
doubled_A


# ================================================================================================================================================
# 6.7
# ================================================================================================================================================
plot(AirPassengers)
hist(AirPassengers)

class(AirPassengers)
mode(AirPassengers)

head(AirPassengers)

help(AirPassengers)
help(Titanic)

mosaicplot(Titanic)

# ================================================================================================================================================
# 6.8
# ================================================================================================================================================
plot(iris)
head(iris)

library(cluster)
n_iris = iris[1:4]
head(n_iris)

d = dist(n_iris) # do the 4 dim at the same time
sil_width = c()

for (k in 2:10) {
  km = kmeans(n_iris, centers = k, nstart = 20) # the re-start frequency for trying the k
  sil = silhouette(km$cluster, d) # $, the extract
  sil_width[k] = mean(sil[, 3])
}

plot(2:10, sil_width[2:10], type = "b",
     xlab = "the K",
     ylab = "Silhouette Width, the sil_width")

# ========================================================================
km_final = kmeans(n_iris, centers = 2, nstart = 20)
iris$n_lable = km_final$cluster

par(mfrow = c(2, 2))

plot(iris$Petal.Length, iris$Petal.Width,
     col = iris$Species,
     pch = 19,
     main = "ori species",
     xlab = "Petal Length", 
     ylab = "Petal Width")

plot(iris$Petal.Length, iris$Petal.Width,
     col = iris$n_lable,
     pch = 19,
     main = "k=2",
     xlab = "Petal Length", 
     ylab = "Petal Width")

plot(iris$Sepal.Length, iris$Sepal.Width, 
     col = iris$Species,
     pch = 19,
     main = "ori species",
     xlab = "Sepal Length", 
     ylab = "Sepal Width")

plot(iris$Sepal.Length, iris$Sepal.Width,
     col = iris$n_lable,
     pch = 19,
     main = "k=2",
     xlab = "Sepal Length", 
     ylab = "Sepal Width")

par(mfrow = c(1, 1))

head(iris)

# ========================================================================
library(class)
n_flower = data.frame(
  Sepal.Length = c(5.5, 6.0, 7.5),
  Sepal.Width  = c(3.0, 2.8, 4.0),
  Petal.Length = c(2.5, 4.9, 1.5), 
  Petal.Width  = c(0.8, 1.6, 0.2)
)

knn_pred = knn(train = iris[1:4],
     test = n_flower, 
     cl = iris$n_lable, 
     k = 4)
     
ori_pred = knn(train = iris[1:4],
     test = n_flower, 
     cl = iris$Species, 
     k = 4)


for (k in 1:10) {
     knn_pred = knn(train = iris[1:4],
     test = n_flower, 
     cl = iris$n_lable, 
     k = k)
     
     ori_pred = knn(train = iris[1:4],
     test = n_flower, 
     cl = iris$Species, 
     k = k)

     cat("knn_pred, the predict with k = ", k, "\n")
     cat(knn_pred,"\n")
     cat("ori_pred, the predict with k = ", k, "\n")
     cat(ori_pred,"\n","\n")
     cat("========================================\n")
}

# ========================================================================
km_final = kmeans(n_iris, centers = 2, nstart = 20)
iris$n_lable = km_final$cluster

par(mfrow = c(2, 2))
plot(iris$Petal.Length, iris$Petal.Width,
     col = iris$Species,
     pch = 19,
     main = "ori species",
     xlab = "Petal Length", 
     ylab = "Petal Width")
points(n_flower$Petal.Length, n_flower$Petal.Width, col = "blue", pch = 17, cex = 2.5)

plot(iris$Petal.Length, iris$Petal.Width,
     col = iris$n_lable,
     pch = 19,
     main = "k=2",
     xlab = "Petal Length", 
     ylab = "Petal Width")
points(n_flower$Petal.Length, n_flower$Petal.Width, col = "blue", pch = 17, cex = 2.5)

plot(iris$Sepal.Length, iris$Sepal.Width, 
     col = iris$Species,
     pch = 19,
     main = "ori species",
     xlab = "Sepal Length", 
     ylab = "Sepal Width")
points(n_flower$Sepal.Length, n_flower$Sepal.Width, col = "blue", pch = 17, cex = 2.5)

plot(iris$Sepal.Length, iris$Sepal.Width,
     col = iris$n_lable,
     pch = 19,
     main = "k=2",
     xlab = "Sepal Length", 
     ylab = "Sepal Width")
points(n_flower$Sepal.Length, n_flower$Sepal.Width, col = "blue", pch = 17, cex = 2.5)
par(mfrow = c(1, 1))