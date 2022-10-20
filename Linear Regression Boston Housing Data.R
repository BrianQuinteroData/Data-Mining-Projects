#Calling data Iris set
data("iris")

##Calling a sample from iris data set of 90% 
iris_sample = iris[sample(nrow(iris), nrow(iris) * .9), ]

###Calling summary data from the 90% sample data of iris
summary(iris_sample)

#Histogram for Iris Sample Sepal Length
hist(iris_sample$Sepal.Length)

#Density for Iris Sample Sepal Length 
plot(density(iris_sample$Sepal.Length))

#Scatter Plot for 
plot(iris$Sepal.Length, iris$Sepal.Width) 


library(MASS)
parcoord(iris_sample[ , 1:4], col = iris_sample$Species)


