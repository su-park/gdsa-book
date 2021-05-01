## Geographical Data Science and Spatial Data Analysis: An Introduction in R
## Lex Comber and Chris Brunsdon
## The code correct as of July 2020. Please let us know if it needs updating
## email: a.comber@leeds.ac.uk

## Chapter 1: Introduction to Geographical Data Science and Spatial Data Analytics


# this is a comment: assign a sequence of values to x
x = 5:12


2+9 


y = c(4.3,7.1,6.3,5.2,3.2,2.1)


y <- c(4.3,7.1,6.3,5.2,3.2,2.1)


y*2
max(y)


help(max)


## alternative way to access help
?max


sum(y)
mean(y)


## result <- function_name(<input>)


# assign 100 random, normally distributed values to x using rnorm
x <- rnorm(100)


2/9
sqrt(1000) # square root
2*3*4*5
pi # pi
2*pi*6378 # earth circumference
sin(c(30,60,90)) #sine of angles in radians


c(2,3,5,2,7,1)
3:10 # the sequence numbers 3, 4, ..., 10
c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE)
c("London","Leeds","New York","Montevideo", NA)


x <- c(2,3,5,2,7,1) 
x
y <- c(10,15,12) 
y
z <- c(x, y)
z


x[c(2,4)]	# Extract elements 2 and 4


x[-c(2,3)]	# Omit elements 2 and 3


x[x > 4]


x > 4


matrix(1:10, ncol = 2)
matrix(1:10, ncol = 2, byrow = T)
matrix(letters[1:10], ncol = 2)


data(iris)


class(iris)


head(iris)
dim(iris)


str(iris)


summary(iris)


names(iris)


plot(iris[,1:4], pch = 1, cex = 0.7, col = "grey30", upper.panel=panel.smooth)


sapply(iris, class)


iris[1:10,]
as.matrix(iris[1:10,])
as.matrix(iris[1:10,-5])


iris$Species


levels(iris$Species)


attributes(iris$Species)


iris$Species[10] = "bananas"


plot(iris[,1:4], pch = 1, cex = 1.5)


# z
z
# 1st element
z[1]
# 5th element
z[5]
# elements 1 to 6
z[1:6]
# elements 1, 3, 6 and 2 ... in that order
z[c(1,3,6,2)]


names(iris)
names(iris)[c(1,4)]
names(iris)[c(4,2)]
plot(iris[,c(3,4,2,1)])


# 1st row
iris[1,]
# 3rd column
iris[,3]
# a selection of rows
iris[c(3:7,10,11),]


x = c(1:3)	# assign index to x
names(iris)[x]	# check
plot(iris[,x], pch = 1, cex = 1.5)


iris[1:10, c(1,3)]


iris[1:10, c("Sepal.Length", "Petal.Length")]


iris[1:10, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)]


n <- iris$Sepal.Length > 6 & iris$Petal.Length > 5
iris[n,]


z[c(2,4)]	# Extract elements (rows) 2 and 4
iris[, "Petal.Width"]


z[-c(2,3)]


z[z > 10]


# 1st row
iris[1,]
# 2nd and 4th column
iris[, c(2,4)]


x[x > 4]
z[z > 10]


x
x > 4


data.frame(x = x, logic = x>4)


x(c(3,5))


which(x > 4)


(x > 4) + 0
(x > 4) * 1


x > 4
!(x > 4)


my.index = x > 4
x[my.index]


iris[iris$Sepal.Length >= 7, ]


iris$Sepal.Length >= 7


# rows in iris
nrow(iris)
# count of TRUE
sum(iris$Sepal.Length >= 7 )


# Sepal.Length between 7 and 7.2 inclusive
iris[iris$Sepal.Length >= 7.0 & iris$Sepal.Length <= 7.2,]
# Sepal.Length greater than 7.0 and Petal.Length less than 6.0 inclusive
iris[iris$Sepal.Length > 7.0 & iris$Petal.Length < 6.0,]
# Sepal.Length greater than 5.5 less than or equal to 5.7
iris[iris$Sepal.Length > 5.5 & iris$Sepal.Length <= 5.7,]
# Sepal.Length greater than 5.5 less than or equal to 5.7 AND
# Species is virginica OR setosa - notice the use of the brackets
iris[(iris$Sepal.Length > 5.5 & iris$Sepal.Length	 <= 5.7) &
       (iris$Species == "virginica" | iris$Species == "setosa"), ]


?Syntax


length(z)
mean(z)
median(z)
range(z)
unique(z)
sort(z)
order(z)
sum(z)
cumsum(z)
cumprod(z)
rev(z)


sapply(iris, is.factor)


index = sapply(iris, is.factor)
sapply(iris[,!index], range)	# The first 4 columns are not factors


index
!index


miles.to.km <- function(miles)miles*8/5
# Distance from Maynooth to Nottingham is ~260 miles
miles.to.km(260)


miles.to.km(c(100,200,300))


plot_iris <- function(x, y){
  x <- iris[,x]
  y <- iris[,y]
  plot(x, y)
}


plot_iris(1,2)
plot_iris("Sepal.Length", 3)
plot_iris("Sepal.Length", "Petal.Width")


## install.packages("package_name", dependencies = TRUE)


install.packages("tidyverse", dependencies = TRUE)


library(tidyverse)


install.packages("maptools", dep = TRUE)
library(maptools)


help(readShapePoly)

