#q1. Create a function to compute the L2-norm (Euclidean distance)
x1 <- rnorm(30)
x2 <- rnorm(30)
l2norm <- sqrt(sum((x1 - x2) ^ 2))
l2norm

#q2.Create a function to compute the L1-norm (Manhattan distance) 
x1 <- rnorm(30)
x2 <- rnorm(30)
l1norm <- sum( abs(x1 - x2) )
l1norm

#q3. Load the Computer data set from the ???le ComputerData.txt and the Cars dataset
# from the ???le Cars2Data.txt. You have now to predict the response variables 
# (PRP and mpg) with the k-NN strategy. Apply the k-NN method to the Computer and Cars2
# data set with K = 2. Is your model useful?
# Be sure to compute the distance using the similar measurements across all the predictors.
# If you don’t, the variable with the biggest variance will dominate the result. 
# You have to normalize these values ???rst (subtract the mean and divide the samples
# by the standard deviation).

setwd("C:\\Users\\Sebastian\\Desktop\\STUDIA MAGISTERSKIE\\3. Semestr\\Statistical Learning Methods\\exercise 6")
#Cars2Data.txt
carsdata = read.table("Cars2Data.txt", header = T)
summary(carsdata)
str(carsdata)
mean <- mean(carsdata[,"mpg"]) 
mean
sd <- sd(carsdata[,"mpg"]) 
newValue <- (carsdata[,"mpg"] - mean) / sd 
carsdata[,"mpg"] <- newValue
carsdata[c(1, 4, 6, 100, 153),]

subset <- !(is.na(carsdata[,"horsepower"]))
carsdata <- carsdata[subset,c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")] 
predictors <- names(carsdata)
summary(carsdata)
str(carsdata)

library(class)
carsdata_train <- carsdata[1:258, ]
carsdata_test <- carsdata[259:398, ]
carsdata_train_labels <- carsdata[1:258, 1]
carsdata_test_labels <- carsdata[259:398, 1]
# I couldn't find function knn.reg in my R repository. Therefore I used knn.cv, even though
# it does not use test database.
carsdata_test_pred <- knn.cv(train = carsdata_train, cl = carsdata_train_labels, k=2, l = 0, prob = FALSE, use.all = TRUE)
carsdata_test_pred

#ComputerData.txt
compdata = read.table("ComputerData.txt", header = T)
summary(compdata)
str(compdata)
mean <- mean(compdata[,"PRP"]) 
mean
sd <- sd(compdata[,"PRP"]) 
newValue <- (compdata[,"PRP"] - mean) / sd 
compdata[,"PRP"] <- newValue
compdata[c(1, 4, 6, 100, 153),]

compdata <- compdata[, c("MYCT", "MMIN", "MMAX", "CACH", "CGMIN", "CHMAX", "PRP")]  
predictors <- names(compdata)
summary(compdata)
str(compdata)

library(class)
compdata_train <- compdata[1:135, ]
compdata_test <- compdata[136:209, ]
compdata_train_labels <- compdata[1:135, 1]
compdata_test_labels <- compdata[135:209, 1]
compdata_test_pred <- knn.cv(train = compdata_train, cl = compdata_train_labels, k=2, l = 0, prob = FALSE, use.all = TRUE)
compdata_test_pred

# q4.
compdata_test_pred2 <- knn.cv(train = compdata_train, cl = compdata_train_labels, k=10, l = 0, prob = FALSE, use.all = TRUE)
compdata_test_pred2
compdata_test_pred3 <- knn.cv(train = compdata_train, cl = compdata_train_labels, k=20, l = 0, prob = FALSE, use.all = TRUE)
compdata_test_pred3
compdata_test_pred4 <- knn.cv(train = compdata_train, cl = compdata_train_labels, k=100, l = 0, prob = FALSE, use.all = TRUE)
compdata_test_pred4
# My conclusion: the higher the number of k- the better clasification it causes.