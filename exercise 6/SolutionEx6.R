#
# Solution Exercise 5
# Statistical Learning - 2017
# University of Neuchâtel
#

setwd("C:\\Users\\Sebastian\\Desktop\\STUDIA MAGISTERSKIE\\3. Semestr\\Statistical Learning Methods\\exercise 6")


##################################################################################
# QUESTION 1
##################################################################################

l2norm = function(x, y) 
{
  return(sqrt( sum( (x - y)^2 )))
}

# Verify the computation
x = c(2, 4);  y <- c(3, 2)
l2norm(x, y)
sqrt(5)

##################################################################################
# QUESTION 2
##################################################################################

l1norm = function(x, y) 
{
  return(sum( abs(x - y) ))
}

# Verify the computation
x = c(2, 4);  y <- c(3, 2)
l1norm(x, y)

##################################################################################
# QUESTION 3
##################################################################################

# Install and import FNN package
# install.packages("FNN")
library(FNN)

###########################
# 3.1 : Computer data set
###########################

# Computer data : need to predict PRP.
computerData = read.table("ComputerData.txt", header=T)

# Remove the variable model (name), vendor (name) and ERP
usefulData = computerData[, c("MYCT", "MMIN", "MMAX", "CACH", "CGMIN", "CHMAX", "PRP")]
predictors = names(usefulData)
summary(usefulData)

# Standardized the values
means = lapply(usefulData, mean)
sd = lapply(usefulData, sd)
usefulData = (usefulData - means) / sd
summary(usefulData)
attach(usefulData)

# Test K-NN model
computer.knn = knn.reg(usefulData, test = NULL, y = PRP, k = 2)
summary(computer.knn)
computer.knn$PRESS
computer.knn$R2Pred

###########################
# 3.2 : Cars data set
###########################

# Cars2 data:  need to predict mpg
carData = read.table("Cars2Data.txt", header=T)
subset = !(is.na(carData[,"horsepower"]))

# Remove the variable name to infer mpg
usefulDataCars <- carData[subset, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration")]

# Standardized the values
means = lapply(usefulDataCars, mean)
sd = lapply(usefulDataCars, sd)
usefulDataCars = (usefulDataCars - means) / sd
summary(usefulDataCars)
attach(usefulDataCars)

# Test K-NN model
cars.knn = knn.reg(usefulDataCars, test = NULL, y = mpg, k = 2)
summary(cars.knn)
cars.knn$PRESS
cars.knn$R2Pred

##################################################################################
# QUESTION 4
##################################################################################

###########################
# 4.1 : Computer data set
###########################

# Test each values of K
for (i in 1:11) 
{
  computer.knn <- knn.reg(usefulData, test = NULL, y = PRP, k = i)
  cat("k: ", i, "press: ", computer.knn$PRESS, "R^2: ", computer.knn$R2Pred,"\n")
}

# The highest R^2 is 0.8826 corresponding to k = 1
# So we choose k = 1
computer.knn <- knn.reg(usefulData, test = NULL, y = PRP, k = 1)

###########################
# 4.2 : Cars data set
###########################

# Test each values of K
for (i in 1:11) 
{
  cars.knn <- knn.reg(usefulDataCars, test = NULL, y = mpg, k = i)
  cat("k: ", i, "press: ", cars.knn$PRESS, "R^2: ", cars.knn$R2Pred,"\n")
}

# The highest R^2 is 0.9699 corresponding to k = 2
# So we choose k = 2
cars.knn <- knn.reg(usefulDataCars, test = NULL, y = mpg, k = 2)

