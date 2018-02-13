# I am going to explain how Principal Component Analysis(PCA) works when applying on dataset in R.
# It is mandatory convert variable to numeric data type before applying PCA
# load dummies library for converting categorical variable to numeric
#library(dummies)

# read the csv file
# I am using Analytics Vidhya data set
# URL: https://datahack.analyticsvidhya.com/contest/practice-problem-big-mart-sales-iii/ 
data1 <- read.csv("Train.csv", na.strings=c("","NA"))

# Fill NA's with the median
data1$Item_Weight[is.na(data1$Item_Weight)] <- median(data1$Item_Weight, na.rm = TRUE)

# Fix the Fat Content value with the consistent data
# Replacing lf, low fat to Low Fat and reg to Regular 
levels(data1$Item_Fat_Content)[1] <- "Low Fat"
levels(data1$Item_Fat_Content)[2] <- "Low Fat"
levels(data1$Item_Fat_Content)[2] <- "Regular"

#impute 0 with median
data1$Item_Visibility <- ifelse(data1$Item_Visibility == 0, median(data1$Item_Visibility),data1$Item_Visibility)

#find mode and add level as other to Outlet Size
levels(data1$Outlet_Size)[4] <- "Other"
data1$Outlet_Size[is.na(data1$Outlet_Size)] <- "Other"

#remove the dependent and identifier variables
my_data <- subset(data1, select = -c(Item_Outlet_Sales, Item_Identifier,Outlet_Identifier))

# convert categorical variable to numeric
final_data <- dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type",
                                                   "Outlet_Establishment_Year","Outlet_Size",
                                                   "Outlet_Location_Type","Outlet_Type"))

# view modified dataset
colnames(final_data)
summary(final_data)
#--------------------- Principal Component Analysis -----------------------------------------
# To use the same scale for each variable, we need to normalize the data
# by subtracting each value by its column mean
standardize <- function(x) {(x - mean(x))}
centredData <- apply(final_data,2,standardize)

# Find Eigen values of covariance matrix
# Co variance matrix is derived from normalized dataset by using cov in built function
my.cov <- cov(centredData)
my.eigen <- eigen(my.cov)

# when vector transformed with matrix then the resulting vector is same with the magnitude is known as eigen vector and 
# value is known as eigen value
rownames(my.eigen$vectors) <- colnames(centredData)
  
# Changing column names to PC1 n so on to represent Principal Component. 
# PC1 is known as First Principal Component and PC2 as Second Principal Component and so on.
colnames(my.eigen$vectors) <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10",
                                "PC11","PC12","PC12","PC14","PC15","PC16","PC17","PC18","PC19","PC20",
                                "PC21","PC22","PC23","PC24","PC25","PC26","PC27","PC28","PC29","PC30",
                                "PC31","PC32","PC33","PC34","PC35","PC36","PC37","PC38","PC39","PC40","PC41"
)

# Note that the sum of the eigen values equals the total variance of the data
sum(my.eigen$values)
sum(var(centredData[,1:41]))

# The Eigen vectors are the principal components. Also called as loadings when using PCA function and use Biplot for viz.
loadings <- my.eigen$vectors

# Let's plot them 

# variation each eigenvector accounts for

pc1.var <- 100*round(my.eigen$values[1]/sum(my.eigen$values),digits=3)
pc2.var <- 100*round(my.eigen$values[2]/sum(my.eigen$values),digits=3)


xlab=paste("PC1 - ",pc1.var," % ",sep="")
ylab=paste("PC2 - ",pc2.var," % ",sep="")

# transformaed data by multiplying centred data with eigen vector(PCA's)
scores <- centredData %*% loadings

sd <- sqrt(my.eigen$values)
rownames(loadings) = colnames(centredData)
plot(scores,ylim=c(-3,3),main="PCA Analysis",xlab=xlab,ylab=ylab)
abline(0,0,col="red")
abline(0,90,col="green")

# Correlation BiPlot

scores.min <- min(scores[,1:2])
scores.max <- max(scores[,1:2])

plot(scores[,1]/sd[1],scores[,2]/sd[2],main="My First BiPlot",xlab=xlab,ylab=ylab,type="n")
rownames(scores)=seq(1:nrow(scores))
abline(0,0,col="red")
abline(0,90,col="green")

# This is to make the size of the lines more visible
factor <- .01

# First plot the variables as vectors
arrows(0,0,loadings[,1]*sd[1]/factor,loadings[,2]*sd[2]/factor,length=0.1, lwd=2,angle=20, col="red")
text(loadings[,1]*sd[1]/factor*1.2,loadings[,2]*sd[2]/factor*1.2,rownames(loadings), col="red", cex=1.2)

# Second plot the scores as points
text(scores[,1]/sd[1],scores[,2]/sd[2], rownames(scores),col="blue", cex=0.7)
