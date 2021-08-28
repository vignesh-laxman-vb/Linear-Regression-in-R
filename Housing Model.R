#Read the data
Housing_Data = read.csv("/Users/LVaradharajen/Desktop/R Data Science/Boston_Housing.csv")

#Required libraries
library(dplyr)
library(ggplot2)

#Glimpse of the data
glimpse(Housing_Data)

#Getting detailed summary of data
summary(Housing_Data)

#Missing value treatment in columns nox and tax
Housing_Data$nox[is.na(Housing_Data$nox)] = mean(Housing_Data$nox, na.rm = TRUE)
Housing_Data$tax[is.na(Housing_Data$tax)] = mean(Housing_Data$tax, na.rm = TRUE)

#Check summary of data again
summary(Housing_Data)

#Check for outliers in crim
boxplot(Housing_Data$crim, col = "blue")
outlier_crim = boxplot(Housing_Data$crim, plot = FALSE)$out
length(outlier_crim)

#Check for outliers in zn
boxplot(Housing_Data$zn, col = "blue")
outlier_zn = boxplot(Housing_Data$zn, plot = FALSE)$out
length(outlier_zn)

#Check for outliers in indus
boxplot(Housing_Data$indus, col = "blue")
outlier_indus = boxplot(Housing_Data$indus, plot = FALSE)$out
length(outlier_indus)

#Check for outliers in chas
boxplot(Housing_Data$chas, col = "blue")
outlier_chas = boxplot(Housing_Data$chas, plot = FALSE)$out
length(outlier_chas)

#Check for outliers in nox
boxplot(Housing_Data$nox, col = "blue")
outlier_nox = boxplot(Housing_Data$nox, plot = FALSE)$out
length(outlier_nox)

#Check for outliers in rm
boxplot(Housing_Data$rm, col = "blue")
outlier_rm = boxplot(Housing_Data$rm, plot = FALSE)$out
length(outlier_rm)

#Check for outliers in age
boxplot(Housing_Data$age, col = "blue")
outlier_age = boxplot(Housing_Data$age, plot = FALSE)$out
length(outlier_age)

#Check for outliers in dis
boxplot(Housing_Data$dis, col = "blue")
outlier_dis = boxplot(Housing_Data$dis, plot = FALSE)$out
length(outlier_dis)

#Check for outliers in rad
boxplot(Housing_Data$rad, col = "blue")
outlier_rad = boxplot(Housing_Data$rad, plot = FALSE)$out
length(outlier_rad)

#Check for outliers in tax
boxplot(Housing_Data$tax, col = "blue")
outlier_tax = boxplot(Housing_Data$tax, plot = FALSE)$out
length(outlier_tax)

#Check for outliers in ptratio 
boxplot(Housing_Data$ptratio , col = "blue")
outlier_ptratio  = boxplot(Housing_Data$ptratio , plot = FALSE)$out
length(outlier_ptratio )

#Check for outliers in black
boxplot(Housing_Data$black, col = "blue")
outlier_black = boxplot(Housing_Data$black, plot = FALSE)$out
length(outlier_black)

#Check for outliers in lstat
boxplot(Housing_Data$lstat, col = "blue")
outlier_lstat = boxplot(Housing_Data$lstat, plot = FALSE)$out
length(outlier_lstat)

#Check for outliers in medv
boxplot(Housing_Data$medv, col = "blue")
outlier_medv = boxplot(Housing_Data$medv, plot = FALSE)$out
length(outlier_medv)

#Outlier treatment
Housing_Data$crim[Housing_Data$crim %in% outlier_crim] = quantile(Housing_Data$crim,0.975)

Housing_Data$zn[Housing_Data$zn %in% outlier_zn] = quantile(Housing_Data$zn,0.98)

outlier_rm_up = outlier_rm[outlier_rm>median(Housing_Data$rm)]
outlier_rm_down = outlier_rm[outlier_rm<median(Housing_Data$rm)]
Housing_Data$rm[Housing_Data$rm %in% outlier_rm_up] = quantile(Housing_Data$rm,0.99)
Housing_Data$rm[Housing_Data$rm %in% outlier_rm_down] = quantile(Housing_Data$rm,0.01)

Housing_Data$dis[Housing_Data$dis %in% outlier_dis] = quantile(Housing_Data$dis,0.99)

Housing_Data$rad[Housing_Data$rad %in% outlier_rad] = quantile(Housing_Data$rad,0.99)

Housing_Data$ptratio[Housing_Data$ptratio %in% outlier_ptratio] = quantile(Housing_Data$ptratio,0.01)

Housing_Data$black[Housing_Data$black %in% outlier_black] = quantile(Housing_Data$black,0.05)

Housing_Data$lstat[Housing_Data$lstat %in% outlier_lstat] = quantile(Housing_Data$lstat,0.99)

outlier_medv_up = outlier_medv[outlier_medv>median(Housing_Data$medv)]
outlier_medv_down = outlier_medv[outlier_medv<median(Housing_Data$medv)]
Housing_Data$medv[Housing_Data$medv %in% outlier_medv_up] = quantile(Housing_Data$medv,0.99)

#Plotting pair plots
pairs(~crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = Housing_Data, main = "Housing Data")

#boxplots
par(mfrow=c(3,5))
boxplot(Housing_Data$crim, col = "blue")
boxplot(Housing_Data$zn, col = "blue")
boxplot(Housing_Data$indus, col = "blue")
boxplot(Housing_Data$chas, col = "blue")
boxplot(Housing_Data$nox, col = "blue")
boxplot(Housing_Data$rm, col = "blue")
boxplot(Housing_Data$age, col = "blue")
boxplot(Housing_Data$dis, col = "blue")
boxplot(Housing_Data$rad, col = "blue")
boxplot(Housing_Data$tax, col = "blue")
boxplot(Housing_Data$ptratio, col = "blue")
boxplot(Housing_Data$black, col = "blue")
boxplot(Housing_Data$lstat, col = "blue")
boxplot(Housing_Data$medv, col = "blue")

co_relation = cor(Housing_Data)
write.csv(co_relation, "HousingCorelation.csv")

#Iteration1

Col_drop = c("rad")
Housing_Data_Model = Housing_Data[,!(names(Housing_Data) %in% Col_drop)]

Model1 = lm(medv~., data = Housing_Data_Model)

summary(Model1)

#Iteration2

Col_drop2 = c("chas","age","black")
Housing_Data_Model2 = Housing_Data_Model[,!(names(Housing_Data_Model) %in% Col_drop2)]

Model2 = lm(medv~.,data = Housing_Data_Model2)

summary(Model2)

#Iteration3

Col_drop3 = c("zn","tax","indus","crim")
Housing_Data_Model3 = Housing_Data_Model2[,!(names(Housing_Data_Model2) %in% Col_drop3)]

Model3 = lm(medv~.,data = Housing_Data_Model3)

summary(Model3)

#Iteration4

Model4 = lm(medv~.+I(lstat^2)+I(rm^2),data = Housing_Data_Model3)

summary(Model4)

plot(Model4)
