pacman::p_load(pacman,dplyr,GGally,ggplot2,ggthemes,ggvis,httr,lubricate,plotly,rio,markdown,shiny,stringr,tidyr)
library("stats")
p_load(psych)
library(Hmisc)
DataSet <- read.csv("heart.csv",header=TRUE,sep=",")
View(DataSet)
library(dplyr)
library(magrittr)

#  Data Preprocessing


# 1. Data Cleaning 


# a. Checking for Missing Values
any(is.na(DataSet$age))
any(is.na(DataSet$sex))
any(is.na(DataSet$cp))
any(is.na(DataSet$trtbps))
any(is.na(DataSet$chol))
any(is.na(DataSet$fbs))
any(is.na(DataSet$restecg))
any(is.na(DataSet$thalachh))
any(is.na(DataSet$exng))
any(is.na(DataSet$oldpeak))
any(is.na(DataSet$slp))
any(is.na(DataSet$caa))
any(is.na(DataSet$thall))

# Instead of doing the above we can get descption about each column by 
# describe(DataSet)
# Output : No Missing values has been found

# FInding the Duplicated rows
duplicated(DataSet) # We found an duplicated row at row number 165
DataSet <- DataSet[-c(165), ] # removing it
duplicated(DataSet)
# Here at 165th row it is hsowing that the row is duplicated
# b. Noisy Data -- Outliers

length(DataSet) # There are 14 columns in the data Set
# We'll try to find out the outliers in each column
names(DataSet)
for (i in 1:(length(DataSet)-1) ){
  boxplot(DataSet[i],main = names(DataSet[i]))
}   # We'll get Individual Boxplot for each feature

# boxplot(as.matrix(DataSet)) #Shared Boxplot

# thall -- categorical data
# caa -- categorical data
# slp - categorical data
# oldpeak -- 4 outliers
# exng -- categorical data
# thalachh -- 1 outlier
# restecg -- categorical data
# fbs -- categorical data
# chol -- 3 Outliers
# trtbps -- 6 Outliers
# cp -- categorical data
# sex -- categorical data
# age -- numerical data






# Most physical measures, such as height, weight, systolic blood pressure, distance etc., are interval or ratio scales, so they fall into the general "continuous " category.



detect_outlier <- function(x) {
  
  
  Quantile1 <- quantile(x, probs=.25)
  
  # calculate third quantile
  Quantile3 <- quantile(x, probs=.75)
  
  # calculate inter quartile range
  IQR = Quantile3-Quantile1
  
  # return true or false
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}

# create remove outlier function
remove_outlier <- function(dataframe,
                           columns){
  
  # for loop to traverse in columns vector
  for (col in columns) {
    
    # remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
    
  }
  
  # return dataframe
  dataframe
}


data <- remove_outlier(DataSet,c('oldpeak','thalachh','chol','trtbps','age'))
nrow(data)
# All the outliers have been removed sucesfully

boxplot(data['oldpeak'],main = 'oldpeak')
boxplot(data['thalachh'],main = 'thalachh')
boxplot(data['chol'],main = 'chol')
boxplot(data['trtbps'],main = 'trtbps')
boxplot(data['age'],main = 'age')


describe(data)


View(data)


plotHistogram <- function(dataframe,columns){
  
  for(col in columns){
    # data[col]<- as.numeric(unlist(data[col]))      # turn into numbers
    hist(dataframe$col,xlab = col ,col = "yellow",border = "blue")
  }
  
}



hist(data$oldpeak,xlab = "oldpeak" ,col = "yellow",border = "blue")
hist(data$thalachh,xlab = "thalachh" ,col = "yellow",border = "blue")
hist(data$chol,xlab = "chol" ,col = "yellow",border = "blue")
hist(data$trtbps,xlab = "trtbps" ,col = "yellow",border = "blue")
hist(data$age,xlab = "age" ,col = "yellow",border = "blue")
# We Can Clearly Observe oldPeak doesn't follows normal distribution


hist(data$sex,xlab = "sex" ,col = "red",border = "blue")
hist(data$cp,xlab = "cp" ,col = "red",border = "blue")
hist(data$fbs,xlab = "fbs" ,col = "red",border = "blue")
hist(data$restecg,xlab = "restecg" ,col = "red",border = "blue")
hist(data$exng,xlab = "exng" ,col = "red",border = "blue")
hist(data$slp,xlab = "slp" ,col = "red",border = "blue")
hist(data$caa,xlab = "caa" ,col = "red",border = "blue")





library(ggplot2)
library(reshape2)
###


contData <- data[,c('thalachh','chol','trtbps','age')]
# Correlation between attributes
corr_mat <- cor(contData,method = "pearson")
corMatrix_melt = melt(corr_mat)
corMatrix_melt
ggplot(data = corMatrix_melt,aes(x = Var1,y = Var2,fill = value))+geom_tile()

contData <- data[,c('thalachh','chol','trtbps','age','oldpeak')]
corr_mat <- cor(contData,method = "spearman")
corMatrix_melt = melt(corr_mat)
corMatrix_melt
ggplot(data = corMatrix_melt,aes(x = Var1,y = Var2,fill = value))+geom_tile()

#There are not much highly correlated attributes

# Correlation for categorical values

#Null Hypothesis : X and Y are independent
# SIGNIFICANCE LEVEL = 0.05
# If p < 0.05 -- reject null hypothesis
# else -- accept null hypothesis

table(data$sex)
table(data$cp) # remove
table(data$fbs)
table(data$restecg)
table(data$exng) # remove
table(data$slp)
table(data$caa)
table(data$thall) # remove

simulate.p.value = TRUE
d1 <- data[,c('sex','cp')]
table(d1)
d1 <- matrix(c(30,18,33,4,100,31,50,18),ncol = 4,byrow = TRUE)
d1 <- as.table(d1)
chi1 = chisq.test(d1)
chi1  # 0.02597

d2 <- data[,c('sex','fbs')]
table(d2)
d2 <- matrix(c(76,9,168,31),ncol = 2,byrow = TRUE)
d2 <- as.table(d2)
chi2 = chisq.test(d2)
chi2 # 0.3572



d3 <- data[,c('sex','restecg')]
table(d3)
d3 <- matrix(c(37,46,2,100,99,0),ncol = 3,byrow = TRUE)
d3 <- as.table(d3)
chi3 = chisq.test(d3)
chi3 #0.06513

d4 <- data[,c('sex','exng')]
table(d4)
d4 <- matrix(c(69,16,125,74),ncol = 2,byrow = TRUE)
d4 <- as.table(d4)
chi4 = chisq.test(d4)
chi4 #0.003654

d5 <- data[,c('sex','slp')]
table(d5)
d5 <- matrix(c(3,38,44,13,92,94),ncol = 3,byrow = TRUE)
d5 <- as.table(d5)
chi5 = chisq.test(d5)
chi5 #0.5392


d6 <- data[,c('sex','caa')]
table(d6)
d6 <- matrix(c(59,14,10,2,0,106,49,25,14,5),ncol = 5,byrow = TRUE)
d6 <- as.table(d6)
chi6 = chisq.test(d6)
chi6 #0.06259

d7 <- data[,c('sex','thall')]
table(d7)
d7 <- matrix(c(1,1,74,9,1,16,86,96),ncol = 4,byrow = TRUE)
d7 <- as.table(d7)
chi7 = chisq.test(d7)
chi7 #1.901E-10


d8 <- data[,c('restecg','fbs')]
table(d8)
d8 <- matrix(c(114,23,128,17,2,0),ncol = 2,byrow = TRUE)
d8 <- as.table(d8)
chi8 = chisq.test(d8)
chi8 #0.4019


d9 <- data[,c('fbs','slp')]
table(d9)
d9 <- matrix(c(11,113,120,5,17,18),ncol = 3,byrow = TRUE)
d9 <- as.table(d9)
chi9 = chisq.test(d9)
chi9 #0.1269


d10 <- data[,c('fbs','caa')]
table(d10)
d10 <- matrix(c(147,55,27,11,4,18,8,8,5,1),ncol = 5,byrow = TRUE)
d10 <- as.table(d10)
chi10 = chisq.test(d10)
chi10 #0.1015


d11 <- data[,c('restecg','slp')]
table(d11)
d11 <- matrix(c(9,71,57,7,57,81,0,2,0),ncol = 3,byrow = TRUE)
d11 <- as.table(d11)
chi11 = chisq.test(d11)
chi11 #0.08721

d12 <- data[,c('restecg','caa')]
table(d12)
d12 <- matrix(c(73,33,19,11,1,91,29,16,5,4,1,1,0,0,0),ncol = 5,byrow = TRUE)
d12 <- as.table(d12)
chi12 = chisq.test(d12)
chi12 #0.4868


d13 <- data[,c('slp','caa')]
table(d13)
d13 <- matrix(c(13,2,1,0,0,63,35,19,11,2,89,26,15,5,3),ncol = 5,byrow = TRUE)
d13 <- as.table(d13)
chi13 = chisq.test(d13)
chi13 #0.1237


# Random SubSampling

data <- data[,c('oldpeak','thalachh','chol','trtbps','age','sex','fbs','restecg','slp','caa','cp','output')]

set.seed(123)   

split1<- sample(c(rep(0, 0.7 * nrow(data)), rep(1, 0.3 * nrow(data))))
table(split1)
train <- data[split1 == 0, ] 
test <- data[split1== 1, ] 
dim(train)
dim(test)


X_train <- train[,0:11]
Y_train <- as.character(train[,12:12])
X_test <- as.matrix(test[,0:11])
Y_test <- as.character(test[,12:12])

############################################################################################3
library(e1071)
library(caTools)
library(caret)
library(naivebayes)

classifier_cl <- gaussian_naive_bayes(x = X_train, y = Y_train)
summary(classifier_cl)
classifier_cl

# Predicting on test data

y_pred <- predict(classifier_cl, newdata = X_test, type = "class")

# Confusion Matrix
cm <- table(test$output, y_pred)
cm

# Model Evaluation
# Here the Positive class is 0 - i.e person having less chance of heart attack
confusionMatrix(cm,mode = "everything",positive = "0")

