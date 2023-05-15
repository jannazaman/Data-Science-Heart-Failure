library(class)
library(caret)   #contains a set of functions that can perform data pre-processing, feature selection, model tuning and etc. 
library(corrplot)    
library(ggplot2) #Used for exploratory data analysis (creating graphs)
library(tidyverse) 

#Data Import

setwd("C:/Users/janna/Downloads")
heart <- read.csv("heart.csv")
dim(heart) #returns dimension
str(heart) #structure of data frame

#copy of data set, Original
heart_Original <- heart
heart_Original

#Data Cleaning/Preparation

is.null(heart)        #Check if data frame is NULL
colSums(is.na(heart)) #Check for the number of missing values in data frame

#Convert the features to an appropriate data type.
heart$Sex <- ifelse(heart$Sex == "M", 1, 0)  #Convert it into a numerical dummy variable

heart$ChestPainType <- as.factor(heart$ChestPainType)
heart$ChestPainType <- as.numeric(heart$ChestPainType)

heart$RestingECG <- as.factor(heart$RestingECG)
heart$RestingECG <- as.numeric(heart$RestingECG)

heart$ExerciseAngina <- ifelse(heart$ExerciseAngina == "Y", 1, 0)  #convert it into a numerical dummy variable

heart$ST_Slope <- as.factor(heart$ST_Slope)
heart$ST_Slope <- as.numeric(heart$ST_Slope)

heart$HeartDisease <- as.factor(heart$HeartDisease)
str(heart) #structure of data frame - Quantitative

#Data normalization - So output remains unbiased and data is not affected by different length/measurement
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

#Apply normalize function to data; large range turn into 0-1
heart[1:11] <- as.data.frame(lapply(heart[1:11], normalize))

summary(heart)



#Exploratory Data analysis 

corrplot(cor(heart[-12]), method="circle")  
#The blue circles represent strong correlation between the two variable, while red circles represent low correlation - meaning they are hardly related
#Positive 1 represents "positive" 100% correlation (if one variable increase/decrease, so will the other variable)
#Negative 1 represents "negative" 100% correlation (if one variable increase, the other will decrease and vise versa)
#Age and RestingBP shows a stronger (positive) correlation opposed to Age and MaxHR, which shows a negative correlation

#use original data set because we want to see the types of Chest pain (in terms of qualitative data)
ggplot(heart_Original, aes(x=ChestPainType, fill=ChestPainType)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=1.75, colour = "white") +
  labs(title="Heart Failure by Chest Pain Type") + 
  theme(plot.title = element_text(size=14, color = "red"))
#Based on the diagram we can state that most patients do not feel chest pain (no symptoms in chest) - shown by ASY (496)
#Least amount of patients endured TA - substernal chest pain precipitated by physical exertion or emotional stress & relieved with rest/nitroglycerin

counts <- table(heart_Original$Sex)
barplot(counts, main="Num patients with heart disease by sex",
        xlab="Sex", ylab="Number of patients")
#Vast number of MALE patients have heart disease; also roughly 79% of the patients are male. Male patients is nearly triple in size opposed to female patients.

ggplot(heart_Original, aes(x=Age)) + 
  geom_bar() + 
  labs(title="Heart Failure by Age") + 
  theme(plot.title = element_text(size=14, color = "red"))
#Based on this graph, we can determine which age group experienced more heart failures.
#Heart Failure is very common in the seniors which is composed of age group 50 to 65.
#Heart failure is less among age group 0 to early 30 and late 70 - meaning greater than 70

counts <- table(heart_Original$RestingBP)
barplot(counts, main="RestingBP Distribution" ,
        xlab="RestingBP", ylab="Patients")
#We can conclude that majority of the patients experience heart failure when RestingBP was between 120 to 140 
#So patients are at high risk of heart disease if resting Blood Pressure is 120 or higher



##################        KNN MODELING STARTS HERE          ###################

#Data Modeling   

#Data Splicing -> 80% train, 20% test
ind <- sample(2, nrow(heart), replace=TRUE, prob=c(0.8, 0.2))
train <- heart[ind==1,]
test <- heart[ind==2,]

#knn -> k=1 just to see
set.seed(1)
pred1 <- knn(train = train[1:11], test = test[1:11], cl = train$HeartDisease, k = 1)
mean(pred1 != test$HeartDisease) #test error rate for this particular train/test subdivision
summary(pred1) # 0 -> normal, 1 -> heart disease

set.seed(1)
K.set <- 1:50 #Loops through k from 1 to 50
knn.test.err <- numeric(length(K.set)) #stores all test errors
knn.train.err <- numeric(length(K.set)) #stores all training errors

for (j in 1:length(K.set)){
  ind <- sample(2, nrow(heart), replace=TRUE, prob=c(0.8, 0.2))
  train <- heart[ind==1,]
  test <- heart[ind==2,]
  
  set.seed(1)
  knn.pred <- knn(train = train[1:11], test = test[1:11], cl = train$HeartDisease, k = K.set[j]) #knn process
  print(j)
  
  print(mean(knn.pred != test$HeartDisease)) #test error
  print(mean(knn.pred != train$HeartDisease)) #training error
  knn.test.err[j] <- mean(knn.pred != test$HeartDisease)
  knn.train.err[j] <- mean(knn.pred != train$HeartDisease)
}
#prints which test/train error is smallest -> best
min(knn.test.err) #Smallest test error
which.min(knn.test.err) # index of best K
min(knn.train.err) #smallest train error
which.min(knn.train.err) 

#plot k 1 to 50
K.set[which.min(knn.test.err)] #best k
plot(K.set, knn.test.err,
     type='b',
     xlab="K",
     ylab="Test error",
     main="KNN")

confusionMatrix(table(knn.pred, test$HeartDisease))

#Analyze the performance of our model when K is 9
set.seed(1)
knn.pred <- knn(train = heart[1:11], test = heart[1:11], cl = heart$HeartDisease, k = 9)
confusionMatrix(table(knn.pred, heart$HeartDisease))

##################  Here we start SVM!!! You can clear your environment and start fresh.  ###################

#SVM in R can be performed via svm() function of e1071 library
library(e1071)
library(ISLR)

#Data Import
setwd("C:/Users/janna/Downloads")
heart <- read.csv("heart.csv")

#Data Cleaning/Preparation  

#Convert the features to an appropriate data type.
heart$Sex <- ifelse(heart$Sex == "M", 1, 0)  #Convert it into a numerical dummy variable

heart$ChestPainType <- as.factor(heart$ChestPainType)
heart$ChestPainType <- as.numeric(heart$ChestPainType)

heart$RestingECG <- as.factor(heart$RestingECG)
heart$RestingECG <- as.numeric(heart$RestingECG)

heart$ExerciseAngina <- ifelse(heart$ExerciseAngina == "Y", 1, 0)  #convert it into a numerical dummy variable

heart$ST_Slope <- as.factor(heart$ST_Slope)
heart$ST_Slope <- as.numeric(heart$ST_Slope)

heart$HeartDisease <- as.factor(heart$HeartDisease)
str(heart) #structure of data frame - quantitative

# Data splicing - train 80% and test 20%
set.seed(1)
n <- nrow(heart)
train <- sample(1:n, 0.8*n)
heart.train <- heart[train, ]
heart.test <- heart[-train, ]
#check dimensions of test and train
dim(heart.test)
dim(heart.train)

# linear kernel   
set.seed(1)
tune.Linear <- tune(svm,
                 HeartDisease ~ ., 
                 data = heart.train,
                 kernel="linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 100)))
print(tune.Linear)#best cost is 0.1, best performance 0.1405518 
summary(tune.Linear)
bestmod=tune.Linear$best.model
summary(bestmod)

test.pred <- predict(bestmod, heart.test) #testing error is 0.125 (mainly focus on because we will compare the test errors of the 3 kernels and choose the best kernel)
mean(heart.test$HeartDisease != test.pred) 

train.pred <- predict(bestmod, heart.train)
mean(heart.train$HeartDisease!= train.pred)  #training error is 0.1444142

plot(tune.Linear)

# polynomial kernel  
set.seed(1)
tune.poly <- tune(svm,
                      HeartDisease ~ ., 
                      data = heart.train,
                      kernel="polynomial",
                      ranges = list(cost=c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 100), degree=c(1:10)))
print(tune.poly) # cost is 0.5, degree is 1 and best performance: 0.1471122 
summary(tune.poly) #goes through each degree and presents the results 

test.pred <- predict(tune.poly$best.model, heart.test) #testing error is 0.1195652 (error used to compare with other kernels)
mean(heart.test$HeartDisease != test.pred)

train.pred <- predict(tune.poly$best.model, heart.train) #training error is 0.1416894
mean(heart.train$HeartDisease != train.pred)

plot(tune.poly)

# radial kernel   
set.seed(1)
tune.radial <- tune(svm,
                      HeartDisease ~ ., 
                      data = heart.train,
                      kernel="radial",
                      ranges = list(cost=c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 100), gamma=c(0.01,.1,1,5,10)))
print(tune.radial) # best parameters: cost = 5, gamma = 0.1
summary(tune.radial) 

test.pred <- predict(tune.radial$best.model, heart.test) #testing error is 0.1630435
mean(heart.test$HeartDisease != test.pred) 

train.pred <- predict(tune.radial$best.model, heart.train) #training error is 0.04632153
mean(heart.train$HeartDisease!= train.pred)

plot(tune.radial)

#fit model to polynomial (best kernel)
set.seed(1)
tune.poly <- tune(svm,
                  HeartDisease ~ ., 
                  data = heart.train,
                  kernel="polynomial",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 100), degree=c(1:10)))
print(tune.poly) # cost is 0.5, degree is 1 and best performance: 0.1471122 
summary(tune.poly) 
print(tune.poly$best.parameters)
print(tune.poly$best.performance)

svmpoly_fit = svm(HeartDisease~., data = heart, 
              kernel = "polynomial", 
              cost = 0.5, degree = 1)
summary(svmpoly_fit) 
print(svmpoly_fit)

train.pred <- predict(svmpoly_fit, heart) # training error of 0.1350763
mean(heart$HeartDisease!= train.pred)

#Does not plot entire data at once, as we must reduce the number of covariants
#Since multiple features utilized to predict response variable, we will present a few 2D diagrams (two variables to visualize the decision boundary)
#Some positive relationship examples - blue circle
plot(svmpoly_fit,heart,Age~Oldpeak) #There is some separation (small amount), some black marks are more left skewed
plot(svmpoly_fit,heart,Age~RestingBP)
plot(svmpoly_fit,heart,Cholesterol~MaxHR) #shows a bit more separation opposed to the other positively correlated plots
plot(svmpoly_fit,heart,RestingBP~Oldpeak)

#Some negative relationship examples - warm tone (red) circles
plot(svmpoly_fit,heart,Age~MaxHR) #There is some separation, black marks are more right skewed
plot(svmpoly_fit,heart,Oldpeak~ST_Slope) #Majority looks like red mark but there are a few black marks (focus only middle)
plot(svmpoly_fit,heart,ST_Slope~Age)
