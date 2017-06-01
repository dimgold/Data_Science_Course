# Data Science - Assignment 2
# Yonatan Hadar 
# Dima Goldenberg 


# Set your working directory to be the assignment folder
################################
setwd('C:/Users/user/Downloads')


################################


###########
#  HIGGS  #
###########


# 1. PREPARATION
###################

# 1.a. (1) Load the data in the appropriate format:
###################

data <- read.csv("higgs_train.csv")

# 1.b (1) Set your random seed to some value so that our model comparisons will not be affected by randomness.
###################

set.seed(5)

# 1.c (2) As the Higgs file is large, and will cause our models to run for a long time - take a sample of 50,000 rows,
# and save it with the name data.h.
###################

data.h <- data[sample(nrow(data),50000),]

 
# 1.d. (3) Split the data into test (30%) and train (70%) sets with
# respect to the target variable. Save them as train.h and test.h.
###################
install.packages("caTools")
require(caTools)

data.s = data.h[data.h$Label == 's',]
data.b = data.h[data.h$Label == 'b',]


sample.s = sample.split(data.s$Label, SplitRatio = 0.7) # split
sample.b = sample.split(data.b$Label, SplitRatio = 0.7)

train <- rbind(subset(data.s, sample.s == TRUE),subset(data.b, sample.b == TRUE))
test <- rbind(subset(data.s, sample.s == FALSE),subset(data.b, sample.b == FALSE))


# 2. FEATURE SELECTION AND CORRELATION
###################
install.packages("corrplot")
require(corrplot)

# 2.a. (4) Display the correlation plot of the features. Make sure the plot is clearly visible.
###################
c_mat = cor(train[,1:31])
corrplot(c_mat, type="lower", method = "ellipse", order = "hclust", tl.srt = 0.1)


# 2.b. (2) find features that have a correlation of over 0.65
###################

features = rownames(c_mat)

for (i in 1:length(c_mat[1,])) {
  for (j in 1:i) {
    if (c_mat[i,j] >= 0.65 && i!=j) {
      print(c(features[i],features[j],c_mat[i,j]))  #prints all correlations    
      
    }
  } 
}


# 2.c (1) Save new data frames that will hold your train and test data, with the highly correlated features removed
###################
install.packages("stringi")
install.packages("caret")
require(caret)
features.hc = findCorrelation(c_mat, cutoff=0.65, verbose = TRUE)
train.n = train[,-features.hc]
test.n = test[,-features.hc]




# 3. KNN
###################



# 3.a (4) With the new train and test data frames - predict the test outcomes using knn, with k=1:
###################
install.packages("class")
library(class)

#Scaling
train.cs =  data.frame(scale(train.n[,1:17], center = TRUE, scale = TRUE))
train.cl = train.n[,18]
test.cs = data.frame(scale(test.n[,1:17], center = TRUE, scale = TRUE))
test.cl = test.n[,18]


knn_classes = knn(train=train.cs, test=test.cs,cl=train.cl, k=1)


# 3.b (2) Display the confusion matrix:
###################
install.packages("e1071")
library(e1071)

confusionMatrix(knn_classes,test.cl)

# 3.c (5) Using cross-validation train a knn model. Use input parameters to center and scale beforehand and 
# to have the model train a few different k's (but no more than 3). Your code should run in less than 1 minute.
###################



train.con <- train( preProcess = c("center", "scale"),
  x = train.cs,y =train.cl  , method = "knn"  , trControl = trainControl(method = "cv", repeats = 3, number = 10))

####################################################################################################################

# 3.d (2) Use the model you trained to predict the test data's labels:
###################
test.pred = predict(train.con,test.cs, type="prob")
test.pred.l = predict(train.con,test.cs)

confusionMatrix(test.pred.l,test.cl)

# 4. ROC & F-MEASURE
###################

# 4.a. (6) Display the ROC for the model you trained.
###################
install.packages("pROC")
library(pROC)
roccurve <- roc(test.cl, test.pred[,1], plot = TRUE) # roc - smooth
        
plot(roccurve,  print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, legacy.axes = TRUE ,col = 2) # roc - steps


# 4.b. (6) Show the F1 measure (F-measure with alpha =0.5). Teams that will achieve this using an existing function for f-measure will gain all points.
###################
install.packages("ROCR")
require(ROCR)
pred <- prediction(test.pred[,1], test.cl);
fperf = performance(pred,"f") # calculate f with existing function
print('F-mes:')
print(attr(fperf,"y.values")[[1]][which(attr(fperf,"x.values")[[1]]==0.5)]) #retrive 0.5 value



###########
# TITANIC #
###########


# PREPARATIONS
###################

# 5.a load train file
###################

titanic <- read.csv("titanic_train.csv")


# 5.b (3) impute the missing values for the feature Age using the mean (disregard missing values of categoricals):
###################

titanic$Age[is.na(titanic$Age)] = mean(titanic$Age, na.rm=TRUE)

# LOF 
###################

# 6.a (4) plot the density of the LOF scores using only the following
# "5 features": Age, Fare, Pclass, SibSp and Parch. 
###################
install.packages("DMwR")
library(DMwR)
titanic.sm = titanic[c('Age', 'Fare', 'Pclass', 'SibSp', 'Parch')]

os <- lofactor(titanic.sm, k=5) # calc LOF
os[is.nan(os)] = 0
graphics.off()
plot(density(os))

# 6.b. (4) Based on the plot above - remove outliers above a certain LOF score threshold:
###################

titanic.ro = titanic[which(os<4),] 


# 6.c. (4) add two new columns "male" and "female" which will be dummy variables presenting the sex of the passenger
###################


titanic.ro$male = 0
titanic.ro$female = 0
titanic.ro$male[which(titanic.ro$Sex == 'male')] = 1
titanic.ro$female[which(titanic.ro$Sex == 'female')] = 1


# Made for 7.b - new dummy cols
titanic.ro$C = 0
titanic.ro$Q = 0
titanic.ro$S = 0
titanic.ro$C[which(titanic.ro$Embarked == 'C')] = 1
titanic.ro$Q[which(titanic.ro$Embarked == 'Q')] = 1
titanic.ro$S[which(titanic.ro$Embarked == 'S')] = 1

# 7. SVM 
###################


# 7.a (1) split to train (70%) and test (30%):
###################
require(caTools)
set.seed(777) # lucky seed
sample.tit = sample.split(titanic.ro$Survived, SplitRatio = 0.7)      
tit.train <-(subset(titanic.ro, sample.tit == TRUE))
tit.test <- (subset(titanic.ro, sample.tit == FALSE))


# 7.b (4) Create an SVM model with as many features as possible. 
# Your grade on this will be paritally based on your error rate (computed below)
# and partially on your feature selection. 
# Use it to predict the test labels and save the predictions with the name res (for results).
###################
install.packages("plyr")
library(plyr)
library(e1071)
library(class)
library(caret)


ttrain.cl = tit.train[c("PassengerId", "Pclass" , "Age" ,"SibSp"  , "Parch"  , "Fare"  ,"male" , "female" , "C" , "Q","S","Survived" )]
ttest.cl = tit.test[c("PassengerId", "Pclass" , "Age" ,"SibSp"  , "Parch"  , "Fare"  ,"male" , "female" , "C" , "Q","S" )]
train.label = tit.train$Survived
test.label = tit.test$Survived

tit.svm <- svm(x = ttrain.cl[,1:11],y=train.label, cost = 100, gamma = 0.2, kernel = 'linear', type = 'C-classification', scale = TRUE)
res = predict(tit.svm ,ttest.cl )


# 7.c (1) compute the error rate:
###################

CMT = confusionMatrix(res, test.label) 
print(CMT)
Errorrate = 1- CMT$overall[1]
Errorrate = rename(Errorrate, c("Accuracy" = "Error Rate"))
Errorrate

# 7.d (6) Tune the SVM model using no more than 5 different costs, 5 different gammas and 5 CV.
# Full points if you improve your error rate to < 18%.
###################


tune.out = tune(svm, Survived ~  PassengerId + Pclass + Age + SibSp + Parch + Fare + male + female + C + Q + S, data = ttrain.cl, ranges = list(kernel = c('radial','linear','sigmoid' ) ,gamma = c( 0.1,0.2,0.5,1) ,cost = c( 0.1,0.2,0.5,1)))
#Best performance = 0.1523903 

# 7.e (3) display the best model found (its parametes) and use it to predict the test 
# values - save the predictions with the name res2.
###################
print(tune.out)

tit.svm2 <- svm(x = ttrain.cl[,1:11],y=train.label, cost = 1, gamma = 0.2, kernel = 'radial', type = 'C-classification', scale = TRUE)
res2 = (predict(tit.svm2 ,ttest.cl ))

CMT2 = confusionMatrix(res2, test.label) 


# 7.g (1) show if it improved by computing the new error rate: 
###################

Errorrate2 = 1- CMT2$overall[1]
Errorrate2 = rename(Errorrate2, c("Accuracy" = "Error Rate"))
print (Errorrate2 - Errorrate)
#improved - new errorrate is 0.1653

# 8. RANDOM FOREST
###################
install.packages("randomForest")
library(randomForest)

# 8.a (4) Create a random forest model with as many features as possible (but
# choose them wisely). Use no more than 2000 trees.
###################

ttrain.rf = tit.train[c("PassengerId", "Pclass" ,"Sex", "Age" ,"SibSp"  , "Parch"  , "Fare" )]
ttest.rf = tit.test[c("PassengerId", "Pclass" ,"Sex", "Age" ,"SibSp"  , "Parch"  , "Fare" )]
train.label = tit.train$Survived
test.label = tit.test$Survived 
tl = data.frame(train.label)
tl = factor(tl$train.label)
forest = randomForest(x = ttrain.rf, y =tl, ntree=2000 )


# 8.b (1) Use your model to predict the test outcome and save your predictions as resForest.
###################

resForest = predict(forest ,ttest.rf)

# 8.c (1) display the error rate:
###################

CMT3 = confusionMatrix(resForest, test.label) 
print(CMT3)


Errorrate3 = 1- CMT3$overall[1]
Errorrate3 = rename(Errorrate3, c("Accuracy" = "Error Rate"))
print (Errorrate3) #0.13


# 8.d (3) Find a function that plots the importance of the variables to see how 
# each variable, when taken out, affected the accuracy and gini measures:
###################

#plot(forest, log="y")

varImpPlot(forest)
  


# 9. KMEANS
###################

# 9.a (3) using the "5 features" (see LOF) run kmeans using 3 centers.
###################

#titanic.sm -  needed dataframe
install.packages("graphics")
require(graphics)
install.packages("rattle")
require(rattle)

cent.titanic.sm = scale(titanic.sm, center = TRUE, scale = TRUE)


km.model = kmeans(cent.titanic.sm, 3)




# 9.b (3) plot the clusters:
###################
install.packages("cluster")
install.packages("fpc")
library(cluster) 
library(fpc)

plot(titanic.sm, col = km.model$cluster)
plotcluster(titanic.sm, km.model$cluster)
clusplot(titanic.sm, km.model$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)


  
  # 9.c (2) display the centers - do they seem to make sense?
  ###################

km.model$centers
# Yes they make sense

# 10. PCA (using your train and test data from the Higgs section)
###################

# 10.a (3) Compute the train data's principal components 
###################
cent.train = scale(train[2:31], center = TRUE, scale = TRUE)
cent.test = scale(test[2:31], center = TRUE, scale = TRUE)
higgs.pca.tr = prcomp(cent.train, )
higgs.pca.te = prcomp(cent.test)


# 10.b (2) Plot the drop in the variance explained by the PC's:
###################
ex.var = higgs.pca.tr$sdev^2 / sum(higgs.pca.tr$sdev^2) 
barplot(ex.var)

# 10.c (6) Using the PC's you've created above create two new data frames named 
# train.h.pca and test.h.pca in which the features
# are replaced by PCs (our 'new features').
###################

train.h.pca= data.frame(higgs.pca.tr$x)
test.h.pca = data.frame(higgs.pca.te$x)
# 10.d (3) Using only the first 3 PC's - fit a simple knn model (like the first one we did) with k=7:
###################

library(class)

pca.knn_classes = knn(train=train.h.pca[,1:3], test=test.h.pca[1:3],cl=train[,32], k=7)


# 10.e (2) Show the confusion matrix:
###################

library(caret)

confusionMatrix(pca.knn_classes,test[,32])
