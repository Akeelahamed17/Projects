setwd("~/Desktop/MA429 Mock Project/")
#Load Relevant Libraries
library(e1071)
library(caret)
library(corrplot) 
#Read in Data
accelerometer_data <- read.table("accelerometer.csv", sep = ";", header = TRUE, dec = ",")
head(accelerometer_data)
summary(accelerometer_data)
dim(accelerometer_data)

#Check for any Missing Values
anyNA(accelerometer_data)

# Convert all numerical variables to class "numeric" to enable correlation computation
accelerometer_data[,3:18] <- sapply(accelerometer_data[,3:18],as.numeric)

#Create a subset to work on before trying on full dataset
set.seed(201316007)
subset1 <- sample(165633, 1000)
accelerometer_subset <- accelerometer_data[subset1,]

# Feature Selection
correlations <- cor(accelerometer_data[,3:18])
corrplot(correlations, method = "circle")

#Rank features by importance 
#ensure results are repeatable
set.seed(201316007)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(class~., data=accelerometer_subset, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#Create a training and testing set with a 0.75:0.25 ratio by random sampling
set.seed(201316007)
split <- sample(seq_len(nrow(accelerometer_subset)), size = floor(0.75*nrow(accelerometer_subset)))
#train_set<- accelerometer_subset[split,]

#####Creating training and testing set from subset, having features "weight" and "BMI" removed
train_set<- accelerometer_subset[split,-c(4,5)]
#test_set <- accelerometer_subset[-split,]
test_set <- accelerometer_subset[-split,-c(4,5)]
dim(train_set)
dim(test_set)

############Method 1) : Support Vector Machines ##################
#Perform 10 fold CV on the model, varying the cost, to determine which cost is best for a linear kernel
set.seed(201316007)
tune.out = tune(svm, class~., data = train_set,kernel = "linear", ranges = 
                  list(cost = c(0.0001, 0.01, 0.1,1,5,10,20)))
summary(tune.out)
bestmod = tune.out$best.model
#bestmod says that a cost of 5 gives the lowest error.

classpred <- predict(bestmod, test_set[,-17])
confusionMatrix(table(predict = classpred, truth = test_set$class))


# Trying with a Polynomial kernel, tuning cost and degree
set.seed(201316007)
tune.out = tune(svm, class~., data = train_set,kernel = "polynomial", ranges = 
                  list(cost = c(0.0001, 0.01, 0.1,1,5,10,20),degree = c(1,2,3,4,5)))
summary(tune.out)
bestmod = tune.out$best.model
#bestmod says that a cost of 20 and degree of 1 gives the lowest error.

classpred <- predict(bestmod, test_set[,-17])
confusionMatrix(table(predict = classpred, truth = test_set$class))

#Changing kernel to be radial, tuning cost and gamma
set.seed(201316007)
tune.out = tune(svm, class~., data = train_set,kernel = "radial", ranges = 
                  list(cost = c(0.0001, 0.01, 0.1,1,5,10,20), gamma = c(0.5,1,2,3,4)))
summary(tune.out)
bestmod = tune.out$best.model
#bestmod says that a cost of 5 and gamma of 0.5 gives the lowest error.

classpred <- predict(bestmod, test_set[,-17])
confusionMatrix(table(predict = classpred, truth = test_set$class))



#Now Use Radial Kernel with cost = 10, gamma = 0.5 for entire dataset:
set.seed(201316007)
split <- sample(seq_len(nrow(accelerometer_data)), size = floor(0.75*nrow(accelerometer_data)))
# Remove 2 least important features: 
train_set_full<- accelerometer_data[split,-c(4,5)]
test_set_full <- accelerometer_data[-split,-c(4,5)]

dim(train_set_full)
dim(test_set_full)
set.seed(201316007)
start.time <- Sys.time()
svm_fit = svm(class~., data = train_set_full,kernel = "radial", cost = 10, gamma = 0.5)
end.time <- Sys.time()
end.time - start.time
summary(svm_fit)

classpred <- predict(svm_fit, test_set_full[,-17])
confusionMatrix(table(predict = classpred, truth = test_set_full$class))

# [1] 0.9938178%


##################Method 2) KNN ##########################:
library(class)
#Standardize numeric predictors so that each has a mean of 0 and standard deviation of 1
standardized.train.X = scale(train_set[,c(3:16)])
standardized.test.X = scale(test_set[,c(3:16)])
train.Y = train_set[,17]
test.Y = test_set[,17]

set.seed(201316007)
start.time <- Sys.time()
knn.pred = knn(standardized.train.X, standardized.test.X,train.Y,k=1)
end.time <- Sys.time()
end.time - start.time
confusionMatrix(table(predict = classpred, truth = test_set$class))
# 0.9%

#Trying with k = 3
set.seed(201316007)
knn.pred = knn(standardized.train.X, standardized.test.X,train.Y,k=2)
test_accuracy <- function(table_values){
  sum(diag(table_values))/sum(table_values)
}
test_accuracy(table(knn.pred, test.Y))

#Trying KNN for entire Dataset
set.seed(201316007)
split <- sample(seq_len(nrow(accelerometer_data)), size = floor(0.75 *nrow(accelerometer_data)))
# Remove 2 least important features: 
train_set_full<- accelerometer_data[split,-c(1:6,19)]
test_set_full <- accelerometer_data[-split,-c(1:6,19)]

#standardized.train.X.full = scale(train_set_full[,c(3:16)])
#standardized.test.X.full = scale(test_set_full[,c(3:16)])
train.Y.full = accelerometer_data[split,19]
test.Y.full = accelerometer_data[-split,19]

set.seed(201316007)
knn.pred = knn(train_set_full,test_set_full,train.Y.full,k=6)
confusionMatrix(table(knn.pred, test.Y.full))

############ Method 3) LDA ###################
library(MASS)
set.seed(201316007)
split <- sample(seq_len(nrow(accelerometer_data)), size = floor(0.75*nrow(accelerometer_data)))
# Remove 2 least important features: 
start.time <- Sys.time()
lda.fit <- lda(accelerometer_data$class ~ ., data = accelerometer_data[,-c(1:6)], subset =split )
end.time <- Sys.time()
end.time - start.time
lda.pred <- predict(lda.fit, accelerometer_data[-split,])
confusionMatrix(table(lda.pred$class, accelerometer_data[-split,]$class))


############## Method 4) QDA #################
library(MASS)
set.seed(201316007)
split <- sample(seq_len(nrow(accelerometer_data)), size = floor(0.75*nrow(accelerometer_data)))
# Remove 2 least important features: 
start.time <- Sys.time()
qda.fit <- qda(accelerometer_data$class ~ ., data = accelerometer_data[,-c(1:6)], subset =split )
end.time <- Sys.time()
end.time - start.time
qda.pred <- predict(qda.fit, accelerometer_data[-split,])
confusionMatrix(table(qda.pred$class, accelerometer_data[-split,]$class))

