dat_train = read.csv('/Users/shubham/Desktop/DataMining/Datasets/train-1.csv', header = T)
dat_test = read.csv('/Users/shubham/Desktop/DataMining/Datasets/test-1.csv', header = T)

dat_new_train = dat_train
dat_new_test = dat_test

extract_title = function(dat){
  dat$Name = as.character(dat$Name)
  title = numeric()
  for(i in 1:nrow(dat)){
  dummy = unlist(strsplit(dat$Name[i],", "))[2]
  title[i] = unlist(strsplit(dummy,". "))[1]
  }
  
  for(i in 1:length(title)){
    if(title[i] == "Mme" | title[i] == "Mlle" | title[i] == "Ms")
      title[i] = "Miss"
    if( (title[i]!="Mr" & title[i]!="Mrs" & title[i]!="Miss" & title[i]!="Master"))
      title[i] = "Others"
  }
  return(title)
}

dat_new_train$Title = as.factor(extract_title(dat_new_train))
dat_new_test$Title = as.factor(extract_title(dat_new_test))

# proportion of people saved based on category
prop = numeric()
k = 1
title_num = as.numeric(dat_new_train$Title)
for(i in c(3,4,2,1,5)){
  index = which(title_num == i)
  prop[k] = sum(dat_new_train$Survived[index]) / sum(title_num == i)
  k = k+1
}
prop


# Removing unnecessary Variables
dat_new_train$Name = dat_new_train$Cabin = dat_new_train$Ticket = NULL
dat_new_test$Name = dat_new_test$Cabin = dat_new_test$Ticket = NULL

head(dat_new_train)
head(dat_new_test)

par(mfrow = c(1,1))
boxplot(dat_new_train$Age ~ dat_new_train$Pclass)
boxplot(dat_new_train$Pclass ~ dat_new_train$Sex)
hist(dat_train$Age)

str(dat_new_train)

# Imputing thos two records with Embarked as 'S'
dat_new_train$Embarked[dat_new_train$Embarked == ''] = 'S'
dat_new_train$Embarked = droplevels(dat_new_train$Embarked)
levels(dat_new_train$Embarked)
# Imputing the missing values for training set
for(i in 1:length(colnames(dat_new_train))){
  bool = is.na(dat_new_train[, i])
  if(length(bool) > 0){
    # dat_new_train[bool & (dat_new_train$Sex == "male") & (dat_new_train$Pclass == 1), i] = round(mean(dat_new_train[dat_new_train$Sex == "male" & (dat_new_train$Pclass == 1), i], na.rm = TRUE), 1)
    # dat_new_train[bool & (dat_new_train$Sex == "male") & (dat_new_train$Pclass == 2), i] = round(mean(dat_new_train[dat_new_train$Sex == "male" & (dat_new_train$Pclass == 2), i], na.rm = TRUE), 1)
    # dat_new_train[bool & (dat_new_train$Sex == "male") & (dat_new_train$Pclass == 3), i] = round(mean(dat_new_train[dat_new_train$Sex == "male" & (dat_new_train$Pclass == 3), i], na.rm = TRUE), 1)
    # 
    # dat_new_train[bool & (dat_new_train$Sex == "female") & (dat_new_train$Pclass == 1), i] = round(mean(dat_new_train[dat_new_train$Sex == "female" & (dat_new_train$Pclass == 1), i], na.rm = TRUE), 1)
    # dat_new_train[bool & (dat_new_train$Sex == "female") & (dat_new_train$Pclass == 2), i] = round(mean(dat_new_train[dat_new_train$Sex == "female" & (dat_new_train$Pclass == 2), i], na.rm = TRUE), 1)
    # dat_new_train[bool & (dat_new_train$Sex == "female") & (dat_new_train$Pclass == 3), i] = round(mean(dat_new_train[dat_new_train$Sex == "female" & (dat_new_train$Pclass == 3), i], na.rm = TRUE), 1)
    # 
    dat_new_train$Age[bool & (dat_new_train$Pclass == 1)] = round(mean(dat_new_train$Age[dat_new_train$Pclass == 1], na.rm = TRUE), 1)
    dat_new_train$Age[bool & (dat_new_train$Pclass == 2)] = round(mean(dat_new_train$Age[dat_new_train$Pclass == 2], na.rm = TRUE), 1)
    dat_new_train$Age[bool & (dat_new_train$Pclass == 3)] = round(mean(dat_new_train$Age[dat_new_train$Pclass == 3], na.rm = TRUE), 1)
    
  }
}

# Imputing the missing values for testing set
for(i in 1:length(colnames(dat_new_test))){
  bool = is.na(dat_new_test[, i])
  if(length(bool) > 0){
    # dat_new_test[bool & (dat_new_test$Sex == "male") & (dat_new_test$Pclass == 1), i] = round(mean(dat_new_test[dat_new_test$Sex == "male" & (dat_new_test$Pclass == 1), i], na.rm = TRUE), 1)
    # dat_new_test[bool & (dat_new_test$Sex == "male") & (dat_new_test$Pclass == 2), i] = round(mean(dat_new_test[dat_new_test$Sex == "male" & (dat_new_test$Pclass == 2), i], na.rm = TRUE), 1)
    # dat_new_test[bool & (dat_new_test$Sex == "male") & (dat_new_test$Pclass == 3), i] = round(mean(dat_new_test[dat_new_test$Sex == "male" & (dat_new_test$Pclass == 3), i], na.rm = TRUE), 1)
    # 
    # dat_new_test[bool & (dat_new_test$Sex == "female") & (dat_new_test$Pclass == 1), i] = round(mean(dat_new_test[dat_new_test$Sex == "female" & (dat_new_test$Pclass == 1), i], na.rm = TRUE), 1)
    # dat_new_test[bool & (dat_new_test$Sex == "female") & (dat_new_test$Pclass == 2), i] = round(mean(dat_new_test[dat_new_test$Sex == "female" & (dat_new_test$Pclass == 2), i], na.rm = TRUE), 1)
    # dat_new_test[bool & (dat_new_test$Sex == "female") & (dat_new_test$Pclass == 3), i] = round(mean(dat_new_test[dat_new_test$Sex == "female" & (dat_new_test$Pclass == 3), i], na.rm = TRUE), 1)
    dat_new_test$Age[bool & (dat_new_test$Pclass == 1)] = round(mean(dat_new_test$Age[dat_new_test$Pclass == 1], na.rm = TRUE), 1)
    dat_new_test$Age[bool & (dat_new_test$Pclass == 2)] = round(mean(dat_new_test$Age[dat_new_test$Pclass == 2], na.rm = TRUE), 1)
    dat_new_test$Age[bool & (dat_new_test$Pclass == 3)] = round(mean(dat_new_test$Age[dat_new_test$Pclass == 3], na.rm = TRUE), 1)
    
    
  }
}

# Imputing the missing value for fare column
dat_new_test$Fare[is.na(dat_new_test$Fare)] = round(mean(dat_new_test$Fare[dat_new_test$Pclass == 3], na.rm = TRUE), 2)
dat_new_test[153,]
sum(is.na(dat_new_train))
sum(is.na(dat_new_test$Fare))
head(dat_new_test)

# Function for making model parameters to numeric for training and testing dataset
making_numeric = function(dat){
  for(i in 1:length(colnames(dat))){
    if(class(dat[, i]) == "factor"){
      dat[, i] = as.numeric(dat[, i])
    }
  }
  return(dat)
}

# Grouping Age variable 
dat_new_train[dat_new_train['Age'] <= 16, 'Age'] = 0
dat_new_train[(dat_new_train['Age'] > 16) & (dat_new_train['Age'] <= 32), 'Age'] = 1
dat_new_train[(dat_new_train['Age'] > 32) & (dat_new_train['Age'] <= 48), 'Age'] = 2
dat_new_train[(dat_new_train['Age'] > 48) & (dat_new_train['Age'] <= 64), 'Age'] = 3
dat_new_train[dat_new_train['Age'] > 64, 'Age'] = 4

dat_new_test[dat_new_test['Age'] <= 16, 'Age'] = 0
dat_new_test[(dat_new_test['Age'] > 16) & (dat_new_test['Age'] <= 32), 'Age'] = 1
dat_new_test[(dat_new_test['Age'] > 32) & (dat_new_test['Age'] <= 48), 'Age'] = 2
dat_new_test[(dat_new_test['Age'] > 48) & (dat_new_test['Age'] <= 64), 'Age'] = 3
dat_new_test[dat_new_test['Age'] > 64, 'Age'] = 4

# Grouping Fare variable
dat_new_train[dat_new_train['Fare'] <= 7.91, 'Fare'] = 0
dat_new_train[(dat_new_train['Fare'] > 7.91) & (dat_new_train['Fare'] <= 14.454), 'Fare'] = 1
dat_new_train[(dat_new_train['Fare'] > 14.454) & (dat_new_train['Fare'] <= 31), 'Fare'] = 2
dat_new_train[dat_new_train['Fare'] > 31, 'Fare'] = 3

dat_new_test[dat_new_test['Fare'] <= 7.91, 'Fare'] = 0
dat_new_test[(dat_new_test['Fare'] > 7.91) & (dat_new_test['Fare'] <= 14.454), 'Fare'] = 1
dat_new_test[(dat_new_test['Fare'] > 14.454) & (dat_new_test['Fare'] <= 31), 'Fare'] = 2
dat_new_test[dat_new_test['Fare'] > 31, 'Fare'] = 3


# Making numeric Sex variable
dat_new_train$Sex = ifelse(dat_new_train$Sex == "male", 0, 1)
dat_new_test$Sex = ifelse(dat_new_test$Sex == "male", 0, 1)

# Combining two columns into One
dat_new_train['FamilySize'] = dat_new_train['SibSp'] + dat_new_train['Parch'] + 1
dat_new_test['FamilySize'] = dat_new_test['SibSp'] + dat_new_test['Parch'] + 1

# Creating new column IsAllone
dat_new_train['IsAlone'] = 0
dat_new_test['IsAlone'] = 0
dat_new_train$IsAlone = ifelse(dat_new_train$FamilySize == 1, 0, 1)
dat_new_test$IsAlone = ifelse(dat_new_test$FamilySize == 1, 0, 1)

# Removing All unecessary columns
dat_new_train$Parch = dat_new_train$SibSp = dat_new_train$FamilySize = NULL
dat_new_test$Parch = dat_new_test$SibSp = dat_new_test$FamilySize = NULL

# Adding interaction variable
dat_new_train['Age*Pclass'] = dat_new_train$Age * dat_new_train$Pclass
dat_new_test['Age*Pclass'] = dat_new_test$Age * dat_new_test$Pclass


y.train = dat_new_train[, 2]
dat_new_train = making_numeric(dat_new_train)
dat_new_test = making_numeric(dat_new_test)
head(dat_new_train)
head(dat_new_test)

x.train = dat_new_train[, -1]
x.test = dat_new_test[, -1]
x.train = data.frame(x.train)
x.test = data.frame(x.test)
head(x.train)
head(x.test)


library(class)
set.seed(1)
knn_model = knn(x.train[, -1], x.test, y.train)
summary(knn_model)
write.csv(cbind(PassengerId = dat_new_test$PassengerId, Survived = knn_model), "/Users/shubham/Desktop/DataMining/Result_set_Titanic/prediction_knn.csv", row.names=FALSE)


library(MASS)
set.seed(1)
lda_model = lda(as.factor(Survived) ~., data = x.train)
pred_lda = predict(lda_model, newdata = x.test)$class
summary(pred_lda)
write.csv(data.frame(PassengerId = dat_new_test$PassengerId, Survived = pred_lda), "/Users/shubham/Desktop/DataMining/Result_set_Titanic/prediction_lda.csv", row.names=FALSE)

set.seed(1)
qda_model = qda(as.factor(Survived) ~., data = x.train)
pred_qda = predict(qda_model, newdata = x.test)$class
summary(pred_qda)
write.csv(data.frame(PassengerId = dat_new_test$PassengerId, Survived = pred_qda), "/Users/shubham/Desktop/DataMining/Result_set_Titanic/prediction_qda.csv", row.names=FALSE)

library(glmnet)
set.seed(1)
oglm = glm(Survived~., data = x.train)
pred_glm = predict(oglm, newdata = x.test, type = "response")
glm_out = as.factor(ifelse(pred_glm > 0.5, 1, 0))
summary(glm_out)
write.csv(data.frame(PassengerId = dat_new_test$PassengerId, Survived = glm_out), "/Users/shubham/Desktop/DataMining/Result_set_Titanic/prediction_glm.csv", row.names=FALSE)

library(tree)
set.seed(1)
?prune.misclass
tree.out = tree(as.factor(Survived) ~., data = x.train)
pred_tree = predict(tree.out, newdata = x.test, type = "class")
summary(pred_tree)

cv_tree = cv.tree(tree.out, FUN = prune.misclass)
plot(cv_tree$dev, cv_tree$size, t= 'b')
index1 = which.min(cv_tree$dev)

final_tree = prune.misclass(tree.out, best = cv_tree$size[index1])
pred_tree1 = predict(final_tree, newdata = x.test, type = "class")
summary(pred_tree1)
write.csv(data.frame(PassengerId = dat_new_test$PassengerId, Survived = pred_tree1), "/Users/shubham/Desktop/DataMining/Result_set_Titanic/prediction_decisiontree.csv", row.names=FALSE)

library(randomForest)
set.seed(1)
head(x.train)
rf = randomForest(as.factor(Survived) ~., data = x.train, ntree = 1000)
rf.out = predict(rf, newdata = x.test, type = "class")
summary(rf.out)
varImpPlot(rf)
write.csv(data.frame(PassengerId = dat_new_test$PassengerId, Survived = rf.out), "/Users/shubham/Desktop/DataMining/Result_set_Titanic/prediction_randomForest.csv", row.names=FALSE)


library(gbm)
# set.seed(1)
# head(dat_new_train)
# folds = cut(1:nrow(dat_new_train), breaks = 10, labels = FALSE)
# spec = sens = acc = matrix(NA, 10, 17)
# index_loc = sample(1:nrow(dat_new_train), replace = FALSE)
# new_data = dat_new_train[index_loc, ]
# for(i in 1:10){
#   index = which(folds != i)
#   gbm_model = gbm(Survived ~ ., data = new_data[index, -1], n.trees = 100)
#   pred_gbm = predict(gbm_model, newdata = new_data[-index, -1], type = "response", n.trees = 100)
#   y_test = new_data[-index, 2]
#   head(new_data)
#   k = 1
#   for(s in seq(0.1,0.9,0.05)){
#     tb = table((pred_gbm > s), y_test)
#     spec[i, k] = tb[2,1]/sum(tb[, 1])
#     sens[i, k] = tb[2,2]/sum(tb[, 2])
#     acc[i, k] = sum(diag(tb))/sum(tb)
#     k = k+1
#   }
# }
# library(pROC)
# proc = roc(y_test, pred_gbm)
# plot(proc)
# abline(h = c(0.5, 0.55, 0.6, 0.65))
# points(1-spec, sens)
# 
# count = numeric()
# for(i in 1:17){
#   count[i] = sum(acc[, i])
# }
# which.max(count)
# count[10]
# seq(0.1,0.9,0.05)[10]


set.seed(1)
gbm_model = gbm(Survived ~ ., data = x.train, n.trees = 500)
pred_gbm = predict(gbm_model, newdata = x.test, type = "response", n.trees = 500)
out = as.factor(ifelse(pred_gbm > 0.55, 1, 0))
summary(out)
write.csv(data.frame(PassengerId = dat_new_test$PassengerId, Survived = out), "/Users/shubham/Desktop/DataMining/Result_set_Titanic/prediction_gbm.csv", row.names=FALSE)



