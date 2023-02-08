# XGBoost

# Importing the dataset
dataset = read.csv('pr1_df_model.csv', fileEncoding = "euc-kr")
head(dataset)
str(dataset)

dataset[,c("twin","성별","전자간증","PIH임신중고혈압","고혈압","산과력_출산력P","산과력_출산력A","응급실_횟수","입원_횟수","첫투약시기","outcome")]<-
  lapply(dataset[,c("twin","성별","전자간증","PIH임신중고혈압","고혈압","산과력_출산력P","산과력_출산력A","응급실_횟수","입원_횟수","첫투약시기","outcome")], factor)

# Splitting Training set and Test set (8:2)
set.seed(1670)
split = sample.split(dataset$outcome, SplitRatio = 0.8)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

train_label <- as.numeric(train_set$outcome)-1
test_label <- as.numeric(test_set$outcome)-1


X_train<- model.matrix(~.-1, data = train_set[,-15]) %>% data.frame
X_test <- model.matrix(~.-1, data = test_set[,-15]) %>% data.frame

dtrain <- xgb.DMatrix(data = as.matrix(X_train),
                      label = train_label)

dtest <- xgb.DMatrix(data = as.matrix(X_test))


param <- list(objective   = "binary:logistic",
              eval_metric = "error",
              max_depth   = 5,
              eta         = 0.01,
              subsample = 0.8,
              min_child_weight = 1)

# binary:logistic : 반환값을 확률값으로 반환
# max_depth : 과적합 방지, 3-10
# eta : 학습률, default 0.3, 0,1사이 
# subsample : 개별의사결정나무 모형에 사용되는 임의 표본수 비율로 지정
# min_child_weight : 과적합 방지 목적 


# Fitting XGBoost 
classifier <- xgboost(param=param, data = dtrain, nrounds=10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = dtest)
y_pred = (y_pred >= 0.5)

# get the feature real names
names <- dimnames(X_train[, -c(15)])[[2]]
names

# Making the Confusion Matrix
cm = table(test_set[, 15], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy


# Applying k-Fold Cross Validation
folds = createFolds(training_set$outcome, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_fold[-15]), label = training_fold$outcome, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-15]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 15], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy  #0.43

# feature importance

importance_matrix <- xgb.importance(names, model = classifier)
fig(15,10)
par(family ="AppleGothic")
xgb.plot.importance(importance_matrix)









