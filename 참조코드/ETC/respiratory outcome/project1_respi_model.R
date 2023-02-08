###########################################################################################################################
# Splitting the dataset into the Training set and Test set
set.seed(1234)

split = createDataPartition(paste(model_DF$outcome_3), p = 0.75, list=F)
train_set = model_DF[split,]
test_set = model_DF[-split,]

dim(train_set) # 2775
dim(test_set) # 924

table(train_set$outcome_3) #  0:2621, 1:154
table(test_set$outcome_3) # 0: 873, 1: 51

table(train_set$스테로이드여부) # 0:86, 1:2689
table(test_set$스테로이드여부) # 0:31, 1:893

###########################################################################################################################
# numeric scale 조정 (age, GA, bmi, 입원총기간)

#train_set[c('age','GA_week','bmi','입원총기간')] = scale(train_set[c('age','GA_week','bmi','입원총기간')])
#test_set[c('age','GA_week','bmi','입원총기간')] = scale(test_set[c('age','GA_week','bmi','입원총기간')])

train_set[c('GA_week')] = scale(train_set[c('GA_week')])
test_set[c('GA_week')] = scale(test_set[c('GA_week')])
head(train_set)

###########################################################################################################################
# Fitting glm to the Training set
classifier = glm(formula = outcome_3 ~ .,
                 family = binomial,
                 data = train_set)

summary(classifier) #aic 1221

###########################odds ratio######################################
ss4_or<-data.frame(round(exp(coef(classifier)),2),round(exp(confint(classifier)),2),round(summary(classifier)$coefficient[,4],4))

colnames(ss4_or)<-c("OR","2.5%","97.5%","p")

ss4_or

summary(classifier)$coefficient
confint(classifier)
######################################################################################################################################
# prob_pred2 = predict(classifier, type = "response", newdata = test_set)
# y_pred2 = ifelse(prob_pred2 >0.5, 1,0)
# table(y_pred2)

# stepAIC
stepAIC(classifier, direction = "both")

# 최종 모델 
model_fin <-   glm(formula = outcome_3 ~ 산과력_출산력P + GA_week + 스테로이드여부, 
                   family = binomial, data = train_set)

summary(model_fin) # 1123

#########################################################################################
ss4 <- glm(formula = outcome_3 ~ 스테로이드여부, family = binomial, data = train_set)
ss4_or<-data.frame(round(exp(coef(ss4)),2),round(exp(confint(ss4)),2),round(summary(ss4)$coefficient[,4],4))
colnames(ss4_or)<-c("OR","2.5%","97.5%","p")
ss4_or
# 스테로이드 여부 1 odd ratio 0.95 / p : 0.9134
##################odd ratio##############################################################
ss4<- glm(formula = outcome_3 ~ 산과력_출산력P + GA_week + 스테로이드여부, 
          family = binomial, data = train_set)
ss4_or<-data.frame(round(exp(coef(ss4)),2),round(exp(confint(ss4)),2),round(summary(ss4)$coefficient[,4],4))

colnames(ss4_or)<-c("OR","2.5%","97.5%","p")

ss4_or

summary(ss4)$coefficient
confint(ss4)

###############################################################################################################

# Predicting the Test set results
prob_pred = predict(model_fin, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

table(y_pred) 

#######
y_actual <- as.data.frame(test_set$outcome_3)
y_pred2 <- as.data.frame(y_pred)

db <- cbind(y_actual, y_pred2)
db <- db %>% 
  mutate(accurate = 1*(y_pred2 == y_actual))
sum(db$accurate)/nrow(db) # 0.94

#######
confusionMatrix(data = y_pred,
                reference = test_set$outcome_3,
                positive = "Defalult")


# Making the Confusion Matrix
cm = table(test_set$outcome_3, y_pred)
cm
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy # 0.94 (쓰레기 모델임. 모두 0으로 예측함)

######################################################################################################################################
# ROC 
ROC(test = y_pred, stat = test_set$outcome_3, plot = "ROC", AUC= T)

######################################################################################################################################
# modeling - LASSO
train_X = train_set[,-18]
train_y = train_set[,18]

test_X = test_set[,-18]
test_y = test_set[,18]

str(train_X)
str(train_y)

elasticnet_1 <- glmnet(as.matrix(train_X), as.matrix(train_y), alpha=0.5, family = "binomial")
plot(elasticnet_1, xvar ="lambda") # 람다에 따른 회귀계수 확인하기

# cross validation
set.seed(100)
cv.Elasticnet <- cv.glmnet(data.matrix(train_X), data.matrix(train_y), alpha = 0.5, family = "binomial")
plot(cv.Elasticnet)
best_lambda <- cv.Elasticnet$lambda.min #오차가 제일 작은 값 (최적 람다)
best_lambda #0.01732262

# best_lambda 값으로 회귀계수 확인
Elasticnet_coef <- coef(elasticnet_1, s=best_lambda)
Elasticnet_coef 

Elasticnet_coef_2 <- coef(cv.Elasticnet, s=best_lambda)
Elasticnet_coef_2 

#######
prob_pred <-predict(cv.Elasticnet, type = 'response',newx = data.matrix(test_X))
y_pred = ifelse(prob_pred > 0.5, 1, 0)
confusionMatrix(data = y_pred,
                reference = test_y,
                positive = "Defalult")
table(y_pred)
