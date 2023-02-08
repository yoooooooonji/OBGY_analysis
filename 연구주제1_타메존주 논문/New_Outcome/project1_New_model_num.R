###########################################################################################################################
model_DF <- subset(model_DF, select = -c(age_2,bmi_2, 입원총기간_2))

# Splitting the dataset into the Training set and Test set
set.seed(5678)

split = createDataPartition(paste(model_DF$outcome_3), p = 0.75, list=F)
train_set = model_DF[split,]
test_set = model_DF[-split,]

dim(train_set) # 2773
dim(test_set) # 923

table(train_set$outcome_3) #  0: 1527 , 1: 1246
table(test_set$outcome_3) # 0: 508, 1: 415

table(train_set$스테로이드여부) # 0:2685, 1:88
table(test_set$스테로이드여부) # 0:894, 1:29

###########################################################################################################################
# numeric scale 조정 (age, GA, bmi, 입원총기간)

train_set[c('age','GA_week','bmi','입원총기간')] = scale(train_set[c('age','GA_week','bmi','입원총기간')])
test_set[c('age','GA_week','bmi','입원총기간')] = scale(test_set[c('age','GA_week','bmi','입원총기간')])

head(train_set)
str(model_DF)

summary(model_DF)
summary(train_set)
summary(test_set)

#########################################################################################################################
# 극도로 수 작은 변수 삭제하고 돌리기 #100건 이하 
# test_df <- subset(model_DF, select = -c(PIH임신중고혈압, 고혈압, 태아성장지연, 태반조기박리, 부인과수술력, 자궁봉축술))
# 
# split = createDataPartition(paste(test_df$outcome_3), p = 0.75, list=F)
# test_tr = test_df[split,]
# test_te = test_df[-split,]
# 
# model1 <- glm(formula = outcome_3 ~ .,
#               family = binomial,
#               data = test_tr)
# 
# stepAIC(model1, direction = "both") 
# model2 <-  glm(formula = outcome_3 ~ age + twin + bmi + 전자간증 + 수축억제제 + 
#                  산과력_출산력A + GA_week + 스테로이드여부, family = binomial, 
#                data = test_tr)
# summary(model2)
# 
# prob_pred = predict(model2, type = 'response', newdata = test_set)
# y_pred = ifelse(prob_pred > 0.5, 1, 0)
# 
# # Making the Confusion Matrix
# cm = table(test_te$outcome_3, y_pred)
# cm
# accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
# accuracy # 0.68

###########################################################################################################################
# Fitting glm to the Training set
classifier = glm(formula = outcome_3 ~ .,
                 family = binomial,
                 data = train_set)

summary(classifier) #aic 3320

###########################odds ratio######################################
ss4_or<-data.frame(round(exp(coef(classifier)),2),round(exp(confint(classifier)),2),round(summary(classifier)$coefficient[,4],4))

colnames(ss4_or)<-c("OR","2.5%","97.5%","p")

ss4_or

summary(classifier)$coefficient
confint(classifier)
######################################################################################################################################
# stepAIC
stepAIC(classifier, direction = "both")

# 최종 모델 
model_fin <-  glm(formula = outcome_3 ~ age + twin + bmi + 전자간증 + 수축억제제 + 
                    태아성장지연 + 태반조기박리 + 산과력_출산력A + 
                    GA_week, family = binomial, data = train_set)

summary(model_fin) # 3306

##################odd ratio##############################################################
model_fin_or<-data.frame(round(exp(coef(model_fin)),2),round(exp(confint(model_fin)),2),round(summary(model_fin)$coefficient[,4],4))

colnames(model_fin_or)<-c("OR","2.5%","97.5%","p")

model_fin_or

summary(model_fin)$coefficient
confint(model_fin)

###############################################################################################################
# Predicting the Test set results
prob_pred = predict(model_fin, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set$outcome_3, y_pred)
cm
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy # 0.68
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
