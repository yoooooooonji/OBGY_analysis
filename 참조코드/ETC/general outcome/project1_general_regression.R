# Polynomial Regression
library(MASS)

# Importing the dataset
dataset = read.csv("project1_general_model_dataset.csv", fileEncoding = "euc-kr")

dataset<- dataset %>%
  rename("age" = "age_2",
         "bmi" = "bmi_2",
         "입원총기간" = "입원총기간_2",
         "outcome" = "outcome_2")


# 3번째 그룹만 
dataset <- dataset %>% 
  filter(project1_sub_group == "3rd_group")

#str(dataset)

# outcome check
dataset$outcome <- as.factor(dataset$outcome)
summary(dataset$outcome) # 0 - 940, 1- 2340 (0 : 비정상 범위, 1 : 정상범위)

summary(dataset)

# 스테로이드 투여 여부 파생 변수 만들기 - 대조군1, 실험군 0 
dataset <- dataset %>% 
  mutate(스테로이드투여 = ifelse (project1_group =="control_group", 1, 0))

dataset <- subset(dataset, select = -c(저체중아, project1_group, 첫투약시기, project1_sub_group)) 
str(dataset)
var <-  c('twin','전자간증','PIH임신중고혈압','고혈압','산과력_출산력P', '산과력_출산력A','수축억제제','태아성장지연',
          '태반조기박리','부인과수술력','자궁봉축술','입원횟수','스테로이드투여','outcome', 'age','입원총기간','bmi')

dataset[,var]<- lapply(dataset[,var], factor)

dim(dataset) # 3280
table(dataset$스테로이드투여) # 0 - 3175 1 - 105 
table(dataset$outcome) # 0 - 940, 1 - 2340

summary(dataset)

# # level 주기, 뒤로 갈 수록 좋은 것
# dataset$outcome = factor(dataset$outcome, levels = c("1","2","3"), ordered = TRUE)
# colSums(is.na(dataset))

# Splitting the dataset into the Training set and Test set
set.seed(0908)

split = createDataPartition(dataset$outcome, p=0.8, list=F)
train_set = dataset[split,]
test_set = dataset[-split,]

dim(train_set) # 2624
dim(test_set) # 656

table(train_set$스테로이드투여) # 0: 85 1:2539 
table(test_set$스테로이드투여) # 0:21, 1:635 

table(train_set$outcome) #  0: 752, 1: 1872
table(test_set$outcome) # 0: 188 , 1: 468


# # Feature Scaling - numeric 변수
# train_set[,c(1,3,14,17)] = scale(train_set[,c(1,3,14,17)])
# test_set[,c(1,3,14,17)] = scale(test_set[,c(1,3,14,17)])

train_set[,17] = scale(train_set[,17])
test_set[,17] = scale(test_set[,17])

head(train_set)

# Fitting Linear Regression to the dataset
options(digits = 5)
options(scipen=1)

######################################################################################################################################
# modeling - GLM 
glm_1 = glm(formula = outcome ~ .,
            family = binomial,
            data = train_set)

summary(glm_1)

# result <- coef(summary(polr_sum))
# pval <- pnorm(abs(result[,"t value"]),lower.tail=FALSE)*2
# result <- cbind(result, "p value" = round(pval,3))
#write.csv(result, "general_outcome_result.csv", fileEncoding = "euc-kr")

# stepAIC
stepAIC(glm_1, direction = "both")

glm_aic = glm(formula = outcome ~ twin + bmi + 전자간증 + 수축억제제 + 
                태아성장지연 + 태반조기박리 + 부인과수술력 + 
                자궁봉축술 + GA_week + 스테로이드투여, family = binomial, 
              data = train_set)

summary(glm_aic)
#write.csv(result2, "general_outcome_result_AIC.csv", fileEncoding = "euc-kr")

# accuracy
# Predicting the Test set results
prob_pred = predict(glm_aic, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.5, 1,0)

# Making the Confusion Matrix
cm = table(test_set$outcome, y_pred)
cm
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy # 0.80  
######################################################################################################################################
# ROC 커브
ROC(test = y_pred, stat = test_set$outcome, plot = "ROC", AUC= T) #, main = "logisitc regression")

######################################################################################################################################
# modeling - LASSO
train_X = train_set[,-16]
train_y = train_set[,16]

test_X = test_set[,-16]
test_y = test_set[,16]

elasticnet_1 <- glmnet(train_X,train_y, alpha=0.5, family = "binomial")
plot(elasticnet_1, xvar ="lambda") # 람다에 따른 회귀계수 확인하기

# cross validation
set.seed(100)
cv.Elasticnet <- cv.glmnet(data.matrix(train_X), train_y, alpha=0.5, family = "binomial")
plot(cv.Elasticnet)
best_lambda <- cv.Elasticnet$lambda.min #오차가 제일 작은 값 (최적 람다)
best_lambda #0.014888

# best_lambda 값으로 회귀계수 확인
Elasticnet_coef <- coef(elasticnet_1, s=best_lambda)
Elasticnet_coef 





