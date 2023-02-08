library(knitr)
library(kableExtra)

# multinomial logistic regression (ordinar)

# Importing the dataset
dataset = read.csv("project1_model_dataset.csv", fileEncoding = "euc-kr")

dataset <- dataset %>% 
  rename("age" = "age_2",
         "bmi" = "bmi_2",
         "입원총기간" = "입원총기간_2")


dataset <- subset(dataset, select = -저체중아) #모두 NO 

var <-  c('twin','전자간증','PIH임신중고혈압','고혈압','산과력_출산력P', '산과력_출산력A','수축억제제','태아성장지연',
              '태반조기박리','부인과수술력','자궁봉축술','입원횟수','첫투약시기','age','bmi','입원총기간')

dataset[,var]<- lapply(dataset[,var], factor)
dataset$outcome <- as.factor(dataset$outcome)

str(dataset)
colSums(is.na(dataset))


# # one- hot-encoder 
# df_encode <- dummy_cols(dataset, select_columns = c('twin','전자간증','PIH임신중고혈압','고혈압','산과력_출산력P', '산과력_출산력A','수축억제제','태아성장지연',
#                         '태반조기박리','부인과수술력','자궁봉축술','입원횟수','첫투약시기','age_2','bmi_2','입원총기간_2'), remove_selected_columns = TRUE)
# head(df_encode)

# Splitting the dataset into the train set and Test set
set.seed(123)
split = sample.split(dataset$outcome, SplitRatio = 0.8)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

dim(train_set) #12082
dim(test_set) #4027


# modeling 
train_set$outcome <- relevel(train_set$outcome, ref = "3")  #37주 이상 
mlr_1 <- multinom(outcome~., data = train_set)
summary(mlr_1)

result <- tidy(mlr_1, conf.int = TRUE) %>% 
  kable() %>% 
  kable_styling("basic", full_width = FALSE)

print(result)






