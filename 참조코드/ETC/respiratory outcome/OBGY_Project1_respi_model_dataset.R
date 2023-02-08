# dataframe 
pr1_df_baby_D <- left_join(pr1_df_baby_D[c(1,2,3,4,7,8,9,11,18)], pr1_df_mom[c("연구번호", "임신추정일", "임신종결일","bmi", "전자간증", 
                                                                                  "PIH임신중고혈압", "고혈압", "수축억제제","태아성장지연",
                                                                                  "태반조기박리","부인과수술력","자궁봉축술","입원총기간","입원횟수",
                                                                                  "첫투약시기", "산과력_출산력P_1","산과력_출산력A_1","저체중아")],
                           by = c("연구번호", "임신추정일","임신종결일")) %>% 
  rename("산과력_출산력P" = "산과력_출산력P_1",
         "산과력_출산력A" = "산과력_출산력A_1")


table(pr1_df_baby_D$project1_group) # 15900, 842
table(pr1_df_baby_D$project1_group, pr1_df_baby_D$project1_sub_group) # 143,617,3630,11510/ # 195, 522, 119, 6

dim(n_baby_pr1) # 16739
dim(pr1_df_baby_D) # 16742
###########################################################################################################################
# join
pr1_baby_model <-left_join(pr1_df_baby_D, n_baby_pr1[c("연구번호","호흡처방여부","RDS_호흡곤란증후군","임신추정일","임신종결일")], 
                           by = c("baby_num" = "연구번호",
                                  "임신추정일" = "임신추정일",
                                  "임신종결일" = "임신종결일"))
dim(pr1_baby_model) # 16742
colSums(is.na(pr1_baby_model))

# 3rd group만 추출
pr1_baby_model <- pr1_baby_model %>% 
  filter(project1_sub_group == "3rd_group")

dim(pr1_baby_model) # 3749
table(pr1_baby_model$project1_group) # 3630, 119

###########################################################################################################################
# dataset 정리 
# GA_week 변수 추가
pr1_baby_model <- pr1_baby_model %>% 
  mutate(GA_week= GA %/% 7)

summary(pr1_baby_model$GA_week)

# NA값 삭제 
pr1_baby_model<- pr1_baby_model %>% 
  filter(!is.na(호흡처방여부) & !is.na(산과력_출산력A) & !is.na(산과력_출산력P))

# dataset 정리 -> bmi -> 0 인 데이터 날리기기
pr1_baby_model <- pr1_baby_model %>% 
  filter(bmi > 0)

table(pr1_baby_model$project1_group) # 3582, 117
pr1_baby_model$태반조기박리[is.na(pr1_baby_model$태반조기박리)]<- 0

# 산과력_출산력 P 정리
pr1_baby_model <- pr1_baby_model %>% 
  mutate(산과력_출산력P = ifelse(산과력_출산력P ==0,0,1))

summary(pr1_baby_model$산과력_출산력P)
#table(pr1_baby_model$산과력_출산력P, pr1_baby_model$outcome_3)

# 입원 횟수 정리 
pr1_baby_model <- pr1_baby_model %>% 
  mutate(입원횟수 = case_when(입원횟수 == 0~0,
                               입원횟수 == 1 ~1,
                               입원횟수 >=2 ~2))
table(pr1_baby_model$입원횟수) # 0:3168, 1:418, 2: 113
###########################################################################################################################
# numeric 변수 -> 카테고리화 
# numeric - 범주화 하기 
# age
summary(pr1_baby_model$age) # 40세 이상 - 3, 35세~39세 - 2, 35세 미만 - 1
pr1_baby_model <- pr1_baby_model %>%
  mutate(age_2 = case_when (age >=40 ~3,
                            age >=35 & age <=39 ~ 2,
                            age < 35 ~ 1 ))

table(pr1_baby_model$age_2) #1 : 2216, 2 : 1265, 3: 218

#bmi
# 20 미만 : 저체중 위험 - 1
# 20~24.9 : 정상 - 2
# 25 이상  ~ 29.9: 과체중 - 3
# 30 ~ : 비만 -4

summary(pr1_baby_model$bmi)

pr1_baby_model <- pr1_baby_model %>%
  mutate(bmi_2 = case_when (bmi < 20 ~ 1,
                            20 <= bmi & bmi < 25 ~2,
                            25 <= bmi & bmi < 30 ~3,
                            30 <= bmi ~4))

table(pr1_baby_model$bmi_2) #1: 65, 2: 1197, 3:1695, 4:742


#입원 총 기간
table(pr1_baby_model$입원총기간)

# 1일 - 1
# 2일- 2
# 3일 이상~ -3
# 4일 이상 ~ 10일 이하 - 4
# 11일 이상 - 5

pr1_baby_model <- pr1_baby_model %>%
  mutate(입원총기간_2 = case_when(입원총기간 == 1 ~1,
                             1 < 입원총기간 & 입원총기간 <= 2 ~2,
                             2 < 입원총기간 & 입원총기간 <= 3 ~3,
                             4 <= 입원총기간 & 입원총기간 <=10 ~4,
                             11<= 입원총기간 ~5))

table(pr1_baby_model$입원총기간_2) # 1: 1190, 2: 1294, 3: 158, 4: 735, 5: 322

###########################################################################################################################
# 파생 변수 생성 
# 스테로이드 투약 여부 변수 파생
pr1_baby_model$스테로이드여부 <- ifelse(pr1_baby_model$project1_group == "test_group", 0,1)
table(pr1_baby_model$스테로이드여부) # 117, 3582

# outcome 만들기 (RDS_호흡곤란증후군, 호흡처방여부 : 둘 중 하나라도 1이면 1)
pr1_baby_model <- pr1_baby_model %>% 
  mutate(outcome = ifelse(RDS_호흡곤란증후군 + 호흡처방여부 == 0, 0, 1))


table(pr1_baby_model$outcome) #0: 3512, 1 : 187
table(pr1_baby_model$RDS_호흡곤란증후군) # 3532,167
table(pr1_baby_model$호흡처방여부) # 3563,136
dim(pr1_baby_model) # 3699

summary(pr1_baby_model$입원총기간) # 1~133
summary(pr1_baby_model$입원횟수) # 0 ~ 4
###########################################################################################################################
# dataset 정리 -> twin 하나로 묶기 (둘 중 하나라도 호흡곤란증후군 존재 시 1로)

pr1_baby_model <- pr1_baby_model %>% 
  group_by(연구번호,임신추정일,임신종결일) %>%
  mutate(outcome_2 = sum(outcome))

pr1_baby_model <- pr1_baby_model %>% 
  mutate(outcome_3 = ifelse(outcome_2 == 0, 0, 1))

table(pr1_baby_model$outcome_3) #0: 3494, 1 : 205
dim(pr1_baby_model) # 3699
20500/3494 #5.86%

###########################################################################################################################
# dataset 정리 -> 변수 제거
model_DF <- pr1_baby_model[c(9:24,27:31,34)]

model_DF <- subset(model_DF, select = -c(저체중아,첫투약시기,입원총기간,bmi))  
colSums(is.na(model_DF))
dim(model_DF) # 3699

model_DF <- model_DF %>% 
  rename("age" = "age_2",
         "bmi" = "bmi_2",
         "입원총기간" = "입원총기간_2")
###########################################################################################################################
# dataset 정리 -> factor 변환 

var <-  c('twin','전자간증','PIH임신중고혈압','고혈압','수축억제제','태아성장지연','태반조기박리','부인과수술력',
          '자궁봉축술','입원횟수','산과력_출산력P','산과력_출산력A','outcome_3', '스테로이드여부', "age","bmi","입원총기간")

model_DF[,var]<- lapply(model_DF[,var], factor)
dim(model_DF) # 3699
table(model_DF$outcome_3) # 3494, 205
###########################################################################################################################
# 데이터셋 확인 

#str(model_DF)
summary(model_DF)

