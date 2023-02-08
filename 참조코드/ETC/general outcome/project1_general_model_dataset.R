# project1 risk score modeling 

#################################################################################################
# dataframe 
pr1_df_model <- left_join(n_baby_pr1[c("참조한.코호트.연구번호","출생시체중","연구번호")], 
                          pr1_df_baby_D[c("임신추정일", "임신종결일","GA", "age", "twin","baby_num")], 
                          by = c("연구번호" = "baby_num")) %>% 
  rename("baby_num" = "연구번호",
         "연구번호" = "참조한.코호트.연구번호")


colSums(is.na(pr1_df_model))
dim(pr1_df_model) # 16739

# left_join
pr1_df_model <- left_join(pr1_df_model,
                          pr1_df_mom[c("연구번호","임신추정일","임신종결일","project1_group","project1_sub_group",
                                       "산과력_출산력P_1","산과력_출산력A_1","bmi","PIH임신중고혈압","고혈압",
                                       "전자간증","저체중아","태아성장지연","태반조기박리","부인과수술력","자궁봉축술",
                                       "수축억제제","입원총기간","입원횟수","첫투약시기")], 
                          by = c("연구번호" = "연구번호",
                                 "임신추정일" = "임신추정일",
                                 "임신종결일" = "임신종결일")) %>% 
  rename("산과력_출산력P" = "산과력_출산력P_1",
         "산과력_출산력A" = "산과력_출산력A_1")


table(pr1_df_model$project1_group) #15898, 841
table(pr1_df_model$project1_group, pr1_df_model$project1_sub_group) # 143,617, 3630, 11508 / 841, 522, 118, 6

pr1_df_model <- pr1_df_model %>% 
  filter(!is.na(출생시체중) & !is.na(bmi) & !is.na(산과력_출산력P) & !is.na(산과력_출산력A))
dim(pr1_df_model) # 16,536

table(pr1_df_model$project1_group) # 15706, 830
table(pr1_df_model$project1_group, pr1_df_model$project1_sub_group) # 143,612,3592,11359 / 194, 513,117,6
#####################################################################################################################
# outcome 
# GA_week 생성
pr1_df_model <- pr1_df_model %>% 
  mutate(GA_week = GA %/% 7)

summary(pr1_df_model$GA_week)
table(pr1_df_model$GA_week)

# outcome 
pr1_df_model <- pr1_df_model %>% 
  mutate(outcome = ifelse(출생시체중>2.5 & 출생시체중 < 4.0, 1, 0))

tt <- pr1_df_model[c("출생시체중","outcome")]
table(pr1_df_model$outcome) # 0-3059, 1- 13477(정상그룹)
############################################################################
# 쌍둥이의 경우 하나라도 0이면 0으로 계산
pr1_df_model <- pr1_df_model %>% 
  group_by(연구번호,임신추정일,임신종결일) %>%
  mutate(outcome_2 = prod(outcome))

table(pr1_df_model$outcome_2) #0 - 3264, 1 -13272
dim(pr1_df_model) # 16536

#################################################################################################
# numeric - 범주화 하기 
# age
summary(pr1_df_model$age) # 40세 이상 - 3, 35세~39세 - 2, 35세 미만 - 1
pr1_df_model <- pr1_df_model %>%
  mutate(age_2 = case_when (age >=40 ~3,
                            age >=35 & age <=39 ~ 2,
                            age < 35 ~ 1 ))

table(pr1_df_model$age_2) #1 : 10325, 2 : 5087, 3: 957

#bmi
# 20 미만 : 저체중 위험 - 1
# 20~24.9 : 정상 - 2
# 25 이상  ~ 29.9: 과체중 - 3
# 30 ~ 39.9 : 비만 -4
# 40 : 초고도 비만 -5

summary(pr1_df_model$bmi)

pr1_df_model <- pr1_df_model %>%
  mutate(bmi_2 = case_when (bmi < 20 ~ 1,
                            20 <= bmi & bmi < 25 ~2,
                            25 <= bmi & bmi < 30 ~3,
                            30 <= bmi & bmi < 40 ~4,
                            40 <= bmi ~5))

table(pr1_df_model$bmi_2) #1: 528, 2: 5780, 3:7519, 4:2405, 5:137


#입원 총 기간
table(pr1_df_model$입원총기간)

# 1일 이하 - 1
# 2일 이하 - 2
# 3일 이하 -3
# 4일 이상 ~ 10일 이하 - 4
# 11일 이상 - 5

pr1_df_model <- pr1_df_model %>%
  mutate(입원총기간_2 = case_when(입원총기간 == 1 ~1,
                             1 < 입원총기간 & 입원총기간 <= 2 ~2,
                             2 < 입원총기간 & 입원총기간 <= 3 ~3,
                             4 <= 입원총기간 & 입원총기간 <=10 ~4,
                             11<= 입원총기간 ~5))

table(pr1_df_model$입원총기간_2) # 1: 5967, 2: 5754, 3: 779, 4: 2769, 5:1100

###########################################################################################################
# 동일한 쌍둥이 데이터 삭제
pr1_df_model <- pr1_df_model[!duplicated(pr1_df_model[,c("연구번호","임신추정일","임신종결일")]),]
dim(pr1_df_model) # 15766

table(pr1_df_model$project1_group) # 15046, 720
table(pr1_df_model$project1_group,pr1_df_model$project1_sub_group) # 114, 506, 3175, 11251 / 163, 446, 105, 6
###############################################################################################################
# 필요없는 변수 삭제
table(pr1_df_model$outcome_2)# 0 - 2677, 1 - 13089
table(pr1_df_model$입원횟수) #0 ~ 6
pr1_df_model_dataset <-pr1_df_model[c("age_2","twin","bmi_2","전자간증","PIH임신중고혈압","고혈압","산과력_출산력P","산과력_출산력A","수축억제제","저체중아","태아성장지연",
                                      "태반조기박리","부인과수술력","자궁봉축술","입원총기간_2","입원횟수","첫투약시기","project1_group","project1_sub_group","outcome_2","GA_week")]

#str(pr1_df_model_dataset) 
write.csv(pr1_df_model_dataset, "project1_general_model_dataset.csv",row.names = FALSE, fileEncoding = "euc-kr") 








