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

# # GA 기간 안 맞는 데이터 수정 - > 추후 데이터 따라 달라짐 (25~42주 사이)
# pr1_df_model <- pr1_df_model %>% 
#   filter(25<=GA_week & GA_week<=42)
# 
# dim(pr1_df_model) # 16,369
# table(pr1_df_model$project1_group) # 15632, 737
# table(pr1_df_model$project1_group, pr1_df_model$project1_sub_group) # 70, 612, 3592, 11358 / 101, 513, 117,6


# # 아기 체중 weight 생성
# kg_wei = read.csv("C:/Users/Owner/Desktop/obgy/data/baby_kg_1090.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
# head(kg_wei)
# 
# pr1_df_model <- left_join(pr1_df_model, kg_wei, by = c("GA_week" = "GA"))
# pr1_df_model <- pr1_df_model %>% 
#   mutate(weight = case_when (twin ==0 & 단태아_min <= 출생시체중 & 출생시체중<=단태아_max ~1,
#                              twin == 1 & 단태아_min <= 출생시체중 & 출생시체중<=단태아_max ~1))
# pr1_df_model$weight[is.na(pr1_df_model$weight)]<- 0.8
# table(pr1_df_model$weight) #0.8- 2863, 1= 13,506

# 쌍둥이의 경우 하나라도 0.8이면 0.8로 변경 
pr1_df_model <- pr1_df_model %>%
  group_by(연구번호,임신추정일, 임신종결일) %>% 
  mutate(weight2 = prod(weight))

table(pr1_df_model$weight2) #0.64 - 135 /0.8 - 2886 / 1- 13348

pr1_df_model <-pr1_df_model %>% 
  mutate(weight3 = ifelse(weight2 == 1, 1,0.8))

table(pr1_df_model$weight3) #0.8 - 3021, 1- 13348

# outcome 생성 - 0.8이면 0, 1이면 1 
# pr1_df_model <- pr1_df_model %>% 
#   mutate(outcome = ifelse(weight3 == 0.8,0,1))
# 
# table(pr1_df_model$outcome) # 0 - 3021, 1 - 13348


# outcome_pre 생성 (GA * 체중 별 weight)
pr1_df_model$outcome_pre <- (pr1_df_model$GA_week * pr1_df_model$weight3)

table(pr1_df_model$outcome_pre) # 20~ 42

test <- pr1_df_model %>% 
  filter(project1_sub_group =="3rd_group")

table(test$outcome_pre) # 27.2, 28, 28.8, 34,35,36 

table(test$weight3) #0.8 - 731, 1- 2978


# outcome - weight3 0.8 -> 0, 1->1 
pr1_df_model <- pr1_df_model %>% 
  mutate(outcome = ifelse(weight3 == 0.8,0,1))
table(pr1_df_model$outcome) # 0 - 3021, 1-13348

# outcome_pre 기준으로 3그룹으로 분리
# # GA 37주 이상 - 3
# # GA 25~36사이, outcome_pre 29~36 사이 - 2
# # GA 25~36 사이, outcome_pre 28 이하 -1
# 
# pr1_df_model <- pr1_df_model %>%
#   mutate(outcome = case_when(GA_week >=37 ~ 3,
#                             GA_week < 37 &  outcome_pre >28 & outcome_pre <= 36 ~ 2,
#                             GA_week < 37 &  outcome_pre <=28 ~ 1))
# 
# table(pr1_df_model$outcome) # 1-743, 2 - 3628, 3- 11256
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

######################################################################################
# # 스테로이드 투약 시기 제거하기 (28주 미만만 남기기)
# pr1_df_model <- pr1_df_model %>% 
#   mutate(첫투약시기 = ifelse(첫투약시기 %in% c(0,3,4), 0, 첫투약시기))
# 
# table(pr1_df_model$첫투약시기) #0 : 15716, 1: 150, 2:503
# 
# table(pr1_df_model$project1_group) # 15632,737
# table(pr1_df_model$project1_group,pr1_df_model$project1_sub_group) # 70,612,3592,11358 / 101,513,117,6

#################################################################################################
# 동일한 쌍둥이 데이터 삭제
pr1_df_model <- pr1_df_model[!duplicated(pr1_df_model[,c("연구번호","임신추정일","임신종결일")]),]
dim(pr1_df_model) # 15627

table(pr1_df_model$project1_group) # 14987, 640
table(pr1_df_model$project1_group,pr1_df_model$project1_sub_group) # 56, 506, 3175, 11250 / 83, 446,105,6
#################################################################################################
str(pr1_df_model)

# 필요없는 변수 삭제

table(pr1_df_model$outcome)# 0 - 2796, 1 - 12831
table(pr1_df_model$입원횟수) #0 ~ 6
pr1_df_model_dataset <-pr1_df_model[c("age_2","twin","bmi_2","전자간증","PIH임신중고혈압","고혈압","산과력_출산력P","산과력_출산력A","수축억제제","저체중아","태아성장지연",
                               "태반조기박리","부인과수술력","자궁봉축술","입원총기간_2","입원횟수","첫투약시기","project1_group","project1_sub_group","outcome","GA_week")]

str(pr1_df_model_dataset) 
write.csv(pr1_df_model_dataset, "project1_general_model_dataset.csv",row.names = FALSE, fileEncoding = "euc-kr")                          

