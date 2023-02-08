# score 매기기

pr1_baby_model <-subset(pr1_baby_model, select = -c(age,outcome, outcome_2))

dim(pr1_baby_model) # 3229
table(pr1_baby_model$project1_group) #3125, 104

pr1_baby_model <- pr1_baby_model %>% 
   filter(project1_sub_group == "3rd_group")

pr1_baby_model <- pr1_baby_model %>% 
  mutate(bmi_s = bmi * 2,
         전자간증_s = case_when(전자간증 ==0 ~0,
                                전자간증 ==1 ~ 6),
         산과력_출산력A_s = case_when(산과력_출산력A == 0 ~ 0,
                                      산과력_출산력A == 1 ~ 2,
                                      산과력_출산력A == 2~ 2,
                                      산과력_출산력A ==3 ~7),
         GA_s = GA_week *-7,
         age_s =case_when(age_2 == 1 ~ 0,
                          age_2 == 2~-2,
                          age_2 == 3~-1))
              

colSums(is.na(pr1_baby_model))

pr1_baby_model <- pr1_baby_model %>% 
  mutate(score = bmi_s + 전자간증_s + 산과력_출산력A_s + GA_s + age_s)

pr1_baby_model <- left_join(pr1_baby_model, pr1_df[c(1,2,3,4,9)], by = c("연구번호","임신추정일","임신종결일","project1_group","project1_sub_group"))

table(pr1_baby_model$project1_group) # 3125,104
dim(pr1_baby_model) # 3229

#######################################################################################################
# mom data - pr1_df_mom_score
pr1_df_mom_score <- left_join(pr1_df_mom, pr1_baby_model[c("연구번호","임신추정일","임신종결일","score")], by = c("연구번호","임신추정일","임신종결일"))
colSums(is.na(pr1_df_mom_score)) 

pr1_df_mom_score <- pr1_df_mom_score %>% 
  filter(!is.na(score))

dim(pr1_df_mom_score) # 3229

table(pr1_df_mom_score$project1_group) # 3125,104

# baby_data - n_baby_score 
n_baby_score<- left_join(n_baby_pr1, pr1_baby_model[c("연구번호","임신추정일","임신종결일","score")], 
                       by = c("참조한.코호트.연구번호" = "연구번호", "임신추정일" = "임신추정일", "임신종결일" = "임신종결일"))
colSums(is.na(n_baby_score)) 

n_baby_score <- n_baby_score %>% 
  filter(!is.na(score))

dim(n_baby_score) # 3658

table(n_baby_score$project1_group) # 3542,116





