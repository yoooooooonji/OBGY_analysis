# score 매기기

#str(pr1_baby_model) 

pr1_baby_model <-subset(pr1_baby_model, select = -c(age, bmi, 입원총기간, outcome, outcome_2, outcome_3))

pr1_baby_model <- pr1_baby_model %>% 
  filter(project1_sub_group == "3rd_group")

pr1_baby_model <- pr1_baby_model %>% 
  rename("age" = "age_2",
         "bmi" = "bmi_2",
         "입원총기간" = "입원총기간_2")

pr1_baby_model <- pr1_baby_model %>% 
  mutate(GA_s = GA_week *6)

colSums(is.na(pr1_baby_model))


pr1_baby_model <- left_join(pr1_baby_model, pr1_df[c(1,2,3,4,9)], by = c("연구번호","임신추정일","임신종결일","project1_group","project1_sub_group"))

table(pr1_baby_model$project1_group) # 3582, 117
dim(pr1_baby_model) # 3699

#######################################################################################################
# mom data - pr1_df_mom_score
pr1_df_mom_score <- left_join(pr1_df_mom, pr1_baby_model[c("연구번호","임신추정일","임신종결일","GA_s")], by = c("연구번호","임신추정일","임신종결일"))
colSums(is.na(pr1_df_mom_score)) 

pr1_df_mom_score <- pr1_df_mom_score %>% 
  filter(!is.na(GA_s))

dim(pr1_df_mom_score) # 3699

table(pr1_df_mom_score$project1_group) # 3582, 117

# baby_data - n_baby_score 
n_baby_score<- left_join(n_baby_pr1, pr1_baby_model[c("연구번호","임신추정일","임신종결일","GA_s")], 
                       by = c("참조한.코호트.연구번호" = "연구번호", "임신추정일" = "임신추정일", "임신종결일" = "임신종결일"))
colSums(is.na(n_baby_score)) 

n_baby_score <- n_baby_score %>% 
  filter(!is.na(GA_s))

dim(n_baby_score) # 4571

table(n_baby_score$project1_group) # 4430,141





