# score 매기기
pr1_df_model <-subset(pr1_df_model, select = -c(age,bmi,입원총기간, outcome))

pr1_df_model <- pr1_df_model %>% 
  filter(project1_sub_group == "3rd_group")

table(pr1_df_model$project1_group) #  3175, 105

pr1_df_model <- pr1_df_model %>% 
  rename("age" = "age_2",
         "bmi" = "bmi_2",
         "입원총기간" = "입원총기간_2")

pr1_df_model <- pr1_df_model %>% 
  mutate(스테로이드투여 = ifelse(project1_group == "control_group", 1, 0))

pr1_df_model <- pr1_df_model %>% 
  mutate(
         twin_s = case_when(twin == 0 ~ 0,
                            twin == 1 ~ 24),
         bmi_s = case_when(bmi == 1 ~ 0,
                           bmi == 2 ~ -1,
                           bmi == 3 ~ -4,
                           bmi == 4 ~ -8,
                           bmi == 5 ~ -13),
         전자간증_s = case_when(전자간증 == 0 ~ 0,
                                전자간증 == 1 ~ 14),
         수축억제제_s = case_when(수축억제제 == 0 ~0,
                                  수축억제제 == 1 ~-3),
         스테로이드투여_s = case_when(스테로이드투여 == 0 ~0,
                               스테로이드투여 == 1 ~-7),
        GA_s = GA_week *-9)

    
colSums(is.na(pr1_df_model))

pr1_df_model <- pr1_df_model %>% 
  mutate(score = (twin_s + bmi_s + 전자간증_s + 수축억제제_s + 스테로이드투여_s + GA_s))

pr1_df_model <- left_join(pr1_df_model, pr1_df[c(1,2,3,4,9)], by = c("연구번호","임신추정일","임신종결일","project1_group","project1_sub_group"))

table(pr1_df_model$project1_group) # 3175, 105
dim(pr1_df_model) # 3280

# histogram : control, test
#####
# pr1_df_model_1 <- pr1_df_model %>% filter(project1_sub_group =="1st_group")
# pr1_df_model_2 <-pr1_df_model %>% filter(project1_sub_group =="2nd_group")
# pr1_df_model_3 <- pr1_df_model %>% filter(project1_sub_group =="3rd_group")
# pr1_df_model_4 <-pr1_df_model %>% filter(project1_sub_group =="4th_group")
# 
# table(pr1_df_model_1$project1_group) #c : 59, t : 97
# table(pr1_df_model_2$project1_group) # c : 572, t : 499
# table(pr1_df_model_3$project1_group) # c: 3551, t : 118
# table(pr1_df_model_4$project1_group) # c: 11207, t: 6

#####
#hist(pr1_df_model$score)
#summary(pr1_df_model$score)

#####
# q1 <- ggplot(pr1_df_model,aes(x = score)) +
#   geom_density(alpha=0.6, adjust = 3) +
#   labs(tag = 'All_group')+
#   geom_vline(aes(xintercept = mean(score)),
#              color= "blue")+
#   geom_histogram(aes(y=..density..),alpha=0.5,
#                  position="identity")
# 
# 
# q2 <- ggplot(pr1_df_model,aes(x = score)) +
#   geom_density(alpha=0.6, adjust = 3) +
#   labs(tag = 'All_group')+
#   geom_vline(aes(xintercept = mean(score)),
#              color= "blue")+
#   geom_histogram(aes(y=..density..),alpha=0.5,
#                  position="identity")+
#   scale_x_continuous(limits = c(0,20), breaks = c(0,2.5,5,7.5,10,12.5,15,17.5,20))
# 
# combined_2 <- q1 + q2   & theme(legend.position = "bottom")
# combined_2 + plot_layout(guides = "collect")
# combined_2

 
# graph
#####
# q0 <-  ggplot(pr1_df_model,aes(x = score, fill = project1_group)) +
#   geom_density(alpha=0.6, adjust = 3) + 
#   labs(tag = 'All_group')+
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# 
# q1 <- ggplot(pr1_df_model_1,aes(x = score, fill = project1_group)) +
#   geom_density(alpha=0.6, adjust = 3) + 
#   labs(tag = '1st_group')+
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# 
# q2 <- ggplot(pr1_df_model_2,aes(x = score, fill = project1_group)) +
#   geom_density(alpha=0.6, adjust = 3) + 
#   labs(tag = '2nd_group')+
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# 
# q3 <- ggplot(pr1_df_model_3,aes(x = score, fill = project1_group)) +
#   geom_density(alpha=0.6, adjust = 3) + 
#   labs(tag = '3rd_group')+
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# 
# q4 <- ggplot(pr1_df_model_4,aes(x = score, fill = project1_group)) +
#   geom_density(alpha=0.6, adjust = 3) + 
#   labs(tag = '4th_group')+
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# 
# combined_2 <- q0 + q1 + q2 + q3 + q4   & theme(legend.position = "bottom") 
# combined_2 + plot_layout(guides = "collect", ncol = 2)
# combined_2

# score - median 값 기준으로 나누기 (median : -4.84)
# summary(pr1_df_model$score)
# 
# pr1_df_model <- pr1_df_model %>% 
#   mutate(group_score = ifelse(score <= -5, "high_risk", "low_risk"))
# table(pr1_df_model$group_score) #low : 8292, high : 7817
#######################################################################################################
# mom data - pr1_df_mom_score
pr1_df_mom_score <- left_join(pr1_df_mom, pr1_df_model[c("연구번호","임신추정일","임신종결일","score")], by = c("연구번호","임신추정일","임신종결일"))
colSums(is.na(pr1_df_mom_score)) 

pr1_df_mom_score <- pr1_df_mom_score %>% 
  filter(!is.na(score))

dim(pr1_df_mom_score) # 3280

table(pr1_df_mom_score$project1_group) # 3175, 105

# baby_data - n_baby_score 
n_baby_score<- left_join(n_baby_pr1, pr1_df_model[c("연구번호","임신추정일","임신종결일","score")], 
                       by = c("참조한.코호트.연구번호" = "연구번호", "임신추정일" = "임신추정일", "임신종결일" = "임신종결일"))
colSums(is.na(n_baby_score)) 

n_baby_score <- n_baby_score %>% 
  filter(!is.na(score))

dim(n_baby_score) # 3714

table(n_baby_score$project1_group) # 3597,117





