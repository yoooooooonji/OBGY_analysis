pr1_mom_3 <- pr1_df_mom %>% 
  filter(project1_sub_group == "3rd_group" & type == "D") 

n_baby_3 <- n_baby_pr1 %>% 
  filter(project1_sub_group == "3rd_group")

dim(pr1_mom_3) #3264
table(pr1_mom_3$project1_group) # 3158, 106
table(n_baby_3$project1_group) # 3593, 117

pr1_mom_3 <- pr1_mom_3 %>% 
  mutate(match_group = ifelse(project1_group == "control_group", 0, 1))

table(pr1_mom_3$match_group) # 3158, 106

# GA로 매칭하기
set.seed(1234)
mod_3rd <-matchit(match_group ~ GA, data = pr1_mom_3, ratio = 10, caliper = .01)

dta_3rd <- match.data(mod_3rd)
dim(dta_3rd)  # 931
table(dta_3rd$match_group) # 106,825

tbl_summary(dta_3rd[c(4,7,8,12,14,15,19:21,24,33,40,43,50:58)] , by = project1_group) %>% 
  add_p()

# 아기 데이터에 붙이기 
n_baby_3 <- left_join(n_baby_3, dta_3rd[c("연구번호","임신추정일","임신종결일","match_group")], 
                      by = c("참조한.코호트.연구번호" = "연구번호",
                             "임신추정일" = "임신추정일",
                             "임신종결일" = "임신종결일"))

colSums(is.na(n_baby_3))

n_baby_match <- n_baby_3 %>% 
  filter(!is.na(match_group))
