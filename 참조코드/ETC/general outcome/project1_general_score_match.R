# 산모 데이터 (쌍둥이 제외)
dim(pr1_df_mom_score) # 3280
table(pr1_df_mom_score$project1_group) # 3175, 105

# 신생아 데이터(쌍둥이 모두 포함)
dim(n_baby_score) # 3714
table(n_baby_score$project1_group) # 3597, 117

pr1_df_mom_score <- pr1_df_mom_score %>% 
  mutate(match_group = ifelse(project1_group == "control_group",1,0)) # 대조군 1 , 실험군 0 

# score로만 매칭시키기
mod_3rd <-matchit(match_group ~ score, data = pr1_df_mom_score, ratio = 2, caliper = .01)

dta_3rd <- match.data(mod_3rd)
dim(dta_3rd)  # 169
table(dta_3rd$match_group) # 90,79

tbl_summary(dta_3rd[c(7,8,12,14,15,19,20,24,33,35,36,40,43,50:60)] , by = match_group) %>% 
  add_p()

###################################################################################################
n_baby_score <- left_join(n_baby_score, dta_3rd[c("연구번호","임신추정일","임신종결일","match_group")], 
                          by = c("참조한.코호트.연구번호" = "연구번호",
                                 "임신추정일" = "임신추정일",
                                 "임신종결일" = "임신종결일"))
colSums(is.na(n_baby_score))

n_baby_score <- n_baby_score %>% 
  filter(!is.na(match_group))

