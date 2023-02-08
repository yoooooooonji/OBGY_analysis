# 산모 데이터 (쌍둥이 제외)
dim(pr1_df_mom_score) # 3229
table(pr1_df_mom_score$project1_group) # 3125,104

# 신생아 데이터(쌍둥이 모두 포함)
dim(n_baby_score) # 3658
table(n_baby_score$project1_group) # 3542,116

pr1_df_mom_score <- pr1_df_mom_score %>% 
  mutate(match_group = ifelse(project1_group == "control_group",0,1)) # 대조군 0 , 실험군 1 

# score로만 매칭시키기
set.seed(1234)
mod_3rd <-matchit(match_group ~ score, data = pr1_df_mom_score, ratio = 2, caliper = .01)

dta_3rd <- match.data(mod_3rd)
dim(dta_3rd)  # 285
table(dta_3rd$match_group) # 189, 96

tbl_summary(dta_3rd[c(7,8,12,14,15,19,20,24,33,35,36,40,43,50:60)] , by = match_group) %>% 
  add_p()
# score : -188
###################################################################################################
n_baby_score <- left_join(n_baby_score, dta_3rd[c("연구번호","임신추정일","임신종결일","match_group")], 
                          by = c("참조한.코호트.연구번호" = "연구번호",
                                 "임신추정일" = "임신추정일",
                                 "임신종결일" = "임신종결일"))
colSums(is.na(n_baby_score))

n_baby_score <- n_baby_score %>% 
  filter(!is.na(match_group) & !is.na(출생시체중))


# var <-  c('유도분만','분만장소','성별','불만형태_코드','PROM','태변착색','Ture.knot','Nuchal.cord','산소흡입','심폐소생술.유무',
#           '병실','재원기간','치료결과','Apgar.Score1_v2', 'Apgar.Score5_v2', 'BSP기관지폐형성이상','NEC_괴사성장염','PDA','Pulmonrary_sequestration',
#           'RDS_호흡곤란증후군', '미숙아망막병증','성장지연','신생아패혈증','신생아황달','저체중아_2.5kg','저혈당','호흡처방여부','호흡장애진단여부')
# 
# n_baby_score[,var]<- lapply(n_baby_score[,var], factor)


