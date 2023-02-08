# dataset

pr1_df_model_filter <- pr1_df_model[!duplicated(pr1_df_model[,c("연구번호","임신추정일","임신종결일")]),] 

pr1_df_mat <- left_join(pr1_df_mom[c("연구번호","임신추정일","임신종결일","project1_group","project1_sub_group")],
                        pr1_df_model_filter[c(1,2,3,5:21,23,24,25,41)], by = c("연구번호","임신추정일","임신종결일"))

#tt <- pr1_df_mat  %>%  filter(연구번호 == "R000003238")
ss <- pr1_df_mom %>%  filter(연구번호 == "R000022325")

colSums(is.na(pr1_df_mat))
pr1_df_mat <- pr1_df_mat %>% filter(!is.na(score))
dim(pr1_df_mat) #15375

pr1_df_mat <- pr1_df_mat %>% 
  filter(project1_group %in% c("test_group","control_group")) %>% 
  mutate(match_group = ifelse(project1_group == "test_group", 1, 0 )) #실험군 - 1, 대조군 - 0

table(pr1_df_mat$project1_group) #c : 14749, t : 626
table(pr1_df_mat$match_group) 

pr1_df_mat[c(4,5,8:24,27)] = lapply(pr1_df_mat[c(4,5,8:24,27)], factor)

#dataset 나누기 
pr1_mat_1st <- pr1_df_mat %>% 
  filter(project1_sub_group == "1st_group") 

pr1_mat_2nd <- pr1_df_mat %>% 
  filter(project1_sub_group == "2nd_group") 


pr1_mat_3rd <- pr1_df_mat %>% 
  filter(project1_sub_group == "3rd_group") 


table(pr1_mat_1st$match_group) # 46,80
table(pr1_mat_2nd$match_group) # 476,434
table(pr1_mat_3rd$match_group) # 3130,106


# matching
set.seed(2589)

# 1그룹 
mod_1st <-matchit(match_group ~ score , data = pr1_mat_1st, ratio = 2, caliper = .01)

dta_1st <- match.data(mod_1st)
dim(dta_1st) #16
table(dta_1st$match_group) #9 7 

#tbl_summary(dta_1st[,c(4,7,8:27)] , by = match_group) %>% 
#  add_p()

# 2그룹 
mod_2nd <-matchit(match_group ~ score + GA, data = pr1_mat_2nd, ratio = 2, caliper = .001)

dta_2nd <- match.data(mod_2nd)
dim(dta_2nd) #  168
table(dta_2nd$match_group) # 91,77

#tbl_summary(dta_2nd[,c(4,7,8:27)] , by = match_group) %>% 
#  add_p()

# 3그룹 
mod_3rd <-matchit(match_group ~ GA + score, data = pr1_mat_3rd, ratio = 2, caliper = .01)

dta_3rd <- match.data(mod_3rd)
dim(dta_3rd)  # 254
table(dta_3rd$match_group) #164,90 

#tbl_summary(dta_3rd[,c(4,7,8:27)] , by = match_group) %>% 
#  add_p()


###########################################################################################################################################################
# histogram - GA, Age
# p1 <- ggplot(pr1_mat_1st,aes(x = GA, fill = match_group)) +
#   geom_density(alpha=0.6) + 
#   labs(tag = '1st_group')+
#   scale_fill_manual(values=c("lightpink", "lightyellow")) + theme_minimal()
# 

# q1 <- ggplot(pr1_mat_1st,aes(x = age, fill = match_group)) +
#   geom_density(alpha=0.6) + 
#   labs(tag = '1st_group')+
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# q2 <- ggplot(dta_1st,aes(x = age, fill = match_group)) +
#   geom_density(alpha=0.6) +
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# q3 <-  ggplot(pr1_mat_2nd,aes(x = age, fill = match_group)) +
#   geom_density(alpha=0.6)+
#   labs(tag = '2nd_group')+
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# q4 <-  ggplot(dta_2nd,aes(x = age, fill = match_group)) +
#   geom_density(alpha=0.6) +
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# q5 <-  ggplot(pr1_mat_3rd,aes(x = age, fill = match_group)) +
#   geom_density(alpha=0.6) +
#   labs(tag = '3rd_group')+
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# q6 <-  ggplot(dta_3rd,aes(x = age, fill = match_group)) +
#   geom_density(alpha=0.6) +
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# q7 <-  ggplot(pr1_mat_4th,aes(x = age, fill = match_group))  +
#   geom_density(alpha=0.6) +
#   labs(tag = '4th_group')+
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# 
# q8 <-  ggplot(dta_4th,aes(x = age, fill = match_group)) +
#   geom_density(alpha=0.6) +
#   scale_fill_manual(values=c("lightgreen", "#56B4E9")) + theme_minimal()
# 
# #grid.arrange(q1,q2,q3,q4,q5,q6,q7,q8, ncol = 2)
# 
# combined <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 & theme(legend.position = "bottom") 
# combined + plot_layout(guides = "collect", ncol = 2)

###########################################################################################################################################################
# made outcome dataframe -> 산모 pr1_df_mom

pr1_mat_1st <- left_join(pr1_mat_1st, dta_1st[c('연구번호','임신추정일','임신종결일','subclass')], by = c('연구번호','임신추정일','임신종결일')) %>% 
  rename("match_1st" = "subclass")

pr1_mat_2nd <- left_join(pr1_mat_2nd, dta_2nd[c('연구번호','임신추정일','임신종결일','subclass')], by = c('연구번호','임신추정일','임신종결일')) %>% 
  rename("match_2nd" = "subclass")

pr1_mat_3rd <- left_join(pr1_mat_3rd, dta_3rd[c('연구번호','임신추정일','임신종결일','subclass')], by = c('연구번호','임신추정일','임신종결일')) %>% 
  rename("match_3rd" = "subclass")

pr1_mat_1st$match_1st <- ifelse(!is.na(pr1_mat_1st$match_1st), 1, 0)
pr1_mat_2nd$match_2nd<- ifelse(!is.na(pr1_mat_2nd$match_2nd), 1, 0)
pr1_mat_3rd$match_3rd <- ifelse(!is.na(pr1_mat_3rd$match_3rd), 1, 0)

table(pr1_mat_1st$match_1st) # 16
table(pr1_mat_2nd$match_2nd) # 168
table(pr1_mat_3rd$match_3rd) # 254

pr1_mat_1st[c(8:19)][is.na(pr1_mat_1st[c(8:19)])]<- 0
pr1_mat_2nd[c(8:19)][is.na(pr1_mat_2nd[c(8:19)])]<- 0
pr1_mat_3rd[c(8:19)][is.na(pr1_mat_3rd[c(8:19)])]<- 0

###########################################################################################################################################################
# outcome dataframe -> 신생아 n_baby_pr1

n_baby_mat_1st <- left_join(n_baby_pr1, dta_1st[c('연구번호','임신추정일','임신종결일','subclass')], by = c('참조한.코호트.연구번호' = '연구번호', '임신추정일' = '임신추정일', '임신종결일' = '임신종결일')) %>% 
                          rename("match_1st" = "subclass")

n_baby_mat_2nd <- left_join(n_baby_pr1, dta_2nd[c('연구번호','임신추정일','임신종결일','subclass')], by = c('참조한.코호트.연구번호' = '연구번호', '임신추정일' = '임신추정일', '임신종결일' = '임신종결일')) %>% 
  rename("match_2nd" = "subclass")

n_baby_mat_3rd <- left_join(n_baby_pr1, dta_3rd[c('연구번호','임신추정일','임신종결일','subclass')], by = c('참조한.코호트.연구번호' = '연구번호', '임신추정일' = '임신추정일', '임신종결일' = '임신종결일')) %>% 
  rename("match_3rd" = "subclass")

                    

n_baby_mat_1st$match_1st <- ifelse(!is.na(n_baby_mat_1st$match_1st), 1, 0)
n_baby_mat_2nd$match_2nd <- ifelse(!is.na(n_baby_mat_2nd$match_2nd), 1, 0)
n_baby_mat_3rd$match_3rd <- ifelse(!is.na(n_baby_mat_3rd$match_3rd), 1, 0)

                        
table(n_baby_mat_1st$match_1st) # 23
table(n_baby_mat_2nd$match_2nd) # 203
table(n_baby_mat_3rd$match_3rd) #303

n_baby_mat_1st[c(42:110)][is.na(n_baby_mat_1st[c(42:110)])] <- 0
n_baby_mat_2nd[c(42:110)][is.na(n_baby_mat_2nd[c(42:110)])] <- 0
n_baby_mat_3rd[c(42:110)][is.na(n_baby_mat_3rd[c(42:110)])] <- 0


                  
