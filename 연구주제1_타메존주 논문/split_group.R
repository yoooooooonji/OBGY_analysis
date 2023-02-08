# 그룹 분리 
# 1. import data
p_drug = read.csv("C:/Users/Owner/Desktop/obgy/data/p_drug.csv",header = T, stringsAsFactors = F, fileEncoding = "euc-kr")

n_distinct(p_drug$연구번호) #7,380명
dim(p_drug) #47,595건

#(스테로이드) 약제 필터
p_drug <-p_drug %>% 
  filter(처방코드 %in% c("DH-FC","DH-HC","DH-DFZ6","DH-MPD125J","DH-MPD4","DH-MPD500J","DH-MPDS40J","DH-PD5","DH-TRI40J",
                     "DH-TRI50J","DH-BET4J","DH-DX0.5","DH-DX5J"))

n_distinct(p_drug$연구번호) #7,028명
dim(p_drug) #43,808건

# 코호트 목록에 있는 산모만
# p_drug <-p_drug %>% 
#   filter(연구번호 %in% total_df_v3$연구번호)
# 
# n_distinct(p_drug$연구번호) #5,378명
# dim(p_drug) #33,779건

p_drug$진료일자 <-as.Date(as.character(p_drug$진료일자),format='%Y%m%d')
p_drug$처방일자 <-as.Date(as.character(p_drug$처방일자),format='%Y%m%d')
p_drug$실시일자 <-as.Date(as.character(p_drug$실시일자),format='%Y%m%d')

# 2. 투여종료기간 생성 - arrange 중요! 

p_drug_v2 <- p_drug[,c(1,2,4,6,8,9,11,17,20)] %>% 
  arrange(연구번호,진료일자,처방일자,실시일자)


p_drug_v2$투여종료일자 <- p_drug$실시일자 + p_drug$투여기간

#같은 날, 같은 사람, 같은 약제, 같은 투여기간 중복 제거 
p_drug_v2 <- p_drug_v2[!duplicated(p_drug_v2[,c("연구번호","진료일자","실시일자","처방코드","투여기간","X1일기준총용량")]),]

# 3.  merge (total_df_v3, p_drug_v2)
total_df_drug <- full_join(total_df_v3[c(1,2,3,4)], p_drug_v2, key = "연구번호") %>% arrange("연구번호")
n_distinct(total_df_drug$연구번호) # 17,264명


# 4. 임신추정일 - 365 < 투여종료일자  & 임신종결일 <= 처방일자 
total_df_drug <- total_df_drug %>% 
  filter(임신추정일-365 < 투여종료일자 & 실시일자 <= 임신종결일)

n_distinct(total_df_drug$연구번호) #3859 
dim(total_df_drug) #12064


# 5. 중단 기간 : 2주 이내 합치기  
total_df_drug <- total_df_drug %>% 
  group_by(연구번호,처방코드,X1일기준총용량) %>% 
  mutate(diff = difftime(실시일자,lag(투여종료일자), units = "days")) %>% 
  ungroup() %>% 
  mutate(group=cumsum(diff >14 | is.na(diff) | (diff < 14 & 처방코드 != lag(처방코드)) | (X1일기준총용량 != lag(X1일기준총용량)))) 

total_df_drug <- total_df_drug %>% 
  group_by(group) %>% 
  mutate(start_drug = min(실시일자),
         end_drug = max(투여종료일자))

dim(total_df_drug) #12064

# 6.  같은 그룹 중복 제거
total_df_drug_v2 <-total_df_drug[!duplicated(total_df_drug[,c("group","start_drug","end_drug")]),]
dim(total_df_drug_v2) #6752


# 7. label 부착 
# 임신추정일 전 -> 1
# between 임신 추정일 and 임신종결일 사이 -> 2
# 임신 종결일 이후 - 3

total_df_drug_v2 <- total_df_drug_v2 %>% 
  mutate(label_1 = ifelse(start_drug < 임신추정일, "1",
                          ifelse(임신추정일 <= start_drug & start_drug <= 임신종결일, "2",
                                 ifelse(임신종결일 < start_drug,"3",NA))),
         label_2 = ifelse(end_drug < 임신추정일, "1",
                          ifelse(임신추정일 <= end_drug & end_drug <=임신종결일, "2",
                                 ifelse(임신종결일 < end_drug, "3",NA))))
#8. 총 투약기간 계산 
total_df_drug_v2 <- total_df_drug_v2 %>% 
  mutate(period = difftime(end_drug ,start_drug, units = "days")) %>% 
  group_by(연구번호,처방코드, label_1, label_2) %>% 
  mutate(sum = ifelse((label_1 ==1 & label_2 ==1), cumsum(as.numeric(period)),NA))

total_df_drug_v2$group_id<- total_df_drug_v2 %>% 
  group_indices(연구번호,처방코드, !is.na(sum))

# 9. label 1-1 총 기간 30일 미만 삭제 
total_df_drug_v2 <- total_df_drug_v2 %>%
  group_by(group_id) %>% 
  filter(max(sum)>=30 |is.na(sum))

# 10. 최종 그룹군으로 분리 
table  <- total_df_drug_v2[,c(1,2,3,4,6,7,8,9,10,11,12,13,16,17,18,19)] %>%
  group_by(연구번호,임신추정일,임신종결일) %>% 
  summarise(unique = paste(unique(c(label_1,label_2)), collapse = ','))

table(table$unique) 
# 1 : 28,  # 1.2.: 40 , # 1,2,3, : 96, # 1,3, : 21, # 2: 2,2194, # 2,3: 1148

table <- table %>% 
  mutate(group = ifelse(unique ==1, "Group_exp_A",
                        ifelse(unique %in% c("1,2","1,2,3","1,3"), "Group_exp_B",
                               ifelse(unique %in% c("2","2,3"),"Group_exp_C"))))
# 11. 실험군_steroid
exp_steroid <- left_join(subset(total_df_drug_v2, select = -c(diff,group,sum,group_id)),table, key =c("연구번호","임신추정일","임신종결일"))
dim(exp_steroid) #5620


# 12. final_total
total_df_v4 <- full_join(table,total_df_v3, key= c("연구번호","임신추정일","임신종결일"))
n_distinct(total_df_v4$연구번호) #17,264
total_df_v4$group[is.na(total_df_v4$group)] <-"Group_Control"

# count
table(total_df_v4$unique) # 1: 28 / 1,2 : 40 / 1,2,3: 96 / 1,3 : 21 / 2: 2194 / 2,3:1148
table(is.na(total_df_v4$unique)) #false 3,527, true 16,509

total_df_v4 %>% 
  group_by(group,type) %>% 
  summarise(산모수 = n_distinct(연구번호), 데이터건 = n())

####################################### c_prime, c_rest, b_prime, b_rest############

# 13. C_prime, 타메존주, 휴메딕스만 복용한 사람! 
c_steroid <- exp_steroid %>% 
  filter(group == "Group_exp_C")

dim(c_steroid) # 3,920
n_distinct(c_steroid$연구번호) #3,230명

c_steroid<-c_steroid %>% 
  group_by(연구번호,임신추정일,임신종결일) %>% 
  summarise(dose = paste(unique(처방한글명),collapse = ','))

table(c_steroid$dose) #타메존 : 664, 타메존&휴메딕: 53, 휴메딕스 : 2437, 휴메딕스&타메존 : 9

c_prime<-c_steroid %>% 
  filter(!grepl(pattern = "디솔린주40mg|메치론정4mg|소론도정5mg|유한덱사메타손정0.5mg|트리암시놀론주40mg|트리암시놀론주50mg|프란딘정6mg|프레디솔주사125mg|프레디솔주사500mg" , x = dose)) %>% 
  mutate(group_s = "c_prime")

dim(c_prime) #3163
n_distinct(c_prime$연구번호) #3,050명
table(c_prime$dose)

# 14. B_prime
b_steroid <- exp_steroid %>% 
  filter(group == "Group_exp_B")

dim(b_steroid) #921
n_distinct(b_steroid$연구번호) #135

# 임신 시작 후 복용약만 가져오기 
b_steroid<- b_steroid %>% 
  filter(label_1 == 2 | label_2 == 3 | label_2 == 2)

# 타메존주, 덱사만 복용한 사람 뽑아내기 
b_steroid <-b_steroid %>% 
  group_by(연구번호,임신추정일,임신종결일) %>% 
  summarise(dose = paste(unique(처방한글명),collapse = ','))

table(b_steroid$dose)

b_prime<-b_steroid %>% 
  filter(!grepl(pattern = "디솔린주40mg|메치론정4mg|소론도정5mg|유한덱사메타손정0.5mg|트리암시놀론주40mg|트리암시놀론주50mg|프란딘정6mg|프레디솔주사125mg|프레디솔주사500mg" , x = dose)) %>% 
  mutate(group_s = "b_prime")

b_c_prime <- rbind(b_prime, c_prime)

# 15. mutate : group_s 
# exp_steroid : 실험군의 스테로이드 복용 리스트
# total_df_v4 : 전체 대상자 리스트

exp_steroid <- left_join(exp_steroid,b_c_prime[c(1,2,3,5)], by = c("연구번호","임신추정일","임신종결일"))
exp_steroid$group_s[exp_steroid$group == "Group_exp_B"&is.na(exp_steroid$group_s)] <- "b_rest"
exp_steroid$group_s[exp_steroid$group == "Group_exp_C"&is.na(exp_steroid$group_s)] <- "c_rest"
table(exp_steroid$group, exp_steroid$group_s)
total_df_v4 <- left_join(total_df_v4,b_c_prime[c(1,2,3,5)], by = c("연구번호","임신추정일","임신종결일"))
total_df_v4$group_s[total_df_v4$group == "Group_exp_B"&is.na(total_df_v4$group_s)] <- "b_rest"
total_df_v4$group_s[total_df_v4$group == "Group_exp_C"&is.na(total_df_v4$group_s)] <- "c_rest"
table(total_df_v4$group, total_df_v4$group_s)


# group_s 
total_df_v4$group_s <- ifelse(is.na(total_df_v4$group_s), total_df_v4$group, total_df_v4$group_s)
exp_steroid$group_s <- ifelse(is.na(exp_steroid$group_s), exp_steroid$group, exp_steroid$group_s)

table(exp_steroid$group_s, exp_steroid$group)
table(total_df_v4$group_s, total_df_v4$group)

total_df_v4 %>% 
  group_by(group_s,type) %>% 
  summarise(산모수 = n_distinct(연구번호), 데이터건 = n())


