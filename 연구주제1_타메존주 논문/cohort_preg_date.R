# 1. p_coht - 연구 대상자 산모 29,681

p_coht = read.csv("C:/Users/Owner/Desktop/obgy/data/P_COHT.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
p_coht = p_coht[1]

#2. 분만장부 
n_mom = read.csv("C:/Users/Owner/Desktop/obgy/data/N_MOM.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
#n_mom = read.csv("/Users/NOHYOONJI/Desktop/data/N_MOM.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
n_mom[,5] <-as.Date(as.character(n_mom[,5]),format='%Y%m%d') # 분만일자
dim(n_mom) #21072 
n_distinct(n_mom$참조한.코호트.연구번호) #16,515명 

n_mom$산과력_출산력T[n_mom$산과력_출산력T == ""] <- NA
n_mom$산과력_출산력P[n_mom$산과력_출산력P == ""] <- NA
n_mom$산과력_출산력A[n_mom$산과력_출산력A == ""] <- NA
n_mom$산과력_출산력AA[n_mom$산과력_출산력AA == ""] <- NA
n_mom$산과력_출산력SA[n_mom$산과력_출산력SA == ""] <- NA
n_mom$산과력_출산력L[n_mom$산과력_출산력L == ""] <- NA

# 2.1. 분만 장부 - 임신추정일, twin 변수 생성
n_mom <-n_mom[,c(1,2,5,6,7,8,9,10,11,12,13,14)]
n_mom <-n_mom %>% 
  group_by(참조한.코호트.연구번호,분만일자) %>% 
  mutate(nbaby=n(), twin = as.factor(ifelse(nbaby==1,0,1)), 임신추정일 = 분만일자 - 임신주수*7 + 임신주수.1)

table(n_mom$twin)  # 0:18834 / 1:2238

# 2.2. deliver diff 
n_mom <-n_mom %>%  
  group_by(참조한.코호트.연구번호) %>%
  arrange(참조한.코호트.연구번호, 분만일자) %>% 
  mutate(diff_분만일 = difftime(분만일자,lag(분만일자),units = "days"))

# R000015516 , R000002725 : deliver 1days, 8days 
n_mom$twin[n_mom$참조한.코호트.연구번호 %in% c("R000015516" , "R000002725")] <- 1
n_mom$nbaby[n_mom$참조한.코호트.연구번호 %in% c("R000015516" , "R000002725")] <- 2

table(n_mom$twin) #0 : 18830, 1:2242

cas1 <- n_mom %>% 
  filter(참조한.코호트.연구번호 %in% c("R000015516" , "R000002725")) %>% 
  filter(!is.na(diff_분만일))

dim(n_mom) #21072 
n_distinct(n_mom$참조한.코호트.연구번호) #16,515명 

#->특이한 쌍둥이 케이스의 경우 뒤의 출산 케이스만 우선 보관 

# 3. 유산 수술 데이터
p_oprs = read.csv("C:/Users/Owner/Desktop/obgy/data/P_OPRS.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr") #70711
#p_oprs = read.csv("/Users/NOHYOONJI/Desktop/data/P_OPRS.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")

p_oprs$수술처방일자 <- as.Date(as.character(p_oprs$수술처방일자), format='%Y%m%d')

p_oprs_filter <- p_oprs[,c(-6,-7)] %>% 
  filter(수술처방코드 %in% c('JR4441','JR4442','JR4452','JR4456','JR4457','JR4458','JR4459','JR4521')) %>% 
  mutate(임신추정일 = 수술처방일자-days(364))

p_oprs_filter <- p_oprs_filter %>% filter(연구번호 %in% p_func_v2$연구번호) #초음파 있는 산모만

dim(p_oprs_filter) #1,811건 
n_distinct(p_oprs_filter$연구번호) #1,634명

# 4. total_df : 임신종결일, 분만, 유산 붙이기
n_mom_v2 <-n_mom[!duplicated(n_mom[,c("참조한.코호트.연구번호","분만일자")]),] #쌍둥이 제거...
n_mom_v2 <- setdiff(n_mom_v2, cas1) #앞전의 특이한 쌍둥이 출산의 경우 앞의 출산 데이터만 남김

dim(n_mom_v2) #19950
n_distinct(n_mom_v2$참조한.코호트.연구번호) #16515

p_oprs_filter <- p_oprs_filter[!duplicated(p_oprs_filter[,c("연구번호","수술처방일자")]),]  #동일 산모, 동일 수술일자 제거

dim(p_oprs_filter) #1797
n_distinct(p_oprs_filter$연구번호) #1634

deliver_df <- full_join(p_coht, n_mom_v2[,c(2,3,15)], by= c("연구번호"="참조한.코호트.연구번호")) %>% 
  mutate(type = ifelse(is.na(분만일자), NA,"D")) %>% 
  rename("임신종결일"="분만일자") 
dim(deliver_df) #33,116
n_distinct(deliver_df$연구번호) #29,681

abortion_df <- full_join(p_coht,p_oprs_filter[,c(1,2,6)], by = "연구번호") %>% 
  mutate(type = ifelse(is.na(수술처방일자),NA,"A")) %>% 
  rename("임신종결일" = "수술처방일자") 
dim(abortion_df) #29,844
n_distinct(deliver_df$연구번호) #29,681

total_df <-rbind(deliver_df, abortion_df) %>% 
  arrange(연구번호, 임신종결일) %>% 
  na.omit("임신종결일| 임신추정일"|"type")  
dim(total_df) #62,960 -> 21,728
n_distinct(total_df$연구번호) #17,486명


# 5. 각 임신종결일 별 기간 차이 구하기
# 5.1
total_df <-total_df %>% 
  group_by(연구번호) %>% 
  mutate(diff = difftime(임신종결일,lag(임신종결일),units="days")) # 21,728
n_distinct(total_df$연구번호) #17,486명

# 5.2 각 연구번호 별 이전의 임신종결일 구하기
total_df<-total_df %>% group_by(연구번호) %>% 
  mutate(prev_임신종결일 = lag(임신종결일))

#5.3. 분만 or 유산 수술 후 90일 이후의 유산 수술은 유산이 아님. -> 삭제
total_df_remove<-total_df %>% 
  filter((diff <=90 & type=="A") & (lag(type)=="D" |lag(type)=="A")) #64건

total_df_v2 <- setdiff(total_df,total_df_remove)

dim(total_df_v2)#21,664건
n_distinct(total_df_v2$연구번호) #17,486명


# 6. 분만 / 유산 나눠놓기 
total_df_deliver_v2 <-total_df_v2 %>% 
  filter(type == "D")

dim(total_df_deliver_v2) # 19931
n_distinct(total_df_deliver_v2$연구번호) #16501명
 
total_df_abor_v2<-total_df_v2 %>% 
  filter(type == "A")

dim(total_df_abor_v2) #1733건
n_distinct(total_df_abor_v2$연구번호) #1610명


# 7. 초음파 : 유산 수술 데이터에 존재하는 연구번호만 추출
p_func_v2_oprs <-p_func_v2[,c(1,3,12)] %>% 
  filter(연구번호 %in% p_oprs_filter$연구번호)

dim(p_func_v2_oprs) #9,317건
n_distinct(p_func_v2_oprs$연구번호) # 1,634

# 8. merge (only abortion)
total_df_abor_v2 <-full_join(total_df_abor_v2, p_func_v2_oprs, by="연구번호")

n_distinct(total_df_abor_v2$연구번호) #1634명
dim(total_df_abor_v2) #10,283건

# 9. filter
# abortion -> diff <= 365, prev_임신종결일 < 실행일자 < 임신종결일
# abortion -> diff > 365, 임신추정일 < 실행일자 < 임신종결일

total_df_abor_v3 <- total_df_abor_v2 %>% 
  filter((diff <= 365 & prev_임신종결일<=실행일자 & 실행일자<=임신종결일)|((diff > 365|is.na(diff)) & 임신추정일 <=실행일자 & 실행일자<=임신종결일)) 

n_distinct(total_df_abor_v3$연구번호) #1279명
dim(total_df_abor_v3) #2316건

total_df_abor_v3 <- total_df_abor_v3 %>% 
  arrange(연구번호,임신종결일, desc(실행일자))

total_df_abor_v3 <-total_df_abor_v3[!duplicated(total_df_abor_v3[,c("연구번호","임신종결일")]),]

n_distinct(total_df_abor_v3$연구번호) #1279명
dim(total_df_abor_v3) #1335건

# 임신추정일(1년전 임의) ->임신추정일_func 
total_df_abor_v4 <- total_df_abor_v3[,c(1,2,8,4,5,6)] %>% rename("임신추정일" = "임신추정일_func")

dim(total_df_abor_v4) #1335
n_distinct(total_df_abor_v4$연구번호) #1279

# 10. merge (total_df_deliver_v2, total_df_abor_v4)
total_df_v3 <- rbind(total_df_deliver_v2,total_df_abor_v4) %>% arrange(연구번호,임신종결일)

n_distinct(total_df_v3$연구번호) #17,428
dim(total_df_v3) #21266

table(total_df_v3$type) # 유산 1335, 출산 19931

# 11. 임신기간 충족 여부 (2012.1.1~ 2021.2.28)
total_df_v3 <- total_df_v3 %>% 
  filter("2012-01-01"<=임신추정일 & 임신추정일 <="2021-02-28")

dim(total_df_v3) #20,036건
n_distinct(total_df_v3$연구번호) #17,264명
table(total_df_v3$type) # 유산 1318, 출산 18718

# 12. 임신종결기간 충족 여부 (2012.1.1.~2012.12.31.) 
total_df_v3 <- total_df_v3 %>% 
  filter("2012-01-01"<=임신종결일 & 임신종결일 <="2021-12-31") 
total_df_v3 <- total_df_v3[,c(1,3,2,4,5,6)]
  

dim(total_df_v3) #20036건
n_distinct(total_df_v3$연구번호) #17264명

table(total_df_v3$type) # D: 18,718 A: 1,318


