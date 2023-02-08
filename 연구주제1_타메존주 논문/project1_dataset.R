# Project 1 - 덱사, 타메존주 복용한 산모 vs 대조군 비교 
# cohort : b_prime, c_prime -> 실험군

# 필요한 데이터셋 
# 1. 산모 데이터 - pr1_df_mom 
# 1-1. 기본 정보 (출산데이터, bmi) pr1_df_mom
# 1-2. 동반질환 p_diag_pr1
# 1-3. 수술 데이터
# 1-4. 약제 데이터
# 1-5. 입원 횟수(28주 이내) 및 총 입원기간
# 1-6. 스테로이드 첫 투약시기

# 2. 아기 데이터 n_baby_pr1
# 2-1. outcome - n_baby_pr1
# 2-2. 기형아 진단 - n_diag_pr1 
# 2-3. 처방정보 - n_prcp_pr1 

######################################################################################################################################
# 1. project1 기본 데이터 -  pr1_df 
total_df_v4 <- total_df_v4 %>% 
  mutate(project1_group = case_when(group_s %in% c("b_prime","c_prime") ~ "test_group",
                                    group_s == "Group_Control" ~ "control_group"))

exp_steroid <- exp_steroid %>% 
  mutate(project1_group = case_when(group_s %in% c("b_prime","c_prime") ~ "test_group",
                                    group_s == "Group_Control" ~ "control_group"))

######################################################################################################################################
pr1_df <- total_df_v4 %>% 
  filter(!is.na(project1_group))

dim(pr1_df) #19673

table(pr1_df$project1_group) #control : 16509, test 3164 
n_distinct(pr1_df$연구번호) #16985

pr1_steroid <- exp_steroid %>% 
  filter(!is.na(project1_group))

dim(pr1_steroid) #3756 
######################################################################################################################################
# age
p_coht = read.csv("C:/Users/Owner/Desktop/obgy/data/P_COHT.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
pr1_df = left_join(pr1_df[c('연구번호','임신추정일','임신종결일','project1_group', 'type')],p_coht[c(1,2)], key="연구번호")
dim(pr1_df) #19673

pr1_df$age <-year(pr1_df$임신추정일) - year(ymd(pr1_df$생년월))


# GA기준으로 4그룹으로 분리 
# ~28주(<196) / 28~34주<196<= <=238) / 34주~ 37주(238< ~ < 259) / 259<= 

pr1_df$GA <- difftime(pr1_df$임신종결일, pr1_df$임신추정일, units = "days")
pr1_df$GA <- as.numeric(pr1_df$GA)

pr1_df <- pr1_df %>% 
  mutate(project1_sub_group = case_when (GA < 196 ~ "1st_group",
                                         196 <= GA & GA < 238 ~ "2nd_group",
                                         238 <= GA & GA < 259 ~ "3rd_group",
                                         259 <= GA ~ "4th_group"))

table(pr1_df$project1_group,pr1_df$project1_sub_group)

summary(pr1_df$GA)

pr1_df <- pr1_df %>% 
  filter(GA < 600) #GA 이상한 4건 삭제 

dim(pr1_df) #19669

# # 마지막 투여일 - 임신종료일 기간 계산: 1~7일만 추출 
# 실험군 1/3 가량이 실시일자 = 임신종결일-> 즉, 주사 맞자마자 같은 날 출산함 

#####################################################################################################################################
pr1_steroid <- pr1_steroid %>%
  group_by(연구번호,임신추정일,임신종결일) %>%
  mutate(last_drug = max(end_drug))

pr1_steroid <- pr1_steroid %>%
  mutate(GA_last_drug = difftime(임신종결일, last_drug,units = "days"))

pr1_steroid$GA_last_drug <- as.numeric(pr1_steroid$GA_last_drug)

pr1_steroid <- pr1_steroid[!duplicated(pr1_steroid[,c("연구번호","임신추정일","임신종결일")]),]

test <- pr1_steroid[c(1,2,3,12,14,15,23,24)]

pr1_df <- left_join(pr1_df, pr1_steroid[,c("연구번호","임신추정일","임신종결일", "GA_last_drug")], by = c("연구번호","임신추정일","임신종결일"))
dim(pr1_df)  #19669

table(pr1_df$GA_last_drug)

pr1_df <- pr1_df%>%
  filter(GA_last_drug >=0 & GA_last_drug <8 | is.na(GA_last_drug))

dim(pr1_df) #17237

table(pr1_df$project1_group) #control : 16505, test : 732 
table(pr1_df$project1_group,pr1_df$project1_sub_group) # 1383, 513, 3206, 11403  # 169, 450, 107, 6

######################################################################################################################################

# 2. 산모 데이터
# 2.1. 산모 출산 데이터 
pr1_df_baby <- left_join(pr1_df, n_mom[c(1,2,3,5,6,7,8,9,10,14)], by=c("연구번호" = "참조한.코호트.연구번호", "임신종결일" = "분만일자"))
pr1_df_baby <- pr1_df_baby %>% 
  rename( "baby_num" = "연구번호.y")
dim(pr1_df_baby) #쌍둥이 모두 붙음 , 18,020건

table(pr1_df_baby$project1_group) #17173, 847
table(pr1_df_baby$project1_group, pr1_df_baby$project1_sub_group) #1412,619,3630,11512 / 200, 522, 119,6

# baby_num 다 있는 산모 데이터 중 유산 제외 
pr1_df_baby_D <- pr1_df_baby %>% filter(type == "D")
table(pr1_df_baby_D$project1_group) # 15900, 842
table(pr1_df_baby_D$project1_group, pr1_df_baby_D$project1_sub_group) # 143,617,3630, 11510 / 195, 522, 119,6

#쌍둥이 지우기 - 엄마 출산 데이터만 남기 
pr1_df_mom <- pr1_df_baby[!duplicated(pr1_df_baby[,c("연구번호","임신추정일","임신종결일")]),]
dim(pr1_df_mom) #17237

# 변수 정리
pr1_df_mom$산과력_출산력A[pr1_df_mom$산과력_출산력A == "00"] <- 0
pr1_df_mom$산과력_출산력A[pr1_df_mom$산과력_출산력A == "01"] <- 1
pr1_df_mom$산과력_출산력A[pr1_df_mom$산과력_출산력A == "1s"] <- 1
pr1_df_mom$산과력_출산력A[pr1_df_mom$산과력_출산력A %in% c("2s","2 ")] <- 2
pr1_df_mom$산과력_출산력A[pr1_df_mom$산과력_출산력A == "3s"] <- 3

pr1_df_mom$산과력_출산력SA[pr1_df_mom$산과력_출산력SA == "00"] <- 0
pr1_df_mom$산과력_출산력SA[pr1_df_mom$산과력_출산력SA == "1s"] <- 1

pr1_df_mom$산과력_출산력AA[pr1_df_mom$산과력_출산력AA %in% c("00"," 0")] <- 0
pr1_df_mom$산과력_출산력AA[pr1_df_mom$산과력_출산력AA %in% c("1s","1S","01")] <- 1
pr1_df_mom$산과력_출산력AA[pr1_df_mom$산과력_출산력AA == "2s"] <- 2
pr1_df_mom$산과력_출산력AA[pr1_df_mom$산과력_출산력AA == "3s"] <- 3

pr1_df_mom$산과력_출산력L[pr1_df_mom$산과력_출산력L %in% c("00", " 0")] <- 0 
pr1_df_mom$산과력_출산력L[pr1_df_mom$산과력_출산력L %in% c("1s"," 1")] <- 1  

# numeric으로 변환 
pr1_df_mom[,c("산과력_출산력T","산과력_출산력P","산과력_출산력A","산과력_출산력SA","산과력_출산력AA","산과력_출산력L")]<-
  lapply(pr1_df_mom[,c("산과력_출산력T","산과력_출산력P","산과력_출산력A","산과력_출산력SA","산과력_출산력AA","산과력_출산력L")], as.numeric)

# 3회 이상은 3으로 표현 
pr1_df_mom$산과력_출산력T_1 <-ifelse(pr1_df_mom$산과력_출산력T >=3, 3, pr1_df_mom$산과력_출산력T)
pr1_df_mom$산과력_출산력P_1 <-ifelse(pr1_df_mom$산과력_출산력P >=3, 3, pr1_df_mom$산과력_출산력P)
pr1_df_mom$산과력_출산력A_1 <-ifelse(pr1_df_mom$산과력_출산력A >=3, 3, pr1_df_mom$산과력_출산력A)
pr1_df_mom$산과력_출산력SA_1 <-ifelse(pr1_df_mom$산과력_출산력SA >=3, 3, pr1_df_mom$산과력_출산력SA)
pr1_df_mom$산과력_출산력AA_1 <-ifelse(pr1_df_mom$산과력_출산력AA >=3, 3, pr1_df_mom$산과력_출산력AA)
pr1_df_mom$산과력_출산력L_1 <-ifelse(pr1_df_mom$산과력_출산력L >=3, 3, pr1_df_mom$산과력_출산력L)

pr1_df_mom <- subset(pr1_df_mom, select = -c(산과력_출산력T,산과력_출산력P,산과력_출산력A,산과력_출산력SA,산과력_출산력AA,산과력_출산력L))

table(pr1_df_mom$산과력_출산력A_1)

#  bmi
# height : 최빈값
p_vtls_pr1 = read.csv("C:/Users/Owner/Desktop/obgy/data/P_VTLS.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")

p_vtls_pr1<-p_vtls_pr1[c(1,4,5,6)] 

p_vtls_pr1 <-p_vtls_pr1 %>% 
  filter(!is.na(키) & !is.na(몸무게)&!is.na(기록일자))

table(is.na(p_vtls_pr1$키)) 

p_vtls_pr1$몸무게 <- ifelse(p_vtls_pr1$몸무게<30 | 200<p_vtls_pr1$몸무게, NA,p_vtls_pr1$몸무게) #30, 200 사이 벗어난 값 NA처리
p_vtls_pr1$키 <- ifelse(p_vtls_pr1$키<100 ,NA, p_vtls_pr1$키) #100미만 NA처리

p_vtls_pr1$키 <- round(p_vtls_pr1$키)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

p_vtls_pr1_h <- summarise(group_by(p_vtls_pr1,연구번호),height=getmode(키))
p_vtls_pr1<-inner_join(p_vtls_pr1,p_vtls_pr1_h) 
summary(p_vtls_pr1)

p_vtls_pr1 <-full_join(p_vtls_pr1,pr1_df_mom[c(1,2,3)],key = "연구번호")
p_vtls_pr1$기록일자 <- ymd(p_vtls_pr1$기록일자)
p_vtls_pr1 <- p_vtls_pr1 %>% filter(임신추정일 <=기록일자 & 기록일자<=임신종결일)%>% arrange(연구번호,desc(기록일자)) 
p_vtls_pr1 <-p_vtls_pr1[!duplicated(p_vtls_pr1[,c("연구번호","임신추정일","임신종결일")]),]
p_vtls_pr1$bmi <- round(p_vtls_pr1$몸무게/(((p_vtls_pr1$height)/100)^2),2)
summary(p_vtls_pr1$bmi) #na - 11 

pr1_df_mom<-left_join(pr1_df_mom,p_vtls_pr1[c(1,6,7,8)],key=c("연구번호","임신추정일","임신종결일"))
dim(pr1_df_mom) #17237

tt <- pr1_df_mom %>% 
  filter(연구번호 == "R000004604")

ss <- pr1_df_mom %>%  filter(연구번호 == "R000022325")

#check
table(pr1_df$project1_group) #control : 16505, test : 732 
table(pr1_df$project1_group,pr1_df$project1_sub_group) # 1383, 513, 3206, 11403  # 169, 450, 107, 6
######################################################################################################################################
# 2.2. 산모 - 동반질환
p_diag_pr1 <- read.csv("C:/Users/Owner/Desktop/obgy/data/P_DIAG.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")

p_diag_pr1 <- p_diag_pr1[c(1,3,6,7,8,14)]
p_diag_pr1 <- p_diag_pr1 %>% filter(연구번호 %in% pr1_df_mom$연구번호) 
dim(p_diag_pr1) #66792
p_diag_pr1 <- p_diag_pr1 %>% filter(진단분류.주진단.부진단. == "M") 
dim(p_diag_pr1) #27639

# 동반질환 묶기
p_diag_pr1 <- p_diag_pr1 %>% 
  mutate(진단코드_2 = regmatches(p_diag_pr1$진단코드, gregexpr("([[:alpha:]]+[[:digit:]]+)", p_diag_pr1$진단코드)))

p_diag_pr1<- p_diag_pr1 %>% 
  mutate(진단명_2 = case_when(grepl("D473", 진단코드_2, ignore.case = T) ~ "혈소판증가증",
                           grepl("D610|D613|D619", 진단코드_2, ignore.case = T) ~ "재생불량성빈혈",               
                           grepl("D693|D694|D695|D696", 진단코드_2, ignore.case = T) ~ "혈소판감소증",
                           grepl("D690|D692", 진단코드_2, ignore.case = T) ~ "자반증",
                           grepl("E03", 진단코드_2, ignore.case = T) ~ "갑상선기능저하증",
                           grepl("E05", 진단코드_2, ignore.case = T) ~ "갑상선기능항진증",
                           grepl("E10", 진단코드_2, ignore.case = T) ~ "제1형당뇨병",
                           grepl("E11", 진단코드_2, ignore.case = T) ~ "제2형당뇨병",
                           grepl("I10|I11|I12|I13|I15|O10", 진단코드_2, ignore.case = T) ~ "고혈압",
                           grepl("I20", 진단코드_2, ignore.case = T) ~ "협심증",
                           grepl("I26", 진단코드_2, ignore.case = T) ~ "폐색전증",
                           grepl("I27", 진단코드_2, ignore.case = T) ~ "폐성고혈압",
                           grepl("I34|I35|I36|I38", 진단코드_2, ignore.case = T) ~ "심장판막질환",
                           grepl("I44|I47|I48|I49", 진단코드_2, ignore.case = T) ~ "부정맥",
                           grepl("I50", 진단코드_2, ignore.case = T) ~ "심부전",
                           grepl("L93|M32", 진단코드_2, ignore.case = T) ~ "SLE루프스",
                           grepl("M34", 진단코드_2, ignore.case = T) ~ "전신경화증",
                           grepl("M350", 진단코드_2, ignore.case = T) ~ "쇼그렌증후군",
                           grepl("M351|M358|M359", 진단코드_2, ignore.case = T) ~ "기타결합조직질환",
                           grepl("M352", 진단코드_2, ignore.case = T) ~ "베체트병",
                           grepl("D686", 진단코드_2, ignore.case = T) ~ "항인지질항체증후군",
                           grepl("O11|O14", 진단코드_2, ignore.case = T) ~ "전자간증",
                           grepl("O13|O16", 진단코드_2, ignore.case = T) ~ "PIH 임신중고혈압",
                           grepl("O15", 진단코드_2, ignore.case = T) ~ "자간증",
                           grepl("O244|O249", 진단코드_2, ignore.case = T) ~ "임신당뇨병",
                           grepl("O262", 진단코드_2, ignore.case = T) ~ "습관성유산",
                           grepl("O365", 진단코드_2, ignore.case = T) ~ "태아성장제한",
                           grepl("P95|O364", 진단코드_2, ignore.case = T) ~ "자궁내태아사망",
                           grepl("O121", 진단코드_2, ignore.case = T) ~ "임신단백뇨",
                           grepl("O120", 진단코드_2, ignore.case = T )~ "임신부종",
                           grepl("P071", 진단코드_2,ignore.case = T) ~ "저체중아",
                           grepl("O365|P05", 진단코드_2, ignore.case = T )~ "태아성장지연",
                           grepl("O459|P021", 진단코드_2, ignore.case = T )~ "태반조기박리"))
         
         

p_diag_pr1 <-full_join(p_diag_pr1, pr1_df_mom[c('연구번호','임신추정일','임신종결일','project1_group','project1_sub_group')],key="연구번호")
p_diag_pr1$진단일자 <- ymd(p_diag_pr1$진단일자)

p_diag_pr1 <- p_diag_pr1 %>% 
  filter((임신추정일-365 <=진단일자 & 진단일자 <= 임신종결일))
dim(p_diag_pr1) #13597

# 진단명 중복 제거 (같은 연구번호, 같은 진단명_2)
p_diag_pr1 <-p_diag_pr1[!duplicated(p_diag_pr1[,c("연구번호","임신추정일","임신종결일","진단명_2")]),]  
dim(p_diag_pr1) #3301

# 파생변수 생성
table(p_diag_pr1$진단명_2)

p_diag_pr1 <-p_diag_pr1 %>% 
  mutate(PIH임신중고혈압 = ifelse(진단명_2 == "PIH 임신중고혈압", 1, 0),
         SLE루프스 = ifelse(진단명_2 == "SLE루프스", 1, 0),
         갑상선기능저하증 = ifelse(진단명_2 == "갑상선기능저하증", 1, 0),
         갑상선기능항진증 = ifelse(진단명_2 == "갑상선기능항진증",1, 0),
         고혈압 = ifelse(진단명_2 == "고혈압", 1, 0),
         기타결합조직질환 = ifelse(진단명_2 == "기타결합조직질환", 1, 0),
         베체트병 = ifelse(진단명_2 == "베체트병", 1, 0),
         부정맥 = ifelse(진단명_2 == "부정맥 ", 1, 0),
         쇼그렌증후군 = ifelse(진단명_2 == "쇼그렌증후군", 1, 0),
         습관성유산 = ifelse(진단명_2 == "습관성유산", 1, 0),
         심부전 = ifelse(진단명_2 == "심부전", 1, 0),
         심장판막질환 = ifelse(진단명_2 == "심장판막질환", 1, 0),
         임신단백뇨 = ifelse(진단명_2 == "임신단백뇨", 1, 0),
         임신당뇨병 = ifelse(진단명_2 == "임신당뇨병", 1, 0),
         임신부종 = ifelse(진단명_2 == "임신부종", 1, 0),
         자간증 = ifelse(진단명_2 == "자간증", 1, 0),
         자궁내태아사망 = ifelse(진단명_2 == "자궁내태아사망", 1, 0),
         자반증 = ifelse(진단명_2 == "자반증", 1, 0),
         재생불량성빈혈 = ifelse(진단명_2 == "재생불량성빈혈", 1, 0),
         전신경화증 = ifelse(진단명_2 == "PIH 임신중고혈압", 1, 0),
         전자간증 = ifelse(진단명_2 == "전자간증", 1, 0),
         제1형당뇨병 = ifelse(진단명_2 == "제1형당뇨병", 1, 0),
         제2형당뇨병 = ifelse(진단명_2 == "제2형당뇨병", 1, 0),
         태아성장제한 = ifelse(진단명_2 == "태아성장제한", 1, 0),
         폐색전증 = ifelse(진단명_2 == "폐색전증", 1, 0),
         폐성고혈압 = ifelse(진단명_2 == "폐성고혈압", 1, 0),
         항인지질항체증후군 = ifelse(진단명_2 == "항인지질항체증후군", 1, 0),
         혈소판감소증 = ifelse(진단명_2 == "혈소판감소증", 1, 0),
         혈소판증가증 = ifelse(진단명_2 == "혈소판증가증", 1, 0),
         협심증 = ifelse(진단명_2 == "협심증", 1, 0),
         PIH임신중고혈압 = ifelse(진단명_2 == "PIH 임신중고혈압", 1, 0),
         저체중아 = ifelse(진단명_2 == "저체중아", 1, 0),
         태아성장지연 = ifelse(진단명_2 == "태아성장지연", 1, 0),
         태반조기박리 = ifelse(진단명_2 == "태반조기박리", 1, 0))

# 연구번호 합치기 
p_diag_pr1 <- p_diag_pr1 %>% 
  group_by(연구번호,임신추정일,임신종결일) %>% 
  mutate(PIH임신중고혈압 = sum(PIH임신중고혈압),
         SLE루프스 = sum(SLE루프스),
         갑상선기능저하증 = sum(갑상선기능저하증),
         갑상선기능항진증 = sum(갑상선기능항진증),
         고혈압 = sum(고혈압),
         기타결합조직질환 = sum(기타결합조직질환),
         베체트병 = sum(베체트병),
         부정맥 = sum(부정맥),
         쇼그렌증후군 = sum(쇼그렌증후군),
         습관성유산 = sum(습관성유산),
         심부전 = sum(심부전),
         심장판막질환 = sum(심장판막질환),
         임신단백뇨 = sum(임신단백뇨),
         임신당뇨병 = sum(임신당뇨병),
         임신부종 = sum(임신부종),
         자간증 = sum(자간증),
         자궁내태아사망 = sum(자궁내태아사망),
         자반증 = sum(자반증),
         재생불량성빈혈 = sum(재생불량성빈혈),
         전신경화증 = sum(전신경화증),
         전자간증 = sum(전자간증),
         제1형당뇨병 = sum(제1형당뇨병),
         제2형당뇨병 = sum(제2형당뇨병),
         태아성장제한 = sum(태아성장제한),
         폐색전증 = sum(폐색전증),
         폐성고혈압 = sum(폐성고혈압),
         항인지질항체증후군 = sum(항인지질항체증후군),
         혈소판감소증 = sum(혈소판감소증),
         혈소판증가증 = sum(혈소판증가증),
         협심증 = sum(협심증),
         저체중아 = sum(저체중아),
         태아성장지연 = sum(태아성장지연),
         태반조기박리 = sum(태반조기박리))

#중복제거
p_diag_pr1 <- p_diag_pr1[!duplicated(p_diag_pr1[,c('연구번호','임신추정일','임신종결일')]),]
dim(p_diag_pr1) #2875

# na -> 0
p_diag_pr1[13:42][is.na(p_diag_pr1[13:42])] <- 0 

# join - pr1_df_mom
pr1_df_mom <- left_join(pr1_df_mom, subset(p_diag_pr1, select = -진단명_2), by = c("연구번호","임신추정일","임신종결일","project1_group","project1_sub_group"))
dim(pr1_df_mom)  # 17237

pr1_df_mom <- subset(pr1_df_mom, select = -c(진단일자,진단코드,진단명,진단한글명,진단분류.주진단.부진단.,진단코드_2))

pr1_df_mom[c(19:51)][is.na(pr1_df_mom[c(19:51)])] <- 0 

#check
table(pr1_df$project1_group) #control : 16505, test : 732 
table(pr1_df$project1_group,pr1_df$project1_sub_group) # 1383, 513, 3206, 11403  # 169, 450, 107, 6

######################################################################################################################################
# 2-3. 수술 데이터 
# 부인과 수술력 : 임신 전에 했는지 여부 (JR4124, JR4127, JR4128, JR4129)
# 자궁봉축술 : 임신 기간 내에 했는지 여부 (JR4281, JR4282, JR4283)
p_oprs <- read.csv("C:/Users/Owner/Desktop/obgy/data/P_OPRS.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
p_oprs <- p_oprs %>% 
  filter(수술처방코드 %in% c("JR4124","JR4127","JR4128","JR4129","JR4281","JR4282","JR4283"))

dim(p_oprs) #1141

p_oprs <- left_join(p_oprs, pr1_df_mom[c("연구번호","임신추정일","임신종결일")],by = "연구번호")
p_oprs <- p_oprs %>% 
  filter(!is.na(임신추정일)) %>% 
  dplyr::select(연구번호,수술처방일자,수술처방코드,임신추정일,임신종결일)

dim(p_oprs) #815
table(p_oprs$수술처방코드)

p_oprs$수술처방일자 <- ymd(p_oprs$수술처방일자)

p_oprs <- p_oprs %>% 
  filter((수술처방코드 %in% c("JR4124","JR4127","JR4128","JR4129") & 수술처방일자 <= 임신추정일) | 
           (수술처방코드 %in% c("JR4281","JR4282","JR4283") & 수술처방일자 >=임신추정일 & 수술처방일자 <=임신종결일))

p_oprs <- p_oprs %>% 
  mutate(부인과수술력 = ifelse(수술처방코드 %in% c("JR4124","JR4127","JR4128","JR4129") & 수술처방일자 <= 임신추정일, 1, 0),
         자궁봉축술 = ifelse(수술처방코드 %in% c("JR4281","JR4282","JR4283") & 수술처방일자 >=임신추정일 & 수술처방일자 <=임신종결일, 1, 0))

p_oprs <- p_oprs[!duplicated(p_oprs[,c("연구번호","임신추정일","임신종결일")]),]  

pr1_df_mom <- left_join(pr1_df_mom, p_oprs[c(1,4,5,6,7)], by = c("연구번호","임신추정일","임신종결일"))
pr1_df_mom[c("자궁봉축술","부인과수술력")][is.na(pr1_df_mom[c("자궁봉축술","부인과수술력")])] <- 0 

table(pr1_df_mom$자궁봉축술,pr1_df_mom$project1_group)
table(pr1_df_mom$부인과수술력,pr1_df_mom$project1_group)

#check
table(pr1_df$project1_group) #control : 16505, test : 732 
table(pr1_df$project1_group,pr1_df$project1_sub_group) # 1383, 513, 3206, 11403  # 169, 450, 107, 6

######################################################################################################################################
# 2-4. 약제 데이터
# 리토드린, 아토시반, 수축억제제 처방 받았는지 여부 (임신기간 동안)
p_drug <- read.csv("C:/Users/Owner/Desktop/obgy/data/P_DRUG.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")

p_drug <- p_drug %>% 
  filter(처방코드 %in% c("DRTD50J", "DATB37.5J", "DATB6.75J","DNFPSR30", "DH-PRO100", "DVG-PG200"))
dim(p_drug) #146978

p_drug <- left_join(p_drug, pr1_df_mom[c(1,2,3)], by = "연구번호")
p_drug <- p_drug %>% 
  filter(!is.na(임신추정일))

dim(p_drug) #80288

p_drug <- p_drug[c("연구번호","처방코드","실시일자","임신추정일","임신종결일")]
p_drug$실시일자 <- ymd(p_drug$실시일자)

p_drug <- p_drug %>% 
  filter(임신추정일 <= 실시일자 & 실시일자<=임신종결일)

p_drug <- p_drug %>% 
  mutate(수축억제제 = 1)

p_drug <- p_drug[!duplicated(p_drug[,c("연구번호","임신추정일","임신종결일")]),]
dim(p_drug) #3623

pr1_df_mom <- left_join(pr1_df_mom, p_drug[c(1,4,5,6)], by = c("연구번호","임신추정일","임신종결일"))
pr1_df_mom$수축억제제[is.na(pr1_df_mom$수축억제제)] <- 0

#check
table(pr1_df$project1_group) #control : 16505, test : 732 
table(pr1_df$project1_group,pr1_df$project1_sub_group) # 1383, 513, 3206, 11403  # 169, 450, 107, 6

######################################################################################################################################
# 입원횟수 및 총 입원기간
p_acpt = read.csv("C:/Users/Owner/Desktop/obgy/data/P_ACPT.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
dim(p_acpt) #720784

p_acpt <- p_acpt[c("연구번호","입원.진료.일","퇴원일","진료구분.I.O.E.")]
p_acpt <- p_acpt %>% 
  rename ("진료구분" = "진료구분.I.O.E.", "입원일" = "입원.진료.일")

#진료구분 = 입원만 필터
table(p_acpt$진료구분)

p_acpt <- p_acpt %>% 
  filter(진료구분  == "입원")
dim(p_acpt) #38190

#연구번호, 임신추정일, 임신종료일 join
p_acpt[,2] <- as.Date(as.character(p_acpt[,2]), format ='%Y%m%d')
p_acpt[,3] <- as.Date(as.character(p_acpt[,3]), format ='%Y%m%d')

p_acpt <- left_join(p_acpt, pr1_df_mom[c("연구번호","임신추정일","임신종결일")], by = "연구번호" )
dim(p_acpt) #45197

#twin
p_acpt <-  p_acpt[!duplicated(p_acpt[,c("연구번호","입원일","퇴원일","임신추정일","임신종결일")]),]
dim(p_acpt) #44794

colSums(is.na(p_acpt))

p_acpt <- p_acpt %>% 
  filter(!is.na(임신추정일))

dim(p_acpt) #33376
n_distinct(p_acpt$연구번호) #14868

p_acpt <- p_acpt %>% 
  mutate(입원일_1 = case_when(입원일 < 임신추정일 ~ 1,
                           임신추정일 <= 입원일 & 입원일 <= 임신종결일 ~ 2,
                           임신종결일 < 입원일 ~3 ),
         퇴원일_1 = case_when(퇴원일 < 임신추정일 ~ 1,
                           임신추정일 <= 퇴원일 & 퇴원일 <= 임신종결일 ~ 2,
                           임신종결일 < 퇴원일 ~3 ))

p_acpt <- p_acpt %>% 
  filter(입원일_1 %in% c(1,2) & 퇴원일_1 %in% c(2,3)) %>% 
  arrange(연구번호,임신추정일)

dim(p_acpt) #21718

#퇴원일 임신종결일에 맞추기 
p_acpt <- p_acpt %>% 
  mutate(퇴원일_2 = case_when (퇴원일 > 임신종결일 ~ 임신종결일,
                            퇴원일 <= 임신종결일 ~ 퇴원일))

p_acpt$퇴원일 <- p_acpt$퇴원일_2

p_acpt <- p_acpt[c(1,2,3,5,6)]
head(p_acpt) #연구번호,입원일, 퇴원일, 임신추정일, 임신종결일

#전체 입원기간 산출
p_acpt$전체입원기간 <- as.numeric(difftime(p_acpt$퇴원일, p_acpt$입원일, units = "days") +1)

#28주 미만 입원 횟수 산출 
p_acpt <- p_acpt %>% 
  mutate(GA28Day = 임신추정일 + 28*7)

p_acpt <- p_acpt %>% 
  mutate(입원횟수_28주 = ifelse(퇴원일 <= GA28Day, 1, 0))

#총 입원 횟수, 입원 기간 
p_acpt <- p_acpt %>% 
  group_by(연구번호,임신추정일,임신종결일) %>% 
  mutate(입원총기간 = sum(as.numeric(전체입원기간)),
         입원횟수 = sum(as.numeric(입원횟수_28주)))

table(p_acpt$입원총기간)

# 중복 제거
p_acpt <- p_acpt[!duplicated(p_acpt[,c("연구번호","임신추정일","임신종결일")]),]
p_acpt <- p_acpt[c(1,4,5,9,10)] #연구번호, 임신추정일, 임신종결일,입원총기간, 입원횟수(28주이내)
colSums(is.na(p_acpt))

# join
pr1_df_mom <- left_join(pr1_df_mom, p_acpt, by = c("연구번호","임신추정일","임신종결일"))
pr1_df_mom <- pr1_df_mom %>% 
  arrange(연구번호,임신추정일,임신종결일)


table(pr1_df_mom$project1_group) #16505, 732
table(pr1_df_mom$project1_group, pr1_df_mom$project1_sub_group) #1383,513,3206,11403 / 169, 450, 107,6
#################################################################################################
# 스테로이드 첫 투약 시기 
# 0 : 대조군
# 1 : 28주 미만 (<196)
# 2 : 28주 ~ 33주 (196<= , <= 238)
# 3 : 34주 ~ 36주  (238 < ~ < 259)
# 4 : 37주 이상 (259<= )

pr1_steroid <- pr1_steroid %>%
  group_by(연구번호,임신추정일,임신종결일) %>%
  mutate(first_drug = min(end_drug))

pr1_steroid <- pr1_steroid %>%
  mutate(GA_first_drug = as.numeric(difftime(first_drug, 임신추정일, units = "days")))

pr1_ste_2 <- pr1_steroid[c("연구번호","임신추정일","임신종결일","GA_first_drug")]
pr1_ste_2 <- pr1_ste_2[!duplicated(pr1_ste_2[,c("연구번호","임신추정일","임신종결일")]),]

pr1_ste_2 <- pr1_ste_2 %>% 
  mutate(첫투약시기 = case_when(GA_first_drug < 196 ~ 1,
                           196 <= GA_first_drug & GA_first_drug <=238 ~ 2,
                           238 < GA_first_drug & GA_first_drug < 259 ~ 3,
                           259 <= GA_first_drug ~ 4))


# join
pr1_df_mom <- left_join(pr1_df_mom, pr1_ste_2[c(1,2,3,5)], by = c("연구번호","임신추정일","임신종결일"))
pr1_df_mom$첫투약시기[is.na(pr1_df_mom$첫투약시기)] <- 0 

colSums(is.na(pr1_df_mom))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
table(pr1_df_mom$project1_group) #16505, 732
table(pr1_df_mom$project1_group, pr1_df_mom$project1_sub_group) #1383,513,3206,11403 / 169, 450, 107,6


######################################################################################################################################
# bmi 이상한 값 삭제하기
pr1_df_mom <- pr1_df_mom %>% 
  filter(bmi >0)


colSums(is.na(pr1_df_mom))

# NA값 삭제
pr1_df_mom <- pr1_df_mom %>% 
  filter(!is.na(산과력_출산력P_1) & !is.na(산과력_출산력A_1))

dim(pr1_df_mom) # 15677
table(pr1_df_mom$project1_group) # 14972, 705
table(pr1_df_mom$project1_group, pr1_df_mom$project1_sub_group) # 199, 478, 3158, 11237/ 155,438,106,6

######################################################################################################################################
# 3. 아기 outcome
n_baby_pr1 = read.csv("C:/Users/Owner/Desktop/obgy/data/N_BABY.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")

dim(n_baby_pr1) #21489
n_distinct(n_baby_pr1$연구번호) #21069명 

sum(is.na(n_baby_pr1$출생시체중)) #221

#중복된 아기 데이터 처리 - 입원날짜가 빠른 것이 분만일과 가까움 
n_baby_pr1 <- n_baby_pr1 %>% 
  arrange(연구번호,입원날짜)

n_baby_pr1 <- n_baby_pr1[!duplicated(n_baby_pr1$연구번호),]
dim(n_baby_pr1) #21069

n_baby_pr1 <- n_baby_pr1[c('연구번호','유도분만','분만장소','성별','출생시체중','감염정보_유무','수직감염_유무',
                           'Maternal.Antenatal.History_유무','분만일자','분만형태_코드','Apgar.Score1',
                           'Apgar.Score5','PROM','태변착색','True.knot','Nuchal.cord','산소흡입','심폐소생술.유무',
                           '머리','얼굴','눈','제대','근골격계','위장관계','비뇨기계','첫배변','생식기계','신경계',
                           '병실','재원기간','치료결과','퇴원형태','태반_중량','태반_길이')]

# get 그룹명 
n_baby_pr1 <- left_join(n_baby_pr1, pr1_df_baby[c('연구번호','임신추정일','임신종결일','project1_group','project1_sub_group','baby_num',"twin")], by = c("연구번호" = "baby_num"))
n_baby_pr1 <- n_baby_pr1 %>% rename("참조한.코호트.연구번호" = "연구번호.y") #산모번호 

n_baby_pr1 <- n_baby_pr1 %>% 
  filter(!is.na(project1_group))

sum(is.na(n_baby_pr1$출생시체중)) #188

dim(n_baby_pr1) #16739
ss <- pr1_df_baby %>% group_by(연구번호,임신추정일,임신종결일) %>% filter(twin == 1) %>% mutate(n=n())

table(n_baby_pr1$project1_group) # 15898, 841
table(n_baby_pr1$project1_group, n_baby_pr1$project1_sub_group) # 143, 617, 3630, 11508  # 195, 522, 118, 6 


# 데이터 정리
n_baby_pr1$분만일자 <- as.Date(as.character(n_baby_pr1$분만일자),format = "%Y%m%d")
n_baby_pr1$출생시체중 <- ifelse(n_baby_pr1$출생시체중 >=10, n_baby_pr1$출생시체중/1000,n_baby_pr1$출생시체중)
n_baby_pr1$태반_중량 <-ifelse(n_baby_pr1$태반_중량>=1000, n_baby_pr1$태반_중량/100, n_baby_pr1$태반_중량)
n_baby_pr1$태반_길이 <- ifelse(n_baby_pr1$태반_길이 >1000, n_baby_pr1$태반_길이/10, n_baby_pr1$태반_길이)
n_baby_pr1$Apgar.Score1 <- as.numeric(n_baby_pr1$Apgar.Score1)
n_baby_pr1$Apgar.Score5 <- as.numeric(n_baby_pr1$Apgar.Score5)
n_baby_pr1$Apgar.Score1_v2 <- ifelse(n_baby_pr1$Apgar.Score1>=7, 1, 0)
n_baby_pr1$Apgar.Score5_v2 <- ifelse(n_baby_pr1$Apgar.Score5>=7, 1, 0)
n_baby_pr1$치료결과[n_baby_pr1$치료결과 == ""] <-NA

sum(is.na(n_baby_pr1$출생시체중)) #188

table(n_baby_pr1$project1_group) # 15898, 841
table(n_baby_pr1$project1_group, n_baby_pr1$project1_sub_group) # 143, 617, 3630, 11508  # 195, 522, 118, 6 

test <- n_baby_pr1 %>% 
  filter(연구번호 == "R000014819")
######################################################################################################################################
# 3.2 신생아 기형아 진단
n_diag_pr1 <- read.csv("C:/Users/Owner/Desktop/obgy/data/N_DIAG.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")

n_diag_pr1 <- left_join(n_diag_pr1,n_baby_pr1[,c("연구번호","project1_group","project1_sub_group")], key = "연구번호") #get group number
dim(n_diag_pr1) #215,024

n_diag_pr1 <- n_diag_pr1 %>% 
  filter(!is.na(project1_group))

dim(n_diag_pr1) #134,307

n_diag_pr1 <- n_diag_pr1 %>% 
  mutate(진단코드_2 = regmatches(n_diag_pr1$진단코드, gregexpr("([[:alpha:]]+[[:digit:]]+)", n_diag_pr1$진단코드)))

n_diag_pr1<- n_diag_pr1 %>% 
  mutate(진단명_2 = case_when(진단코드 %in% c("P700.000.01", "P701.000.03") ~ "저혈당",
                           grepl("A411|A415|A418|A419|P369", 진단코드_2, ignore.case = T) ~ "신생아패혈증",
                           grepl("E161|E162|P704", 진단코드_2, ignore.case = T) ~ "저혈당",
                           grepl("G80", 진단코드_2, ignore.case = T) ~ "뇌성마비",
                           grepl("H351", 진단코드_2, ignore.case = T) ~ "미숙아망막병증",
                           
                           grepl("P05", 진단코드_2, ignore.case = T) ~ "성장지연",
                           
                           grepl("P0710|P0713|P0714|P0719", 진단코드_2, ignore.case = T) ~ "저체중아<2.5kg",               
                           grepl("P080|P081", 진단코드_2, ignore.case = T) ~ "LGA",
                           grepl("P22", 진단코드_2, ignore.case = T) ~ "RDS 호흡곤란증후군",
                           grepl("P271", 진단코드_2, ignore.case = T) ~ "BPS 기관지폐형성이상",
                           
                           grepl("P579|P583|P588|P589|P590|P593|P598|P599", 진단코드_2, ignore.case = T) ~ "신생아황달",
                           grepl("P592", 진단코드_2, ignore.case = T) ~ "신생아 간염",
                           grepl("P77", 진단코드_2, ignore.case = T) ~ "NEC 괴사성장염",
                           grepl("P912", 진단코드_2, ignore.case = T) ~ "대뇌백질연화",
                           grepl("P916", 진단코드_2, ignore.case = T) ~ "HIE 전산소성 허혈성 뇌병증",
                           
                           grepl("Q100|Q103|Q105|Q112|Q120|Q13|Q14|Q15", 진단코드_2, ignore.case = T) ~ "선천성질환(안과)",
                           grepl("Q16|Q17|Q18|Q311|Q315|Q320|Q321", 진단코드_2, ignore.case = T) ~ "선천성질환(이비인후과)",
                           grepl("Q201|Q202|Q203|Q204|Q206|Q21|Q22|Q23|Q24|Q251|Q254|Q255|Q256|Q257|Q259|Q26", 진단코드_2, ignore.case = T) ~ "선천성 심장질환",
                           grepl("Q250", 진단코드_2, ignore.case = T) ~ "PDA",
                           grepl("Q270", 진단코드_2, ignore.case = T) ~ "단일제대동맥",
                           
                           grepl("Q332", 진단코드_2, ignore.case = T) ~ "Pulmonary sequestration",
                           grepl("Q336", 진단코드_2, ignore.case = T) ~ "LUNG hypoplasia",
                           grepl("Q339|Q349", 진단코드_2, ignore.case = T) ~ "폐선천기형",
                           grepl("Q341", 진단코드_2, ignore.case = T) ~ "종격동 낭종",
                           
                           grepl("Q351|Q353|Q357|Q359|Q369|Q379", 진단코드_2, ignore.case = T) ~ "구개열",
                           
                           grepl("Q39", 진단코드_2, ignore.case = T) ~ "식도폐쇄",
                           grepl("Q40", 진단코드_2, ignore.case = T) ~ "유문협착",
                           grepl("Q41", 진단코드_2, ignore.case = T) ~ "소장협착",
                           grepl("Q423", 진단코드_2, ignore.case = T) ~ "항문폐쇄",
                           grepl("Q429", 진단코드_2, ignore.case = T) ~ "대장협착",
                           grepl("Q430", 진단코드_2, ignore.case = T) ~ "메켈게실",
                           grepl("Q431", 진단코드_2, ignore.case = T) ~ "선천성거대결장",
                           grepl("Q433", 진단코드_2, ignore.case = T) ~ "malrotation",
                           grepl("Q439", 진단코드_2, ignore.case = T) ~ "항문직장기형",
                           
                           grepl("Q442", 진단코드_2, ignore.case = T )~ "담관선천폐쇄",
                           grepl("Q444", 진단코드_2, ignore.case = T) ~ "총담관낭",
                           
                           grepl("Q524", 진단코드_2, ignore.case = T) ~ "질기형",
                           grepl("Q525", 진단코드_2, ignore.case = T) ~ "fusion of labia",
                           grepl("Q531|Q539|Q552", 진단코드_2, ignore.case = T) ~ "잠복고환",
                           grepl("Q540|Q541|Q542|Q548|Q549", 진단코드_2, ignore.case = T) ~ "요도하열",
                           grepl("Q556", 진단코드_2, ignore.case = T) ~ "잠복음경",
                           grepl("Q564", 진단코드_2, ignore.case = T) ~ "모호생식기",
                           
                           grepl("Q600|Q602|Q603|Q604|Q605", 진단코드_2, ignore.case = T) ~ "신장무발생",
                           grepl("Q610|Q613|Q614|Q619", 진단코드_2, ignore.case = T) ~ "콩팥 낭종성질환",
                           grepl("Q620", 진단코드_2, ignore.case = T )~ "수신증",
                           grepl("Q621", 진단코드_2, ignore.case = T) ~ "요관폐쇄",
                           grepl("Q622", 진단코드_2, ignore.case = T) ~ "거대요관",
                           grepl("Q625", 진단코드_2, ignore.case = T) ~ "요관중복",
                           grepl("Q630", 진단코드_2, ignore.case = T) ~ "신장중복",
                           grepl("Q632", 진단코드_2, ignore.case = T) ~ "딴곳신장",
                           
                           grepl("Q652", 진단코드_2, ignore.case = T) ~ "선천성고관절탈구",
                           grepl("Q658", 진단코드_2, ignore.case = T) ~ "고관절형성장애",
                           grepl("Q668", 진단코드_2, ignore.case = T) ~ "Club foot",
                           grepl("Q665", 진단코드_2, ignore.case = T )~ "편평족",
                           grepl("Q692|Q699", 진단코드_2, ignore.case = T) ~ "다지증",
                           grepl("Q700|Q703|Q704|Q709", 진단코드_2, ignore.case = T) ~ "합지증",
                           grepl("Q713|Q718}Q723", 진단코드_2, ignore.case = T )~ "선천성결단",
                           grepl("Q676", 진단코드_2, ignore.case = T) ~ "누두흉",
                           grepl("Q680", 진단코드_2, ignore.case = T) ~ "사경",
                           grepl("Q750|Q751|Q759", 진단코드_2, ignore.case = T) ~ "두개골유합",
                           grepl("Q760", 진단코드_2, ignore.case = T) ~ "척추이분증",
                           grepl("Q764", 진단코드_2, ignore.case = T) ~ "척추기형",
                           grepl("Q774", 진단코드_2, ignore.case = T) ~ "연골무형성증",
                           
                           grepl("Q790|Q791", 진단코드_2, ignore.case = T) ~ "CDH",
                           grepl("Q792", 진단코드_2, ignore.case = T) ~ "배꼽류",
                           grepl("Q793", 진단코드_2, ignore.case = T )~ "Gastroschisis",
                           grepl("Q872", 진단코드_2, ignore.case = T )~ "VATER"))


#중복 제거
# 진단명 중복 제거 (같은 아이, 같은 진단명_2)
n_diag_pr1 <-n_diag_pr1[!duplicated(n_diag_pr1[,c("연구번호","진단명_2")]),]  
dim(n_diag_pr1) #13830

# 파생변수 생성
table(n_diag_pr1$진단명_2)

n_diag_pr1 <- n_diag_pr1 %>% 
  mutate(
    BPS기관지폐형성이상 = ifelse(진단명_2 =="BPS 기관지폐형성이상", 1, 0),
    CDH = ifelse(진단명_2 =="CDH", 1, 0),
    Club_foot = ifelse(진단명_2 =="Club foot", 1, 0),
    fusion_of_labia = ifelse(진단명_2 =="fusion of labia", 1, 0),
    Gastroschisis = ifelse(진단명_2 =="Gastroschisis", 1, 0),
    HIE_전산소성_허혈성_뇌병증 = ifelse(진단명_2 =="HIE 전산소성 허혈성 뇌병증", 1, 0),
    LGA = ifelse(진단명_2 =="LGA", 1, 0),
    LUNG_hypoplasia = ifelse(진단명_2 =="LUNG hypoplasia", 1, 0),
    malrotation  = ifelse(진단명_2 =="malrotation", 1, 0),
    NEC_괴사성장염 = ifelse(진단명_2 =="NEC 괴사성장염", 1, 0),
    PDA = ifelse(진단명_2 =="PDA", 1, 0),
    Pulmonary_sequestration = ifelse(진단명_2 ==" Pulmonary sequestration", 1, 0),
    RDS_호흡곤란증후군  = ifelse(진단명_2 =="RDS 호흡곤란증후군", 1, 0),
    VATER= ifelse(진단명_2 =="VATER", 1, 0),
    거대요관 = ifelse(진단명_2 =="거대요관", 1, 0),
    고관절형성장애 = ifelse(진단명_2 =="고관절형성장애", 1, 0),
    구개열 = ifelse(진단명_2 =="구개열", 1, 0),
    뇌성마비 = ifelse(진단명_2 =="뇌성마비", 1, 0),
    누두흉 = ifelse(진단명_2 =="누두흉", 1, 0),
    다지증 = ifelse(진단명_2 =="다지증", 1, 0),
    단일제대동맥 = ifelse(진단명_2 =="단일제대동맥", 1, 0),
    담관선천폐쇄 = ifelse(진단명_2 =="담관선천폐쇄", 1, 0),
    대뇌백질연화 = ifelse(진단명_2 =="대뇌백질연화", 1, 0),
    대장협착  = ifelse(진단명_2 =="대장협착 ", 1, 0),
    두개골유합 = ifelse(진단명_2 =="두개골유합", 1, 0),
    딴곳신장 = ifelse(진단명_2 =="딴곳신장", 1, 0),
    메켈게실  = ifelse(진단명_2 =="메켈게실", 1, 0),
    모호생식기= ifelse(진단명_2 =="모호생식기", 1, 0),
    미숙아망막병증  = ifelse(진단명_2 =="미숙아망막병증", 1, 0),
    배꼽류 = ifelse(진단명_2 =="배꼽류", 1, 0),
    사경 = ifelse(진단명_2 =="사경", 1, 0),
    선천성_심장질환= ifelse(진단명_2 =="선천성 심장질환", 1, 0),
    선천성거대결장= ifelse(진단명_2 =="선천성거대결장", 1, 0),
    선천성결단= ifelse(진단명_2 =="선천성결단", 1, 0),
    선천성고관절탈구 = ifelse(진단명_2 =="선천성고관절탈구", 1, 0),
    선천성질환_안과 = ifelse(진단명_2 =="선천성질환(안과)", 1, 0),
    선천성질환_이비인후과= ifelse(진단명_2 =="선천성질환(이비인후과)", 1, 0),
    성장지연 = ifelse(진단명_2 =="성장지연", 1, 0),
    소장협착 = ifelse(진단명_2 =="소장협착", 1, 0),
    수신증 = ifelse(진단명_2 =="수신증", 1, 0),
    식도폐쇄 = ifelse(진단명_2 =="식도폐쇄", 1, 0),
    신생아_간염= ifelse(진단명_2 =="신생아 간염", 1, 0),
    신생아패혈증 = ifelse(진단명_2 =="신생아패혈증", 1, 0),
    신생아황달 = ifelse(진단명_2 =="신생아황달", 1, 0),
    신장무발생 = ifelse(진단명_2 =="신장무발생", 1, 0),
    신장중복  = ifelse(진단명_2 =="신장중복", 1, 0),
    연골무형성증= ifelse(진단명_2 =="연골무형성증", 1, 0),
    요관중복= ifelse(진단명_2 =="요관중복", 1, 0),
    요관폐쇄 = ifelse(진단명_2 =="요관폐쇄", 1, 0),
    요도하열= ifelse(진단명_2 =="요도하열", 1, 0),
    유문협착 = ifelse(진단명_2 =="유문협착", 1, 0),
    잠복고환 = ifelse(진단명_2 =="잠복고환", 1, 0),
    잠복음경= ifelse(진단명_2 =="잠복음경", 1, 0),
    저체중아_2.5kg = ifelse(진단명_2 =="저체중아<2.5kg", 1, 0),
    저혈당 = ifelse(진단명_2 =="저혈당", 1, 0),
    종격동_낭종 = ifelse(진단명_2 =="종격동 낭종", 1, 0),
    질기형= ifelse(진단명_2 =="질기형", 1, 0),
    척추기형 = ifelse(진단명_2 =="척추기형", 1, 0),
    척추이분증 = ifelse(진단명_2 =="척추이분증", 1, 0),
    총담관낭 = ifelse(진단명_2 =="총담관낭", 1, 0),
    콩팥_낭종성질환 = ifelse(진단명_2 =="콩팥 낭종성질환", 1, 0),
    편평족 = ifelse(진단명_2 =="편평족", 1, 0),
    폐선천기형 = ifelse(진단명_2 =="폐선천기형", 1, 0),
    합지증 = ifelse(진단명_2 =="합지증", 1, 0),
    항문직장기형 = ifelse(진단명_2 =="항문직장기형", 1, 0),
    항문폐쇄 = ifelse(진단명_2 =="항문폐쇄", 1, 0))


# 연구번호 합치기
n_diag_pr1 <- n_diag_pr1 %>% 
  group_by(연구번호) %>% 
  mutate(
    BPS기관지폐형성이상 = sum(BPS기관지폐형성이상),
    CDH = sum(CDH),
    Club_foot = sum(Club_foot),
    fusion_of_labia = sum(fusion_of_labia),
    Gastroschisis = sum(Gastroschisis),
    HIE_전산소성_허혈성_뇌병증 = sum(HIE_전산소성_허혈성_뇌병증),
    LGA = sum(LGA),
    LUNG_hypoplasia = sum(LUNG_hypoplasia),
    malrotation  = sum(malrotation),
    NEC_괴사성장염 = sum(NEC_괴사성장염),
    PDA = sum(PDA),
    Pulmonary_sequestration = sum(Pulmonary_sequestration),
    RDS_호흡곤란증후군  = sum(RDS_호흡곤란증후군),
    VATER= sum(VATER),
    거대요관 = sum(거대요관),
    고관절형성장애 = sum(고관절형성장애),
    구개열 = sum(구개열),
    뇌성마비 = sum(뇌성마비),
    누두흉 = sum(누두흉),
    다지증 = sum(다지증),
    단일제대동맥 = sum(단일제대동맥),
    담관선천폐쇄 = sum(담관선천폐쇄),
    대뇌백질연화 = sum(대뇌백질연화),
    대장협착  = sum(대장협착 ),
    두개골유합 = sum(두개골유합),
    딴곳신장 = sum(딴곳신장),
    메켈게실  = sum(메켈게실),
    모호생식기= sum(모호생식기),
    미숙아망막병증  = sum(미숙아망막병증),
    배꼽류 = sum(배꼽류),
    사경 = sum(사경),
    선천성_심장질환= sum(선천성_심장질환),
    선천성거대결장= sum(선천성거대결장),
    선천성결단= sum(선천성결단),
    선천성고관절탈구 = sum(선천성고관절탈구),
    선천성질환_안과 = sum(선천성질환_안과),
    선천성질환_이비인후과= sum(선천성질환_이비인후과),
    성장지연 = sum(성장지연),
    소장협착 = sum(소장협착),
    수신증 = sum(수신증),
    식도폐쇄 = sum(식도폐쇄),
    신생아_간염= sum(신생아_간염),
    신생아패혈증 = sum(신생아패혈증),
    신생아황달 = sum(신생아황달),
    신장무발생 = sum(신장무발생),
    신장중복  = sum(신장중복),
    연골무형성증= sum(연골무형성증),
    요관중복= sum(요관중복),
    요관폐쇄 = sum(요관폐쇄),
    요도하열= sum(요도하열),
    유문협착 = sum(유문협착),
    잠복고환 = sum(잠복고환),
    잠복음경= sum(잠복음경),
    저체중아_2.5kg = sum(저체중아_2.5kg),
    저혈당 = sum(저혈당),
    종격동_낭종 = sum(종격동_낭종),
    질기형= sum(질기형),
    척추기형 = sum(척추기형),
    척추이분증 = sum(척추이분증),
    총담관낭 = sum(총담관낭),
    콩팥_낭종성질환 = sum(콩팥_낭종성질환),
    편평족 = sum(편평족),
    폐선천기형 = sum(폐선천기형),
    합지증 = sum(합지증),
    항문직장기형 = sum(항문직장기형),
    항문폐쇄 = sum(항문폐쇄))


#중복제거
n_diag_pr1 <- n_diag_pr1[!duplicated(n_diag_pr1$연구번호),]
dim(n_diag_pr1) #7601

n_diag_pr1<- n_diag_pr1[c(1,15,16,19:84)]

# join - n_baby_pr1
n_baby_pr1<- left_join(n_baby_pr1, n_diag_pr1, by = c("연구번호","project1_group","project1_sub_group"))
dim(n_baby_pr1)  # 16739

n_baby_pr1[c(43:108)][is.na(n_baby_pr1[c(43:108)])] <- 0
sum(is.na(n_baby_pr1$출생시체중)) #188

table(pr1_df_mom$project1_group) #control : 16505, test : 732 
table(pr1_df_mom$project1_group, pr1_df_mom$project1_sub_group) # 1383, 513,3206,11403 / 169, 450, 107, 6

table(n_baby_pr1$project1_group) #control :15898 , test :841 
table(n_baby_pr1$project1_group, n_baby_pr1$project1_sub_group) # 143, 617, 3630, 11508 / 195, 522, 118, 6

test <- n_baby_pr1 %>% 
  filter(연구번호 == "R000014819")
######################################################################################################################################
# 3.3 신생아 호흡 관련 처방 정보 
n_prcp_pr1 <- read.csv("C:/Users/Owner/Desktop/obgy/data/N_PRCP.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
dim(n_prcp_pr1) #10228

n_prcp_pr1<- n_prcp_pr1 %>% filter(처방코드 %in% c("DIFSF3J", "DCUSF120J"))
dim(n_prcp_pr1) #2011

n_prcp_pr1 <- n_prcp_pr1[c("연구번호","진료일자","처방일자","처방코드")] %>% 
  arrange(연구번호,진료일자,처방일자)

n_distinct(n_prcp_pr1$연구번호) #1434

# 중복제거 -> 동일한 처방코드이기에 연구번호 별 1개씩만 필요 
n_prcp_pr1 <- n_prcp_pr1[!duplicated(n_prcp_pr1[,c("연구번호")]),]
dim(n_prcp_pr1) #1434
n_distinct(n_prcp_pr1$연구번호) #1434

n_prcp_pr1 <- n_prcp_pr1 %>% 
  mutate(호흡처방여부 = 1)

# join 
n_baby_pr1 <- left_join(n_baby_pr1, n_prcp_pr1[c("연구번호","호흡처방여부")], by = c("연구번호"))
dim(n_baby_pr1)  # 16739

n_baby_pr1$호흡처방여부[is.na(n_baby_pr1$호흡처방여부)] <- 0
table(n_baby_pr1$호흡처방여부) # 0 - 15884 1-855 

test <- n_baby_pr1 %>% 
  filter(연구번호 == "R000014819")
######################################################################################################################################
# 호흡장애진단여부 (RDS_호흡곤란증후군, 호흡처방여부 : 둘 중 하나라도 1이면 1)
n_baby_pr1 <- n_baby_pr1 %>% 
  mutate(호흡장애진단여부 = ifelse(RDS_호흡곤란증후군 + 호흡처방여부 == 0, 0, 1))

# NA 처리
n_baby_pr1 <- n_baby_pr1 %>% 
  filter(!is.na(출생시체중) & !is.na(Apgar.Score1) & !is.na(Apgar.Score5))

table(n_baby_pr1$project1_group) #control :15716 , test :830
table(n_baby_pr1$project1_group,n_baby_pr1$project1_sub_group) # 143,614,3593,11366 /195, 512,117,6
######################################################################################################################################

pr1_df_mom$태반조기박리[is.na(pr1_df_mom$태반조기박리)]<- 0

test <- n_baby_pr1 %>% 
  filter(연구번호 == "R000014819")



