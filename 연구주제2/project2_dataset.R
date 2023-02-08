# Project 2 - 누적 스테로이드 투여량 vs 대조군 
# cohort : b_prime, exp_A, b_rest, c_rest -> 실험군


# 필요한 데이터셋 
# 1. 산모 데이터 - pr2_df_mom 
# 1-1. 기본 정보 (출산데이터, bmi) pr2_df_mom
# 1-2. 동반질환 p_diag_pr2

# 2. 아기 데이터 n_baby_pr2
# 2-1. outcome - n_baby_pr2
# 2-2. 기형아 진단 - n_diag_pr2 

# 1. project2 기본 데이터 -  pr2_df 
total_df_v4 <- total_df_v4 %>% 
  mutate(project2_group = case_when(group_s %in% c("b_prime","Group_exp_A") ~ "test_group_before",
                                    group_s %in% c("c_rest","b_rest") ~ "test_group_after",
                                    group_s == "Group_Control" ~ "control_group"))

exp_steroid <- exp_steroid %>% 
  mutate(project2_group = case_when(group_s %in% c("b_prime","Group_exp_A") ~ "test_group_before",
                                    group_s %in% c("c_rest","b_rest") ~ "test_group_after",
                                    group_s == "Group_Control" ~ "control_group"))

pr2_df <- total_df_v4 %>% 
  filter(!is.na(project2_group))

dim(pr2_df) #16,878

pr2_steroid <- exp_steroid %>% 
  filter(!is.na(project2_group))

dim(pr2_steroid) #1895

# age
p_coht = read.csv("data/P_COHT.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")

pr2_df = left_join(pr2_df[c('연구번호','임신추정일','임신종결일','project2_group', 'type')],p_coht[c(1,2)], key="연구번호")
dim(pr2_df) #16,878

pr2_df$age <-year(pr2_df$임신추정일) - year(ymd(pr2_df$생년월))
pr2_df$GA <- difftime(pr2_df$임신종결일, pr2_df$임신추정일, units = "days")

# 2. 산모 데이터
# 2.1. 산모 출산 데이터 
pr2_df_baby <- left_join(pr2_df,n_mom[c(1,2,3,5,6,7,8,9,10,14)], by=c("연구번호" = "참조한.코호트.연구번호", "임신종결일"="분만일자"))
pr2_df_baby <- pr2_df_baby %>% 
  rename( "baby_num" = "연구번호.y")
dim(pr2_df_baby) #쌍둥이 모두 붙음 , 17558건 

#쌍둥이 지우기 - 엄마 출산 데이터만 남기 
pr2_df_mom <- pr2_df_baby[!duplicated(pr2_df_baby[,c("연구번호","임신추정일","임신종결일")]),]
dim(pr2_df_mom) #16878

# 변수 정리
pr2_df_mom$산과력_출산력A[pr2_df_mom$산과력_출산력A == "00"] <- 0
pr2_df_mom$산과력_출산력A[pr2_df_mom$산과력_출산력A == "01"] <- 1
pr2_df_mom$산과력_출산력A[pr2_df_mom$산과력_출산력A == "1s"] <- 1
pr2_df_mom$산과력_출산력A[pr2_df_mom$산과력_출산력A %in% c("2s","2 ")] <- 2
pr2_df_mom$산과력_출산력A[pr2_df_mom$산과력_출산력A == "3s"] <- 3

pr2_df_mom$산과력_출산력SA[pr2_df_mom$산과력_출산력SA == "00"] <- 0
pr2_df_mom$산과력_출산력SA[pr2_df_mom$산과력_출산력SA == "1s"] <- 1

pr2_df_mom$산과력_출산력AA[pr2_df_mom$산과력_출산력AA %in% c("00"," 0")] <- 0
pr2_df_mom$산과력_출산력AA[pr2_df_mom$산과력_출산력AA %in% c("1s","1S","01")] <- 1
pr2_df_mom$산과력_출산력AA[pr2_df_mom$산과력_출산력AA == "2s"] <- 2
pr2_df_mom$산과력_출산력AA[pr2_df_mom$산과력_출산력AA == "3s"] <- 3

pr2_df_mom$산과력_출산력L[pr2_df_mom$산과력_출산력L %in% c("00", " 0")] <- 0 
pr2_df_mom$산과력_출산력L[pr2_df_mom$산과력_출산력L %in% c("1s"," 1")] <- 1  

pr2_df_mom$산과력_출산력T_1 <-ifelse(pr2_df_mom$산과력_출산력T >=2, "3",pr2_df_mom$산과력_출산력T)
pr2_df_mom$산과력_출산력P_1 <-ifelse(pr2_df_mom$산과력_출산력P >=2, "3",pr2_df_mom$산과력_출산력P)
pr2_df_mom$산과력_출산력A_1 <-ifelse(pr2_df_mom$산과력_출산력A >=2, "3",pr2_df_mom$산과력_출산력A)
pr2_df_mom$산과력_출산력SA_1 <-ifelse(pr2_df_mom$산과력_출산력SA >=2, "3",pr2_df_mom$산과력_출산력SA)
pr2_df_mom$산과력_출산력AA_1 <-ifelse(pr2_df_mom$산과력_출산력AA >=2, "3",pr2_df_mom$산과력_출산력AA)
pr2_df_mom$산과력_출산력L_1 <-ifelse(pr2_df_mom$산과력_출산력L >=2, "3",pr2_df_mom$산과력_출산력L)

pr2_df_mom <- subset(pr2_df_mom, select = -c(산과력_출산력T,산과력_출산력P,산과력_출산력A,산과력_출산력SA,산과력_출산력AA,산과력_출산력L))

#  bmi
# height : 최빈값
p_vtls_pr2 = read.csv("data/P_VTLS.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
p_vtls_pr2<-p_vtls_pr2[c(1,4,5,6)] 

p_vtls_pr2 <-p_vtls_pr2 %>% 
  filter(!is.na(키) & !is.na(몸무게)&!is.na(기록일자))

table(is.na(p_vtls_pr2$키)) 

p_vtls_pr2$몸무게 <- ifelse(p_vtls_pr2$몸무게<30 | 200<p_vtls_pr2$몸무게, NA,p_vtls_pr2$몸무게) #30, 200 사이 벗어난 값 NA처리
p_vtls_pr2$키 <- ifelse(p_vtls_pr2$키<100 ,NA, p_vtls_pr2$키) #100미만 NA처리

p_vtls_pr2$키 <- round(p_vtls_pr2$키)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

p_vtls_pr2_h <- summarise(group_by(p_vtls_pr2,연구번호),height=getmode(키))
p_vtls_pr2<-inner_join(p_vtls_pr2,p_vtls_pr2_h) 
summary(p_vtls_pr2)

p_vtls_pr2 <-full_join(p_vtls_pr2,pr2_df_mom[c(1,2,3)],key = "연구번호")
p_vtls_pr2$기록일자 <- ymd(p_vtls_pr2$기록일자)
p_vtls_pr2 <- p_vtls_pr2 %>% filter(임신추정일 <=기록일자 & 기록일자<=임신종결일)%>% arrange(연구번호,desc(기록일자)) 
p_vtls_pr2 <-p_vtls_pr2[!duplicated(p_vtls_pr2[,c("연구번호","임신추정일","임신종결일")]),]
p_vtls_pr2$bmi <- round(p_vtls_pr2$몸무게/(((p_vtls_pr2$height)/100)^2),2)
summary(p_vtls_pr2$bmi) #na - 10 

pr2_df_mom<-left_join(pr2_df_mom,p_vtls_pr2[c(1,6,7,8)],key=c("연구번호","임신추정일","임신종결일"))
dim(pr2_df_mom) #16878 

# 2.2. 산모 - 동반질환
p_diag_pr2 <- read.csv("data/P_DIAG.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
p_diag_pr2 <- p_diag_pr2[c(1,3,6,7,8,14)]
p_diag_pr2 <- p_diag_pr2 %>% filter(연구번호 %in% pr2_df_mom$연구번호) 
dim(p_diag_pr2) #76678
p_diag_pr2 <- p_diag_pr2 %>% filter(진단분류.주진단.부진단. == "M") 
dim(p_diag_pr2) #33501

# 동반질환 묶기
p_diag_pr2 <- p_diag_pr2 %>% 
  mutate(진단코드_2 = regmatches(p_diag_pr2$진단코드, gregexpr("([[:alpha:]]+[[:digit:]]+)", p_diag_pr2$진단코드)))

p_diag_pr2<- p_diag_pr2 %>% 
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
                           grepl("O120", 진단코드_2, ignore.case = T )~ "임신부종"))

p_diag_pr2 <-full_join(p_diag_pr2, pr2_df_mom[c('연구번호','임신추정일','임신종결일','project2_group')],key="연구번호")
p_diag_pr2$진단일자 <- ymd(p_diag_pr2$진단일자)

p_diag_pr2 <- p_diag_pr2 %>% 
  filter((임신추정일-365 <=진단일자 & 진단일자 <= 임신종결일))
dim(p_diag_pr2) #15529

# 진단명 중복 제거 (같은 연구번호, 같은 진단명_2)
p_diag_pr2 <-p_diag_pr2[!duplicated(p_diag_pr2[,c("연구번호","임신추정일","임신종결일","진단명_2")]),]  
dim(p_diag_pr2) #3203

# 파생변수 생성
table(p_diag_pr2$진단명_2)

p_diag_pr2 <-p_diag_pr2 %>% 
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
         PIH임신중고혈압 = ifelse(진단명_2 == "PIH 임신중고혈압", 1, 0))

# 연구번호 합치기 
p_diag_pr2 <- p_diag_pr2 %>% 
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
         협심증 = sum(협심증))

#중복제거
p_diag_pr2 <- p_diag_pr2[!duplicated(p_diag_pr2[,c('연구번호','임신추정일','임신종결일')]),]
dim(p_diag_pr2) #2781

# na -> 0
p_diag_pr2[is.na(p_diag_pr2)] <- 0 

# join - pr2_df_mom
pr2_df_mom <- left_join(pr2_df_mom,p_diag_pr2, by = c("연구번호","임신추정일","임신종결일","project2_group"))
dim(pr2_df_mom)  # 16878

pr2_df_mom <- subset(pr2_df_mom, select = -c(진단일자,진단코드,진단명,진단한글명,진단분류.주진단.부진단.,진단코드_2))

##########################################################################################################
# 3. 아기 outcome
n_baby_pr2 = read.csv("data/N_BABY.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
dim(n_baby_pr2) #21489
n_distinct(n_baby_pr2$연구번호) #21069명 

#중복된 아기 데이터 처리 - 입원날짜가 빠른 것이 분만일과 가까움 
n_baby_pr2 <- n_baby_pr2 %>% 
  arrange(입원날짜)

n_baby_pr2 <- n_baby_pr2[!duplicated(n_baby_pr2$연구번호),]
dim(n_baby_pr2) #21069

n_baby_pr2 <- n_baby_pr2[c('연구번호','유도분만','분만장소','성별','출생시체중','감염정보_유무','수직감염_유무',
                           'Maternal.Antenatal.History_유무','분만일자','분만형태_코드','Apgar.Score1',
                           'Apgar.Score5','PROM','태변착색','True.knot','Nuchal.cord','산소흡입','심폐소생술.유무',
                           '머리','얼굴','눈','제대','근골격계','위장관계','비뇨기계','첫배변','생식기계','신경계',
                           '병실','재원기간','치료결과','퇴원형태','태반_중량','태반_길이')]

# get 그룹명 
n_baby_pr2 <- left_join(n_baby_pr2, pr2_df_baby[c('연구번호','임신추정일','임신종결일','project2_group','baby_num')], by = c("연구번호" = "baby_num"))
n_baby_pr2 <- n_baby_pr2 %>% rename("참조한.코호트.연구번호" = "연구번호.y") #산모번호 

n_baby_pr2 <- n_baby_pr2 %>% 
  filter(!is.na(project2_group))

dim(n_baby_pr2) #16253

# 데이터 정리
n_baby_pr2$분만일자 <- as.Date(as.character(n_baby_pr2$분만일자),format = "%Y%m%d")
n_baby_pr2$출생시체중 <- ifelse(n_baby_pr2$출생시체중 >=10, n_baby_pr2$출생시체중/1000,n_baby_pr2$출생시체중)
n_baby_pr2$태반_중량 <-ifelse(n_baby_pr2$태반_중량>=1000, n_baby_pr2$태반_중량/100, n_baby_pr2$태반_중량)
n_baby_pr2$태반_길이 <- ifelse(n_baby_pr2$태반_길이 >1000, n_baby_pr2$태반_길이/10, n_baby_pr2$태반_길이)
n_baby_pr2$Apgar.Score1 <- as.numeric(n_baby_pr2$Apgar.Score1)
n_baby_pr2$Apgar.Score5 <- as.numeric(n_baby_pr2$Apgar.Score5)
n_baby_pr2$Apgar.Score1_v2 <- ifelse(n_baby_pr2$Apgar.Score1>=7, 1, 0)
n_baby_pr2$Apgar.Score5_v2 <- ifelse(n_baby_pr2$Apgar.Score5>=7, 1, 0)
n_baby_pr2$치료결과[n_baby_pr2$치료결과 == ""] <-NA

# 3.2 신생아 기형아 진단
n_diag_pr2 <- read.csv("data/N_DIAG.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
n_diag_pr2 <- left_join(n_diag_pr2,n_baby_pr2[,c("연구번호","project2_group")], key = "연구번호") #get group number
dim(n_diag_pr2) #215,024

n_diag_pr2 <- n_diag_pr2 %>% 
  filter(!is.na(project2_group))

dim(n_diag_pr2) #208,182

n_diag_pr2 <- n_diag_pr2 %>% 
  mutate(진단코드_2 = regmatches(n_diag_pr2$진단코드, gregexpr("([[:alpha:]]+[[:digit:]]+)", n_diag_pr2$진단코드)))

n_diag_pr2<- n_diag_pr2 %>% 
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
n_diag_pr2 <-n_diag_pr2[!duplicated(n_diag_pr2[,c("연구번호","진단명_2")]),]  
dim(n_diag_pr2) #11631

# 파생변수 생성
table(n_diag_pr2$진단명_2)

n_diag_pr2 <- n_diag_pr2 %>% 
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
n_diag_pr2 <- n_diag_pr2 %>% 
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
n_diag_pr2 <- n_diag_pr2[!duplicated(n_diag_pr2$연구번호),]
dim(n_diag_pr2) #7008

n_diag_pr2<- n_diag_pr2[c(1,15,18:83)]

# join - n_baby_pr2
n_baby_pr2<- left_join(n_baby_pr2, n_diag_pr2, by = c("연구번호","project2_group"))
dim(n_baby_pr2)  # 16253

