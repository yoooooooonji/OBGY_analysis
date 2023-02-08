######## Maternal Characteristics######## 
# 1. age (임신추정일 기준)
# 2. 임신주수
# 3. 산과력/출산력
# 4. twin 
# 4. bmi : weight/height^2

# 1.  age
p_coht = read.csv("data/P_COHT.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
demo_df = left_join(total_df_v4,p_coht[c(1,2)], key="연구번호")
demo_df$age <-year(demo_df$임신추정일) - year(ymd(demo_df$생년월))


# 2. 임신추정일 - 임신종결일 
demo_df$GA <- difftime(demo_df$임신종결일, demo_df$임신추정일, units = "days")

# 3. 산과력 출산력 
demo_df <- left_join(demo_df,n_mom[c(1,2,3,5,6,7,8,9,10,14)], by=c("연구번호" = "참조한.코호트.연구번호", "임신종결일"="분만일자"))
demo_df<- demo_df %>% rename( "baby_num" = "연구번호.y")

demo_df$산과력_출산력A[demo_df$산과력_출산력A == "00"] <- 0
demo_df$산과력_출산력A[demo_df$산과력_출산력A == "01"] <- 1
demo_df$산과력_출산력A[demo_df$산과력_출산력A == "1s"] <- 1
demo_df$산과력_출산력A[demo_df$산과력_출산력A %in% c("2s","2 ")] <- 2
demo_df$산과력_출산력A[demo_df$산과력_출산력A == "3s"] <- 3

demo_df$산과력_출산력SA[demo_df$산과력_출산력SA == "00"] <- 0
demo_df$산과력_출산력SA[demo_df$산과력_출산력SA == "1s"] <- 1

demo_df$산과력_출산력AA[demo_df$산과력_출산력AA %in% c("00"," 0")] <- 0
demo_df$산과력_출산력AA[demo_df$산과력_출산력AA %in% c("1s","1S","01")] <- 1
demo_df$산과력_출산력AA[demo_df$산과력_출산력AA == "2s"] <- 2
demo_df$산과력_출산력AA[demo_df$산과력_출산력AA == "3s"] <- 3
  
demo_df$산과력_출산력L[demo_df$산과력_출산력L %in% c("00", " 0")] <- 0 
demo_df$산과력_출산력L[demo_df$산과력_출산력L %in% c("1s"," 1")] <- 1  

table(demo_df$산과력_출산력T) 
table(demo_df$산과력_출산력P) 
table(demo_df$산과력_출산력A) 
table(demo_df$산과력_출산력SA) 
table(demo_df$산과력_출산력AA)
table(demo_df$산과력_출산력L) 

demo_df$산과력_출산력T_1 <-ifelse(demo_df$산과력_출산력T >=2, "3",demo_df$산과력_출산력T)
demo_df$산과력_출산력P_1 <-ifelse(demo_df$산과력_출산력P >=2, "3",demo_df$산과력_출산력P)
demo_df$산과력_출산력A_1 <-ifelse(demo_df$산과력_출산력A >=2, "3",demo_df$산과력_출산력A)
demo_df$산과력_출산력SA_1 <-ifelse(demo_df$산과력_출산력SA >=2, "3",demo_df$산과력_출산력SA)
demo_df$산과력_출산력AA_1 <-ifelse(demo_df$산과력_출산력AA >=2, "3",demo_df$산과력_출산력AA)
demo_df$산과력_출산력L_1 <-ifelse(demo_df$산과력_출산력L >=2, "3",demo_df$산과력_출산력L)

# 4. bmi
# height : 최빈값
p_vtls_total = read.csv("data/p_vtls.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
p_vtls_total<-p_vtls_total[c(1,4,5,6)] 

p_vtls_total <-p_vtls_total %>% 
  filter(!is.na(키) & !is.na(몸무게)&!is.na(기록일자))

table(is.na(p_vtls_total$키)) 
summary(p_vtls_total) 
# min(키) : 41.02 / min(몸무게) : 3
# max(키) : 192, max(몸무게) : 701

p_vtls_total$몸무게 <- ifelse(p_vtls_total$몸무게<30 | 200<p_vtls_total$몸무게, NA,p_vtls_total$몸무게) #30, 200 사이 벗어난 값 NA처리
p_vtls_total$키 <- ifelse(p_vtls_total$키<100 ,NA, p_vtls_total$키) #100미만 NA처리
summary(p_vtls_total)
#min(키) : 102, #max(키): 192 
#min(w) : 30, max(w): 173
p_vtls_total$키 <- round(p_vtls_total$키)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

p_vtls_total_h <- summarise(group_by(p_vtls_total,연구번호),height=getmode(키))
p_vtls_total<-inner_join(p_vtls_total,p_vtls_total_h) 
summary(p_vtls_total)
#min(height) : 103, max(height) : 188

p_vtls_total <-full_join(p_vtls_total,demo_df[c(1,2,3)],key = "연구번호")
p_vtls_total$기록일자 <- ymd(p_vtls_total$기록일자)
p_vtls_total <- p_vtls_total %>% filter(임신추정일 <=기록일자 & 기록일자<=임신종결일)%>% arrange(연구번호,desc(기록일자)) 
p_vtls_total <-p_vtls_total[!duplicated(p_vtls_total[,c("연구번호","임신추정일","임신종결일")]),]
p_vtls_total$bmi <- round(p_vtls_total$몸무게/(((p_vtls_total$height)/100)^2),2)
summary(p_vtls_total$bmi)

demo_df<-left_join(demo_df,p_vtls_total[c(1,6,7,8)],key=c("연구번호","임신추정일","임신종결일"))

####### 그룹별 약제 사용 비율 ########
table_steroid <- exp_steroid %>%
  group_by(연구번호,group,group_s) %>%
  summarise(steroid_name=unique(처방한글명)) %>%
  ungroup()

round(prop.table(table(table_steroid$steroid_name,table_steroid$group),2)*100,2)

####### 동반질환 ########
# 주진단만 가져오기! 

p_diag_total <- read.csv("data/p_diag.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
p_diag_total <- p_diag_total[c(1,3,6,7,8,14)]
p_diag_total <- p_diag_total %>% filter(연구번호 %in% demo_df$연구번호) 
dim(p_diag_total) #88703
p_diag_total <- p_diag_total %>% filter(진단분류.주진단.부진단. == "M") #38,500

# 동반질환 묶기
p_diag_total <- p_diag_total %>% 
  mutate(진단코드_2 = regmatches(p_diag_total$진단코드, gregexpr("([[:alpha:]]+[[:digit:]]+)", p_diag_total$진단코드)))

p_diag_total<- p_diag_total %>% 
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

p_diag_total <-full_join(p_diag_total, demo_df[c(1,2,3,5,6,9)],key="연구번호")
p_diag_total$진단일자 <- ymd(p_diag_total$진단일자)

p_diag_total <- p_diag_total %>% 
  filter((임신추정일-365 <=진단일자 & 진단일자 <= 임신종결일))
dim(p_diag_total) #18511건


# 진단명 중복 제거 (같은 임신 건, 같은 진단명_2)
p_diag_total <-p_diag_total[!duplicated(p_diag_total[,c("연구번호","임신추정일","임신종결일","진단명_2")]),]  
dim(p_diag_total) #4074

# 파생변수 생성
table(p_diag_total$진단명_2)

p_diag_total <-p_diag_total %>% 
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
         협심증 = ifelse(진단명_2 == "협심증", 1, 0))


# table_diag <-p_diag_total %>%
#   group_by(연구번호,group) %>% 
#   summarise(diag_name = unique(진단명_2)) %>% 
#   ungroup()
# 
# 
# round(prop.table(table(table_diag$diag_name,table_diag$group),2)*100,2)


####### baby outcome ########
n_baby_total = read.csv("data/n_baby.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
n_distinct(n_baby_total$연구번호) #21069

#중복된 아기 데이터 처리 - 입원날짜가 빠른 것이 분만일과 가까움 
n_baby_total <- n_baby_total %>% 
  arrange(입원날짜)
n_baby_total <- n_baby_total[!duplicated(n_baby_total$연구번호),]

n_baby_total <- left_join(n_baby_total,n_mom[c(1,2,14)], key = "연구번호") #get 산모 code 
n_baby_total <- n_baby_total[c(1,2,6,8,11,12,13,22,24,26,27,28,29,30,31,32,33,36,39,41,43,47,49,51,
                   54,56,60,61,62,65,66,67,68)] %>% 
  filter(참조한.코호트.연구번호 %in% demo_df$연구번호)

index = c(2,3,5,6,7,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,33)
n_baby_total[,index] = sapply(n_baby_total[,index],FUN = "as.character")

n_baby_total$분만일자 <- as.Date(as.character(n_baby_total$분만일자),format = "%Y%m%d")
n_baby_total <-left_join(n_baby_total,demo_df[c(1,3,9)], by = c("참조한.코호트.연구번호" = "연구번호","분만일자" = "임신종결일"))

n_baby_total <- n_baby_total %>% filter(!is.na(group_s))
n_baby_total$출생시체중 <- ifelse(n_baby_total$출생시체중 >=10, n_baby_total$출생시체중/1000,n_baby_total$출생시체중)
n_baby_total$태반_중량 <-ifelse(n_baby_total$태반_중량>=1000, n_baby_total$태반_중량/100, n_baby_total$태반_중량)
n_baby_total$태반_길이 <- ifelse(n_baby_total$태반_길이 >1000, n_baby_total$태반_길이/10, n_baby_total$태반_길이)

n_baby_total$Apgar.Score1 <- as.numeric(n_baby_total$Apgar.Score1)
n_baby_total$Apgar.Score5 <- as.numeric(n_baby_total$Apgar.Score5)

n_baby_total$Apgar.Score1_v2 <- ifelse(n_baby_total$Apgar.Score1>=7, "1","0")
n_baby_total$Apgar.Score5_v2 <- ifelse(n_baby_total$Apgar.Score5>=7, "1","0")
n_baby_total$치료결과[n_baby_total$치료결과 == ""] <-NA
summary(n_baby_total$출생시체중)
summary(n_baby_total$태반_중량)
summary(n_baby_total$태반_길이)


############# c_prime,  약물 사용 첫 날짜 가져오기 ##############
# c_prime_steroid <- exp_steroid %>% 
#   filter(group_s == "c_prime")
# 
# c_prime_steroid<-c_prime_steroid %>% 
#   group_by(연구번호,임신추정일,임신종결일) %>% 
#   mutate(first_start_drug = min(start_drug))
# 
# c_prime_steroid <- c_prime_steroid[!duplicated(c_prime_steroid[,c("연구번호","임신추정일","임신종결일")]),]
# c_prime_steroid$ga_steroid <- as.numeric(c_prime_steroid$first_start_drug - c_prime_steroid $임신추정일)
# 
# summary(c_prime_steroid$ga_steroid)




