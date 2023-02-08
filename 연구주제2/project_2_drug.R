library(data.table)

# 변수 형태 
#####
# index = c(4:53,55,56)
# pr2_drug_dcast[,index] <- lapply(pr2_drug_dcast[,index], as.numeric)
# pr2_drug_dcast[,c(4:53,55,56)] <- sapply(pr2_drug_dcast[,c(4:53,55,56)],as.numeric)
# 
# 
# sum(is.na(pr2_drug_dcast))
#####

# project2 _ steroid data

# 1. import data
pr2_drug <- subset(pr2_steroid, select = -c(처방일자, 실시일자,투여기간, 투여종료일자, label_1, label_2, period, unique, group, group_s)) %>% 
  arrange(연구번호, 임신추정일,임신종결일,진료일자,start_drug) %>% 
  select(c(1,2,3,4,6,6,7,8,9,12,5,10,11))

dim(pr2_drug) #1895


# 2. 약물 투여 종료 날짜 조정
pr2_drug <- pr2_drug %>% 
  group_by(연구번호,임신추정일,임신종결일,처방코드) %>% 
  mutate(end_drug_2 = case_when((진료일자 != lead(진료일자) & end_drug >= lead(start_drug)) ~ lead(start_drug)-1,
                                (진료일자 == lead(진료일자) | 진료일자 == lag(진료일자))  ~ end_drug,
                                (end_drug < lead(start_drug)) ~ end_drug,
                                (is.na(lead(start_drug))) ~ end_drug))

  
# 3. 스테로이드 복용 시기에 따라 4기간으로 나누기
# section 1 = 임신 1년 전 ~ 임신추정일 전
# section 2 = 임신추정일 ~ 임신추정일 + 97
# section 3 = 임신추정일 + 98 ~ 임신추정일 + 195
# section 4 = 임신추정일 + 196 ~ 임신종결일

pr2_drug <- subset(pr2_drug, select = -c(진료일자, end_drug)) %>% 
  mutate(section2 = 임신추정일 + 97,
         section3 = 임신추정일 + 195)


# 각 약제 투약 데이터가 section 별로 몇 일씩 포함되어 있는지 계산
pr2_drug <- pr2_drug %>% 
  mutate(section1_overlap = pmax(pmin(end_drug_2,임신추정일-1) - pmax(start_drug, 임신추정일-365) + 1,0),
         section2_overlap = pmax(pmin(end_drug_2,section2) - pmax(start_drug, 임신추정일) + 1,0),
         section3_overlap = pmax(pmin(end_drug_2, section3) - pmax(start_drug, section2+1) + 1,0),
         section4_overlap = pmax(pmin(end_drug_2,임신종결일) - pmax(start_drug, section3+1) + 1,0))



# calculate drug - 각 구간별
pr2_drug <- pr2_drug %>% 
  mutate(section1_drug = as.numeric(section1_overlap) * X1일기준총용량, 
         section2_drug = as.numeric(section2_overlap) * X1일기준총용량,
         section3_drug = as.numeric(section3_overlap) * X1일기준총용량,
         section4_drug = as.numeric(section4_overlap) * X1일기준총용량)



# sum drug - 누적합 계산 
pr2_drug_v2 <- pr2_drug %>% 
  group_by(연구번호,임신추정일,임신종결일,처방코드) %>% 
  summarise(section1 = sum(section1_drug),
            section2 = sum(section2_drug),
            section3 = sum(section3_drug),
            section4 = sum(section4_drug),
            sum_1to4 = sum(section1_drug,section2_drug,section3_drug,section4_drug),
            sum_2to4 = sum(section2_drug,section3_drug,section4_drug))


#unique(pr2_drug_v2$처방코드) # 11개

# 각 약제, 구간 별 변수 생성
pr2_drug_dcast <- dcast(setDT(pr2_drug_v2), 연구번호+임신추정일+임신종결일 ~ 처방코드, value.var = c("section1","section2","section3","section4","sum_1to4","sum_2to4"))
pr2_drug_dcast[is.na(pr2_drug_dcast)] <- 0

dim(pr2_drug_dcast) # 367          

# 4. outcome 
# GA, 출생시체중, 재원기간, 치료결과, 태반중량, 태반길이, Apgar Score1, Apgar Score5
# GA : pr2_df_mom 
# etc : n_baby

# 4.1 maternal outcome join
pr2_drug_mom <-left_join(pr2_drug_dcast,pr2_df_mom[,c('연구번호','임신추정일','임신종결일','GA','age','baby_num','type')], by = c("연구번호" ,"임신추정일","임신종결일"))
dim(pr2_drug_mom) #367  -> 쌍둥이 제외


# 4.2 baby outcome join 
n_baby = read.csv("data/N_BABY.csv",header=T, stringsAsFactors = F, fileEncoding = "euc-kr")

#두번 입원한 아기 데이터 삭제
n_baby <- n_baby %>% 
  arrange(입원날짜)

n_baby <- n_baby[!duplicated(n_baby$연구번호),]
dim(n_baby) #21069


pr2_drug_baby <- left_join(pr2_drug_dcast,pr2_df_baby[,c('연구번호','임신추정일','임신종결일','baby_num','type')], by = c("연구번호" ,"임신추정일","임신종결일"))
pr2_drug_baby <- left_join(pr2_drug_baby,n_baby, by = c("baby_num" = "연구번호"))
dim(pr2_drug_baby) #379


pr2_drug_baby$출생시체중 <- ifelse(pr2_drug_baby$출생시체중 >=10, pr2_drug_baby$출생시체중/1000,pr2_drug_baby$출생시체중)
pr2_drug_baby$태반_중량 <-ifelse(pr2_drug_baby$태반_중량>=1000, pr2_drug_baby$태반_중량/100, pr2_drug_baby$태반_중량)
pr2_drug_baby$태반_길이 <- ifelse(pr2_drug_baby$태반_길이 >1000, pr2_drug_baby$태반_길이/10, pr2_drug_baby$태반_길이)


# 5. GA, age predict 데이터프레임 생성 

# 5.1 maternal 
pr2_mom_pred <- pr2_drug_mom[,c(1:71)]
colSums(is.na(pr2_mom_pred)) #0개 

# 5.2 baby outcome predict
#baby predict df 유산 삭제 
pr2_baby_pred <- pr2_drug_baby[,c(1:69,71,78,96,97,131,135,136)]

pr2_baby_pred <- pr2_baby_pred %>% 
  filter(type=="D" & !is.na(출생시체중))

#colSums(is.na(pr2_baby_pred)) #출생시 체중 4개 na 존재
dim(pr2_baby_pred) #345


# 6. correlation 

# 약제 리스트 
# 타메존주 4mg : section1_DH-BET4J,section2_DH-BET4J,section3_DH-BET4J,section4_DH-BET4J
# 프란딘정 6mg: section1_DH-DFZ6 , section2_DH-DFZ6 , section3_DH-DFZ6 , section4_DH-DFZ6 ,
# 유한덱사메타손정 0.5mg : section1_DH-DX0.5 ,section2_DH-DX0.5 ,section3_DH-DX0.5 ,section4_DH-DX0.5 ,
# 휴메딕스덱사메타손포스페이트이나트륨주사 5mg : section1_DH-DX5J ,section2_DH-DX5J ,section3_DH-DX5J ,section4_DH-DX5J ,
# 프레디솔주사 125mg : section1_DH-MPD125J ,section2_DH-MPD125J ,section3_DH-MPD125J ,section4_DH-MPD125J ,
# 메치론정 4mg : section1_DH-MPD4 ,section2_DH-MPD4 ,section3_DH-MPD4 ,section4_DH-MPD4 ,
# 프레디솔주사 500mg : section1_DH-MPD500J ,section2_DH-MPD500J ,section3_DH-MPD500J ,section4_DH-MPD500J ,
# 디솔린주 40mg : section1_DH-MPDS40J ,section2_DH-MPDS40J ,section3_DH-MPDS40J ,section4_DH-MPDS40J ,
# 소론도정 5mg : section1_DH-PD5 ,section2_DH-PD5 ,section3_DH-PD5 ,section4_DH-PD5 ,
# 트리암시놀론주 40mg : section1_DH-TRI40J ,section2_DH-TRI40J ,section3_DH-TRI40J ,section4_DH-TRI40J ,
# 트리암시놀론주 50mg : 0section1_DH-TRI50J ,section2_DH-TRI50J ,section3_DH-TRI50J ,section4_DH-TRI50J ,

# 6.1 maternal correlation - GA
pr2_mom_pred$GA <-as.numeric(pr2_mom_pred$GA)
#psych::describe(pr2_mom_pred[,4:48]) #표준편차 0 존재 -> 제외 안함(특정 약제를 특정 기간에 안 썼는지 보기 위해)

# Function to add correlation coefficients
#####
# panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
#   usr <- par("usr")
#   on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   Cor <- abs(cor(x, y)) # Remove abs function if desired
#   txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
#   if(missing(cex.cor)) {
#     cex.cor <- 0.4 / strwidth(txt)
#   }
#   text(0.5, 0.5, txt,
#        cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
# }
#####
#####
# cor1 : 베타 
# chart.Correlation(pr2_mom_pred[,c('section1_DH-BET4J','section2_DH-BET4J','section3_DH-BET4J','section4_DH-BET4J','GA')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor2 : prandin
# chart.Correlation(pr2_mom_pred[,c('section1_DH-DFZ6' , 'section2_DH-DFZ6' , 'section3_DH-DFZ6' , 'section4_DH-DFZ6' ,'GA')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor3 : 덱사
# chart.Correlation(pr2_mom_pred[,c('section1_DH-DX0.5' ,'section2_DH-DX0.5' ,'section3_DH-DX0.5' ,'section4_DH-DX0.5' ,
#                                   'section1_DH-DX5J' ,'section2_DH-DX5J' ,'section3_DH-DX5J' ,'section4_DH-DX5J' ,'GA')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor4 : predisol
# chart.Correlation(pr2_mom_pred[,c('section1_DH-MPD125J' ,'section2_DH-MPD125J' ,'section3_DH-MPD125J' ,'section4_DH-MPD125J' ,'GA')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor5 : DH_MPD4
# chart.Correlation(pr2_mom_pred[,c('section2_DH-MPD4' ,'section3_DH-MPD4' ,'section4_DH-MPD4' ,'GA')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor6 : DH-MPD500J
# chart.Correlation(pr2_mom_pred[,c('section1_DH-MPD500J','section2_DH-MPD500J' ,'section3_DH-MPD500J' ,'section4_DH-MPD500J' ,'GA')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor7 : DH-MPD540J
# chart.Correlation(pr2_mom_pred[,c('section1_DH-MPDS40J' ,'section2_DH-MPDS40J' ,'section3_DH-MPDS40J' ,'section4_DH-MPDS40J' ,'GA')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor8 : solondo
# chart.Correlation(pr2_mom_pred[,c('section1_DH-PD5' ,'section2_DH-PD5' ,'section3_DH-PD5' ,'section4_DH-PD5' ,'GA')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor9 : triamcinolone
# chart.Correlation(pr2_mom_pred[,c('section1_DH-TRI40J' ,'section2_DH-TRI40J' ,'section3_DH-TRI40J' ,'section4_DH-TRI40J' ,'GA')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor10 : DH-TRI50J
# chart.Correlation(pr2_mom_pred[,c('section1_DH-TRI50J' ,'section2_DH-TRI50J' ,'section3_DH-TRI50J' ,'section4_DH-TRI50J' ,'GA')],
#                   histogram = TRUE, method = "pearson")

#####

# 6.2. baby outcome
pr2_baby_pred$Apgar.Score1 <-as.numeric(pr2_baby_pred$Apgar.Score1)
pr2_baby_pred$Apgar.Score5 <-as.numeric(pr2_baby_pred$Apgar.Score5)
pr2_baby_pred$출생시체중 <-as.numeric(pr2_baby_pred$출생시체중)

# # 6.2.1. 재원기간
# #####
# #cor1 : 타메존주 4mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-BET4J','section2_DH-BET4J','section3_DH-BET4J','section4_DH-BET4J',
#                                    'sum_1to4_DH-BET4J','sum_2to4_DH-BET4J','재원기간')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor2 : 프란딘정 6mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DFZ6' , 'section2_DH-DFZ6' , 'section3_DH-DFZ6' , 'section4_DH-DFZ6' ,
#                                    'sum_1to4_DH-DFZ6','sum_2to4_DH-DFZ6','재원기간')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor3 : 유한덱사메타손정0.5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX0.5' ,'section2_DH-DX0.5' ,'section3_DH-DX0.5' ,'section4_DH-DX0.5' ,
#                                   'sum_1to4_DH-DX0.5','sum_2to4_DH-DX0.5','재원기간')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor4 : 휴메딕스덱사메타손포스페이트이나트륨주사5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX5J' ,'section2_DH-DX5J' ,'section3_DH-DX5J' ,'section4_DH-DX5J' ,
#                                    'sum_1to4_DH-DX5J', 'sum_2to4_DH-DX5J','재원기간')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor5 : 프레디솔주사 125mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD125J' ,'section2_DH-MPD125J' ,'section3_DH-MPD125J' ,'section4_DH-MPD125J' ,
#                                    'sum_1to4_DH-MPD125J', 'sum_2to4_DH-MPD125J','재원기간')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor6 : 메치론정 4mg
# chart.Correlation(pr2_baby_pred[,c('section2_DH-MPD4' ,'section3_DH-MPD4' ,'section4_DH-MPD4' ,
#                                    'sum_1to4_DH-MPD4', 'sum_2to4_DH-MPD4','재원기간')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor7 : 프레디솔주사 500mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD500J','section2_DH-MPD500J' ,'section3_DH-MPD500J' ,'section4_DH-MPD500J' ,
#                                    'sum_1to4_DH-MPD500J', 'sum_2to4_DH-MPD500J','재원기간')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor8 : 디솔린주 40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPDS40J' ,'section2_DH-MPDS40J' ,'section3_DH-MPDS40J' ,'section4_DH-MPDS40J' ,
#                                    'sum_1to4_DH-MPDS40J', 'sum_2to4_DH-MPDS40J','재원기간')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor9 : 소론도정5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-PD5' ,'section2_DH-PD5' ,'section3_DH-PD5' ,'section4_DH-PD5' ,
#                                    'sum_1to4_DH-PD5', 'sum_2to4_DH-PD5','재원기간')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor10 : 트리암시놀론주40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI40J' ,'section2_DH-TRI40J' ,'section3_DH-TRI40J' ,'section4_DH-TRI40J' ,
#                                    'sum_1to4_DH-TRI40J', 'sum_2to4_DH-TRI40J','재원기간')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor11 : 트리암시놀론주50mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI50J' ,'section2_DH-TRI50J' ,'section3_DH-TRI50J' ,'section4_DH-TRI50J' ,
#                                    'sum_1to4_DH-TRI50J', 'sum_2to4_DH-TRI50J','재원기간')],
#                   histogram = TRUE, method = "pearson")
# #####
# # 6.2.2. 태반길이
# #####
# #cor1 : 타메존주 4mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-BET4J','section2_DH-BET4J','section3_DH-BET4J','section4_DH-BET4J',
#                                    'sum_1to4_DH-BET4J','sum_2to4_DH-BET4J','태반_길이')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor2 : 프란딘정 6mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DFZ6' , 'section2_DH-DFZ6' , 'section3_DH-DFZ6' , 'section4_DH-DFZ6' ,
#                                    'sum_1to4_DH-DFZ6','sum_2to4_DH-DFZ6','태반_길이')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor3 : 유한덱사메타손정0.5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX0.5' ,'section2_DH-DX0.5' ,'section3_DH-DX0.5' ,'section4_DH-DX0.5' ,
#                                    'sum_1to4_DH-DX0.5','sum_2to4_DH-DX0.5','태반_길이')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor4 : 휴메딕스덱사메타손포스페이트이나트륨주사5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX5J' ,'section2_DH-DX5J' ,'section3_DH-DX5J' ,'section4_DH-DX5J' ,
#                                    'sum_1to4_DH-DX5J', 'sum_2to4_DH-DX5J','태반_길이')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor5 : 프레디솔주사 125mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD125J' ,'section2_DH-MPD125J' ,'section3_DH-MPD125J' ,'section4_DH-MPD125J' ,
#                                    'sum_1to4_DH-MPD125J', 'sum_2to4_DH-MPD125J','태반_길이')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor6 : 메치론정 4mg
# chart.Correlation(pr2_baby_pred[,c('section2_DH-MPD4' ,'section3_DH-MPD4' ,'section4_DH-MPD4' ,
#                                    'sum_1to4_DH-MPD4', 'sum_2to4_DH-MPD4','태반_길이')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor7 : 프레디솔주사 500mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD500J','section2_DH-MPD500J' ,'section3_DH-MPD500J' ,'section4_DH-MPD500J' ,
#                                    'sum_1to4_DH-MPD500J', 'sum_2to4_DH-MPD500J','태반_길이')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor8 : 디솔린주 40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPDS40J' ,'section2_DH-MPDS40J' ,'section3_DH-MPDS40J' ,'section4_DH-MPDS40J' ,
#                                    'sum_1to4_DH-MPDS40J', 'sum_2to4_DH-MPDS40J','태반_길이')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor9 : 소론도정5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-PD5' ,'section2_DH-PD5' ,'section3_DH-PD5' ,'section4_DH-PD5' ,
#                                    'sum_1to4_DH-PD5', 'sum_2to4_DH-PD5','태반_길이')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor10 : 트리암시놀론주40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI40J' ,'section2_DH-TRI40J' ,'section3_DH-TRI40J' ,'section4_DH-TRI40J' ,
#                                    'sum_1to4_DH-TRI40J', 'sum_2to4_DH-TRI40J','태반_길이')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor11 : 트리암시놀론주50mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI50J' ,'section2_DH-TRI50J' ,'section3_DH-TRI50J' ,'section4_DH-TRI50J' ,
#                                    'sum_1to4_DH-TRI50J', 'sum_2to4_DH-TRI50J','태반_길이')],
#                   histogram = TRUE, method = "pearson")
# #####
# 
# 
# # 6.2.3. 태반중량
# #####
# #cor1 : 타메존주 4mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-BET4J','section2_DH-BET4J','section3_DH-BET4J','section4_DH-BET4J',
#                                    'sum_1to4_DH-BET4J','sum_2to4_DH-BET4J','태반_중량')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor2 : 프란딘정 6mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DFZ6' , 'section2_DH-DFZ6' , 'section3_DH-DFZ6' , 'section4_DH-DFZ6' ,
#                                    'sum_1to4_DH-DFZ6','sum_2to4_DH-DFZ6','태반_중량')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor3 : 유한덱사메타손정0.5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX0.5' ,'section2_DH-DX0.5' ,'section3_DH-DX0.5' ,'section4_DH-DX0.5' ,
#                                    'sum_1to4_DH-DX0.5','sum_2to4_DH-DX0.5','태반_중량')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor4 : 휴메딕스덱사메타손포스페이트이나트륨주사5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX5J' ,'section2_DH-DX5J' ,'section3_DH-DX5J' ,'section4_DH-DX5J' ,
#                                    'sum_1to4_DH-DX5J', 'sum_2to4_DH-DX5J','태반_중량')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor5 : 프레디솔주사 125mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD125J' ,'section2_DH-MPD125J' ,'section3_DH-MPD125J' ,'section4_DH-MPD125J' ,
#                                    'sum_1to4_DH-MPD125J', 'sum_2to4_DH-MPD125J','태반_중량')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor6 : 메치론정 4mg
# chart.Correlation(pr2_baby_pred[,c('section2_DH-MPD4' ,'section3_DH-MPD4' ,'section4_DH-MPD4' ,
#                                    'sum_1to4_DH-MPD4', 'sum_2to4_DH-MPD4','태반_중량')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor7 : 프레디솔주사 500mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD500J','section2_DH-MPD500J' ,'section3_DH-MPD500J' ,'section4_DH-MPD500J' ,
#                                    'sum_1to4_DH-MPD500J', 'sum_2to4_DH-MPD500J','태반_중량')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor8 : 디솔린주 40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPDS40J' ,'section2_DH-MPDS40J' ,'section3_DH-MPDS40J' ,'section4_DH-MPDS40J' ,
#                                    'sum_1to4_DH-MPDS40J', 'sum_2to4_DH-MPDS40J','태반_중량')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor9 : 소론도정5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-PD5' ,'section2_DH-PD5' ,'section3_DH-PD5' ,'section4_DH-PD5' ,
#                                    'sum_1to4_DH-PD5', 'sum_2to4_DH-PD5','태반_중량')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor10 : 트리암시놀론주40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI40J' ,'section2_DH-TRI40J' ,'section3_DH-TRI40J' ,'section4_DH-TRI40J' ,
#                                    'sum_1to4_DH-TRI40J', 'sum_2to4_DH-TRI40J','태반_중량')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor11 : 트리암시놀론주50mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI50J' ,'section2_DH-TRI50J' ,'section3_DH-TRI50J' ,'section4_DH-TRI50J' ,
#                                    'sum_1to4_DH-TRI50J', 'sum_2to4_DH-TRI50J','태반_중량')],
#                   histogram = TRUE, method = "pearson")
# #####
# 
# # 6.2.4. Apgar.Score1
# #####
# #cor1 : 타메존주 4mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-BET4J','section2_DH-BET4J','section3_DH-BET4J','section4_DH-BET4J',
#                                    'sum_1to4_DH-BET4J','sum_2to4_DH-BET4J','Apgar.Score1')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor2 : 프란딘정 6mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DFZ6' , 'section2_DH-DFZ6' , 'section3_DH-DFZ6' , 'section4_DH-DFZ6' ,
#                                    'sum_1to4_DH-DFZ6','sum_2to4_DH-DFZ6','Apgar.Score1')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor3 : 유한덱사메타손정0.5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX0.5' ,'section2_DH-DX0.5' ,'section3_DH-DX0.5' ,'section4_DH-DX0.5' ,
#                                    'sum_1to4_DH-DX0.5','sum_2to4_DH-DX0.5','Apgar.Score1')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor4 : 휴메딕스덱사메타손포스페이트이나트륨주사5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX5J' ,'section2_DH-DX5J' ,'section3_DH-DX5J' ,'section4_DH-DX5J' ,
#                                    'sum_1to4_DH-DX5J', 'sum_2to4_DH-DX5J','Apgar.Score1')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor5 : 프레디솔주사 125mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD125J' ,'section2_DH-MPD125J' ,'section3_DH-MPD125J' ,'section4_DH-MPD125J' ,
#                                    'sum_1to4_DH-MPD125J', 'sum_2to4_DH-MPD125J','Apgar.Score1')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor6 : 메치론정 4mg
# chart.Correlation(pr2_baby_pred[,c('section2_DH-MPD4' ,'section3_DH-MPD4' ,'section4_DH-MPD4' ,
#                                    'sum_1to4_DH-MPD4', 'sum_2to4_DH-MPD4','Apgar.Score1')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor7 : 프레디솔주사 500mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD500J','section2_DH-MPD500J' ,'section3_DH-MPD500J' ,'section4_DH-MPD500J' ,
#                                    'sum_1to4_DH-MPD500J', 'sum_2to4_DH-MPD500J','Apgar.Score1')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor8 : 디솔린주 40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPDS40J' ,'section2_DH-MPDS40J' ,'section3_DH-MPDS40J' ,'section4_DH-MPDS40J' ,
#                                    'sum_1to4_DH-MPDS40J', 'sum_2to4_DH-MPDS40J','Apgar.Score1')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor9 : 소론도정5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-PD5' ,'section2_DH-PD5' ,'section3_DH-PD5' ,'section4_DH-PD5' ,
#                                    'sum_1to4_DH-PD5', 'sum_2to4_DH-PD5','Apgar.Score1')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor10 : 트리암시놀론주40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI40J' ,'section2_DH-TRI40J' ,'section3_DH-TRI40J' ,'section4_DH-TRI40J' ,
#                                    'sum_1to4_DH-TRI40J', 'sum_2to4_DH-TRI40J','Apgar.Score1')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor11 : 트리암시놀론주50mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI50J' ,'section2_DH-TRI50J' ,'section3_DH-TRI50J' ,'section4_DH-TRI50J' ,
#                                    'sum_1to4_DH-TRI50J', 'sum_2to4_DH-TRI50J','Apgar.Score1')],
#                   histogram = TRUE, method = "pearson")
# #####
# 
# # 6.2.5. Apgar.Score5
# #####
# #cor1 : 타메존주 4mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-BET4J','section2_DH-BET4J','section3_DH-BET4J','section4_DH-BET4J',
#                                    'sum_1to4_DH-BET4J','sum_2to4_DH-BET4J','Apgar.Score5')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor2 : 프란딘정 6mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DFZ6' , 'section2_DH-DFZ6' , 'section3_DH-DFZ6' , 'section4_DH-DFZ6' ,
#                                    'sum_1to4_DH-DFZ6','sum_2to4_DH-DFZ6','Apgar.Score5')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor3 : 유한덱사메타손정0.5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX0.5' ,'section2_DH-DX0.5' ,'section3_DH-DX0.5' ,'section4_DH-DX0.5' ,
#                                    'sum_1to4_DH-DX0.5','sum_2to4_DH-DX0.5','Apgar.Score5')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor4 : 휴메딕스덱사메타손포스페이트이나트륨주사5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX5J' ,'section2_DH-DX5J' ,'section3_DH-DX5J' ,'section4_DH-DX5J' ,
#                                    'sum_1to4_DH-DX5J', 'sum_2to4_DH-DX5J','Apgar.Score5')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor5 : 프레디솔주사 125mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD125J' ,'section2_DH-MPD125J' ,'section3_DH-MPD125J' ,'section4_DH-MPD125J' ,
#                                    'sum_1to4_DH-MPD125J', 'sum_2to4_DH-MPD125J','Apgar.Score5')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor6 : 메치론정 4mg
# chart.Correlation(pr2_baby_pred[,c('section2_DH-MPD4' ,'section3_DH-MPD4' ,'section4_DH-MPD4' ,
#                                    'sum_1to4_DH-MPD4', 'sum_2to4_DH-MPD4','Apgar.Score5')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor7 : 프레디솔주사 500mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD500J','section2_DH-MPD500J' ,'section3_DH-MPD500J' ,'section4_DH-MPD500J' ,
#                                    'sum_1to4_DH-MPD500J', 'sum_2to4_DH-MPD500J','Apgar.Score5')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor8 : 디솔린주 40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPDS40J' ,'section2_DH-MPDS40J' ,'section3_DH-MPDS40J' ,'section4_DH-MPDS40J' ,
#                                    'sum_1to4_DH-MPDS40J', 'sum_2to4_DH-MPDS40J','Apgar.Score5')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor9 : 소론도정5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-PD5' ,'section2_DH-PD5' ,'section3_DH-PD5' ,'section4_DH-PD5' ,
#                                    'sum_1to4_DH-PD5', 'sum_2to4_DH-PD5','Apgar.Score5')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor10 : 트리암시놀론주40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI40J' ,'section2_DH-TRI40J' ,'section3_DH-TRI40J' ,'section4_DH-TRI40J' ,
#                                    'sum_1to4_DH-TRI40J', 'sum_2to4_DH-TRI40J','Apgar.Score5')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor11 : 트리암시놀론주50mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI50J' ,'section2_DH-TRI50J' ,'section3_DH-TRI50J' ,'section4_DH-TRI50J' ,
#                                    'sum_1to4_DH-TRI50J', 'sum_2to4_DH-TRI50J','Apgar.Score5')],
#                   histogram = TRUE, method = "pearson")
# #####
# 
# 
# # 6.2.6. 출생시체중
# #####
# #cor1 : 타메존주 4mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-BET4J','section2_DH-BET4J','section3_DH-BET4J','section4_DH-BET4J',
#                                    'sum_1to4_DH-BET4J','sum_2to4_DH-BET4J','출생시체중')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor2 : 프란딘정 6mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DFZ6' , 'section2_DH-DFZ6' , 'section3_DH-DFZ6' , 'section4_DH-DFZ6' ,
#                                    'sum_1to4_DH-DFZ6','sum_2to4_DH-DFZ6','출생시체중')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor3 : 유한덱사메타손정0.5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX0.5' ,'section2_DH-DX0.5' ,'section3_DH-DX0.5' ,'section4_DH-DX0.5' ,
#                                    'sum_1to4_DH-DX0.5','sum_2to4_DH-DX0.5','출생시체중')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor4 : 휴메딕스덱사메타손포스페이트이나트륨주사5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-DX5J' ,'section2_DH-DX5J' ,'section3_DH-DX5J' ,'section4_DH-DX5J' ,
#                                    'sum_1to4_DH-DX5J', 'sum_2to4_DH-DX5J','출생시체중')],
#                   histrogram = TRUE, method = "pearson")
# 
# # cor5 : 프레디솔주사 125mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD125J' ,'section2_DH-MPD125J' ,'section3_DH-MPD125J' ,'section4_DH-MPD125J' ,
#                                    'sum_1to4_DH-MPD125J', 'sum_2to4_DH-MPD125J','출생시체중')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor6 : 메치론정 4mg
# chart.Correlation(pr2_baby_pred[,c('section2_DH-MPD4' ,'section3_DH-MPD4' ,'section4_DH-MPD4' ,
#                                    'sum_1to4_DH-MPD4', 'sum_2to4_DH-MPD4','출생시체중')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor7 : 프레디솔주사 500mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPD500J','section2_DH-MPD500J' ,'section3_DH-MPD500J' ,'section4_DH-MPD500J' ,
#                                    'sum_1to4_DH-MPD500J', 'sum_2to4_DH-MPD500J','출생시체중')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor8 : 디솔린주 40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-MPDS40J' ,'section2_DH-MPDS40J' ,'section3_DH-MPDS40J' ,'section4_DH-MPDS40J' ,
#                                    'sum_1to4_DH-MPDS40J', 'sum_2to4_DH-MPDS40J','출생시체중')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor9 : 소론도정5mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-PD5' ,'section2_DH-PD5' ,'section3_DH-PD5' ,'section4_DH-PD5' ,
#                                    'sum_1to4_DH-PD5', 'sum_2to4_DH-PD5','출생시체중')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor10 : 트리암시놀론주40mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI40J' ,'section2_DH-TRI40J' ,'section3_DH-TRI40J' ,'section4_DH-TRI40J' ,
#                                    'sum_1to4_DH-TRI40J', 'sum_2to4_DH-TRI40J','출생시체중')],
#                   histogram = TRUE, method = "pearson")
# 
# # cor11 : 트리암시놀론주50mg
# chart.Correlation(pr2_baby_pred[,c('section1_DH-TRI50J' ,'section2_DH-TRI50J' ,'section3_DH-TRI50J' ,'section4_DH-TRI50J' ,
#                                    'sum_1to4_DH-TRI50J', 'sum_2to4_DH-TRI50J','출생시체중')],
#                   histogram = TRUE, method = "pearson")
# #####

















