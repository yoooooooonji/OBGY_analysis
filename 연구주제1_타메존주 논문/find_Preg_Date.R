# 0. install packages 
ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}


pkg<-c("readr","dplyr","tidytext","tidyverse","lubridate","reshape2","psych",
       "PerformanceAnalytics","MatchIt", "gtsummary","gridExtra", "ggridges", "patchwork","caret", "glmnet", "caTools", "xgboost", "fastDummies", "nnet",
       "MASS","car","Epi")
ipak(pkg)

# 1. import data - 산모 초음파
p_func = read.csv("C:/Users/Owner/Desktop/obgy/data/P_FUNC.csv", header=T, stringsAsFactors=F,fileEncoding = "euc-kr")
p_func[,5] <-as.Date(as.character(p_func[,5]),format='%Y%m%d') # 실행일자
p_func[,7] <- as.Date(as.character(p_func[,7]),format='%Y%m%d') # 판독일자
p_func_v2<-p_func[,c(1,2,5,7,10)]

n_distinct(p_func_v2$연구번호) #29,608 


# 2. func data 에서 임신 추정일 산출하기
#####
# # 2.1. to uppper p_func_v2$결과서식문<-toupper(p_func_v2$결과서식문)
#
# # 2.2. 빈도수 파악 (단어 통합을 위해) stop_words <- tibble(word = c("
# ",":","+","&","-","=","_"))
#
# text_df <- p_func_v2[,c(1,3,5)]
#
# # 띄어쓰기 기준으로 token화 
#text_df <-text_df %>% unnest_tokens(output=word,input=결과서식문, token = stringr::str_split, pattern = " ", to_lower=FALSE) 
# 숫자 제외, 빈도수 파악 
#text_tk <- text_df %>% anti_join(stop_words) %>%
  #filter(!str_detect(word,"\\d+")) %>% count(word, sort=TRUE)
#
# #write.csv(text_tk,"text_tk.csv",row.names=TRUE, fileEncoding="euc-kr")
#
# # 2.3. GA와 동일한 의미를 가질 것 같은 단어 검출 - location 파악 
#stop_words<-c("GESTAIONAL","GESTATION","GESTATION.","GESTATONAL") text_df_v2 <-text_df
# %>% filter(str_detect(word,paste(stop_words,collapse="|"))) %>%
# count(word,sort=TRUE)
#####

# 2.4. 단어 통합
# GESTATIONAL(3994건) AGE -> GA change 
p_func_v2$결과서식문2 <-gsub("GESTATIONAL AGE","GA", p_func_v2$결과서식문,fixed=TRUE)

# 2.5. GA : 00 + 0 extract
p_func_v2$결과서식문2 <-gsub(" ","",p_func_v2$결과서식문2, fixed=TRUE) #띄어쓰기 제거 

p_func_v2<-p_func_v2 %>% 
  mutate(GA_period=regmatches(p_func_v2$결과서식문2, gregexpr("(GA:)+([[:digit:]]+)+(?:\\+[[:digit:]])",p_func_v2$결과서식문2)))

# 2.6. 임신주수 GA 값 분해 

p_func_v2$GA_period_2 <-p_func_v2$GA_period[1:length(p_func_v2$GA_period)] %>% 
  map(1) #purrr check!!! 

# 2.7. GA 값 없는 초음파 데이터 삭제
p_func_v2$GA_period_2[p_func_v2$GA_period_2=="NULL"]<-NA
table(is.na(p_func_v2$GA_period_2)) # na : 75,361건, exist : 127,863
p_func_v2<-p_func_v2 %>% 
  filter(!is.na(GA_period_2))

dim(p_func_v2) #127,863건


# 2.8 임신 week + day 분해

p_func_v2 <- p_func_v2 %>% separate(GA_period_2,
                                    into = c("text","week","day"),
                                    sep = ":|\\+",
                                    remove = FALSE)
n_distinct(p_func_v2$연구번호) #21,401명 산모 
 
# 2.9. 임신추정일 계산 (실행일자 기준으로)
p_func_v2$week <-as.numeric(p_func_v2$week)
p_func_v2$day <-as.numeric(p_func_v2$day)
p_func_v2$임신추정일_func <-(p_func_v2$실행일자)-(p_func_v2$week*7+p_func_v2$day) #임신추정일 계산

