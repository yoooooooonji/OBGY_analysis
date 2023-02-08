# find score point
score_DF <- n_baby_pr1[c("score","project1_group","Apgar.Score1","Apgar.Score5", "project1_sub_group")]
score_DF <- score_DF %>% filter(project1_sub_group == "3rd_group")

summary(score_DF$score) #-13.8 ~ 169.92 / -8 ~ 52 

score_DF <- score_DF %>% 
  mutate(apgar1 = ifelse(Apgar.Score1 >=7, 1,0),
         apgar5 = ifelse(Apgar.Score5 >=7, 1,0))

colSums(is.na(score_DF))
score_DF <-score_DF %>% 
  filter(!is.na(Apgar.Score1))

#write.csv(score_DF, "score_df.csv", fileEncoding = "euc-kr")
options(scipen = 100)
########################################################################################################################
# function - apgar.score1 
find_score_1 <- function(data) {
  p_value.1 <- c()
  #p_value.5 <- c()
  score = data$score
  List_1 = list()
  for (i in 1:40) {
      data$group = ifelse(data$score <= i, "low", "high")
      data_filter = data %>%  filter(group == "high")
      apgar1 = chisq.test(table(data_filter$apgar1,data_filter$project1_group))
      p_value.1 = apgar1$p.value
      List_1[[i]] <- p_value.1
    }
  return(List_1)
}

result_1 <- find_score_1(score_DF)
result_1

result_1 <- do.call(rbind.data.frame, result_1)
write.csv(result_1, "apgar.score1(chisq).csv", fileEncoding = "euc-kr")

#########################################################################################################################
#########################################################################################################################
# function - apgar.score5 
find_score_5 <- function(data) {
  #p_value.1 <- c()
  p_value.5 <- c()
  score = data$score
  List_1 = list()
  for (i in 1:40) {
    data$group = ifelse(data$score <= i, "low", "high")
    data_filter = data %>%  filter(group == "high")
    apgar5 = chisq.test(table(data_filter$apgar5,data_filter$project1_group))
    p_value.5 = apgar5$p.value
    List_1[[i]] <- p_value.5
  }
  return(List_1)
}

result_5 <- find_score_5(score_DF)
result_5 <- do.call(rbind.data.frame, result_5)
write.csv(result_5, "apgar.score5(chisq).csv", fileEncoding = "euc-kr")

#########################################
# List1.0 = list()
# data = score_DF
# data$group = ifelse(data$score <= 40, "low", "high")
# data_filter = data %>%  filter(group == "high")
# apgar1 = wilcox.test(data_filter$Apgar.Score1 ~ data_filter$project1_group)
# p_value.1 = apgar1$p.value
# List1.0[[1]] <- p_value.1
# List1.0


