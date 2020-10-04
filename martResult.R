#마트 결과

library(dplyr)
mart_finalresult
result_topten %>%
  arrange(행정구역별) %>%
  select(행정구역별, 이삼십대합계) -> arranged_seoul


mart_result <- cbind(arranged_seoul, mart_finalresult$합계)
names(mart_result) <- c("행정구역별", "이삼십대합계", "마트합계")

mart_result %>%
  mutate(마트당인원수= 이삼십대합계/마트합계) %>%
  arrange(마트당인원수) %>%
  select(행정구역별, 마트당인원수) -> mart_middle

single_mart <- mart_middle$마트당인원수
single_mart[10] <- NA
mart_middle$마트당인원수 <- single_mart

마트점수<- 100-(single_mart/single_mart[9] *100) + 10
마트점수[10] <- 0

mart_end_result <- cbind(mart_middle, 마트점수)

mart_end_result %>%
  select(행정구역별, 마트점수) %>%
  arrange(행정구역별) -> mart_end_result

mart_end_result