# 패스트푸드점 점수

library(dplyr)
bugger_lastresult
result_topten %>%
  arrange(행정구역별) %>%
  select(행정구역별, 이삼십대합계) -> arranged_seoul

cbind(arranged_seoul, bugger_lastresult)
bugger_result <- cbind(arranged_seoul, bugger_lastresult$sum_buggerplace)
names(bugger_result) <- c("행정구역별", "이삼십대합계", "버거합계")

bugger_result %>%
  mutate(패스트푸드점당인원수= 이삼십대합계/버거합계) %>%
  arrange(패스트푸드점당인원수) %>%
  select(행정구역별, 패스트푸드점당인원수) -> bugger_middle
single_bugger <- bugger_middle$패스트푸드점당인원수

패스트푸드점수<- 100-(single_bugger/single_bugger[10] *100) + 10

bugger_end_result <- cbind(bugger_middle, 패스트푸드점수)

bugger_end_result %>%
  select(행정구역별, 패스트푸드점수) %>%
  arrange(행정구역별) -> bugger_end_result

bugger_end_result