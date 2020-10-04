# 카페 점수

library(dplyr)
coffee_finalresult
result_topten %>%
  arrange(행정구역별) %>%
  select(행정구역별, 이삼십대합계) -> arranged_seoul

cbind(arranged_seoul, coffee_finalresult)
cafe_result <- cbind(arranged_seoul, coffee_finalresult$sum_cafe)
names(cafe_result) <- c("행정구역별", "이삼십대합계", "카페합계")

cafe_result %>%
  mutate(카페당인원수= 이삼십대합계/카페합계) %>%
  arrange(카페당인원수) %>%
  select(행정구역별, 카페당인원수) -> cafe_middle
single_cafe <- cafe_middle$카페당인원수

카페점수<- 100-(single_cafe/single_cafe[10] *100) + 10

cafe_end_result <- cbind(cafe_middle, 카페점수)

cafe_end_result %>%
  select(행정구역별, 카페점수) %>%
  arrange(행정구역별) -> cafe_end_result

cafe_end_result