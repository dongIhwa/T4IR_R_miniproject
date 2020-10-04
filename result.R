# 최종결과
library(dplyr)

mart_end_result
drugstore_end_result
bugger_end_result
cafe_end_result

result <- cbind(mart_end_result, drugstore_end_result$드럭스토어점수,
      bugger_end_result$패스트푸드점수, cafe_end_result$카페점수)

names(result) <- c("행정구역별", "마트점수", "드럭스토어점수",
                   "패스트푸드점수", "카페점수")

result %>%
  mutate(총점수=마트점수+드럭스토어점수+패스트푸드점수+카페점수,
            평균점수=총점수/4) %>%
  arrange(desc(평균점수)) %>%
  select(행정구역별, 평균점수) -> result_NoBias

result %>%
  mutate(마트선호점수=(마트점수*0.55)+(드럭스토어점수*0.15)+
                 (패스트푸드점수*0.15)+(카페점수*0.15)) %>%
  arrange(desc(마트선호점수)) %>%
  select(행정구역별, 마트선호점수) -> result_mart

result %>%
  mutate(드럭스토어선호점수=(드럭스토어점수*0.55)+(마트점수*0.15)+
                 (패스트푸드점수*0.15)+(카페점수*0.15)) %>%
  arrange(desc(드럭스토어선호점수)) %>%
  select(행정구역별, 드럭스토어선호점수) -> result_drugstore

result %>%
  mutate(패스트푸드선호점수=(패스트푸드점수*0.55)+(마트점수*0.15)+
                    (드럭스토어점수*0.15)+(카페점수*0.15)) %>%
  arrange(desc(패스트푸드선호점수)) %>%
  select(행정구역별, 패스트푸드선호점수) -> result_bugger

result %>%
  mutate(카페선호점수=(카페점수*0.55)+(마트점수*0.15)+
                    (패스트푸드점수*0.15)+(드럭스토어점수*0.15)) %>%
  arrange(desc(카페선호점수)) %>%
  select(행정구역별, 카페선호점수) -> result_cafe




seoultopten <- c("관악구", "광진구", "마포구", "영등포구", "동작구",
                 "성동구", "강서구", "용산구", "금천구", "동대문구")

rate_result %>%
  mutate(한평당월세평균=평균*3.305785) %>%
  select(자치구명, 한평당월세평균) %>%
  filter(자치구명 %in% seoultopten) %>%
  arrange(자치구명) -> rate_com

with_rate<- cbind(rate_com, result_NoBias$평균점수)
names(with_rate) <- c("자치구명", "한평당월세평균", "평균점수")
with_rate %>%
  mutate(월세고려점수=평균점수/한평당월세평균) %>%
  arrange(desc(월세고려점수)) %>%
  select(자치구명, 월세고려점수) -> with_rate_result

result_NoBias
result_mart
result_bugger
result_cafe
result_drugstore
with_rate_result