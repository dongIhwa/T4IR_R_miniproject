library(dplyr)

rate <- read.csv('data/rate.csv')


rate %>%
  filter(전월세구분=="월세") %>%
  select(자치구명, 임대면적, 보증금, 임대료) %>%
  mutate(면적당보증금=보증금/임대면적,
               면적당임대료=임대료/임대면적,
               기준금리반영임대료=(면적당보증금*0.005/12)+면적당임대료) %>%
  select(자치구명, 기준금리반영임대료) -> rate_2

rate_2 <- as.data.frame(rate_2)

rate_2 %>%
  group_by(자치구명) %>%
  summarise(평균 = mean(기준금리반영임대료)) %>%
  arrange(desc(평균)) -> rate_result

rate_result <- as.data.frame(rate_result)

View(rate_result)
# 한평 기준 평균 월세
rate_result %>%
  mutate(한평당월세평균=평균*3.305785) %>%
  select(자치구명, 한평당월세평균)