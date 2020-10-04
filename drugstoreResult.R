#드럭스토어 결과

library(dplyr)
drugstore_result
result_topten %>%
  arrange(행정구역별) %>%
  select(행정구역별, 이삼십대합계) -> arranged_seoul


drugstore_result <- cbind(arranged_seoul, drugstore_result$합계)
names(drugstore_result) <- c("행정구역별", "이삼십대합계", "드럭스토어합계")


drugstore_result %>%
  mutate(드럭스토어당인원수= 이삼십대합계/드럭스토어합계) %>%
  arrange(드럭스토어당인원수) %>%
  select(행정구역별, 드럭스토어당인원수) -> drugstore_middle

single_drugstore <- drugstore_middle$드럭스토어당인원수


드럭스토어점수<- 100-(single_drugstore/single_drugstore[10] *100) + 10


drugstore_end_result <- cbind(drugstore_middle, 드럭스토어점수)

drugstore_end_result %>%
  select(행정구역별, 드럭스토어점수) %>%
  arrange(행정구역별) -> drugstore_end_result

drugstore_end_result