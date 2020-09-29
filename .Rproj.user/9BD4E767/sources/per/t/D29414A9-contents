library(dplyr)

# 다운받은 공공DB 파일 읽은 후 전처리
age <- readLines('data/age.txt', encoding="UTF-8")
age <- gsub(",", "", age)
age <- gsub("\t", ",", age)


# 기존 벡터형식을 데이터 프레임 형식으로
total <- NULL
for(i in seq(1:length(age))){
  age_line <- strsplit(age[i], ",")
  age_line <- unlist(age_line)
  total <- rbind(total, age_line)
}

result <- as.data.frame(total)


# 데이터프레임의 열 이름 변경
age_first_line <- strsplit(age[1], ",")
age_first_line <- unlist(age_first_line)
names(result) <- age_first_line
View(result)


# 한국인이면서 20-30대인 인구 수를 추출
result %>%
  filter(구분=="한국인") %>%
  select(행정구역별, 계, '20~24세', '25~29세', '30~34세', '35~39세') -> result_2
View(result_2)

# 계산을 위해 형변환 수행 및 이름 정리
for(i in c(2:6)) {
  result_2[i] <- as.numeric(unlist(result_2[i]))
}
names(result_2)<- gsub("~", "", names(result_2))
names(result_2) <- c("행정구역별", "계", "이십사세", "이십구세",
                     "삼십사세", "삼십구세")

# 20대 30대 인구 합계와 비율을 구해서 상위 10개 구 표시
result_2 %>%
  filter(행정구역별!="합계") %>%
  mutate(이삼십대합계= 이십사세+이십구세+삼십사세+삼십구세,
           이삼십대비율= 이삼십대합계/계*100) %>%
  select(행정구역별, 계, 이삼십대합계, 이삼십대비율) %>%
  arrange(desc(이삼십대비율)) %>%
  head(10)