#java -Dwebdriver.chrome.driver="chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

library(RSelenium)
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
  head(10) -> result_topten

seoultopten <- result_topten$행정구역별


#월세정리
rate <- read.csv('data/rate.csv')

# 기준금리 반영한 임대료 구하기
rate %>%
  filter(전월세구분=="월세") %>%
  select(자치구명, 임대면적, 보증금, 임대료) %>%
  mutate(면적당보증금=보증금/임대면적,
               면적당임대료=임대료/임대면적,
               기준금리반영임대료=(면적당보증금*0.005/12)+면적당임대료) %>%
  select(자치구명, 기준금리반영임대료) -> rate_2

rate_2 <- as.data.frame(rate_2)


#반영한 임대료의 평균
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


#올리브영
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
area <- NULL
oliveyoung <- NULL
for (i in 1:10){
  remDr$navigate("https://www.oliveyoung.co.kr/store/store/getStoreMain.do")
  guName <- seoultopten[i]
  Sys.sleep(1)
  webElem <- remDr$findElement(using = "css", "#searchWord")
  webElem$sendKeysToElement(list(guName, key = "enter"))
  Sys.sleep(2)
  storeNumCss <- "#searchWordDiv > div > div.store_sch > dl > dt > span"
  storeNum <- remDr$findElements(using='css selector',storeNumCss)
  num <-sapply(storeNum,function(x){x$getElementText()})
  oliveyoung <- append(oliveyoung, unlist(num))
  area <- append(area, unlist(guName))
}
area <- data.frame(area)
oliveyoung <- data.frame(oliveyoung)

oliveyoung_result <- data.frame(area,oliveyoung)


#롭스
lohbs <- NULL
for (i in 1:10){
  remDr$navigate("https://www.lotteon.com/p/lotteplus/offlinestore/offLineStoreInfo?mall_no=7")
  guName <- seoultopten[i]
  webElem <- remDr$findElement(using = "css", "div.searchAreaWrap.centerSearchColor > div > input[type=search]")
  webElem$sendKeysToElement(list(guName, key = "enter"))
  Sys.sleep(2)
  storeNum <- remDr$findElements(using='css', ".storeContents")
  num <-sapply(storeNum,function(x){x$getElementText()})
  lohbs <- append(lohbs, unlist(length(num)))
}
lohbs <- data.frame(lohbs)


lohbs_result <- data.frame(seoultopten, lohbs)


oliveyoung_result %>%
  arrange(area) -> oliveyoung_result
oliveyoung_result$oliveyoung <- as.numeric(oliveyoung_result$oliveyoung)

lohbs_result %>%
  arrange(seoultopten) -> lohbs_result
lohbs_result$lohbs

drugstore_result <- cbind(oliveyoung_result, lohbs_result$lohbs)
drugstore_result %>%
  mutate(합계=oliveyoung+`lohbs_result$lohbs`) %>%
  select(area, 합계) -> drugstore_result

#드럭스토어 결과
drugstore_result
result_topten %>%
  arrange(행정구역별) %>%
  select(행정구역별, 이삼십대합계) -> arranged_seoul


drugstore_result <- cbind(arranged_seoul, drugstore_result$합계)
names(drugstore_result) <- c("자치구명", "이삼십대합계", "드럭스토어합계")


drugstore_result %>%
  mutate(드럭스토어당인원수= 이삼십대합계/드럭스토어합계) %>%
  arrange(드럭스토어당인원수) %>%
  select(자치구명, 드럭스토어당인원수) -> drugstore_middle

single_drugstore <- drugstore_middle$드럭스토어당인원수


드럭스토어점수<- 100-(single_drugstore/single_drugstore[10] *100) + 10


drugstore_end_result <- cbind(drugstore_middle, 드럭스토어점수)

drugstore_end_result %>%
  select(자치구명, 드럭스토어점수) %>%
  arrange(자치구명) -> drugstore_end_result

drugstore_end_result


#롯데마트
lottemart <- NULL
for (i in 1:10){
  remDr$navigate("https://www.lotteon.com/p/lotteplus/offlinestore/offLineStoreInfo?mall_no=4")
  guName <- seoultopten[i]
  webElem <- remDr$findElement(using = "css", "#content > div > div > div.searchForm > div.searchAreaWrap.centerSearchColor > div > input[type=search]")
  webElem$sendKeysToElement(list(guName, key = "enter"))
  Sys.sleep(2)
  storeNum <- remDr$findElements(using='css', ".storeContents")
  num <-sapply(storeNum,function(x){x$getElementText()})
  lottemart <- append(lottemart, unlist(length(num)))
}
lottemart <- data.frame(lottemart)

lottemart_result <- data.frame(seoultopten, lottemart)


#이마트
url_emart <- "https://store.emart.com/branch/list.do"
emart <- NULL

remDr$navigate(url_emart)
Sys.sleep(3)

emart_seoul <- remDr$findElement(using="css",
                                 "#area_A > a")

emart_seoul$clickElement()
emart_storelist <- remDr$findElement(using="css",
                                     "#branchType")
emart_store <- remDr$findElement(using="css",
                                 "#branchType > option:nth-child(2)")
emart_storelist$clickElement()
emart_store$clickElement()



emartElem <- remDr$findElement("css", "#frm > div.conts > div.store-top > div.sorting-wrap.sorting-wrap-branch > div.sorting-bottom > div")


for(i in c(1:30)){
  remDr$executeScript("scrollTo(0, 0)", args = list(emartElem))
  Sys.sleep(1)
  remDr$executeScript("scrollBy(0, 3200)", args = list(emartElem))
  Sys.sleep(1)
  emart_seoulstore <- remDr$findElement(using="css",
                                        paste0("#branchList > li:nth-child(", i, ") > a"))
  emart_seoulstore$clickElement()
  Sys.sleep(2)
  emart_storename <- remDr$findElement(using="css",
                                       "#conts > div > div.store-intro > div.store-header > h2")
  emart_name <- unlist(emart_storename$getElementText())
  emart_storeaddr <- remDr$findElement(using="css",
                                       "#conts > div > div.map-wrap > div.branch-info > div.branch-info1 > ul > li:nth-child(2) > dl > dd:nth-child(2)")
  emart_addr <- unlist(emart_storeaddr$getElementText())
  emart_addr <- unlist(strsplit(emart_addr, " "))[2]
  emart_list <- data.frame(emart_name, emart_addr)
  emart <- rbind(emart, emart_list)
  
}

emart %>%
  group_by(emart_addr) %>%
  tally() -> emart_result

emart_result <- as.data.frame(emart_result)
emart_result %>%
  filter(emart_addr %in% seoultopten) -> emart_result

emart_basic <- data.frame(emart_addr=seoultopten, n=rep(0,10))

emart_result <- union(emart_basic, emart_result)
emart_result %>%
  group_by(emart_addr) %>%
  summarise(sum(n)) -> emart_result
emart_result <- as.data.frame(emart_result)


#마트 분석

lottemart_result %>%
  arrange(seoultopten) -> lottemart_result

emart_result %>%
  arrange(emart_addr) -> emart_result

mart_result <- cbind(lottemart_result, emart_result$`sum(n)`)
mart_result %>%
  mutate(합계=lottemart+emart_result$`sum(n)`) %>%
  select(seoultopten, 합계) -> mart_finalresult


mart_finalresult
result_topten %>%
  arrange(행정구역별) %>%
  select(행정구역별, 이삼십대합계) -> arranged_seoul


mart_result <- cbind(arranged_seoul, mart_finalresult$합계)
names(mart_result) <- c("자치구명", "이삼십대합계", "마트합계")

mart_result %>%
  mutate(마트당인원수= 이삼십대합계/마트합계) %>%
  arrange(마트당인원수) %>%
  select(자치구명, 마트당인원수) -> mart_middle

single_mart <- mart_middle$마트당인원수
single_mart[10] <- NA
mart_middle$마트당인원수 <- single_mart

마트점수<- 100-(single_mart/single_mart[9] *100) + 10
마트점수[10] <- 0

mart_end_result <- cbind(mart_middle, 마트점수)

mart_end_result %>%
  select(자치구명, 마트점수) %>%
  arrange(자치구명) -> mart_end_result

mart_end_result


#버거킹

url_buggerking <- "https://www.burgerking.co.kr/#/store"
buggerking <- NULL

remDr$navigate(url_buggerking)
remDr$navigate(url_buggerking)
buggerking_place <- remDr$findElement(using='css', "#app > div > div.contentsWrap > div.contentsBox01.nopadding > div > div.map_searchWrap > div.map_search_head > div.tab01 > ul > li:nth-child(3)")
buggerking_place$clickElement()

buggerking_seoul <- remDr$findElement(using='css', "#app > div > div.contentsWrap > div.contentsBox01.nopadding > div > div.map_searchWrap > div.map_search_head > div.searchWrap > div:nth-child(5) > div > select:nth-child(1)")
buggerking_seoul2 <- remDr$findElement(using='css', "#app > div > div.contentsWrap > div.contentsBox01.nopadding > div > div.map_searchWrap > div.map_search_head > div.searchWrap > div:nth-child(5) > div > select:nth-child(1) > option:nth-child(2)")
buggerking_seoul$clickElement()
buggerking_seoul2$clickElement()

for(i in c(2:26)){
  buggerking_seoul3 <- remDr$findElement(using='css', "#app > div > div.contentsWrap > div.contentsBox01.nopadding > div > div.map_searchWrap > div.map_search_head > div.searchWrap > div:nth-child(5) > div > select:nth-child(2)")
  buggerking_seoul3$clickElement()
  buggerking_seoul4<- remDr$findElement(using='css', 
                                        paste0("#app > div > div.contentsWrap > div.contentsBox01.nopadding > div > div.map_searchWrap > div.map_search_head > div.searchWrap > div:nth-child(5) > div > select:nth-child(2) > option:nth-child(",i,")"))
  buggerking_seoul4$clickElement()
  Sys.sleep(2)
  seoul <- buggerking_seoul4$getElementText()
  seoul <- unlist(seoul)
  buggerking_storenum <- remDr$findElement(using='css',
                                           "#app > div > div.contentsWrap > div.contentsBox01.nopadding > div > div.map_searchWrap > div.container01.search_result > div > p > em > span")
  buggerking_num <- buggerking_storenum$getElementText()
  buggerking_num <- unlist(buggerking_num)
  buggerking_num <- as.numeric(buggerking_num)
  buggerking_result <- data.frame(seoul, buggerking_num)
  buggerking <- rbind(buggerking, buggerking_result)
}



# 맥도날드
buggerking %>%
  filter(seoul %in% seoultopten) -> buggerking_topten

gulist <- buggerking$seoul
url_mcdonalds <- "https://www.mcdonalds.co.kr/kor/store/list.do"
mcdonalds <- NULL

for(j in gulist){
  remDr$navigate(url_mcdonalds)
  mc_search <- remDr$findElement(using="css",
                                 "#searchWord")
  mc_search$sendKeysToElement(list(j, key = "enter"))
  Sys.sleep(2)
  seoul_mc <- j
  mclist <- remDr$findElements(using='css',
                               "#container > div.content > div.contArea > div > div > div.mcStore > table > tbody > tr > td.tdName > dl > dt > strong > a")
  mclist_2 <- sapply(mclist,function(x){x$getElementText()})
  mclist_2 <- unlist(mclist_2)
  length(mclist_2)
  mcdonalds_result <- data.frame(seoul_mc, length(mclist_2))
  mcdonalds <- rbind(mcdonalds, mcdonalds_result)
}

mcdonalds %>%
  filter(seoul_mc %in% seoultopten) -> mcdonalds_topten

bugger_result <- cbind(mcdonalds_topten, buggerking_topten$buggerking_num)
names(bugger_result) <- c("seoul", "mcdonalds", "buggerking")
bugger_result

bugger_result %>%
  mutate(sum_buggerplace=mcdonalds+buggerking) %>%
  select(seoul, sum_buggerplace) -> bugger_lastresult

# 패스트푸드점 점수

bugger_lastresult
result_topten %>%
  arrange(행정구역별) %>%
  select(행정구역별, 이삼십대합계) -> arranged_seoul

cbind(arranged_seoul, bugger_lastresult)
bugger_result <- cbind(arranged_seoul, bugger_lastresult$sum_buggerplace)
names(bugger_result) <- c("자치구명", "이삼십대합계", "버거합계")

bugger_result %>%
  mutate(패스트푸드점당인원수= 이삼십대합계/버거합계) %>%
  arrange(패스트푸드점당인원수) %>%
  select(자치구명, 패스트푸드점당인원수) -> bugger_middle
single_bugger <- bugger_middle$패스트푸드점당인원수

패스트푸드점수<- 100-(single_bugger/single_bugger[10] *100) + 10

bugger_end_result <- cbind(bugger_middle, 패스트푸드점수)

bugger_end_result %>%
  select(자치구명, 패스트푸드점수) %>%
  arrange(자치구명) -> bugger_end_result

bugger_end_result


#스타벅스
url_starbucks <- "https://www.starbucks.co.kr/store/store_map.do?disp=quick"
starbucks <- NULL

remDr$navigate(url_starbucks)
Sys.sleep(3)
gulist

for(j in gulist){
  starbucks_search <- remDr$findElement(using="css",
                                        "#quickSearchText")
  starbucks_search$sendKeysToElement(list(j, key = "enter"))
  Sys.sleep(2)
  seoul_starbucks <- j
  starbucks_num <- remDr$findElement(using='css',
                                     "#container > div > form > fieldset > div > section > article.find_store_cont > article > article:nth-child(2) > div.result_num_wrap.myStoreInfo > span.en.t_006633.resultCtnNumberTab1")
  starbucks_storenum <- starbucks_num$getElementText()
  starbucks_storenum <- unlist(starbucks_storenum)
  starbucks_storenum <- as.numeric(starbucks_storenum)
  
  starbucks_result <- data.frame(seoul_starbucks, starbucks_storenum)
  starbucks <- rbind(starbucks, starbucks_result)
}

starbucks %>%
  filter(seoul_starbucks %in% seoultopten) -> starbucks_finalresult



#커피빈
url_coffeebean <- "https://www.coffeebeankorea.com/store/store.asp"
coffeebean <- NULL

remDr$navigate(url_coffeebean)
Sys.sleep(3)

gulist

for(j in gulist){
  remDr$navigate(url_coffeebean)
  Sys.sleep(2)
  bean_search <- remDr$findElement(using="css",
                                   "#keyword")
  bean_search$sendKeysToElement(list(j, key = "enter"))
  Sys.sleep(2)
  seoul_bean <- j
  
  webElem <- remDr$findElement("css", ".cont")
  remDr$executeScript("scrollTo(0, 0)", args = list(webElem))
  Sys.sleep(1)
  remDr$executeScript("scrollBy(0, 3200)", args = list(webElem))
  Sys.sleep(1)
  beanlist <- remDr$findElements(using="css",
                                 "#storeListUL > li")
  beanlist_2 <- sapply(beanlist,function(x){x$getElementText()})
  beanlist_2 <- unlist(beanlist_2)
  beanlist_2 <- length(beanlist_2)
  
  bean_result <- data.frame(seoul_bean, beanlist_2)
  coffeebean <- rbind(coffeebean, bean_result)
}

coffeebean %>%
  filter(seoul_bean %in% seoultopten) -> coffeebean_finalresult

#카페 분석

coffee_list <- cbind(starbucks_finalresult, coffeebean_finalresult$beanlist_2)
names(coffee_list) <- c("seoul", "starbucks", "coffeebean")

coffee_list %>%
  mutate(sum_cafe=starbucks+coffeebean) %>%
  select(seoul, sum_cafe) -> coffee_finalresult

coffee_finalresult
result_topten %>%
  arrange(행정구역별) %>%
  select(행정구역별, 이삼십대합계) -> arranged_seoul

cbind(arranged_seoul, coffee_finalresult)
cafe_result <- cbind(arranged_seoul, coffee_finalresult$sum_cafe)
names(cafe_result) <- c("자치구명", "이삼십대합계", "카페합계")

cafe_result %>%
  mutate(카페당인원수= 이삼십대합계/카페합계) %>%
  arrange(카페당인원수) %>%
  select(자치구명, 카페당인원수) -> cafe_middle
single_cafe <- cafe_middle$카페당인원수

카페점수<- 100-(single_cafe/single_cafe[10] *100) + 10

cafe_end_result <- cbind(cafe_middle, 카페점수)

cafe_end_result %>%
  select(자치구명, 카페점수) %>%
  arrange(자치구명) -> cafe_end_result

cafe_end_result


#최종 분석

mart_end_result
drugstore_end_result
bugger_end_result
cafe_end_result

score_result <- cbind(mart_end_result, drugstore_end_result$드럭스토어점수,
                bugger_end_result$패스트푸드점수, cafe_end_result$카페점수)

names(score_result) <- c("자치구명", "마트점수", "드럭스토어점수",
                   "패스트푸드점수", "카페점수")

score_result %>%
  mutate(총점수=마트점수+드럭스토어점수+패스트푸드점수+카페점수,
            평균점수=총점수/4) %>%
  arrange(desc(평균점수)) %>%
  select(자치구명, 평균점수) -> result_NoBias

score_result %>%
  mutate(마트선호점수=(마트점수*0.55)+(드럭스토어점수*0.15)+
                 (패스트푸드점수*0.15)+(카페점수*0.15)) %>%
  arrange(desc(마트선호점수)) %>%
  select(자치구명, 마트선호점수) -> result_mart

score_result %>%
  mutate(드럭스토어선호점수=(드럭스토어점수*0.55)+(마트점수*0.15)+
                    (패스트푸드점수*0.15)+(카페점수*0.15)) %>%
  arrange(desc(드럭스토어선호점수)) %>%
  select(자치구명, 드럭스토어선호점수) -> result_drugstore

score_result %>%
  mutate(패스트푸드선호점수=(패스트푸드점수*0.55)+(마트점수*0.15)+
                    (드럭스토어점수*0.15)+(카페점수*0.15)) %>%
  arrange(desc(패스트푸드선호점수)) %>%
  select(자치구명, 패스트푸드선호점수) -> result_bugger

score_result %>%
  mutate(카페선호점수=(카페점수*0.55)+(마트점수*0.15)+
                 (패스트푸드점수*0.15)+(드럭스토어점수*0.15)) %>%
  arrange(desc(카페선호점수)) %>%
  select(자치구명, 카페선호점수) -> result_cafe




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


result_topten %>%
  select(자치구명,이삼십대비율)-> result_topten2

result_topten2 %>%
  inner_join(result_NoBias) -> result_NoBias2

result_topten2 %>%
  inner_join(result_mart) -> result_mart2

result_topten2 %>%
  inner_join(result_bugger) -> result_bugger2

result_topten2 %>%
  inner_join(result_cafe) -> result_cafe2

result_topten2 %>%
  inner_join(result_drugstore) -> result_drugstore2

result_topten2 %>%
  inner_join(with_rate_result) -> with_rate_result2

result_topten2 %>%
  inner_join(rate_com) -> rate_com2

#상관계수에 대한 가설검정
cor.test(result_NoBias2$이삼십대비율, result_NoBias2$평균점수)
cor.test(with_rate_result2$이삼십대비율, with_rate_result2$월세고려점수)
cor.test(rate_com2$이삼십대비율, rate_com2$한평당월세평균)
cor.test(result_mart2$이삼십대비율, result_mart2$마트선호점수)
cor.test(result_bugger2$이삼십대비율, result_bugger2$패스트푸드선호점수)
cor.test(result_cafe2$이삼십대비율, result_cafe2$카페선호점수)
cor.test(result_drugstore2$이삼십대비율, result_drugstore2$드럭스토어선호점수)


#데이터 시각화
install.packages("corrplot")
library(corrplot)
#상관계수 행렬
result_topten %>% arrange(자치구명) %>% select(자치구명, 이삼십대비율) ->popul
with_rate_result %>% arrange(자치구명) -> rate2
all_store <- cbind(popul, result[2:5])
all_store1 <- cbind(all_store, rate2[2])
all_store1_cor <- cor(all_store1[2:7])
all_store1_cor

corrplot(all_store1_cor, method="circle")
dev.copy(png, "corr_graph.png", height=600, width=900, bg="white")
dev.off()



library(ggplot2)
topten_palette <- c("관악구"="#003399", "광진구"="#00cc99", "마포구"="#33cc33", "영등포구"="#009900", "동작구"="#99ccff",
                 "성동구"="#0099ff", "강서구"="#6666ff", "용산구"="#3366ff", "금천구"="#cccc00", "동대문구"="#77b300")

#지역별 인구비율
result_topten
names(result_topten)[names(result_topten)=="행정구역별"] <- c("자치구명")
topten_data <- result_topten %>% arrange(이삼십대비율)
topten_data$자치구명 <- factor(topten_data$자치구명, levels=topten_data$자치구명)
population <- ggplot(topten_data, aes(x=자치구명, y=이삼십대비율, fill=자치구명))
population1 <- population + geom_bar(stat="identity") + coord_cartesian(ylim=c(0, 100)) + scale_fill_manual(values = topten_palette) + coord_flip()
population2 <- population1 + labs(x="자치구", y="인구비율(%)", title="지역별 점수 그래프", subtitle="20-30대 인구 비율")
population3 <- population2 + theme(plot.title=element_text(face="bold", color="steelblue", size = 20),
                                   plot.subtitle=element_text(face="bold", color="tomato", size = 15),
                                   axis.title.x=element_text(face="bold", size = 15), axis.title.y=element_text(face="bold", size = 15),
                                   axis.text.x=element_text(size=15), axis.text.y=element_text(size=15))
population3
ggsave("population.png")

#모든 편의시설을 선호하는 경우
NoBias_data <- result_NoBias %>% arrange(평균점수)
NoBias_data$자치구명 <- factor(NoBias_data$자치구명, levels=NoBias_data$자치구명)
all <- ggplot(NoBias_data, aes(x=자치구명, y=평균점수, fill=자치구명)) 
all1 <- all + geom_bar(stat="identity") + coord_cartesian(ylim=c(0, 100)) + scale_fill_manual(values = topten_palette) + coord_flip()
all2 <- all1 + labs(x="자치구", y="점수", title="지역별 점수 그래프", subtitle="모든 편의시설을 선호하는 경우")
all3 <- all2 + theme(plot.title=element_text(face="bold", color="steelblue", size = 20),
                     plot.subtitle=element_text(face="bold", color="tomato", size = 15),
             axis.title.x=element_text(face="bold", size = 15), axis.title.y=element_text(face="bold", size = 15),
             axis.text.x=element_text(size=15), axis.text.y=element_text(size=15))
all3
ggsave("NoBias.png")

#마트를 선호하는 경우
mart_data <- result_mart %>% arrange(마트선호점수)
mart_data$자치구명 <- factor(mart_data$자치구명, levels=mart_data$자치구명)
mart <- ggplot(mart_data, aes(x=자치구명, y=마트선호점수, fill=자치구명)) 
mart1 <- mart + geom_bar(stat="identity") + coord_cartesian(ylim=c(0, 100)) + scale_fill_manual(values = topten_palette) + coord_flip()
mart2 <- mart1 + labs(x="자치구", y="점수", title="지역별 점수 그래프", subtitle="마트를 선호하는 경우")
mart3 <- mart2 + theme(plot.title=element_text(face="bold", color="steelblue", size = 20),
                       plot.subtitle=element_text(face="bold", color="tomato", size = 15),
                   axis.title.x=element_text(face="bold", size = 15), axis.title.y=element_text(face="bold", size = 15),
                   axis.text.x=element_text(size=15), axis.text.y=element_text(size=15))
mart3
ggsave("mart.png")

#패스트푸드점을 선호하는 경우
bugger_data <- result_bugger %>% arrange(패스트푸드선호점수)
bugger_data$자치구명 <- factor(bugger_data$자치구명, levels=bugger_data$자치구명)
bugger <- ggplot(bugger_data, aes(x=자치구명, y=패스트푸드선호점수, fill=자치구명))
bugger1 <- bugger + geom_bar(stat="identity") + coord_cartesian(ylim=c(0, 100)) + scale_fill_manual(values = topten_palette) + coord_flip()
bugger2 <- bugger1 + labs(x="자치구", y="점수", title="지역별 점수 그래프", subtitle="패스트푸드점을 선호하는 경우")
bugger3 <- bugger2 + theme(plot.title=element_text(face="bold", color="steelblue", size = 20),
                           plot.subtitle=element_text(face="bold", color="tomato", size = 15),
                   axis.title.x=element_text(face="bold", size = 15), axis.title.y=element_text(face="bold", size = 15),
                   axis.text.x=element_text(size=15), axis.text.y=element_text(size=15))
bugger3
ggsave("bugger.png")

#카페를 선호하는 경우
cafe_data <- result_cafe %>% arrange(카페선호점수)
cafe_data$자치구명 <- factor(cafe_data$자치구명, levels=cafe_data$자치구명)
cafe <- ggplot(cafe_data, aes(x=자치구명, y=카페선호점수, fill=자치구명))
cafe1 <- cafe + geom_bar(stat="identity") + coord_cartesian(ylim=c(0, 100)) + scale_fill_manual(values = topten_palette) + coord_flip()
cafe2 <- cafe1 + labs(x="자치구", y="점수", title="지역별 점수 그래프", subtitle="카페를 선호하는 경우")
cafe3 <- cafe2 + theme(plot.title=element_text(face="bold", color="steelblue", size = 20),
                       plot.subtitle=element_text(face="bold", color="tomato", size = 15),
                   axis.title.x=element_text(face="bold", size = 15), axis.title.y=element_text(face="bold", size = 15),
                   axis.text.x=element_text(size=15), axis.text.y=element_text(size=15))
cafe3
ggsave("cafe.png")

#드럭스토어를 선호하는 경우
drugstore_data <- result_drugstore %>% arrange(드럭스토어선호점수)
drugstore_data$자치구명 <- factor(drugstore_data$자치구명, levels=drugstore_data$자치구명)
drugstore <- ggplot(drugstore_data, aes(x=자치구명, y=드럭스토어선호점수, fill=자치구명))
drugstore1 <- drugstore + geom_bar(stat="identity") + coord_cartesian(ylim=c(0, 100)) + scale_fill_manual(values = topten_palette) + coord_flip()
drugstore2 <- drugstore1 + labs(x="자치구", y="점수", title="지역별 점수 그래프", subtitle="드럭스토어를 선호하는 경우")
drugstore3 <- drugstore2 + theme(plot.title=element_text(face="bold", color="steelblue", size = 20),
                                 plot.subtitle=element_text(face="bold", color="tomato", size = 15),
                   axis.title.x=element_text(face="bold", size = 15), axis.title.y=element_text(face="bold", size = 15),
                   axis.text.x=element_text(size=15), axis.text.y=element_text(size=15))
drugstore3
ggsave("drugstore.png")

#월세를 고려하는 경우
with_rate_data <- with_rate_result %>% arrange(월세고려점수)
with_rate_data$자치구명 <- factor(with_rate_data$자치구명, levels=with_rate_data$자치구명)
wolse <- ggplot(with_rate_data, aes(x=자치구명, y=월세고려점수, fill=자치구명))
wolse1 <- wolse + geom_bar(stat="identity") + coord_cartesian(ylim=c(0, 100)) + scale_fill_manual(values = topten_palette) + coord_flip()
wolse2 <- wolse1 + labs(x="자치구", y="점수", title="지역별 점수 그래프", subtitle="월세를 고려하는 경우")
wolse3 <- wolse2 + theme(plot.title=element_text(face="bold", color="steelblue", size = 20),
                         plot.subtitle=element_text(face="bold", color="tomato", size = 15),
                   axis.title.x=element_text(face="bold", size = 15), axis.title.y=element_text(face="bold", size = 15),
                   axis.text.x=element_text(size=15), axis.text.y=element_text(size=15))
wolse3
ggsave("wolse.png")


library(gridExtra)
all_graph <- grid.arrange(all3, mart3, bugger3, cafe3, drugstore3, wolse3, nrow = 2)
all_graph
dev.copy(png, "all_graph.png", height=600, width=900, bg="white")
dev.off()

