library(RSelenium)
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
  arrange(평균) -> rate_result

rate_result <- as.data.frame(rate_result)

View(rate_result)

# 7평 기준 평균 월세
rate_result %>%
  mutate(한평당월세평균=평균*3.305785) %>%
  select(자치구명, 한평당월세평균)


remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome") #셀레니움 서버에 접속해달라
remDr$open()
area <- NULL
oliveyoung <- NULL
for (i in 1:10){
  remDr$navigate("https://www.oliveyoung.co.kr/store/store/getStoreMain.do")
  guName <- rate_result$자치구명[i]
  webElem <- remDr$findElement(using = "css", "#searchWord")
  webElem$sendKeysToElement(list(guName, key = "enter"))
  Sys.sleep(1)
  storeNumCss <- "#searchWordDiv > div > div.store_sch > dl > dt > span"
  storeNum <- remDr$findElements(using='css selector',storeNumCss)
  num <-sapply(storeNum,function(x){x$getElementText()})
  oliveyoung <- append(oliveyoung, unlist(num))
  area <- append(area, unlist(guName))
}
area <- data.frame(area)
View(area)
oliveyoung <- data.frame(oliveyoung)
View(oliveyoung)

lohbs <- NULL
for (i in 1:10){
  remDr$navigate("https://www.lotteon.com/p/lotteplus/offlinestore/offLineStoreInfo?mall_no=7")
  guName <- rate_result$자치구명[i]
  webElem <- remDr$findElement(using = "css", "div.searchAreaWrap.centerSearchColor > div > input[type=search]")
  webElem$sendKeysToElement(list(guName, key = "enter"))
  Sys.sleep(1)
  storeNum <- remDr$findElements(using='css', ".storeContents")
  num <-sapply(storeNum,function(x){x$getElementText()})
  lohbs <- append(lohbs, unlist(length(num)))
}
lohbs <- data.frame(lohbs)
View(lohbs)

lottemart <- NULL
for (i in 1:10){
  remDr$navigate("https://www.lotteon.com/p/lotteplus/offlinestore/offLineStoreInfo?mall_no=4")
  guName <- rate_result$자치구명[i]
  webElem <- remDr$findElement(using = "css", "#content > div > div > div.searchForm > div.searchAreaWrap.centerSearchColor > div > input[type=search]")
  webElem$sendKeysToElement(list(guName, key = "enter"))
  Sys.sleep(1)
  storeNum <- remDr$findElements(using='css', ".storeContents")
  num <-sapply(storeNum,function(x){x$getElementText()})
  lottemart <- append(lottemart, unlist(length(num)))
}
lottemart <- data.frame(lottemart)
View(lottemart)

store <- data.frame(area, oliveyoung, lohbs, lottemart)
View(store)


