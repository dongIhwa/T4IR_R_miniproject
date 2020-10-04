library(RSelenium)
library(dplyr)

seoultopten


remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome") #셀레니움 서버에 접속해달라
remDr$open()
area <- NULL
oliveyoung <- NULL
for (i in 1:10){
  remDr$navigate("https://www.oliveyoung.co.kr/store/store/getStoreMain.do")
  guName <- seoultopten[i]
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
View(area)
oliveyoung <- data.frame(oliveyoung)
View(oliveyoung)

oliveyoung_result <- data.frame(area,oliveyoung)

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
View(lohbs)

lohbs_result <- data.frame(seoultopten, lohbs)

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
View(lottemart)

lottemart_result <- data.frame(seoultopten, lottemart)


#java -Dwebdriver.chrome.driver="chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

library(RSelenium)
library(dplyr)

seoultopten <- c("관악구", "광진구", "마포구", "영등포구", "동작구",
                 "성동구", "강서구", "용산구", "금천구", "동대문구")

remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome") #셀레니움 서버에 접속해달라
remDr$open()

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

test <- data.frame(emart_addr=seoultopten, n=rep(0,10))

test2 <- union(test, emart_result)
test2 %>%
  group_by(emart_addr) %>%
  summarise(sum(n)) -> test3
emart_result <- as.data.frame(test3)


lottemart_result %>%
  arrange(seoultopten) -> lottemart_result

emart_result %>%
  arrange(emart_addr) -> emart_result

mart_result <- cbind(lottemart_result, emart_result$`sum(n)`)
mart_result %>%
  mutate(합계=lottemart+emart_result$`sum(n)`) %>%
  select(seoultopten, 합계) -> mart_finalresult

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