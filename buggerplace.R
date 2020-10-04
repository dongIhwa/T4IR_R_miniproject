#java -Dwebdriver.chrome.driver="chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

library(RSelenium)
library(dplyr)

seoultopten <- c("관악구", "광진구", "마포구", "영등포구", "동작구",
                 "성동구", "강서구", "용산구", "금천구", "동대문구")

remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome") #셀레니움 서버에 접속해달라
remDr$open()
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

buggerking %>%
  filter(seoul %in% seoultopten) -> buggerking_topten



# 맥도날드
gulist <- buggerking$seoul
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome") #셀레니움 서버에 접속해달라
remDr$open()
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