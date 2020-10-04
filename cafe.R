#java -Dwebdriver.chrome.driver="chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

library(RSelenium)
library(dplyr)

seoultopten <- c("관악구", "광진구", "마포구", "영등포구", "동작구",
                 "성동구", "강서구", "용산구", "금천구", "동대문구")

remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome") #셀레니움 서버에 접속해달라
remDr$open()
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

coffee_list <- cbind(starbucks_finalresult, coffeebean_finalresult$beanlist_2)
names(coffee_list) <- c("seoul", "starbucks", "coffeebean")

coffee_list %>%
  mutate(sum_cafe=starbucks+coffeebean) %>%
  select(seoul, sum_cafe) -> coffee_finalresult