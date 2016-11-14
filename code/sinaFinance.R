# 抓取新浪财经行情数据
library(rvest)
url <- "http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/300350.phtml"
extractTable <- function(url){
  stock <- read_html(url)
  tbl <- stock%>%
    html_nodes(xpath = '//*[@id="FundHoldSharesTable"]')%>%
    html_table()%>% `[[` (1)
  if(length(tbl)>0){
  names(tbl) <- tbl[1,]
  tbl <- tbl[-1,]
  }else{
    tbl <- list() }
  return(tbl)
}

query_stock <- function(code, years = 2012:2016, jidus = 1:4){
  code <- as.character(code)
  urls = c()
  for(jidu in jidus){
    url = sprintf("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/%s.phtml?year=%s&jidu=%s",
                  code,
                   years,
                   jidu)
    urls = c(urls, url)
  }
  stock <- lapply(urls, extractTable)%>%
    rbindlist()
  names(stock) <- c('Date', 'Open', 'High', 'Close', 'Low', 'Volume', 'Volume_yuan')
  stock$Date <- as.Date(stock$Date)
  stock <- stock[, (names(stock)[-1]):= lapply(.SD, as.numeric), .SDcols = names(stock)[-1]][order(Date)]
  names(stock) <- c("日期", "开盘价","最高价","收盘价","最低价","交易量(股)","交易金额(元)")
  return(stock)
}

MaoTai <- query_stock("600519", years = c(2015, 2016), jidus = 1:4)

LeSee <- query_stock("300104")


