library(rest)

# 1. 实时行情数据:
dat <-fread("http://finance.yahoo.com/d/quotes.csv?s=GOOGL&f=ohgp")
dat%>%setnames(c("Open", "High", "Low", "Previous_Close"))
dat

# 2. 雅虎日(周月)行情数据`API`:

query_US_stock <- function(Code, startDate = Sys.Date()-365,
                           toDate = Sys.Date(), freq="d"){
  require(lubridate)
  startDate <- as.Date(startDate);startMonth <- month(startDate) -1
  startDay <- day(startDate);startYear <- year(startDate)
  toDate <- as.Date(toDate);toMonth <- month(toDate) -1
  toDay <- day(toDate);toYear <- year(toDate)
  url <- sprintf("http://chart.finance.yahoo.com/table.csv?s=%s&a=%s&b=%s&c=%s&d=%s&e=%s&f=%s&g=%s&ignore=.csv", Code,startMonth, startDay, startYear, toMonth, toDay, toYear,freq)
  downloader::download(url, "stock.csv", quiet = TRUE,mode = "wb")
  stock <- data.table::fread("stock.csv")
  file.remove("stock.csv")
  return(stock)
}

query_US_stock("BABA",freq = "w")

# 3. YQL

url = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(%22AAPL%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="

AAPL.json <- read_html(url)%>%
  html_text()
AAPL.ls <- rjson::fromJSON(AAPL.json)
AAPL.ls$query$results$quote)