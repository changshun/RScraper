# 抓取网站
library(RCurl)
library(XML)
library(stringr)
library(dplyr)
library(rvest)
library(wordcloud2)

setwd("/Users/shihchosen/Documents/R/R-workplace/AirplaneTicketScrapy/Wiley-ADCR/ch-9-scraping/scenario-maryland/wisepapers/")
# 获得论文的每一个分页
pageLinks <- str_c("http://121.192.176.75/index.php?ser_id=2&search=&year=&os=",seq(0, 240, 20))

# 获得
paperlinks <- lapply(pageLinks, getHTMLLinks)%>%
  unlist()%>%
  str_extract("\\?ser_id=2&p_id=\\d+")%>%
  na.omit()%>%
  str_c("http://121.192.176.75/index.php",.)

# Function to scrap
getPaperInfo <- function(paperlink){
  page_parse <- htmlParse(paperlink,encoding = "utf-8")
  # Get author name
  author <- xpathSApply(page_parse,'//*[@id="paged_list"]/dl/dd[1]/strong',xmlValue)
  # insert & between diff names
  author <- str_replace(author, ',','\\&')
  # paper title
  title <- xpathSApply(page_parse,'//*[@id="paged_list"]/dl/dt', xmlValue)
  # paper url
  journal <- xpathSApply(page_parse,'//*[@id="paged_list"]/dl/dd[2]/i', xmlValue)
  # File name include authoe title journal
  keyWords <- xpathSApply(page_parse,'//*[@id="paged_list"]/dl/dd[6]/text()', xmlValue)
  keyWords <- ifelse(length(keyWords)==0, NA, keyWords)
  fullTextLink <- getHTMLLinks(page_parse)%>%
    str_extract(".+\\.pdf")%>%
    na.omit()
  fullTextLink <- ifelse(length(fullTextLink)==0,NA,
                         str_c("http://121.192.176.75",fullTextLink))
  updatedDate <- xpathSApply(page_parse,'//*[@id="paged_list"]/dl/dd[3]', xmlValue)%>%
    str_extract("20\\d{6}")
  paperInfo <- data.frame(author, title,journal,keyWords, paperlink,fullTextLink, updatedDate)
  return(paperInfo)
}

papers.df <- lapply(paperlinks, getPaperInfo)%>%
  do.call("rbind",.)
papers.df <- papers.df[!duplicated(papers.df$title),]
papers.en <- papers.df[str_detect(papers.df$journal,"[A-Za-z]"),]
papers.en$journal <- trimws(papers.en$journal)
 
keyWords <- str_c(na.omit(papers.en$keyWords), collapse = " ")

## Caculate 

library(tm)
keyWords_source <- VectorSource(keyWords)
wordMatrix <- Corpus(keyWords_source)%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)%>%
  tm_map(removeWords, stopwords("english"))%>%
  DocumentTermMatrix()%>%as.matrix()%>%t()%>%
  as.data.frame()
wordMatrix$Term <- rownames(wordMatrix)
names(wordMatrix) <- c("Freq","Term")

termFreq <- select(wordMatrix, Term, Freq)

termFreq$Term <- str_replace(termFreq$Term,"models","model")

termFreq <- ddply(termFreq, .(Term), summarise, sum(Freq))

names(termFreq) <- c("Term","Freq")

wordcloud2(termFreq, shape = "star")

figPath = system.file("examples/h.png",package = "wordcloud2")
wordcloud2(termFreq, figPath = figPath, size = 1.5,color = "red")

