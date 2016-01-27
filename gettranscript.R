#install.packages("devtools")
#require(devtools)
#install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

library(rvest)
library(stringi)
library(tm)
library("plyr")
library("ggplot2")
library("wordcloud")
library("RColorBrewer")
library("SnowballC")
library(sentiment)
library("stringr")
library(xml2)
library(data.table)


#test

# This page claims to be in iso-8859-1:
url <- 'http://www.elections.ca/content.aspx?section=res&dir=cir/list&document=index&lang=e#list'
elections <- read_html(url)
x <- elections %>% html_nodes("table") %>% .[[2]] %>% html_table() %>% .$TO
# But something looks wrong:
x
# It's acutally UTF-8!
guess_encoding(x)
# We can repair this vector:
repair_encoding(x)
# But it's better to start from scratch with correctly encoded file
elections <- read_html(url, encoding = "UTF-8")
elections %>% html_nodes("table") %>% .[[2]] %>% html_table() %>% .$TO

html("http://www.sec.gov/litigation/suspensions.shtml") %>%
  html_nodes("p+ table a") %>% html_attr(name="href")


url <- "http://espn.go.com/golf/player/_/id/11/stuart-appleby"
url %>% 
  html %>% 
  html_nodes(xpath='//li[contains(.,"Age")]') %>% 
  html_text() %>% 
  str_extract("[A-Z][a-z]{2,} [0-9]{1,2}, [0-9]{4}")


#actual work. testing above

tocURL <-"http://www.presidency.ucsb.edu/debates.php"
debateListRaw<- html(tocURL)
t <- html_nodes(debateListRaw,"table")[8]
t2<-html_nodes(t,"table")
html_table(t2[[2]],fill=T,trim=T)
t3<-html_nodes(t2[[2]],"tr")

debateListRaw%>%
  html_nodes(xpath='//tr[contains(.,"Presidential Debate in")]')%>%
  html_text()%>%  
  str_extract("[A-Z][a-z]{2,} [0-9]{1,2}, [0-9]{4}")

debateListRaw%>%
  html_nodes(xpath='//tr[contains(.,"docdate")]')%>%
  html_text()%>%  
  str_extract("[A-Z][a-z]{2,} [0-9]{1,2}, [0-9]{4}")

debateListRaw%>%
  html_nodes(xpath='//tr[contains(.,"Republican Candidates")]')%>%
  html_text()%>%  
  str_extract("[A-Z][a-z]{2,} [0-9]{1,2}, [0-9]{4}")

html_nodes(t2,"a")[1:16] %>%html_attr("href")

xpathSApply(debateListRaw,"//td[@class='docdate']")
xpathSApply(debateListRaw,"//td[@class='doctext']")
xpathSApply(debateListRaw,"//span[@class='doctext']")

# fill missing columns, and match by col names
DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(B=letters[4:5],C=factor(1:2))
l = list(DT1,DT2)
rbindlist(l, use.names=TRUE, fill=TRUE)


#now we're serious
#-------------------------------------
makeDebateList <- function(trow)
  # parse dates and description plus transcript URL, if any
  c(text=trow%>%html_nodes("td")%>%html_text, url=trow%>%html_nodes("a")%>%html_attr("href"))
# -----------------------------------
tocURL <-"http://www.presidency.ucsb.edu/debates.php"
debateListRaw<- html(tocURL)

#narrow down page to relevant table
t <- html_nodes(debateListRaw,"table")[8]
t2<-html_nodes(t,"tr")
t3<-t2[4:length(t3)]

# extract data elements
debateList<-(lapply(t3,makeDebateList))

options(stringsAsFactors=F)
t4<-  do.call("rbind.fill",lapply(lapply(debateList,t),as.data.frame))
t5<-filter(t4,is.na(url)==FALSE)

startDate <- strptime("June 01,2015",format="%B %d, %Y")
strptime(debateList[[166]][1],format="%B %d, %Y")

#not working
rbindlist(debateList,fill=TRUE)
debateTable<-data.frame(date=NULL,description=NULL,url=NULL)

