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

tocURL <-"http://www.presidency.ucsb.edu/debates.php"
debateListRaw<- html(tocURL)
t <- html_nodes(debateListRaw,"table")[8]

t2<-html_nodes(t,"table")
html_nodes(t2,"a") %>%html_attr("href")

xpathSApply(debateListRaw,"//td[@class='docdate']")
xpathSApply(debateListRaw,"//td[@class='doctext']")
xpathSApply(debateListRaw,"//span[@class='doctext']")

