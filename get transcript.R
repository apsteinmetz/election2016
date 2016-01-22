library(rvest)
library(stringi)
library(tm)
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