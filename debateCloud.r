# word cloud for presidential debates
# some packages for scraping and cleaning the data
# adapted from http://enelmargen.org/datascience/republican-debates/index.html

library(rvest)
library(xml2)
library(plyr)
library(dplyr)
library(stringi)
library(magrittr)
library(tm)



# function to partially separate and clean into a data.frame a debate from the presidency project
MakeDebateDF<-function(df){
  newdf <- data.frame(
    person = apply(df, 
                   MARGIN = 1, 
                   function(x){
                     stri_extract_first_regex(x, 
                                              "[A-Z'-]+(?=(:\\s))")
                   }),
    message = apply(df, 
                    MARGIN = 1, 
                    function(x){
                      stri_replace_first_regex(x,
                                               "[A-Z'-]+:\\s+", 
                                               "")
                    }),
    stringsAsFactors=FALSE
  )
  for (j in 2:nrow(newdf)) { 
    if (is.na(newdf[j,'person'])) 
    {newdf[j,'person'] <-  newdf[(j-1),'person'] }
  }
  
  return(newdf)
}
#--------------------------------------------------------------------
# Importing debates --- 

tocURL <-"http://www.presidency.ucsb.edu/debates.php"
debateListRaw<- html(tocURL)


# url for all debates
rootURL <- "http://www.presidency.ucsb.edu/ws/index.php?pid="
### -------- debate in Wisconsin (fourth debate)
wisconsin <- "110908"
url <-paste0(rootURL, wisconsin)
debate_w <- paste0(rootURL, wisconsin) %>% html() %>% 
  html_nodes("p") %>%
  html_text()


debate_w <- ldply(debate_w, rbind)
debate_w <- MakeDebateDF(debate_w)

### -------- debate in Boulder, Col. (third debate)
boulder <- "110906"

debate_b <- html(paste0(rootURL, boulder)) %>% 
  html_nodes("p") %>%
  html_text()

debate_b <- ldply(debate_b, rbind)
debate_b <- MakeDebateDF(debate_b)

### -------- debate in Simi Valley, California (second debate)
california <- "110756"

debate_c <- html(paste0(rootURL, california)) %>% 
  html_nodes("p") %>%
  html_text()

debate_c <- ldply(debate_c, rbind)
debate_c <- MakeDebateDF(debate_c)

### -------- debate in Cleveland, Ohio (first debate)
ohio <- "110489"

debate_h <- html(paste0(rootURL, ohio)) %>% 
  html_nodes("p") %>%
  html_text()

debate_h <- ldply(debate_h, rbind)
debate_h <- MakeDebateDF(debate_h)

# Join into large d.f.
all_debates <- rbind(debate_w, 
                     debate_b,
                     debate_c,
                     debate_h)


# analysis
# these are necesary for plots 
library(ggplot2)
# this is for order_axis and theme_eem
# it can be downloaded using 
# devtools::install_github("eflores/eem")
# library(eem)

trump_words <- apply(subset(all_debates, person == "TRUMP")['message'],
                     1,
                     paste)
all_words <- apply(all_debates['message'],1,paste)
# cloud
all_cloud <- rquery.wordcloud(all_words, 
                                "text", 
                                max.words = 300,
                                excludeWords = c("going","and",
                                                 "applause","get",
                                                 "got","let"))
trump_freq <- trump_cloud$freqTable

# top 10
trump_top <- ggplot(order_axis(
  trump_freq[1:10,],
  word, freq), 
  aes(x = word_o, 
      y = freq))+
  geom_bar(stat="identity",
           fill = eem_colors[1]) +
  # theme_eem() + 
  labs(title = "Top 10 words in Debates \n Donald Trump", 
       x = "Word",
       y = "Frequency")
