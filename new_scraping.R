library(rvest)
library(dplyr)
library(stringr)

URL <- "https://www.tripadvisor.com/Hotels-g293931-Armenia-Hotels.html"
(LINKS<-read_html(URL) %>% 
  html_nodes(css="a.linkText") %>% html_attr("href"))
LINKS<-paste("https://www.tripadvisor.com/", LINKS, sep="")
read_html(LINKS[1])
i<-1
rm(i)

num_pages<-c()

for(i in 1:length(LINKS)){
  HTML<-read_html(LINKS[i])
  number_of_pages<-HTML %>% html_nodes(css=".pageNum") %>% html_text()
  number_of_pages<-as.numeric(rev(number_of_pages)[1])
  link_num_pages<-data.frame(HTML, number_of_pages)
  num_pages<-rbind(num_pages, link_num_pages)
  print(number_of_pages)
}



