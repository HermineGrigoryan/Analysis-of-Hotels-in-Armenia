library(rvest)
library(dplyr)
library(stringr)

setwd("C:\\Users\\Hermine\\Google Drive\\AUA Lessons\\Year3\\2 Semester\\Econometrics\\Project\\Analysis-of-Hotels-in-Yerevan")

links_hotels_20<-read.csv("Links_20_hotels_with_all_pages.csv", stringsAsFactors = F)
url<-links_hotels_20$Link

hotel_name<-c()
main_provider<-c()
main_price<-c()
review_count<-c()
labels<-c()
links<-c()

for (i in url[30:50]) {
  html<-read_html(i)
  name<-html %>% html_nodes(css=".prominent") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  hotel_name<-c(hotel_name, name)
  
  provider <- html %>% html_nodes(css=".provider") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  #provider<-ifelse(length(provider)==0, "Missing", provider)
  main_provider<-c(main_provider, provider)
  
  price <- html %>% html_nodes(css=".price-wrap .price") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  #price<-ifelse(length(price)==0, "Missing", price)
  main_price<-c(main_price, price)
  
  review <- html %>% html_nodes(css=".review_count") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
 #review<-ifelse(length(review)==0, "Missing", review)
  review_count<-c(review_count, review)
  
  lab <- html %>% html_nodes(css=".info-col") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  #review<-ifelse(length(lab)==0, "Missing", lab)
  labels<-c(labels, lab)
  
  link<-html %>% html_nodes(css="a") %>% html_attr("href")
  #link<-ifelse(length(link)==0, "Missing", link)
  links<-c(links, link)
}

rm(i, lab, name, price, provider, review, url)

i<-url[1]

hotel_name2<-c()

for (i in url[30:50]) {
  html<-read_html(i)
  name2<-html %>% html_nodes(css=".price-wrap .price") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  hotel_name2<-c(hotel_name2, name2)
}



str_extract_all(name, , simplify = T)
