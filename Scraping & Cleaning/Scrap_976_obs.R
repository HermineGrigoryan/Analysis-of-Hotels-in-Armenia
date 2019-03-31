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

for (i in url) {
  html<-read_html(i)
  name<-html %>% html_nodes(css=".prominent") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  hotel_name<-c(hotel_name, name)
  
  provider <- html %>% html_nodes(css=".provider") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  main_provider<-c(main_provider, provider)
  
  price <- html %>% html_nodes(css=".price-wrap .price") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  main_price<-c(main_price, price)
  
  review <- html %>% html_nodes(css=".review_count") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  review_count<-c(review_count, review)
  
  lab <- html %>% html_nodes(css=".info-col") %>% html_text(trim=TRUE) %>% ifelse(. == "", NA, .) %>% replace(!nzchar(.), NA)
  labels<-c(labels, lab)
  
  link<-html %>% html_nodes(css="a") %>% html_attr("href")
  links<-c(links, link)
}

rm(i, lab, name, price, provider, review, url)

# separating the details (labels)
free_wifi<-str_extract_all(labels, "Free+\\s+Wifi", simplify = T)
breakfast_included<-str_extract_all(labels, "Breakfast+\\s+included", simplify = T)
free_parking<-str_extract_all(labels, "Free+\\s+parking", simplify = T)
pool<-str_extract_all(labels, "Pool", simplify = T)
hotel_website<-str_extract_all(labels, "Visit+\\s+hotel+\\s+website", simplify = T)
special_offer<-str_extract_all(labels, "Special+\\s+Offer", simplify = T)
restaurant<-str_extract_all(labels, "Restaurant", simplify = T)

data<-as.data.frame(cbind(hotel_name, main_provider, main_price, review_count, 
                          free_wifi, breakfast_included, free_parking[,1], pool, hotel_website,
                          special_offer, restaurant))
colnames(data)<-c("name", "Provider", "Price", "Review_Count", "Free_Wifi", 
                  "Breakfast_Included", "Free_Parking", "Swimming_Pool", "Hotel_Website",
                  "Special_Offer", "Restaurant")
data<-unique(data) #there were some hotels that were sponsored, and appeared in several pages. That is why I leave only unique rows.
data<-data[!duplicated(data$name),] #there are some duplicated hotels however they appeared to have different links, because of having grammatical mistakes in the address of the hotel
data$name<-as.character(data$name)

links<-unique(links)
links<-links[!str_detect(links, "#REVIEWS")]
links<-links[str_detect(links, "Hotel_Review")]
links<-na.omit(links)
links<-data.frame(links)
links$duplicated_names<-str_extract_all(links$links, "Reviews-+[a-zA-Z0-9_]{1,}", simplify = T)
links<-links[!duplicated(links$duplicated_names),]

data<-data.frame(data, link=links$links)
data$link<-paste("https://www.tripadvisor.com/", data$link, sep="")
rownames(data)<-NULL

#write.csv(data, "Data/data_scraped_976_obs.csv", row.names = F)
