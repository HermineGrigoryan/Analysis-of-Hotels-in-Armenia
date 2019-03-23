library(rvest)
library(dplyr)
library(stringr)

setwd("C:\\Users\\Hermine\\Google Drive\\AUA Lessons\\Year3\\2 Semester\\Econometrics\\Project\\Analysis-of-Hotels-in-Yerevan")


data_UNDP<-readxl::read_excel("ObjectData01302019_for_MongoDB.xlsx")
data_UNDP<-filter(data_UNDP, city %in% "Yerevan" & objectType %in% "Hotel" )
data_UNDP<-data_UNDP[!duplicated(data_UNDP$name),]

url<-paste("https://www.tripadvisor.com/Hotels-g293932-oa", seq(0, 510, by=30), "-Yerevan-Hotels.html", sep="")
hotel_name<-c()
first_provider<-c()
first_price<-c()
review_count<-c()
labels<-c()
links<-c()

for (i in url) {
  html<-read_html(i)
  name<-html %>% html_nodes(css=".prominent") %>% html_text()
  hotel_name<-c(hotel_name, name)
  
  provider <- html %>% html_nodes(css=".provider") %>% html_text()
  first_provider<-c(first_provider, provider)
  
  price <- html %>% html_nodes(css=".price-wrap .price") %>% html_text()
  first_price<-c(first_price, price)
  
  review <- html %>% html_nodes(css=".review_count") %>% html_text()
  review_count<-c(review_count, review)
  
  lab <- html %>% html_nodes(css=".info-col") %>% html_text()
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

data<-as.data.frame(cbind(hotel_name, first_provider, first_price, review_count, 
                          free_wifi, breakfast_included, free_parking, pool, hotel_website,
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
links<-links[!str_detect(links, "d10718105")] #deleting duplicated links
links<-links[!str_detect(links, "d8152142")] #these links actually have different structure, but represent the same hotel
links<-links[!str_detect(links, "d15780015")]
links<-links[!str_detect(links, "d14803097")]
links<-links[!str_detect(links, "d10534437")]
links<-links[!str_detect(links, "d15212515")]

data<-data.frame(data, link=links)
data$link<-paste("https://www.tripadvisor.com/", data$link, sep="")
rownames(data)<-NULL



############# Data Cleaning ###############

data[,5:11]<-apply(data[,5:11], 2, factor, labels = c("0", "1")) #turning the variables into dummies
data$Review_Count<-as.numeric(str_remove_all(data$Review_Count, "[review]+s{0,}"))
data$Price_AMD<-as.numeric(str_remove_all(data$Price, "[AMD+\\s]|[:punct:]"))
data$Price<-NULL
data[is.na(data$Price_AMD)=="TRUE", "Price_AMD"]<-round(mean(data$Price_AMD, na.rm = T), 0)
data$Price_USD<-round(data$Price_AMD/484, 0) #turning the price into USD
# Note: Exchange rate as of 11.08.2018

#write.csv(data, "trip_advisor_Hermine.csv", row.names = F)

merged_data<-merge(data, data_UNDP, by=c("name", "link"))
#write.csv(merged_data, "merged_data.csv", row.names = F)

hist(merged_data$Price_AMD)
