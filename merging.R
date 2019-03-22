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
  
}

rm(i, lab, name, price, provider, review, url)

# separating the details (labels)
free_wifi<-str_extract_all(labels, "Free+\\s+Wifi", simplify = T)
breakfast_included<-str_extract_all(labels, "Breakfast+\\s+included", simplify = T)
pool<-str_extract_all(labels, "Pool", simplify = T)
special_offer<-str_extract_all(labels, "Special+\\s+Offer", simplify = T)
restaurant<-str_extract_all(labels, "Restaurant", simplify = T)

data<-as.data.frame(cbind(hotel_name, first_provider, first_price, review_count, 
                          free_wifi, breakfast_included, pool, special_offer, restaurant))

colnames(data)<-c("name", "Provider", "Price", "Review_Count", "Free_Wifi", 
                  "Breakfast_Included", "Swimming_Pool", 
                  "Special_Offer", "Restaurant")

data<-unique(data) #there were some hotels that were sponsored, and appeared in several pages. That is why I leave only unique rows.
data<-data[!duplicated(data$name),] #there are some duplicated hotels however they appeared to have different links, because of having grammatical mistakes in the address of the hotel
data$name<-as.character(data$name)

merged_data<-merge(data, data_UNDP, by="name")
#write.csv(merged_data, "merged_data.csv", row.names = F)
############# Data Cleaning ###############

data[,5:11]<-apply(data[,5:11], 2, factor, labels = c("0", "1")) #turning the variables into dummies
data$Review_Count<-as.numeric(str_remove_all(data$Review_Count, "[review]+s{0,}"))
data$Price_AMD<-as.numeric(str_remove_all(data$Price, "[AMD+\\s]|[:punct:]"))
data$Price<-NULL
data[is.na(data$Price_AMD)=="TRUE", "Price_AMD"]<-round(mean(data$Price_AMD, na.rm = T), 0)
data$Price_USD<-round(data$Price_AMD/484, 0) #turning the price into USD
# Note: Exchange rate as of 11.08.2018

#write.csv(data, "trip_advisor.csv", row.names = F)

#splitting the links for the further use
splitted<-str_split(data[1,"links"], "Reviews+\\-", simplify = T)
paste(splitted[1], "Reviews-", "or", seq(0, 30, by=5), "-", splitted[2], sep="")
