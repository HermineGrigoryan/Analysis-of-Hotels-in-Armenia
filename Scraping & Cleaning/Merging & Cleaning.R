library(dplyr)
library(stringr)

data<-read.csv("Data/data_scraped_976_obs.csv", stringsAsFactors = F)
data_UNDP<-readxl::read_excel("Data/ObjectData18.xlsx")

data_UNDP<-filter(data_UNDP, objectType=="Hotel")
data_UNDP$link<-paste("https://www.tripadvisor.com/", data_UNDP$link, sep="")

###############################################################################
####################### Part 1 ################################################
###############################################################################

data[,5:11]<-apply(data[,5:11], 2, factor, labels = c("0", "1")) #turning the variables into dummies
data$Review_Count<-as.numeric(str_remove_all(data$Review_Count, "[review]+s{0,}"))
data$Price_AMD<-as.numeric(str_remove_all(data$Price, "[AMD+\\s]|[:punct:]"))
data$Price<-NULL
#data[is.na(data$Price_AMD)=="TRUE", "Price_AMD"]<-round(mean(data$Price_AMD, na.rm = T), 0)
data$Price_USD<-round(data$Price_AMD/486, 0) #turning the price into USD
# Note: Exchange rate as of 3.31.2019


merged_data<-merge(data, data_UNDP, by=c("name", "link"))
#write.csv(merged_data, "merged_data.csv", row.names = F)

###############################################################################
####################### Part 2 ################################################
###############################################################################
scoreDistribution<-str_replace_all(merged_data$scoreDistribution, "\\]", "\\,")
scoreDistribution<-data.frame(str_extract_all(scoreDistribution, "[0-9]{1,},", simplify=T))
colnames(scoreDistribution)<-c("Excellent", "Good", "Average", "Poor", "Terrible")
scoreDistribution<-data.frame(apply(scoreDistribution, 2, str_remove_all, "\\,"))
scoreDistribution<-apply(scoreDistribution, 2, as.numeric)
scoreDistribution[is.na(scoreDistribution)]<-0

merged_data<-data.frame(merged_data, scoreDistribution)
dat<-merged_data

final_data<-data.frame(Name=dat$name, Link=dat$link, Price_AMD=dat$Price_AMD, Price_USD=dat$Price_USD,
                       State=dat$state, City=dat$city, Address=dat$address, Zipcode=dat$zipcode, 
                       Photos_Count=dat$photosCount, Ranking_Category=dat$rankingCategory, 
                       Rank_in_Category=dat$objectRank, N_Hotels_in_Category=dat$objectsInCategoryCount, 
                       Review_Count=dat$Review_Count, Average_Score=dat$avgScore, Excellent_Rating=dat$Excellent, 
                       Good_Rating=dat$Good, Average_Rating=dat$Average, Poor_Rating=dat$Poor, Terrible_Rating=dat$Terrible,  
                       Certificate=dat$certificate, Language_distribution=dat$languageDistribution, 
                       Provider=dat$Provider, Free_WiFi=dat$Free_Wifi,Breakfast_Included=dat$Breakfast_Included,
                       Free_Parking=dat$Free_Parking,
                       Swimming_Pool=dat$Swimming_Pool, Hotel_Website=dat$Hotel_Website, Special_Offer=dat$Special_Offer,
                       Restaurant=dat$Restaurant)

final_data$Certificate<-ifelse(final_data$Certificate==FALSE, 0, 1)
str(final_data)

#write.csv(final_data, "Trip_Advisor_Armenian_Hotels.csv", row.names = F)
