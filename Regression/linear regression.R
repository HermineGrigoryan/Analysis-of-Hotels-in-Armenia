library(dplyr)
library(ggplot2)

setwd("C:\\Users\\Hermine\\Google Drive\\AUA Lessons\\Year3\\2 Semester\\Econometrics\\Project\\Analysis-of-Hotels-in-Yerevan")

data<-read.csv("merged_data.csv")

SaryanPalette<-function(picture_name, number){
  palette<-getSaryanPallete(picture_name)
  palette<-palette[1:number]
  return(palette)
}


data %>%
  arrange(desc(Price_USD)) %>%
  head(10) %>%
  ggplot(aes(x=reorder(name, Price_USD), y=Price_USD))+geom_bar(stat="identity")+
  coord_flip()+#tema+
  #scale_color_manual(values = SaryanPalette("armenia", 10))+
  #scale_y_continuous(breaks=number_ticks(10))+
  labs(title="Top 10 Hotels with the Highest Price", y="Price (USD)", x="Hotel")

data %>%
  arrange(desc(Review_Count)) %>%
  head(10) %>%
  ggplot(aes(x=reorder(name, Review_Count), y=Review_Count))+geom_bar(stat="identity")+
  coord_flip()


summary(data)

ggplot()+geom_point(data=data, aes(y=Price_USD, x=Review_Count))+geom_smooth(method="lm")
summary(lm(log(Price_USD)~Review_Count+Free_Wifi, data = data))

mod<-lm(log(Price_USD)~Review_Count+Free_Wifi, data = data)
pred<-predict(mod)
hist(pred-na.omit(data$Price_USD), breaks=15, plot=T)
