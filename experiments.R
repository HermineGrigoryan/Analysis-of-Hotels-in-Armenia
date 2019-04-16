library(ggplot2)
library(corrplot)
library(dplyr)

dat<-read.csv("Trip_Advisor_Armenian_Hotels.csv")

dat$lprice<-log(dat$Price_USD)
dat_num<-select_if(dat, is.numeric)
cor_num<-cor(dat_num, use = "complete.obs")
corrplot(cor_num)

summary(lm(lprice~Review_Count+Free_WiFi+Free_Parking+State+Photos_Count+Photos_Count+Excellent_Rating, data=dat))

summary(lm(lprice~Average_Score+Review_Count+Free_WiFi+Free_Parking+State+Photos_Count+Photos_Count+Excellent_Rating, data=dat))

summary(lm(lprice~Price_USD+Review_Count*Review_Count+Free_WiFi*Price_USD+Free_Parking+State+Other_Offers, data=dat))

ggplot(dat)+
  geom_boxplot(aes(y=Price_USD, x=State))

table(dat$State)

dat$Yerevan<-ifelse(dat$State=="Yerevan", 1, 0)
summary(lm(lprice~Review_Count+Review_Count*Review_Count+factor(Yerevan), data = dat))


