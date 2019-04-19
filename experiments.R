library(ggplot2)
library(corrplot)
library(dplyr)
library(scales)

dat<-read.csv("Trip_Advisor_Armenian_Hotels.csv")

dat$lprice<-log(dat$Price_USD)
dat_num<-select_if(dat, is.numeric)
cor_num<-cor(dat_num, use = "complete.obs")
corrplot(cor_num)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
p.mat <- cor.mtest(cor_num)
corrplot(cor_num, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

summary(lm(lprice~Review_Count+Free_WiFi+Free_Parking+State+Photos_Count+Photos_Count+Excellent_Rating, data=dat))

summary(lm(lprice~Average_Score+Review_Count+Free_WiFi+Free_Parking+State+Photos_Count+Photos_Count+Excellent_Rating, data=dat))

summary(lm(lprice~Price_USD+Review_Count*Review_Count+Free_WiFi*Price_USD+Free_Parking+State+Other_Offers, data=dat))

ggplot(dat)+
  geom_boxplot(aes(y=Price_USD, x=State))

table(dat$State)

dat$Yerevan<-ifelse(dat$State=="Yerevan", 1, 0)
summary(lm(lprice~Review_Count+Review_Count*Review_Count+factor(Yerevan), data = dat))

ggplot(main_df, aes(x=Price_USD))+geom_bar(aes(fill=State))+
  scale_y_continuous(breaks=pretty_breaks(10))+
  scale_x_continuous(breaks=pretty_breaks(10))+
  labs(title="Distribution of Price", x="Price (USD)", y="Frequency")
