#marketing plan
#the problem is to measure the impact of various ways of advertising on sales
#youtube,facebook,newspaper
#1.is there a relationship between advertising and sales?
#2.how strong is the relationship
#3.Which medium contribute to sales the most
#4.how large is the effect of each medium on sales?
#5.is the relationship linear
#6.is there synergy among the advertising media
#2.setting up the environment
install.packages("readr")
library(readr)#reading the csv file
install.packages("dplyr")
library(dplyr)#data wrangling
install.packages("Hmisc")
library(Hmisc)#data description
install.packages("ggplot2")
library(ggplot2)#data visualization
install.packages("datarium")
library(datarium)#dataset
install.packages("caret")
library(caret)#machine learning library for splitting on training and test
data("marketing",package="datarium")
marketing_plan<-marketing
marketing_plan
#3.exploratory analysis
marketing_plan %>% ggplot(aes(x = youtube, y = sales)) +  geom_point() +
  labs(x = "Spending on YouTube ads",y = "Sales", title = "Graph 1: Relationship between YouTube ads and sales") +  stat_smooth(se = FALSE) +   theme(panel.background = element_rect(fill = "white", colour = "grey50"))
#there exist a positive relationship between youtube and sales
marketing_plan %>% ggplot(aes(x = facebook, y = sales)) +  geom_point() +
  labs(x = "Spending on Facebook",y = "Sales", title = "Graph 2: Relationship between Facebook and sales") +  stat_smooth(se = FALSE) +   theme(panel.background = element_rect(fill = "white", colour = "grey50"))
#there exist a positive relationship between facebook and sales
marketing_plan %>% ggplot(aes(x = newspaper, y = sales)) +  geom_point() +
  labs(x = "Spending on Newspaper",y = "Sales", title = "Graph 3: Relationship between Newspaper and sales") +  stat_smooth(se = FALSE) +   theme(panel.background = element_rect(fill = "white", colour = "grey50"))
#there exist a negative relationship between newspaper and sales

#4 training and splitting
set.seed(1)
train_indices<- createDataPartition(y=marketing[["sales"]],p=0.8,list=FALSE)
train_listings<-marketing[train_indices,]
test_listings<-marketing[train_indices,]
#5
model_0<-lm(sales~youtube + facebook + newspaper,data = train_listings)
summary(model_0)
model_1<-lm(sales~youtube+facebook,data=train_listings)
summary(model_1)
model_2<-lm(sales~facebook+I(facebook^2)+youtube+I(youtube^2),data=train_listings)
summary(model_2)
model_3<-lm(sales~facebook+poly(youtube,5),data=train_listings)
summary(model_3)
model_4<-lm(sales~facebook+poly(youtube,3)+facebook*youtube,data=train_listings)
summary(model_4)
#6
#on combining  both facebook and youtube we got a synergic relationship for sales

#2
#Observation
#We can see that the p-values for youtube and facebook is extremely small,
#which means that we reject the null hypothesis that YouTube and Facebook
#donotimpact sales.
#On the other hand, p-value for newspaper is greater than 0.05,
#which means it's not a significant value.
#We fail to reject the null hypothesis that there is any significant relationship between newspaper ads and sales

#3
#the youtube and the facebook contribute more for the sales
#4
#there exist a statistical significant difference between the youtube and facebook
#5
#yes there exist a linear relationship between them 
