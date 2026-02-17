install.packages(c("ggplot2","dplyr"))
library(ggplot2)
library(dplyr)

datCO2<- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

colnames(datCO2)[4]<-"CO2" #overwrites 4th column

datCO2$Entity<- as.factor(datCO2$Entity)

US<- datCO2 %>%
  filter(Entity=='United States')
  
  
plot(US$Year,US$CO2, type='b',pch=19, xlab='Year',
     ylab='Fossil fuel emissions(billions of tons of CO2)', yaxt='n')
axis(2, seq(0,6000000000,by=2000000000),seq(0,6, by=2),las=2 )
#limitations: labels, open circle points, labels on y at weird angle


ggplot(US,aes(x=Year,y=CO2))+ #+ is specific to ggplot
  geom_point()+
  geom_line()+
  labs(x='Year', y="US fossil fuel CO2 emissions (tons CO2)")+
  theme_classic()

NorthA<-datCO2 %>%
  filter(Entity=='United States' | 
           Entity=='Mexico'|
           Entity=='Canada')

ggplot(NorthA,
       aes(x=Year,y=CO2, color=Entity))+
  geom_point()+
  scale_color_manual(values=c("red","royalblue","darkgoldenrod3"))


#In class prompts -----
climate<- read.csv("/cloud/project/activity03/climate-change.csv")

#install.packages('lubridate')
#library(lubridate)

climate$date<-ymd(climate$Day)
plot(climate$date,climate$temperature_anomaly)

ggplot(climate,aes(Day, temperature_anomaly))+
  geom_point() #if thick line below x axis- it isnt reading it well


climate$Entity<- as.factor(climate$Entity)

#Prompt 1:
#Base R:
NorthH<-climate%>%
  filter(Entity=='Northern Hemisphere')

SouthH<-climate%>%
  filter(Entity=='Southern Hemisphere')

plot(NorthH$date,NorthH$temperature_anomaly,type='b',pch=19,xlab='Date',ylab='Temperature Anomaly',col='blue')
points(SouthH$date,SouthH$temperature_anomaly,type='b',pch=19,xlab='Date',ylab='Temperature Anomaly', col='red')

#GGPLOT:
Hemispheres<-climate %>%
  filter(Entity=='Northern Hemisphere'| 
           Entity=='Southern Hemisphere' )

ggplot(Hemispheres,aes(x=date,y=temperature_anomaly,color = Entity))+
  geom_point()+
  labs(x='Year',y='Temperature Anomaly')


#Prompt 2 + 3:
total_emm<-NorthA%>%
  group_by(Entity)%>%
  summarise(total_emmission=sum(CO2))

ggplot(total_emm, aes(Entity,total_emmission))+
         geom_col()+
  labs(x='Country', y=expression('Total Emissions of CO'[2]))

#3 is to use expression() with what we want the subscript to be in[]
       