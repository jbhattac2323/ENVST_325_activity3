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


#Homework Question-----
#Q1: Last 100 years Developing countries in South Asia (Pakistan and India) average emission scatter plot with lines along with a dotted line showing average emission level in USA (developed country)

Comparison<-datCO2%>%
  filter(Entity=='United States' |Entity=='India' |Entity=='Pakistan')%>%
  filter(Year>1920)

ggplot(Comparison, aes(x=Year, y= CO2/1e9,col=Entity))+
  geom_line()+
  scale_color_manual(values = c(
      "United States" = "red",
      "India" = "royalblue",
      "Pakistan" = "darkgreen")) +
  labs(y=expression("Annual Emissions of CO"[2]~"[billions of tons]"),
       title = expression("Trends in CO"[2]~"Emissions: United States, India, and Pakistan(1920-2020)")
  )+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5)) #center

#Q2: Communicate about both issues 
#average temperature fluctuation around the world.
#separating world data from others
worldCO2<-datCO2%>%
  filter(Entity=='World')%>%
  filter(Year>1919)
  

worldTemp<-climate%>% #1920-2020
  filter(Entity=='World')%>%
  filter(date>"1919-12-15")

#CO2 Plot
ggplot(worldCO2,aes(x=Year, y= CO2/1e9))+
  geom_line()+
  labs(y=expression("Annual Emissions of CO"[2]~"[billions of tons]"),
       title = expression("Trends in World CO"[2]~"Emission Levels since 1920")
  )+
  scale_x_continuous(limits = c(1920, 2021))+ #to prvent 2020 label isn't being cut
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5)) #center

#World Temp
ggplot(worldTemp,aes(x=date, y= temperature_anomaly))+
  geom_line()+
  labs(x="Year", y=expression("Temperature Anomaly (in Celsius)"),
       title = expression("Trends in World Temperature Anomalies since 1920")
  )+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5)) #center

