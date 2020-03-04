#import file
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library("growthrates")
setwd("/Users/salimeh/Desktop/Data\ Science/PS_6")
Turkey_ER<- read_xls("Turkey_Monthly_ER.xls")
#read_excel("Turkey_Monthly_ER.xls")
Turkey_ER<-Turkey_ER[-c(1:120,840,839),]

#separate date into separate columns of Month and year
names(Turkey_ER)[2]<-"date"
format(Turkey_ER$date, format="%Y/%M")
date_1<-separate(Turkey_ER, "date", c("Year", "Month"), sep="M")
#ln RER
lRER_CPI_2=log(date_1[,25])
lRER_CPI_1=log(date_1[,24])
#combine another excel file countrycode that I scraped in previos PS
CountryCode<-read_xls("CountryCode.xls")
merged.data<-merge(CountryCode, date_1, by="CountryName")


#combine another excel yearlydata
Yearly_data<-read_xls("RER_DATA_yearly.xls")
Yearly_data<-Yearly_data[-c(1:10),]

#combine another set of RER data
#RER<-read_xls("Dr.Demir_RER_Turkey.xls")
#RER<-data.frame(Month=c(01,02,03,04,05,06,07,08,09),as.character)
#RER <- data.frame %>% mutate_all(Month(replace_(01,02,03,04,05,06,07,08,09,10,11,12)))
#final.merged<-merge(merged.data, RER, by=c("Year","Month"="Month"))

#Calculate Growthrate
library(tidyverse)
library(dplyr)
  # first sort by year
  Yearly_data<- . %>% 
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth =lRER_CPI_2- lag(lRER_CPI_2), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/ lag(lRER_CPI_2)* 100)
  
#Graph#1
ggplot(data=merged.data, aes(x=Date...18, y=RER_CPI_2, group=1)) + geom_line(color="red")+ggtitle(label="Real exchange rate Turkey")+theme_minimal()

#Graph#2
ggplot(data=merged.data,aes(x=Date...18)) + geom_line(aes(y=RER),colour="Red",group=1)+geom_line(aes(y=RER_CPI_2*100),colour="blue",group=2)+
  scale_y_continuous(sec.axis = sec_axis(~./100,name = "RERbyme"))+xlab("date")+ggtitle(label="Real exchange rate Turkey")+scale_colour_manual(values=c("blue","red"))+labs(y="RER",x="Date",colour="Parameter")+theme(legend.position=c(0.8,0.9))

#Graph#3
ggplot(data=Yearly_data,aes(x=year)) + geom_line(aes(y=growthRER_me*100),color="red")+geom_line(aes(y=growthRER_adv*100),color="blue")+
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "growthRER_adv"))+xlab("date")+ggtitle(label="Real exchange rate Turkey")+theme_minimal()

               
                    