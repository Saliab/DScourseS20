rm(list=ls())
library(sampleSelection)
library(tidyverse)
library(stargazer)

#Question4
urlfile<-'https://raw.githubusercontent.com/tyleransom/DScourseS20/master/Structural/wages12.csv'
data<- read.csv(urlfile)

#Question5
str(data)
data$college<-as.factor(data$college)
data$married<-as.factor(data$married)
data$union<-as.factor(data$union)
str(data)

#Question6
stargazer(data)
sum(is.na(data$logwage))

#Question7
# Drop missing hgc and missing exper
data <-data[!is.na(data$hgc) & !is.na(data$exper),]
df<- complete.cases(data)
regression<- lm(logwage ~ hgc + union +college+exper+I(exper^2), data=data, na.action=na.omit)

print(summary(regression))

# Mean imputation
data$logwage_mean_imp <- data$logwage
wagebar <- mean(data$logwage,na.rm=T)
data$logwage_mean_imp[is.na(data$logwage_mean_imp)] <- wagebar

reg.mean.imp <- lm(logwage_mean_imp ~ hgc + union+ college + exper + I(exper^2), data=data, na.action=na.omit)

print(summary(reg.mean.imp))

#sample selection
data <- data %>%
  mutate(
    valid = as.numeric(
      case_when(
        logwage == 0 ~ "0",
        is.na(logwage) ~ "0",
        logwage > 0 ~ "1",
        logwage < 0 ~ "1",
        TRUE ~ as.character(logwage)
      )
    )
  )



library(dplyr)
data <- data%>%
  mutate(logwage = if_else(is.na(logwage), 0, logwage))


Heckit1=heckit(selection=valid ~ hgc + union + college + exper + married + kids, 
          outcome=logwage ~ hgc + union + college + exper + I(exper^2), 
          data=data, method="2step")



stargazer(regression,reg.mean.imp,Heckit1)


#Question8
probit<- glm(union~ hgc+college+exper+married+kids,
             family = binomial(link = "probit"), data = data)
print(summary(probit))

#Question9
data$predProbit <- predict(probit, newdata = data, type = "response")
print(summary(data$predProbit))

probit$coefficients["kids"] <- 0*probit$coefficients["kids"]
probit$coefficients["married"] <- 0*probit$coefficients["married"]
data$predLogitCfl <- predict(probit, newdata = data, type = "response")
print(summary(data$predLogitCfl))







