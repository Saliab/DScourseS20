#Q4
#import data
library(RCurl)
data <- getURL("https://raw.githubusercontent.com/Saliab/DScourseS20/master/ModelingOptimization/wages.csv")
wage <- read.csv(text = data)

#or

urlfile<- "https://raw.githubusercontent.com/Saliab/DScourseS20/master/ModelingOptimization/wages.csv"
wage_data<-read.csv(urlfile)

#Q5
#drop observation hgc or tenure is missing
library(tidyverse)
subset_wage<- wage[, c("hgc", "tenure")] 
#Missing
is.na(subset_wage)
which(is.na(subset_wage))
#to drop all rows with missing values
na.omit(subset_wage)
#or keep
nomissing<-complete.cases(subset_wage)
#to get complete cases
comdata<-wage[complete.cases(subset_wage), ]
#to get incompelet cases
wage[!complete.cases(subset_wage), ]

#full data,by dropping missing values for hgc and tenure
full_data<- wage[complete.cases(subset_wage), ]


#Q6
library(stargazer)
stargazer(full_data)
summary(full_data)

#Missing logwages
sum(is.na(wage$logwage))


#Q7
#estimate regression using only complete cases
df<- complete.cases(wage)
regression<-lm(logwage ~ hgc + college + poly(tenure, 2) + age + married, data=df)
regression
#or
Data<- na.omit(full_data, cols="logwage")
regression<-lm(logwage ~ hgc + college + poly(tenure, 2) + age + married, data=Data)
regression

#replace missing log wage with mean 
full_data$tenure2<-(full_data$tenure)^2
mean_missing <- mutate(full_data, logwage = ifelse(is.na(logwage), mean(logwage, na.rm=TRUE), logwage))
regression_2<-lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data=mean_missing)
regression_2

#Impute missing log wages as their predicted values
wage$tenure2<-(wage$tenure)^2
predict_data<-wage
predict_data$logwage[is.na(predict_data$logwage)] <- predict(regression, wage[is.na(predict_data$logwage),])
regression_3 <-lm(logwage ~ hgc + college + tenure+tenure2 + age + married, data=predict_data)
regression_3

stargazer(regression,regression_2, regression_3, title="Results", align=TRUE)

#mice 
library(mice)
data("wage_data")
head(wage_data)
wage_data.imp=mice(wage_data, seed=12345)
summary(wage_data.imp)
fit=with(wage_data.imp, lm(logwage ~ hgc + college + tenure + poly(tenure, 2) + age + married))
round(summary(pool(fit)),2)





