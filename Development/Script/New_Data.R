#preparation
library(dplyr)
library(lubridate)
library(rvest)
library(ranger)
library(httr)
library(rjson)

setwd('C:/Intercity/development')

#Merge Resnet data

#Resnet data
###ENTER LOGIN INFORMATION HERE
username <- 'AKLIVSK'
password <- 'bousy95vis'

###LOGIN TO RESNET
res <- POST("https://www.resnet.co.nz/live/api/login.r",
            query= list(ID = username, Key = password))

###GET API TOKEN FROM LOGIN RESPONSE
AuthToken <- fromJSON(content(res, "text"))$AuthToken
Session <- fromJSON(content(res, "text"))$Session

###SEND REQUEST WITH TODAY AS END DATE
res <- POST(paste0("https://www.resnet.co.nz/live/rpt/jsl_experiments.w"),
            query = list(
              SearchFromDate= format(Sys.Date()-1,"%d%m%Y"),
              SearchToDate = format(Sys.Date(),"%d%m%Y"),
              Experiment="ZeVLyYGPReKAP9wsI0D48A.",
              ShowSearchData="on",
              ShowResultData="on",
              ShowStats="on",
              OutputTo="csv",
              Session=Session,
              AuthToken=AuthToken))

###DIGEST REQUEST
resnet.data1 <- read.csv(text=content(res, "text"))

###LOGOUT
GET("https://www.resnet.co.nz/live/api/logout.r?Session=99999999999999")

#read past resnet data and combine
resnet.data <- read.csv('data/resnet.data.csv', na.strings = c("", "NA"),stringsAsFactors = FALSE)

resnet.data <- rbind(resnet.data, resnet.data1)

#cleaning resnet.data
resnet.data <- unique(resnet.data)
write.csv(resnet.data, 'data/resnet.data.csv',row.names = F)

resnet.data <- resnet.data %>% filter(Conversion =='Ticketed')
resnet.data <- resnet.data %>% arrange(Departs,Searched)



resnet.data <- resnet.data %>% select(Departs, Searched,Product,Num.Seats,Experiment)

resnet.data <- setNames(resnet.data, c("Travel.Date","Purchase.Date", "Service","booking_per_day","Experiment"))


price_key <- read.csv('data/price_key.csv', na.strings = c("", "NA"),stringsAsFactors = FALSE)

resnet.data <- resnet.data %>% left_join(price_key, by = c("Experiment" = "ExperimentVariant")) %>% select(-Experiment)

resnet.data$Fare <- ifelse(resnet.data$Service =='ICZMZM',137.93, ifelse(resnet.data$Service =='ICZMZG',167.23,158.35))

resnet.data$Price <- resnet.data$Fare * (1 + resnet.data$ExperimentValue)





resnet.data$Travel.Date <- dmy_hm(resnet.data$Travel.Date) 

resnet.data$Purchase.Date <- dmy_hms(resnet.data$Purchase.Date) 

resnet.data <- resnet.data %>% filter(Travel.Date > Purchase.Date)

resnet.data <- resnet.data %>% filter(Purchase.Date >ymd_hms('2018-06-15 15:12:00'))

resnet.data$Purchase.Date <- as.Date(resnet.data$Purchase.Date)

resnet.data<- resnet.data %>% filter(Service %in% c("ICZMZM", "ICZMZML",'ICZMZG' ,'ICZMZGL', 'ICZMZC', 'ICZMZCL')) 

resnet.data$Service <- gsub('L','',resnet.data$Service)

resnet.data <- resnet.data %>% group_by(Travel.Date, Purchase.Date, Service) %>% summarise(booking_per_day = sum(booking_per_day), 
                                                                                           Price = mean(Price) ) %>% as.data.frame()

write.csv(resnet.data, 'data/cleaned.resnet.data.csv',row.names = F)

