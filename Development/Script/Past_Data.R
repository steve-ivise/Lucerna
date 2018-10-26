library(dplyr)
library(lubridate)

#retrieve data
df <- rbind(read.csv('C:/intercity/data/Milford Booking Data (2017).csv', na.strings = c("", "NA"),stringsAsFactors = FALSE)
            ,read.csv('C:/intercity/data/Milford Booking Data (beginning of 2018).csv', na.strings = c("", "NA"),stringsAsFactors = FALSE),
            read.csv('C:/intercity/data/Milford Booking Data (mid 2018).csv', na.strings = c("", "NA"),stringsAsFactors = FALSE))

#data cleaning
df <- unique(df)

df$Travel.Date.Time <- dmy_hm(df$Travel.Date.Time) 

df$Booking.Date.Time <- dmy_hm(df$Booking.Date.Time) 

df <- df %>% filter(Travel.Date.Time > Booking.Date.Time)

df <- df %>%  arrange(Travel.Date.Time, Booking.Date.Time)


df <- df %>% filter(Parent.Product.Code %in% c("ICZMZM", "ICZMZML",'ICZMZG' ,'ICZMZGL', 'ICZMZC', 'ICZMZCL')) 

df <- df %>% filter(Booking.Agent.Sales.Channel == 'ICG Web Sites')



df$Booking.Date <- as.Date(df$Booking.Date.Time)
df$Travel.Date <- as.Date(df$Travel.Date.Time)

df$unique.id <- paste(df$Travel.Date, df$Ticketing.Date.Time)

df$Parent.Product.Code <- as.factor(df$Parent.Product.Code)


df <- df[!grepl("Lunch", df$Product.Description),]
df <- df[!grepl("lunch", df$Product.Description),]

df$Parent.Product.Code <- substr(df$Parent.Product.Code,1,6)

#grouping by person
calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


df <- df %>% group_by(unique.id) %>% summarise(Travel.Date.Time = min(Travel.Date.Time), Passenger.Quantity = mean(Passenger.Quantity), 
                                               Booking.Date.Time = mean(Booking.Date.Time,na.rm=TRUE), Parent.Product.Code = calculate_mode(Parent.Product.Code),
                                               n = n())

df <- df %>% filter(n ==3)

df$n <- NULL
df$unique.id <- NULL

df <- df[complete.cases(df), ]

#add fare
df$Fare <- ifelse(df$Parent.Product.Code =='ICZMZM',137.93, ifelse(df$Parent.Product.Code =='ICZMZG',167.23,158.35))

#save
colnames(df) <- c('Travel.Date','Quantity','Purchase.Date','Service','Price') 

df <- df %>% group_by(Travel.Date, Purchase.Date, Service) %>% summarise(booking_per_day = sum(Quantity), 
                                                                         Price = mean(Price) ) %>% as.data.frame()

df$Purchase.Date <- as.Date(df$Purchase.Date)


write.csv(df, 'C:/Intercity/development/data/cleaned_df.csv',row.names = F)


