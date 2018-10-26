#preparation
library(dplyr)
library(lubridate)
library(rvest)
setwd('C:/Intercity/development')

#Holiday
holidays <- read.csv('data/holidays.csv', na.strings = c("", "NA"),stringsAsFactors = FALSE)
holidays$date <- as.Date(holidays$date)

url <- paste0('https://publicholiday.co.nz/nz-public-holidays-',year(Sys.Date()),'.html')

webpage <- read_html(url)

holidays1 <- webpage %>% html_nodes(xpath = '//*[@id="holidaylist"]') %>% html_table() 
holidays1 <- holidays1[[1]]
holidays1$year <- year(Sys.Date())

#data cleaning
holidays1 <- holidays1[grepl('New Year|Auckland|Waitangi|Good Friday|Easter Monday|Anzac Day|Queen|Labour|Christmas|Boxing',holidays1$Holiday),]
holidays1$Date <- gsub(',','', holidays1$Date)

date_split <- strsplit(holidays1$Date, " ")
holidays1$date <- Sys.Date()

for (i in 1: nrow(holidays1)){
  date <- paste(as.numeric(gsub("([0-9]+).*$", "\\1", date_split[[i]][3])) ,date_split[[i]][1],holidays1$year[i])
  
  holidays1$date[i] <-  as.Date(parse_date_time(date, orders = 'dmy'))
}

holidays1$public_holiday <- 'Yes'

holidays1 <- holidays1[,5:6]

holidays <- rbind(holidays , holidays1)
holidays <- unique(holidays)

holiday <- data.frame(date =  seq(min(holidays$date), max(holidays$date), "days") )
holidays <- merge(x = holiday, y = holidays, by = "date", all.x = TRUE)

holidays$public_holiday[is.na(holidays$public_holiday)] <- 'No'

write.csv(holidays,'data/holidays.csv',row.names = F)





#school
school <- read.csv('data/school.csv', na.strings = c("", "NA"),stringsAsFactors = FALSE)
school$date <- as.Date(school$date)


url <- 'https://publicholiday.co.nz/nz-school-holidays.html'

webpage <- read_html(url)

school1 <- webpage %>% html_nodes(xpath = '//*[@id="holidaylist"]') %>% html_table()   

school1 <- school1[[1]]

year <- as.numeric(colnames(school1)[1])

school1 <- setNames(school1, c("category","starts","ends"))

school1 <- school1[grepl('School Term',school1$category),]

school1$starts <- gsub('Between ','', school1$starts) 
school1$year <- c(year,year,year,year,year+1,year+1,year+1,year+1)

school1$start <- Sys.Date()
school1$end <- Sys.Date()

date_split <- strsplit(school1$starts, " ")

for (i in 1: nrow(school1)){
  date <- paste(as.numeric(gsub("([0-9]+).*$", "\\1", date_split[[i]][2])) ,  date_split[[i]][3]  ,school1$year[i])
  
  school1$start[i] <-  as.Date(parse_date_time(date, orders = 'dmy'))
}

date_split <- strsplit(school1$ends, " ")

for (i in 1: nrow(school1)){
  date <- paste(as.numeric(gsub("([0-9]+).*$", "\\1", date_split[[i]][2])) ,  date_split[[i]][3]  ,school1$year[i])
  
  school1$end[i] <-  as.Date(parse_date_time(date, orders = 'dmy'))
}

for (i in 1: nrow(school1)){
  school2 <- data.frame(date = seq(school1$start[i],school1$end[i], by = "days"), school_holiday = 'No')
  school <- rbind(school, school2)
  
}
school <- unique(school)


school1 <- data.frame(date =  seq(min(school$date), max(school$date), "days") )
school <- merge(x = school1, y = school, by = "date", all.x = TRUE)

school$school_holiday[is.na(school$school_holiday)] <- 'Yes'

write.csv(school,'data/school.csv',row.names = F)
