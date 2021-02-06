library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(funModeling) 
library(Hmisc)
library(lubridate)
install.packages("xlsx")

library(xlsx)

DDevents_table <- read_excel("Data_Dictionary.xlsx", sheet = "Events Table")
DDInstalls_Table <- read_excel("Data_Dictionary.xlsx", sheet = "Installs Table")
a <- read.csv("install_table.csv")
b <- read_excel("events_Table.xlsx", sheet = "Sheet1")

summaryData <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

#change geo_country var name to geo_country_code
#upcase viewer_id
#missing to NA
#TIME FORMAT
#maybe remove timestamp for now as seconds vary slightly 
describe(b$attribution_network_id)
summary(b$attribution_network_id)



b <-b %>% rename('geo_country_code'='geo_country')
#change blanks to NA
b <-mutate_all(b, list(~na_if(.,"")))
a <- mutate_all(a, list(~na_if(.,"")))

a$viewer_id <- toupper(a$viewer_id)
b$viewer_id <- toupper(b$viewer_id)
a$cross_app_id <- toupper(a$cross_app_id)
b$cross_app_id <- toupper(b$cross_app_id)


#remove commas
a$install_id <- as.numeric(gsub(",","",a$install_id))

#change date format
b$received_date <- as.Date(b$received_date,origin = "1899-12-30")

b$timestamp <- as.Date(b$timestamp,origin = "1899-12-30")
#b$timestamp <-as_datetime(b$timestamp)
#b$received_date <-as_datetime(b$received_date)

#a <- a %>% select(-install_date_utc )
#b <- b %>% select(-timestamp)

#change factor to chr
#a <- data.frame(lapply(a, as.character), stringsAsFactors=FALSE)
#b <- data.frame(lapply(b, as.character), stringsAsFactors=FALSE)


#check common variables
#intersect(names(a), names(b))

MERGEDS <- merge(a, b, by=c('platform', 'viewer_id', 'cross_app_id', 'install_id', 'geo_city', 'geo_region', 'geo_country_code', 'attribution_network_id', 'received_date'), all.x=TRUE, all.y=TRUE)

#QUICK check if dataset correctly merged 
c3<- MERGEDS %>% filter(cross_app_id=='00163DE3-46EE-9B22-A19829613DC2')




#missing values
sum(colMeans(is.na(c2))>0.2)


#2.	Use the data sets to determine which attribution method (attribution_network_id) 
#(independent and caterogical)
#was most successful at driving app installations and ad views.*dependent
#answer is NA!! 
q2 <- MERGEDS %>% select(attribution_network_id, event_name, viewer_id,install_id, install_date_utc)
#q2<-q2 %>% unique() don't find unique as they can have watched multiple ads
freq(q2)  # Analyzing categorical variables
plot_num(q2)  # Analyzing numerical variables

summaryData(q2)

#showing only ad views 
q2 <- q2  %>% filter(event_name=="watched-ad") %>% unique()
freq(q2) 
plot_num(q2)

#missing data isn't missing, it could be due user downloadinf straight from app store thus 
#won't have an attribution_network_ID


#checks number of dupes - 0
sum(table(q2$viewer_id)-1)
#bear in mind that multiple attribution network id can cause 1 download of the app


#download for tableau

write.table(q2, file="q2withadviewsonly.csv",sep=",",row.names=F)


#q3
#3.	Is there a group of app users you can identify that you feel is most valuable to Lulu123? If so, why?

#need cross_app_id	string	ID consistent across other Lulu123 apps
q3 <- cmm %>% select(platform,cross_app_id, device_model, attribution_network_id, event_name,install_id, install_date_utc)

sum(table(q3$viewer_id)-1) #98126 dupes

sum(table(q3$cross_app_id)-1) # 83374 dupes
freq(q3$device_model) 
plot_num(q3$device_model)
freq(q3$platform) 
plot_num(q3$platform)

#q4 	Any other interesting stats or trends that you can ascertain from the data? 
q4 <- cmm %>% select(platform,cross_app_id, event_name, install_date_utc, geo_city, geo_region,geo_country_code, timestamp, received_date)
freq(q4)
plot_num(q4)

profiling_num(q4)


xa<- a %>% filter(viewer_id=='KIFFF89F1C1E634D32B8ADDEF499F419D3')
xb<- b %>% filter(viewer_id=='KIFFF89F1C1E634D32B8ADDEF499F419D3')




summaryData <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}
summaryData(c2)
summaryData(b)





#remove time vars

#check the list of vars
print(ls(c2))





c3<-c2 %>% select(-timestamp, -install_date_utc, -received_date.y, -received_date.x)








summaryData(a)
summaryData(b)

glimpse(b)
df_status(b)
freq(b) 
profiling_num(b)
plot_num(b)
describe(b)






d<-merge(a, b, )
duplicated(c)
c4<-unique(c2)
test[b] <- lapply(cols, FUN=function(b) as.numeric(as.character(test[[b]])))
