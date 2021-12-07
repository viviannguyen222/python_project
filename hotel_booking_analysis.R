setwd("D:\\analysis-datasets\\hotel_booking_demand_dataset")
hotel_booking_demand<- read.csv("hotel_bookings.csv")
dim(hotel_booking_demand)
names(hotel_booking_demand)
class(hotel_booking_demand$adults)
#create a new column, total number of toursits in one row

#fill na values with 0
hotel_booking_demand$children[is.na(hotel_booking_demand$children)]<-0
hotel_booking_demand$adults[is.na(hotel_booking_demand$adults)]<-0
hotel_booking_demand$babies[is.na(hotel_booking_demand$babies)]<-0
hotel_booking_demand$nb_tourists<- hotel_booking_demand$adults+hotel_booking_demand$children+hotel_booking_demand$babies
any(is.na(hotel_booking_demand$nb_tourists))#false, it is ok
library('ggplot2')
range(hotel_booking_demand$nb_tourists)
ggplot(hotel_booking_demand)+geom_histogram(aes(x= nb_tourists),bins=30,fill='blue')
count_nb_tourists<-table(hotel_booking_demand$nb_tourists)

type_hotel_by_guest<-as.data.frame(table(hotel_booking_demand$is_repeated_guest,hotel_booking_demand$hotel))
library('reshape2')
#return long form to wide form
type_hotel_by_guest<-dcast(type_hotel_by_guest,Var1~ Var2)
total_cityhotel<- sum(type_hotel_by_guest$`City Hotel`)
total_resorthotel<- sum(type_hotel_by_guest$`Resort Hotel`)
a<- type_hotel_by_guest[1,2]/total_cityhotel*100
b<- type_hotel_by_guest[2,2]/total_cityhotel*100
c<- type_hotel_by_guest[1,3]/total_resorthotel*100
d<- type_hotel_by_guest[2,3]/total_resorthotel*100
#rate new guest by type of hotel
rate_new_guest<- data.frame(Type=c('New','Old'),city_hotel= c(a,b),resort_hotel=c(c,d))
write.csv(rate_new_guest,file='rate_type_guest_by_hotel.csv')

#combine time to date
#use 3 column to combine arrival_date_day_of_month,arrival_date_month,arrival_date_year
hotel_booking_demand$date<- paste(hotel_booking_demand$arrival_date_day_of_month,hotel_booking_demand$arrival_date_month,hotel_booking_demand$arrival_date_year,sep='/')
head(hotel_booking_demand$date)
hotel_booking_demand$date<- strptime(hotel_booking_demand$date,"%d/%B/%Y")
hotel_booking_demand$date<- as.Date(hotel_booking_demand$date)
#total guest by day
library('dplyr')
total_guest_by_dates<- hotel_booking_demand%>%group_by(date)%>%summarise(total_guest= sum(nb_tourists))
write.csv(total_guest_by_dates,file='total_guest_by_dates.csv')

#total guest for type of room
total_guest_by_type_of_room<- hotel_booking_demand%>% group_by(assigned_room_type)%>% summarise(total_guests= sum(nb_tourists))%>%arrange(desc(total_guests))
total_guest_by_type_of_room<-rbind(total_guest_by_type_of_room,data.frame(assigned_room_type='Others',total_guests= sum(total_guest_by_type_of_room[6:12,2])))
total_guest_by_type_of_room<- total_guest_by_type_of_room[-c(6,7,8,9,10,11,12),]

ggplot(total_guest_by_type_of_room)+geom_bar(aes(x=assigned_room_type,y= total_guests),stat = 'identity')
hotel_booking_demand$is_repeated_guest<- as.factor(hotel_booking_demand$is_repeated_guest)
ggplot(hotel_booking_demand,aes(assigned_room_type,nb_tourists))+geom_bar(stat = 'identity')
ggplot(hotel_booking_demand,aes(assigned_room_type,nb_tourists,fill=is_repeated_guest))+geom_bar(stat = 'identity')

write.csv(hotel_booking_demand,file='new_hotel_booking.csv')

#read new dataset
hotel_booking_demand<- read.csv("new_hotel_booking.csv")
tourist_by_day<- read.csv("total_guest_by_dates.csv")
#install.packages('forecast')
library(forecast)
head(tourist_by_day)
#remove abundunt column
tourist_by_day[,1]<-NULL
tourist_by_day<-tourist_by_day[order(tourist_by_day$date),]
tourist_by_day$date<- as.character(tourist_by_day$date)
tourist_by_day$date<- strptime(tourist_by_day$date,"%Y-%m-%d")
library(lubridate)#to use year and month func
library(tidyr)#to use seperate func
new_tourist_by_time<-separate(tourist_by_day,col='date',into= c('year','month','day'))
new_tourist_by_time$date<- paste(new_tourist_by_time$month,new_tourist_by_time$year,sep='-')
new_tourist_by_time$year<-NULL
new_tourist_by_time$month<-NULL
new_tourist_by_time$day<-NULL
new_tourist_by_time$date<- as.factor(new_tourist_by_time$date)
library(dplyr)
total_tourist_by_month<- new_tourist_by_time%>% group_by(date)%>% summarise(tourists= sum(total_guest))
total_tourist_by_month$date<- as.character(total_tourist_by_month$date)
total_tourist_by_month$date<- as.Date(paste("01-",total_tourist_by_month$date,sep=''),format='%d-%m-%Y')
total_tourist_by_month<-total_tourist_by_month[order(total_tourist_by_month$date),]
#ts_tourist_day<- ts(tourist_by_day$total_guest,start = tourist_by_day$date[1],end = tourist_by_day$date[length(tourist_by_day$date)]) 
#ts_tourist_day<- ts(tourist_by_day$total_guest,tourist_by_day$date) 
plot(total_tourist_by_month$date,total_tourist_by_month$tourists)
library('ggplot2')
total_tourist_by_month$date<-as.Date(total_tourist_by_month$date)
ggplot(total_tourist_by_month,aes(as.POSIXct(date),tourists))+geom_line()+scale_x_datetime(date_labels = "%Y-%m",date_breaks = '1 month')+xlab('Time')+theme(axis.text.x = element_text(angle = 90,vjust=1))

#create time series base on data
ts_total_tourist<- ts(total_tourist_by_month$tourists,frequency = 12,start = c(2015,7))
plot(ts_total_tourist)

#holt linear trend model to forcast time series
library(forecast)
holttrend= holt(ts_total_tourist,h=6)#forcast next 5 month
plot(holttrend)
#damped holt's method
plot(holt(ts_total_tourist,h=6,damped = TRUE, phi = 0.85))
#auto arima
autoarima_model<- auto.arima(ts_total_tourist)
plot(forecast(autoarima_model,h=6))

#compare all model
holttrend <- holt(ts_total_tourist, h = 6)
holtdamped <- holt(ts_total_tourist, h = 6, damped = TRUE)
arimafore <- forecast(auto.arima(ts_total_tourist), h = 6)

library(ggplot2)
autoplot(ts_total_tourist) +
  forecast::autolayer(holttrend$mean, series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$mean, series = "Holt Damped Trend") +
  forecast::autolayer(arimafore$mean, series = "ARIMA") +
  xlab("Time") + ylab("Total_tourists") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("hotel_bookings") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "blue", face="bold", size=15))

#seasonal decomposition
plot(stl(ts_total_tourist, s.window = 7))
#seasonal arima model
ts_arima <- auto.arima(ts_total_tourist, stepwise = TRUE,approximation = FALSE, trace = TRUE)
plot(forecast(ts_arima,h=6))
#best arima model is arima(0,1,0)(0,1,0)[12]

