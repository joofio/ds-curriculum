library(zoo)
library(dplyr)
library(prophet)

ndf <- read.csv("data_ts.csv",row.names = 1)
ndf$id<-NULL
ndf$time<-as.POSIXlt(as.character(ndf$MSH_7),format = "%Y%m%d%H%M%S")


ndf$MSH_7<-as.character(ndf$MSH_7)
ndf$MSH_10<-as.character(ndf$MSH_10)
ndf<-ndf[-duplicated(ndf)]
ndf$count<-1



ndf$by1 = cut(ndf$time, breaks="min")
ts <- seq.POSIXt(as.POSIXlt("2019-12-01 0:00"), as.POSIXlt("2020-12-03 0:00"), by="min")
df <- data.frame(time=ts)


full_data <- full_join(df,ndf)
full_data[["count"]][is.na(full_data[["count"]])] <- 0

### by 15 min
full_data$by15<-cut(full_data$time, breaks="15 min")


full_data_by15 = aggregate(count ~ by15, FUN=sum, data=full_data)
full_data_by15

#plot(full_data_by15)

names(full_data_by15)[1]<-"ds"
names(full_data_by15)[2]<-"y"


m <- prophet(full_data_by15,fit = F)
add_country_holidays(m,"Portugal")
m<-fit.prophet(m,full_data_by15)

future <- make_future_dataframe(m, periods = 3000, freq = 60 *15,include_history = F)
forecast <- predict(m, future)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)



test<-as.data.frame(c(as.POSIXct("2021-12-04 12:30:00")))
names(test)<-"ds"
test
predict(m,test)



###by hour

### by 15 min
full_data$by1h<-cut(full_data$time, breaks="1 hour")


write.csv(full_data_by15,"full_data_by15.csv")
full_data_by1h = aggregate(count ~ by1h, FUN=sum, data=full_data)
full_data_by1h

#plot(full_data_by15)

names(full_data_by1h)[1]<-"ds"
names(full_data_by1h)[2]<-"y"


m <- prophet(full_data_by1h,fit = F)
add_country_holidays(m,"Portugal")
m<-fit.prophet(m,full_data_by1h)

future <- make_future_dataframe(m, periods = 3000, freq = 60*60 ,include_history = F)
future
forecast <- predict(m, future)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)



test<-as.data.frame(c(as.POSIXct("2021-12-04 12:30:00")))
names(test)<-"ds"
test
predict(m,test)
