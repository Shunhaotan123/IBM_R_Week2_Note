# IBM_R_Week2_Note
##IBM##

airline = read.csv(choose.files(), header = TRUE)
summary(airline)
head(airline)
nrow(airline)
ncol(airline)

##############

install.packages("tidyverse")
library(tidyverse)
head(airline)
tail(airline)
write_csv(airline, "airline_2m.csv") ## save any changes 

class(airline)
mode(airline)
glimpse(airline)

glimpse(airline)

meandelay = airline%>%
  group_by(Reporting_Airline)%>%
  summarize(ave_delay = mean(ArrDelay,na.rm=TRUE))

##################
###Week2##########
###Data Pre-processing##
##data_cleaning###

##first##
##missing_Values##

replaceNa%>%
  summarize(count = sum(is.na(CarrierDelay)))

airline%>%
  map(~sum(is.na(.))) ## count missing values in all columns. 

airline%>%
  summarize(count = sum(is.na(CarrierDelay)))
carrier_delays = airline%>%
  drop_na(CarrierDelay)

dim(airline)
dim(carrier_delays)

##Replace

replaceNa = airline%>%
  replace_na(list(CarrierDelay = 0,
                  WeatherDelay = 0,
                  NASDelay = 0,
                  SecurityDelay = 0,
                  LateAircraftDelay = 0))
replaceNa

##Data_Formatting


data_airline = airline%>%
  separate(FlightDate,sep="-",
           into = c("year","month","day"))
data_airline$FlightDate
airline$FlightDate
airline
data_airline
head(data_airline)
sapply(airline,typeof)

data_airline%>%
  select(year,month,day)%>%
  mutate_all(type.convert)%>%
  mutate_if(is.character, as.numeric)

##data_normalization##
head(airline$ArrDelay)
simple_scale = airline$ArrDelay/max(airline$ArrDelay)
head(simple_scale)

##binning_in_R##
##groups a set of numerical values into a set of bins, 
##it helps us to grand a better understanding of our data.

library(tidyverse)

x = c(1,1,3,5,7,8,10,11,11,15)
binning = airline%>%
  mutate(quantile_rank = ntile(airline$ArrDelay,4))


##categorical_to_numeric

transfer = airline%>%
  mutate(dummy=1)%>% ##column with single value
  spread(key = Reporting_Airline, ##column to spread
         value = dummy, fill = 0)%>% ##value_the value we want to set the key to; fill_ fill missing value with 0
  slice(1:5)


