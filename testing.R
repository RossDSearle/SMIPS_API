library(httr)
library(jsonlite)


startDate='2017-12-29'
startDate='2019-01-01'
startDate='2019-01-01'
endDate='2019-01-30'
longitude = 150
latitude = -25
product <- 'Openloop_Wetness_Index'

ts <- getTimeSeriesCSIRO(product, startDate, endDate, longitude, latitude)

url <- paste0('http://esoil.io/thredds/dodsC/SMIPS/SMIPSv0.5.nc.ascii?',product,'%5B',startDayNum, ':', endDayNum ,'%5D%5B', rowNum,'%5D%5B', colNum, '%5D')
req <- GET(url)
stop_for_status(req)
d1 <-  content(req)



getSMIPSTimeSeries(product='Openloop_Wetness_Index', startDate='04-01-2019', endDate='07-01-2019', longitude=153.0219, latitude=-27.4374 )


url <- 'http://127.0.0.1:5355/SMIPS/TimeSeries?longitude=153.0219&latitude=-27.4374&sdate=13-06-2018&edate=14-06-2018'
bob <- GET(url)
stop_for_status(bob)
ts <-  content(bob)
ts