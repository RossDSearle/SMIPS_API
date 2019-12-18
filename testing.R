library(httr)
library(jsonlite)


startDate='2017-12-29'
startDate='2019-01-01'
startDate='2019-01-01'
endDate='2019-01-30'
longitude = 150
latitude = -25
product <- 'Openloop_Wetness_Index'

ts <- getSMIPSTimeSeries(product, startDate, endDate, longitude, latitude)

url <- paste0('http://esoil.io/thredds/dodsC/SMIPS/SMIPSv0.5.nc.ascii?',product,'%5B',startDayNum, ':', endDayNum ,'%5D%5B', rowNum,'%5D%5B', colNum, '%5D')
req <- GET(url)
stop_for_status(req)
d1 <-  content(req)



getSMIPSTimeSeries(product='Analysis_Wetness_Index', startDate='01-10-2018', endDate='25-11-2018', longitude=153.0219, latitude=-27.4374 )
getSMIPSTimeSeries(product='Openloop_Wetness_Index', startDate='01-10-2010', endDate=NULL, longitude=153.0219, latitude=-27.4374 )


url <- 'http://127.0.0.1:6214/SMIPS/TimeSeries?longitude=153.0219&latitude=-27.4374&sdate=13-06-2018&edate=14-06-2018'
bob <- GET(url)
stop_for_status(bob)
ts <-  fromJSON( content(bob))
ts

endDate <- '25-11-2015'
startDate <- '01-01-2010'





sdt='10-06-2018'
edt='16-06-2019'
vals <- getSMIPSTimeSeries(product='Analysis_Wetness_Index', startDate=sdt, endDate=edt, longitude=longitude, latitude=latitude )
vals

#getSMIPSTimeSeries(product='Analysis_Wetness_Index', startDate='20-11-2015', endDate='20-11-2015', longitude=longitude, latitude=latitude )

dt='15-06-2018'
r <- getSMIPSrasterCSIRO_OpenDAP(dt=dt)
writeRaster(r, paste0('c:/temp/', dt, '.tif'), overwrite=T)


dt='20-11-2015'
dt='14-06-2019'
bbox <- extent(140, 141, -20, -19)
r <- getSMIPSRasterWindow(product='Openloop_Wetness_Index',  dt=dt, bboxExt=bbox, outcols = 600, outrows = 400)
r <- getSMIPSRasterWindow(product='Openloop_Wetness_Index',  dt=dt, bboxExt=NULL, outcols = 600, outrows = 400)
r
plot(r)
writeRaster(r, paste0('c:/temp/smips/', dt, '_', 'Openloop_Wetness_Index.tif' ))
plot(r)

longitude=153.0219
latitude=-27.4374
endDate <- dt
startDate <- dt

getSMIPSTimeSeries(product='Openloop_Wetness_Index', startDate=dt, endDate=dt, longitude=153.0219, latitude=-27.4374 )
getSMIPSTimeSeries(product='Openloop_Wetness_Index', startDate=dt, endDate=dt, longitude=141.0219, latitude=-20.4374 )





getSMIPSTimeSeries(product='Openloop_Wetness_Index', startDate=dt, endDate=dt, longitude=153.0219, latitude=-27.4374 )


r <- getSMIPSrasterCSIRO_OpenDAP(dt='20-11-2015')
plot(r)
writeRaster(r, 'c:/temp/0.tif')

library(raster)

xyFromCell()

url <- 'http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc.ascii?Openloop_Wetness_Index%5B1481%5D%5B0:1:3473%5D%5B0:1:4109%5D'
bob <- GET(url)
stop_for_status(bob)
ts <-  content(bob)
cat(ts,file='c:/temp/ar.txt',sep="\n")


cell <- cellFromXY(r, cbind(c(112.905), c(-9.005)))



getSMIPSrasterCSIRO_OpenDAP(product='Openloop_Wetness_Index', dt=dt, resFactor=10)

