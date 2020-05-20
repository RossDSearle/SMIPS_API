library(httr)
library(jsonlite)



startDate='27-01-2019'
endDate='30-01-2019'
longitude = 150
latitude = -25
product <- 'CSIRO - SMIPS - Blended Rainfall Estimate'
product <- "CSIRO - SMIPS Simple Volumetric Soil Moisture (mm)"    
product <- "CSIRO - SMIPS Fusion Soil Wetness Index (0-1 unitless)" 
product <- "CSIRO - SMIPS Fusion Volumetric Soil Moisture (mm)"     
product <- "ESA - SMOS - Soil Wetness Index (0-1 unitless)"        
product <- "ANU - Soil Moisture Annual Products"                   

ts <- getSMIPSTimeSeries(product, startDate, endDate, longitude, latitude)
ts


url <- paste0('http://esoil.io/thredds/dodsC/SMIPS/SMIPSv0.5.nc.ascii?','Blended_Precipitation','%5B',startDayNum, ':', endDayNum ,'%5D%5B', rowNum,'%5D%5B', colNum, '%5D')
req <- GET(url)
stop_for_status(req)
d1 <-  content(req)
d1




url <- paste0('http://esoil.io/thredds/wcs/SMIPSall/SMIPSv0.5.nc?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&FORMAT=GeoTIFF_Float&COVERAGE=Analysis_Wetness_Index&CRS=OGC:CRS84&TIME=2016-01-01T00:00:00Z')
outFile <- 'c:/temp/t.tif'
download.file(url, outFile, mode = 'wb', quiet = T)


dt='2016-01-01'

r <- getSMIPSRasterWCS(product, dt, bbox = '140,-26,142,-24')

r <- getSMIPSRaster(dt='16-06-2019', resFactor = 50)
r
plot(r)
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

