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


dt='01-01-2016'

p <- getSMIPSRasterWCS(product, dt, bbox = '140,-26,142,-24')
r <- raster(p)
plot(r)

