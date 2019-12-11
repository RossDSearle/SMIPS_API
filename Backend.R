library(raster)
library(httr)
library(jsonlite)

# Thredds Cataloue
#http://esoil.io/thredds/catalog/SMIPSall/catalog.html?dataset=SMIPS/SMIPSv0.5.nc


RepoPath <- 'http://esoil.io/thredds/dodsC/SMIPS/SMIPSv0.5.nc'

supportedProducts <- c('Openloop_Wetness_Index', 'Analysis_Wetness_Index')

originDay = 42326
originDate <- '20-11-2015'
Ausminx <- 112.905
Ausminy <-  -43.735
Ausmaxx <- 154.005
Ausmaxy <- -9.005
AusRes <- 0.01
Ausnumrows <- 3474
Ausnumcols <- 4110




# startDate='2017-12-29'
# startDate='2019-01-01'
# endDate='2019-01-30'
# longitude = 150
# latitude = -25
# 
# ts <- getTimeSeriesCSIRO(product, startDate, endDate, longitude, latitude)

getSMIPSTimeSeries <- function(product, startDate, endDate, longitude, latitude){
  
  check_getSMIPSTimeSeries(product=product, startDate=startDate, endDate=endDate, longitude=longitude, latitude=latitude)
  
  if(is.null(product)){
    product = 'Analysis_Wetness_Index'
  }
  
  r <- raster(nrows=Ausnumrows, ncols=Ausnumcols, xmn=minx, xmx=maxx, ymn=miny, ymx=maxy, crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
  cell <- cellFromXY(r, cbind(c(longitude), c(latitude)))
  colNum <- colFromCell(r, cell)
  rowNum <- rowFromCell(r, cell)
  print(colNum)
  
  startDayNum = as.numeric(as.Date(paste(startDate), "%d-%m-%Y") - as.Date(paste(originDate), "%d-%m-%Y"))
  endDayNum = as.numeric(as.Date(paste(endDate), "%d-%m-%Y") - as.Date(paste(originDate), "%d-%m-%Y"))
  
  url <- paste0('http://esoil.io/thredds/dodsC/SMIPS/SMIPSv0.5.nc.ascii?',product,'%5B',startDayNum, ':', endDayNum ,'%5D%5B', rowNum,'%5D%5B', colNum, '%5D')
  req <- GET(url)
  stop_for_status(req)
  d1 <-  content(req, 'text')
  
  ndays <- (endDayNum-startDayNum) + 1
  dts <- seq.Date(from=as.Date(paste(startDate), "%d-%m-%Y"), to=as.Date(paste(endDate), "%d-%m-%Y"), by='days')
  pdts <- paste0(dts, 'T00:00:00')
  
  ts1 <- read.table(text=d1, skip=12, nrows = ndays , sep = ',')
  ts2 <- ts1[,-1]
  
  tsdf <- data.frame(t=pdts, v=ts2, stringsAsFactors = F)
  #colnames(tsdf) <- c('t', 'v')
  return(tsdf)
}






check_getSMIPSTimeSeries <- function(product=NULL, startDate=NULL, endDate=NULL, longitude=NULL, latitude=NULL){
  
  if(!is.null(product)){
  if(!product %in% supportedProducts){
    stop(paste0("The specified 'product'", product, " is not supported by the API" ))
  }
  }
  
  if(!is.null(startDate)){
    dt <- strptime(startDate, "%d-%m-%Y")
    if(is.na(dt)){stop(paste0("The specified 'startdate' is not valid or in the correct format. It needs to be in the format %d-%m-%Y eg 20-04-2018"))}
  }

  if(!is.null(endDate)){
    dt <- strptime(endDate, "%d-%m-%Y")
    if(is.na(dt)){stop(paste0("The specified 'enddate' is not valid or in the correct format. It needs to be in the format %d-%m-%Y eg 27-04-2018"))}
  }
  
  if(!is.null(startDate) & !is.null(endDate)){
    d1 <- strptime(startDate, "%d-%m-%Y")
    d2 <- strptime(endDate, "%d-%m-%Y")
    dif <- d2 - d1
    if(dif<0){stop(paste0("Specified 'enddate' is before 'startdate'"))}
  }
  
  X<- as.numeric(longitude)
  Y <- as.numeric(latitude)
  
  if (X >= Ausminx & X <= Ausmaxx & Y >= Ausminy & Y <= Ausmaxy){
  }else{
    stop(paste0("Supplied values for the location is not in Australia. : parameters = longitude, latitude : values = ", longitude, ", ", latitude))
  }
  
}



