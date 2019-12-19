library(raster)
library(httr)
library(jsonlite)
library(stringr)

# Thredds Cataloue
#http://esoil.io/thredds/catalog/SMIPSall/catalog.html?dataset=SMIPS/SMIPSv0.5.nc


threddsPath <- 'http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc'
defaultProduct <- 'Openloop_Wetness_Index'

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


getcellsForALatLon <- function(lon, lat){
  templateR <- raster(nrows=Ausnumrows, ncols=Ausnumcols, xmn=Ausminx, xmx=Ausmaxx, ymn=Ausminy, ymx=Ausmaxy, crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
  cell <- cellFromXY(templateR, cbind(c(lon), c(lat)))
  colNum <- colFromCell(templateR, cell)
  rowNum <- rowFromCell(templateR, cell)
  return(data.frame(colNum=colNum, rowNum=rowNum))
}


getThreddsDay <- function(theDate){
  d <- as.numeric(as.Date(paste(theDate), "%d-%m-%Y") - as.Date(paste(originDate), "%d-%m-%Y"))
 # print(d)
  return(d)
}

getSMIPSAustTemplate <- function(){
  r <- raster(nrows=Ausnumrows, ncols=Ausnumcols, xmn=Ausminx, xmx=Ausmaxx, ymn=Ausminy, ymx=Ausmaxy, crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
  return(r)
}

getGDALrowcolFromSMIPSTemplate <- function(longitude, latitude){
  r <- getSMIPSAustTemplate()
  cell <- cellFromXY(r, cbind(c(longitude), c(latitude)))
  rc <- list()
  rc$colNum <- colFromCell(r, cell)
  rc$rowNum <- rowFromCell(r, cell)
  return(rc)
}

getThreddsMaxDay <- function(){
  # get the max day in the Thredd catalogue
  url <- paste0(threddsPath, '.dds?time')
  t1 <- retrieveData(url)
  k <- regmatches(t1, gregexpr("\\[.+?\\]", t1))[[1]]
  k <- substring(k, 2, nchar(k)-1)
  maxDay <- as.numeric(str_trim(str_split(k, '=')[[1]][2]))
  return(maxDay)
}

retrieveData <- function(url){
  
  req <- GET(url)
  stop_for_status(req)
  t1 <- content(req, 'text', encoding = 'UTF-8')
  return(t1)
}

getSMIPSTimeSeries <- function(product, startDate, endDate, longitude, latitude){
  
  check_getSMIPSTimeSeries(product=product, startDate=startDate, endDate=endDate, longitude=longitude, latitude=latitude)
  
  if(is.null(product)){
    product = defaultProduct
  }
  
  endDate <- getEndDate(endDate)
  startDate <- getStartDate(startDate, endDate)
  
#  r <- getSMIPSAustTemplate()
  
  cell <- getGDALrowcolFromSMIPSTemplate(longitude,latitude)
  colNum <- cell$colNum
  rowNum <- cell$rowNum
  
  startDayNum = getThreddsDay(startDate)
  endDayNum = getThreddsDay(endDate)
  
  if(startDayNum < 0){
    startDayNum=0
    startDate=originDate
  }
  
  maxDay <- getThreddsMaxDay()
  #adjust if requested end date is past the last date in the datacube
  daylag = 0
  if (endDayNum > maxDay){
    daylag <- endDayNum-(maxDay-1)
    endDayNum = endDayNum-daylag
  }
  
  url <- paste0(threddsPath, '.ascii?',product,'%5B',startDayNum, ':', endDayNum ,'%5D%5B', rowNum-1,'%5D%5B', colNum-1, '%5D')
  #url <- paste0('http://esoil.io/thredds/dodsC/SMIPS/SMIPSv0.5.nc.ascii?',product,'%5B',startDayNum,'%5D%5B', rowNum,'%5D%5B', colNum, '%5D')

  d1 <- retrieveData(url)
  ndays <- (endDayNum-startDayNum) + 1
  dts <- seq.Date(from=as.Date(paste(startDate), "%d-%m-%Y"), to=as.Date(paste(endDate), "%d-%m-%Y")-daylag, by='days')
  pdts <- paste0(dts, 'T00:00:00')
  ts1 <- read.table(text=d1, skip=12, nrows = ndays , sep = ',')
  ts2 <- ts1[,-1]
  tsdf <- data.frame(t=pdts, v=ts2, stringsAsFactors = F)
  return(tsdf)
}


checkRasterInputs <- function(dt){
  if(is.null(dt)) { stop("The date parameter is required here")}
}

checkRasterInputs2 <- function(dt){
  print(dt)
  if(is.null(dt)) { stop("The date parameter is required here window")}
}


getSMIPSRaster <- function(product=NULL, dt, resFactor=1){
  
  
 checkRasterInputs(dt)
  
  if(is.null(product)){
    product = defaultProduct
  }
  
 # if (is.null(bboxExt)){
   templateR <- getSMIPSAustTemplate()
   rext <-  extent(templateR)
    minx=rext@xmin ; miny=rext@ymin; maxx=rext@xmax; maxy=rext@ymax
  # }else{
  #   minx=bboxExt@xmin; miny= bboxExt@ymin; maxx=bboxExt@xmax; maxy=bboxExt@ymax
  #   #bboxExt@xmin & outDF$Longitude <= bboxExt@xmax & outDF$Latitude >= bboxExt@ymin & outDF$Latitude <= bboxExt@ymax)
  # }
 
  xext = maxx - minx
  yext = maxy - miny
  
  # stridex <- ceiling(xext / ( AusRes * wmsnumcols))
  # stridey <- ceiling(yext / ( AusRes * wmsnumrows))
  stridey <- resFactor
  
  ll <- getcellsForALatLon(minx, miny)
  ur <- getcellsForALatLon(maxx, maxy)
  
  #subcols <- ceiling( c((ur$colNum-1) - ll$colNum) / stridey )
  subrows <- ceiling( c((ll$rowNum-1) - ur$rowNum) / stridey )
 # print(subrows)
  
  dayNum = getThreddsDay(dt)
  #url <- paste0(threddsPath,'.ascii?',product, '%5B',dayNum ,'%5D%5B', ur$rowNum-1, ':', ll$rowNum-1, '%5D%5B', ll$colNum-1, ':',  ur$colNum-1, '%5D')
  
  url <- paste0(threddsPath, '.ascii?',product, '%5B',dayNum ,'%5D%5B', ur$rowNum-1, ':', stridey, ':', ll$rowNum-1, '%5D%5B', ll$colNum-1, ':', stridey, ':', ur$colNum-1, '%5D')
 # url <- paste0('http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc.ascii?',product, '%5B',dayNum ,'%5D%5B', ur$rowNum-1, ':',  ll$rowNum-1, '%5D%5B', ll$colNum-1, ':', ur$colNum-1, '%5D')
  
   print(url)
   
   d1 <- retrieveData(url)
  
  odData1 <- read.table(text=d1, skip=12, nrows = subrows , sep = ',')
  
  #odData1 <- read.table(text=d1, skip=12, nrows = subrows , sep = ',')
  
  odData2 <- odData1[,-1]
  m1 <- as.matrix(odData2)
  
  r <- raster(nrows=nrow(odData2), ncols=ncol(odData2), xmn=minx, xmx=maxx, ymn=miny, ymx=maxy, crs=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),  vals=m1)
  
  return(r)
}


getSMIPSRasterWindow <- function(product=NULL, dt, bboxExt=NULL, outcols=NULL, outrows=NULL){
  
  checkRasterInputs2(dt)
  
  print(paste0('cols = ', outrows))
  
  if(is.null(outcols)){outcols=600}
  if(is.null(outrows)){outrows=400}
  print(outrows)
 # wmsnumrows <- 348
#  wmsnumcols <- 412

 # outrows <- 348
#   outcols <- 412
  
  
  if(is.null(product)){
    product = defaultProduct
  }
  
  if (is.null(bboxExt)){
    templateR <- getSMIPSAustTemplate()
    rext <-  extent(templateR)
    minx=rext@xmin ; miny=rext@ymin; maxx=rext@xmax; maxy=rext@ymax
  }else{
    minx=bboxExt@xmin; miny= bboxExt@ymin; maxx=bboxExt@xmax; maxy=bboxExt@ymax
    #bboxExt@xmin & outDF$Longitude <= bboxExt@xmax & outDF$Latitude >= bboxExt@ymin & outDF$Latitude <= bboxExt@ymax)
  }
  
  
  
  xext = maxx - minx
  yext = maxy - miny
  
  print(outrows)
 
  
  stridex <- ceiling(xext / ( AusRes * outcols))
  stridey <- ceiling(yext / ( AusRes * outrows))
  

  
  print(paste0('stridey = ', stridey))
 # stridey <- resFactor
  
  ll <- getcellsForALatLon(minx, miny)
  ur <- getcellsForALatLon(maxx, maxy)
  
  subcols <- ceiling( c((ur$colNum-1) - (ll$colNum-1)) / stridey )
  subrows <- ceiling( c((ll$rowNum-1) - (ur$rowNum-1)) / stridey )
  
  dayNum = getThreddsDay(dt)
  print(dayNum)
  url <- paste0(threddsPath, '.ascii?',product, '%5B',dayNum ,'%5D%5B', ur$rowNum-1, ':', stridey, ':', ll$rowNum-1, '%5D%5B', ll$colNum-1, ':', stridey, ':', ur$colNum-1, '%5D')
  # url <- paste0('http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc.ascii?',product, '%5B',dayNum ,'%5D%5B', ur$rowNum-1, ':',  ll$rowNum-1, '%5D%5B', ll$colNum-1, ':', ur$colNum-1, '%5D')
  
  print(url)
  
  req <- GET(url)
  stop_for_status(req)
  d1 <- content(req, 'text', encoding = 'UTF-8')
  
  odData1 <- read.table(text=d1, skip=12, nrows = subrows , sep = ',')
  odData2 <- odData1[,-1]
  m1 <- as.matrix(odData2)
  
  r <- raster(nrows=nrow(odData2), ncols=ncol(odData2), xmn=minx, xmx=maxx, ymn=miny, ymx=maxy, crs=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),  vals=m1)
  r2 <-raster(nrows=outrows, ncols=outcols, xmn=minx, xmx=maxx, ymn=miny, ymx=maxy)
  r3 <- resample(r, r2, method="bilinear")
 
 # plot(r3)
  return(r3)
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


getEndDate <- function(endDate){
  
  dnowYMD <- format(Sys.time(),format="%d-%m-%Y" )
  
  if(is.null(endDate))
  {
    isoEDate <- paste0(dnowYMD)
  }else{
    
    # check to see supplied edate is not greater than today - if so fix it
    bits <- str_split(endDate, 'T')
    dnowYMD <- format(Sys.time(), "%Y-%m-%d")
    dnowPos <- as.POSIXct(dnowYMD)
    endDatePos <- as.POSIXct(endDate, format="%d-%m-%Y")
    dtdiff <- endDatePos-dnowPos
    ndiff <- as.numeric(dtdiff, units = "days")
    
    if(ndiff > 0){
      isoEDate <- paste0( format(Sys.time(), "%d-%m-%Y"))
    }else{
      isoEDate <- endDate
    }
  }
  return(isoEDate)
}

getStartDate <- function(startDate, endDate){
  
  if(is.null(startDate))
  {
    if(is.null(endDate))
    {
      ed <- format(Sys.time(), "%d-%m-%Y")
    }else{
      ed <- endDate
    }
    edp <- strptime(ed, "%d-%m-%Y")
    py <- edp - 31536000
    isoSDate <- format(py, "%d-%m-%Y")
  }else{
    isoSDate <- startDate
  }
  
}

