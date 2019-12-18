library(stringr)
library(XML)
library(xml2)
library(htmlTable)
library(raster)

#projectRoot <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR'

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

  deployDir <-'/srv/plumber/SMIPS_API'
  #server <- 'http://esoil.io'
}else{
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/SMIPS_API'
  source(paste0(deployDir, '/Backend.R'))
  #server <- '0.0.0.0'
}

#source(paste0(deployDir, '/R/Helpers/apiHelpers.R'))
#source(paste0(deployDir, '/R/Backends.R'))

#* @apiTitle SMIPS Web API
#* @apiDescription SMIPs API description



#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){

  logentry <- paste0(as.character(Sys.time()), ",",
       machineName, ",",
       req$REQUEST_METHOD, req$PATH_INFO, ",",
       str_replace_all( req$HTTP_USER_AGENT, ",", ""), ",",
       req$QUERY_STRING, ",",
       req$REMOTE_ADDR
      )

  dt <- format(Sys.time(), "%d-%m-%Y")

  logDir <- paste0(deployDir, "/Logs")

  if(!dir.exists(logDir)){
     dir.create(logDir , recursive = T)
    }

  logfile <- paste0(deployDir, "/Logs/SoilFederationAPI_logs_", dt, ".csv")
  try(writeLogEntry(logfile, logentry), silent = TRUE)

  plumber::forward()
}


writeLogEntry <- function(logfile, logentry){

  if(file.exists(logfile)){
    cat(logentry, '\n', file=logfile, append=T)
  }else{
    hdr <- paste0('System_time,Server,Request_method,HTTP_user_agent,QUERY_STRING,REMOTE_ADDR\n')
    cat(hdr, file=logfile, append=F)
    cat(logentry, '\n', file=logfile, append=T)
  }
}

#* Returns a list of the available SMIPS Products
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json.
#* @tag SMIPS
#* @get /SMIPS/Products
apiGetSMIPSProducts<- function( res, format='json'){
  
  tryCatch({
    label='Products'
    resp <- cerealize(supportedProducts, label, format, res)
    return(resp)
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}



#* Returns a TimeSeries

#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json.
#* @param edate (Optional) Last date to return in the form dd-mm-YYYY. If not supplied defaults to today.
#* @param sdate (Optional) First date to return in the form dd-mm-YYYY. If not supplied defaults to 1 year before edate.
#* @param latitude (Required) Latitude of the location to drill.
#* @param longitude (Required) Longitude of the location to drill.
#* @param product (Optional) SMIPS product to query. Current choices are 'Analysis_Wetness_Index' and 'Openloop_Wetness_Index'. Default='Analysis_Wetness_Index'


#* @tag SMIPS
#* @get /SMIPS/TimeSeries
apiGetSMIPSTimeseries<- function( res, sdate=NULL, edate=NULL, longitude=NULL, latitude=NULL, product=NULL, format='json'){

  tryCatch({
    
    if(is.null(product)){
        product = 'Analysis_Wetness_Index'
    }

    DF <- getSMIPSTimeSeries(product, sdate, edate, as.numeric(longitude), as.numeric(latitude))
    label <- 'SMIPS_TS'

    sd <- DF$t[1]
    ed <- DF$t[nrow(DF)]

    odf <- data.frame("SiteID"= paste0(product, "_", as.numeric(longitude), "_" , as.numeric(latitude)),

    "Provider"="CSIRO",
    "Backend"= "SMIPS",
    "Access"= "Public",
    "Latitude"= as.numeric(latitude),
    "Longitude"= as.numeric(longitude),
    "SensorID"= product,
    "SensorName"= product,
    "UpperDepthCm"= 0,
    "LowerDepthCm"= 90,
    "RequestStartDate"= sd,
    "RequestEndDate"=ed,
    "AggregationPeriod"= "none",
    "DataType"= "Soil-Moisture",
    "Units"= "Percent",
    "Calibrated"= F

    )

    print(DF)
    TSout <- vector("list", ncol(DF)-1)

    for (i in 1 : (ncol(DF)-1)) {
      rdf <- data.frame(t=DF$t, v=DF[i+1])
      TSout[[i]] <- rdf
    }

    odf$DataStream <- I(TSout)

    resp <- cerealize(odf, label, format, res)
    return(resp)

  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}




#* Returns a SMIPS raster as GeoTiff

#* @param date Date for soil moisture map. (format = DD-MM-YYYY)
#* @param product Region to generate soil moisture map for. (SFS is only option currently)
#* @param bbox Depth to generate soil moisture map for. 
#* @param resFactor Depth to generate soil moisture map for. 
#' @html
#* @tag SMIPS
#* @get /SMIPS/Raster

apiGetSMIPSRaster <- function(res, product=NULL, date=NULL, bbox=NULL,  resFactor=1){
  
  tryCatch({
    
    if(is.null(product)){
      product=defaultProduct
    }
    
    if(!is.null(bbox)){
      bits <- str_split(bbox, ';')
      l <- as.numeric(bits[[1]][1])
      r <- as.numeric(bits[[1]][2])
      b <- as.numeric(bits[[1]][3])
      t <- as.numeric(bits[[1]][4])
      bboxExt <- extent(l, r, b, t)
    }else{
      bboxExt <- NULL
    }

    res$setHeader("content-disposition", paste0("attachment; filename=SMIPS_", product, "_", date,  ".tif"));
    res$setHeader("Content-Type", "image/tiff")
    
    r <- getSMIPSRaster(product=product, dt=date, bboxExt=bboxExt, resFactor=resFactor)
    tf <- tempfile(fileext = '.tif')
    print(r)
    print(tf)
    writeRaster(r, tf)
    bin <- readBin(paste0(tf), "raw", n=file.info(paste0(tf))$size)
    unlink(tf)
    
    return(bin)
    
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
  
}

#* Returns a  windowes SMIPS raster as GeoTiff

#* @param date Date for soil moisture map. (format = DD-MM-YYYY)
#* @param product Region to generate soil moisture map for. (SFS is only option currently)
#* @param bbox Depth to generate soil moisture map for (minx;maxx;miny;maxy). 
#* @param resFactor Depth to generate soil moisture map for. 
#' @html
#* @tag SMIPS
#* @get /SMIPS/RasterWindow

apiGetSMIPSRasterWindow <- function(res, product=NULL, date=NULL, bbox=NULL, outcols=NULL, outrows=NULL){
  
  tryCatch({
    
    if(is.null(product)){
      product=defaultProduct
    }
    
    if(!is.null(bbox)){
      bits <- str_split(bbox, ';')
      l <- as.numeric(bits[[1]][1])
      r <- as.numeric(bits[[1]][2])
      b <- as.numeric(bits[[1]][3])
      t <- as.numeric(bits[[1]][4])
      bboxExt <- extent(l, r, b, t)
    }else{
      bboxExt <- NULL
    }
    
    res$setHeader("content-disposition", paste0("attachment; filename=SMIPS_", product, "_", date,  ".tif"));
    res$setHeader("Content-Type", "image/tiff")
    
    r <- getSMIPSRasterWindow(product=product, dt=date, bboxExt=bboxExt, outcols, outrows)
    tf <- tempfile(fileext = '.tif')

    writeRaster(r, tf, overwrite=T)
    bin <- readBin(paste0(tf), "raw", n=file.info(paste0(tf))$size)
    unlink(tf)
    
    return(bin)
    
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
  
}


cerealize <- function(DF, label, format, res){

  if(format == 'xml'){

    res$setHeader("Content-Type", "application/xml; charset=utf-8")
    print(format)
    xmlT <- writexml(DF, label)
    res$body <- xmlT
    return(res)

  }else if(format == 'csv'){
    res$setHeader("content-disposition", paste0("attachment; filename=", label, ".csv"));
    res$setHeader("Content-Type", "application/csv; charset=utf-8")
    res$body <- writecsv(DF)
    return(res)

  }else if(format == 'html'){
    res$setHeader("Content-Type", "text/html ; charset=utf-8")
    res$body <- htmlTable(DF, align = "l", align.header = "l", caption = label)
    return(res)

  }else{
    return(DF)
  }


}



writecsv <- function(DF){

  tc <- textConnection("value_str2", open="w")
  write.table(DF, textConnection("value_str2", open="w"), sep=",", row.names=F, col.names=T)
  value_str2 <- paste0(get("value_str2"), collapse="\n")
  close(tc)
  return(value_str2)

}

writexml <- function(df, label){



  o <- apply(df, 1, DataFrameToXmlwriter, label)
  s <- unlist(o)
  xml <- paste( s, collapse = '')
  xml2 <- str_replace_all(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>\n<', label, 'Records>\n', xml, '</', label, 'Records>'), '&', '')


  #cat(xml2, file='c:/temp/x.xml')
  return(xml2)
}

DataFrameToXmlwriter <- function(x, label){

  v <- paste0('<', label, 'Record>')
  for (i in 1:length(names(x))) {

    v <- paste0(v, '<', names(x)[i], '>', str_replace(x[i], '<', 'less than'), '</', names(x)[i], '> ')
  }
  v <- paste0(v,'</', label, 'Record>\n')

  v2 <- v
  return(v2)
}








