
source('Backend.R')

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



#* Returns a table of configuration values
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json.
#* @param pwd (Required)
#* @tag SMIPS
#* @get /SMIPS/Config
apiGetConfig <- function( res, pwd='', format='json'){
  
  tryCatch({
    label='Config'
    if(pwd!='hdei-44$&lDDDF@1!KLB55hds#+-3hde'){return("Incorrect password")}
    #prods <- data.frame(Product=c('SMIPS-Raw', 'SMIPS-Assim'))
    #prods <- c('SMIPS-Raw', 'SMIPS-Assim')
    config <- getConfig()
    resp <- cerealize(config, label, format, res)
    return(resp)
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}


#* Returns a list of the available SMIPS Products
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json.
#* @tag SMIPS
#* @get /SMIPS/Products
apiGetSMIPSProducts<- function( res, format='json'){
  
  tryCatch({
    label='Product'
    #prods <- data.frame(Product=c('SMIPS-Raw', 'SMIPS-Assim'))
    #prods <- c('SMIPS-Raw', 'SMIPS-Assim')
    prods <- getProducts()
    resp <- cerealize(prods, label, format, res)
    return(resp)
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}

#* Returns a timeSeries of values at a location

#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json.
#* @param edate (Optional) Last date to return in the form dd-mm-YYYY. If not supplied defaults to today.
#* @param sdate (Optional) First date to return in the form dd-mm-YYYY. If not supplied defaults to 1 year before edate.
#* @param latitude (Required) Latitude of the location to drill.
#* @param longitude (Required) Longitude of the location to drill.
#* @param product (Required) SMIPS product to query.


#* @tag SMIPS
#* @get /SMIPS/TimeSeries
apiGetSMIPSTimeseries<- function( res, sdate=NULL, edate=NULL, longitude=NULL, latitude=NULL, product=NULL, format='json'){

  tryCatch({
    
   
    if(is.null(product)){
        product = defaultProduct
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
    "Units"= "Proportion",
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


#* Returns a SMIPS raster of soil moisture estimates for a day as GeoTiff


#* @param bbox (Optional) Subset a window of raster data . 
#* @param product (Required) SMIPS product to return 
#* @param date (Required) Date for soil moisture map (format = dd-mm-yyyy).
#* @tag SMIPS
#* @get /SMIPS/Raster

apiGetSMIPSRaster <- function(res, product=NULL, date=NULL, bbox=NULL,  resFactor=1){
  
  tryCatch({
    
    tf <- getSMIPSRasterWCS(product=product, dt=date, bbox, resFactor=as.numeric(resFactor))

    res$setHeader("content-disposition", paste0("attachment; filename=SMIPS_", product, "_", date,  ".tif"))
    res$setHeader("Content-Type", list(type="application/octet-stream"))
      
    bin <- readBin(paste0(tf), "raw", n=file.info(paste0(tf))$size)
    unlink(tf)

    res$body <- bin
    return(res)
    
  }, error = function(res)
  {
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
  
}

# #* Returns a spatial subsetted window SMIPS raster as GeoTiff
# 
# #* @param cols (Optional) number of columns in the returned image. (Default = 600)
# #* @param rows (Optional) number of rows in the returned image. (Default = 400)
# #* @param bbox (Optional) Bounding box of area to return in the form'minx;maxx;miny;maxy'. (Default = 112.905;154.005;-43.735;-9.005) 
# #* @param product (Optional) SMIPS product to return. ('SMIPS-RawIndex', 'SMIPS-AssimIndex') (Default = SMIPS-RawIndex')
# #* @param date (Required) Date for soil moisture map. (format = DD-MM-YYYY)
# #* @tag SMIPS
# #* @get /SMIPS/RasterWindow
# 
# apiGetSMIPSRasterWindow <- function(res, product=NULL, date=NULL, bbox=NULL, cols=NULL, rows=NULL){
#   
#   tryCatch({
#     
#     
#    prod <- getProduct(product)
#     
#     if(!is.null(bbox)){
#       bits <- str_split(bbox, ';')
#       l <- as.numeric(bits[[1]][1])
#       r <- as.numeric(bits[[1]][2])
#       b <- as.numeric(bits[[1]][3])
#       t <- as.numeric(bits[[1]][4])
#       bboxExt <- extent(l, r, b, t)
#     }else{
#       bboxExt <- NULL
#     }
#     
#   
#     r <- getSMIPSRasterWindow(product=prod, dt=date, bboxExt=bboxExt, as.numeric(cols),  as.numeric(rows))
#     
#     res$setHeader("content-disposition", paste0("attachment; filename=SMIPS_", prod, "_", date,  ".tif"))
#     res$setHeader("Content-Type", list(type="application/octet-stream"))
#     
#     tf <- tempfile(fileext = '.tif')
# 
#     writeRaster(r, tf, overwrite=T)
#     bin <- readBin(paste0(tf), "raw", n=file.info(paste0(tf))$size)
#     unlink(tf)
#     
#     res$body <- bin
#     return(res)
#     
#   }, error = function(res)
#   {
#     print(geterrmessage())
#     res$status <- 400
#     #res$setHeader("Content-Type", list(type="application/json"))
#     list(error=jsonlite::unbox(geterrmessage()))
#   })
#   
# }


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



getProduct <- function(product){
  
  
  if(is.null(product)){
    return('Openloop_Wetness_Index')
  }else if(product == 'SMIPS-Raw'){
    return('Openloop_Wetness_Index')
  }else if(product == 'SMIPS-Assim' ){
    return('Analysis_Wetness_Index')
  }else{
    return(NULL)
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








