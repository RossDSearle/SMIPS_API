library(stringr)
library(XML)
library(xml2)
library(htmlTable)

#projectRoot <- 'C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR'

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

  deployDir <-'/srv/plumber/TERNLandscapes/SoilDataFederatoR'
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





#* Returns a TimeSeries

#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param edate (Optional) EndDate
#* @param sdate (Optional) Startdate
#* @param latitude (Required) Latitude of the location to drill
#* @param longitude (Required) Longitude of the location to drill


#* @tag SMIPS
#* @get /SMIPS/TimeSeries
apiGetSMIPSTimeseries<- function( res, sdate=NULL, edate=NULL, longitude=NULL, latitude=NULL, product=NULL, format='json'){

  tryCatch({

    product = 'Analysis_Wetness_Index'

    DF <- getSMIPSTimeSeries(product, sdate, edate, as.numeric(longitude), as.numeric(latitude))
    label <- 'SMIPS_TS'
    print(DF)

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
    "RequestStartDate"= sdate,
    "RequestEndDate"=edate,
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








