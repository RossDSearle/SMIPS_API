library(stringr)
library(XML)
library(xml2)
library(htmlTable)
library(raster)

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  deployDir <-'/srv/plumber/SMIPS_API'
}else{
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/SMIPS_API'
}

pixelXSize = 0.009999999108758974783
pixelYSize = 0.01000000014279027981


#pixelXSize = 0.01
#pixelYSize = 0.01

threddsPath <- 'http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc'

config <-  read.csv(paste0(deployDir, '/Config/RasterServices_config.csv'), stringsAsFactors=F)
config <- config[!startsWith(config$Name, "!"),]


originDay = 42326
originDate <- '20-11-2015'
Ausminx <- 112.905
Ausminy <-  -43.735
Ausmaxx <- 154.005
Ausmaxy <- -9.005
AusRes <- 0.01
Ausnumrows <- 3474
Ausnumcols <- 4110