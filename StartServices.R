library(plumber)

devel <- F

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

  deployDir <-'/srv/plumber/SMIPS_API'
  server <- 'http://esoil.io'
  options("plumber.host" = "0.0.0.0")
  options("plumber.apiHost" = "0.0.0.0")
  portNum <- 8082
#portNum <- 8073
}else{
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/SMIPS_API'
  server <- '127.0.0.1'
  options("plumber.host" = "127.0.0.1:3077")
  options("plumber.apiHost" = "127.0.0.1:3077")
  portNum <- 3077
}

if(devel){
  server <- '0.0.0.0'
}

source(paste0(deployDir, '/Backend.R'))


r <- plumb(paste0(deployDir, "/apiEndPoints.R"))

#server <- 'http://esoil.io'

#portNum <- 8079

print(r)


r$run(host= server, port=portNum, swagger=TRUE)




