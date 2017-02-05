# TODO: Add comment
# 
# Author: ecor
###############################################################################

rm(list=ls())

library(MeteoDataFrance)
library(stringr)
 
 force.download <- FALSE
 
 url <-  "https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Nivo/nivo.XDATE.csv"
 
 data <- getMeteoDataFrance(url=url,spoutput=TRUE)

 
#' head(data[data$numer_sta=="07729",])
#' 
#'  ## SYNOP DATE
url_synop <- "https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/synop.XDATEXTIME.csv"





url_stationcoord_synop <- c(
 		csv="https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv",
 		json="https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.json"
 )
 
 data_synop <- getMeteoDataFrance(url=url_synop,url_stationcoord=url_stationcoord_synop,spoutput=TRUE,force.download=force.download)
 

 meteofrance <- data
 meteofranceSynop <- data_synop 
 wpath_data <- '/home/ecor/Dropbox/R-packages/RSMET/data' 
 
 meteofrance_file <- paste(wpath_data,"meteofrance.rda",sep="/")
 meteofranceSynop_file <- paste(wpath_data,"meteofranceSynop.rda",sep="/")
 
 save(meteofrance,file=meteofrance_file)
 save(meteofranceSynop,file=meteofranceSynop_file)
 
 
 
 
 
 

