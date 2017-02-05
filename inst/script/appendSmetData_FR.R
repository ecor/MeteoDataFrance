#!/usr/bin/env Rscript

# file appendSmetData_FR.R
#
# This file contains instructions for creating SMET files with snow weather MeteoFrence open data 
# (https://donneespubliques.meteofrance.fr/donnees_libres)
#
#
# author: Emanuele Cordano on 08-02-2015

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################



library(MeteoDataFrance)
library(RSMET)

url <- "https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Nivo/nivo.XDATE.csv"

## SET DOwnload directory

oldsmetdir   <- '/home/ecor/Dropbox/R-packages/MeteoDataFrance/inst/smet' 
dest         <- '/home/ecor/Dropbox/R-packages/MeteoDataFrance/inst/scrab/dest' 
appendsmet_dir <- oldsmetdir ###

###oldsmetdir <- system.file("smet",package="MeteoDataBayern")
### GO ON 
oldsmetfiles <- list.files(oldsmetdir,pattern=".smet",full.name=TRUE)


oldsmet <- lapply(X=oldsmetfiles,FUN=as.smet)

###

force.download <- FALSE

dates = as.character(Sys.Date()-120:1, format= "%Y%m%d") ## 20 days before the present day!
meteofrance <- getMeteoDataFrance(url=url,dest=dest,dates=dates,spoutput=TRUE,force.download=force.download)

#### coercion to a list of smet objects 


#######

variables <- c("timestamp","DW","VW","TA","TD","RH","MFR_rr24",
		"MFR_tn12","MFR_tn24","MFR_tx12","MFR_tx24","HS","HS_fresh")
variables <- c("timestamp","DW","VW","TA","TD","RH","HS")



header <- c("longitude","latitude","station_id" ,"altitude","location")
names(header) <- header


########

newsmet <- as.smet(meteofrance,variables=variables)
newsmet <- lapply(X=newsmet,FUN=function(x) {
			
			     x@header$station_id <- paste("MFR",x@header$station_id,sep="")
				 return(x)
				 
			})
	

names(newsmet) <- paste("MFR",names(newsmet),sep="")
if (length(oldsmet)<1) {
	
	oldsmet <- newsmet
} else {
	
	names(oldsmet) <- sapply(X=oldsmet,FUN=function(x){x@header$station_id})
	
	names_n <- intersect(names(oldsmet),names(newsmet))
	names_n <- names_n[!is.na(names_n)]
	
	oldsmet <- oldsmet[names_n]
	newsmet <- newsmet[names_n]
	
	
	
	
	
	
	
	
	
}

#names(oldsmet) <- paste("MFR",names(newsmet),sep="")

########



####



appendsmet <- mapply(FUN=collapse.smet,x=oldsmet,y=newsmet)

# The new appended smet was created !!
# They are written in the following directory: (please modify as youor purpose)


 

appendsmet <- lapply(X=appendsmet,FUN=function(x,dir) {
			print(x@header$station_id)
			x@file <- sprintf("%s/%s.smet",dir,x@header$station_id)
			print(x,file="internal")
			
		},dir=appendsmet_dir)










