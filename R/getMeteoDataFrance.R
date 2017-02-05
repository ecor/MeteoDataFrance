NULL
#' Get measured wether and snow data for French Alps and Pyrennes
#'  
#' 
#' @name Snow and Weather Data in France
#' @param url URL with data. See  \code{"https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Nivo/nivo.20150810.csv"} or \url{https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=94&id_rubrique=32}
#' @param dest destination directory where the downladed csv file is saved  
#' @param method method applied in \code{\link{download.file}} 
#' @param date_indicator string contained in \code{url} which is replaced by \code{dates}
#' @param time_indicator string contained in \code{url} which is replaced by \code{times}
#' @param dates string containg dates on which date are requested to be downloaded. Actually the date format is \code{\%Y\%m\%d}, es. \code{20150810} for 10 August 2015.
#' @param times vector containg the hours at which date are requested to be downloaded (in case of subdaily data)
#' @param url_stationcoord string containings the \code{csv} and \code{json} files with stataion geographic metadata.
#' @param spoutput logical value. Default is \code{FALSE}. If it \code{TRUE}, spatial information (station coordinates,etc.) are added through  \code{url_stationcoord}.
#' @param smet.output logical value. Default is \code{FALSE}. If it \code{TRUE}, output field names are translated in accordance with SMET format files. (\url{http://models.slf.ch/docserver/meteoio/html/smetio.html})
#' @param metaparam \code{data.frame} table containg all metainformation for each variable: name, ID, type, SMET name, measurement unit,unit multiplier and description.
#' @param force.download logical value. If \code{TRUE} files are forced to be downloaded and then overwritten, if it is \code{FALSE} (default) and the files already exist in \code{destdir} directory are not downloaded.
#' @param ... further arguments
#' 
#' @import stringr
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @references \url{https://donneespubliques.meteofrance.fr/?fond=rubrique},\url{https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=94&id_rubrique=32}
#' 
#' 
#' @seealso \code{\link{download.file}} ,\code{\link{DownloadFiles}}
#' @author Emanuele Cordano
#' 
#' 
#' @examples 
#' 
#'
#' library(stringr)
#' 
#' 
#' url <-  "https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Nivo/nivo.XDATE.csv"
#' 
#' data <- getMeteoDataFrance(url=url,spoutput=TRUE)
#' 
#' head(data[data$numer_sta=="07729",])
#' 
#'  ## SYNOP DATE
#' url_synop <- "https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/synop.XDATEXTIME.csv"
#' 
#' url_stationcoord_synop <- c(
#' 		csv="https://donneespubliques.meteofrance.fr/
#' donnees_libres/Txt/Synop/postesSynop.csv",
#' 		json="https://donneespubliques.meteofrance.fr/
#' donnees_libres/Txt/Synop/postesSynop.json"
#' )
#' 
#' data_synop <- getMeteoDataFrance(url=url_synop,
#' 	url_stationcoord=url_stationcoord_synop,
#'  spoutput=TRUE)
#' 
#' ##https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.json
#' 
#https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/synop.2015090700.csv
#https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/synop.2015090700.csv


# url1 <-"http://www.lawinenwarndienst-bayern.de/daten_meldungen/messstationen/messstation.php?id=8"
### # @time 18 Aufust 2015 

# @importFrom rvest html_children 


getMeteoDataFrance <- function(url= "https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Nivo/nivo.XDATE.csv",
		dest=system.file("dest",package="MeteoDataFrance"),method=c("wget","curl"),
		dates=as.character(Sys.Date()-10:1,format="%Y%m%d"),
		times=c(0,3,6,9,12,15,18,21),
		date_indicator="XDATE",
		time_indicator="XTIME",
		force.download=FALSE,
		smet.output=TRUE,
		url_stationcoord=c(
						csv="https://donneespubliques.meteofrance.fr/
donnees_libres/Txt/Nivo/postesNivo.csv",
						json="https://donneespubliques.meteofrance.fr/
donnees_libres/Txt/Nivo/postesNivo.json"),
						
						spoutput=FALSE,
		metaparam=read.table(system.file('metadata/paramnames.csv',package="MeteoDataFrance"),sep=",",header=TRUE,stringsAsFactors=FALSE),

		...) {
	
	url  <- str_replace_all(url,"\n","")

	
	method <- method[1]
	

	if (any(str_detect(url,date_indicator)==TRUE)) {
		
		if (length(url)==1) {
			
			url <- str_replace(url,date_indicator,dates)
			
			
		} else {
			
			url <- sapply(X=url,FUN=str_replace,pattern=date_indicator,replacement=dates)
			url <- unique(url)
			
			
		}
		
		
	}
	
	if (any(str_detect(url,time_indicator)==TRUE)) {
		
		if (is.numeric(times)) times <- sprintf("%02d",times)
		if (length(url)==1) {
			
			url <- str_replace(url,time_indicator,times)
			
			
		} else {
			
			url <- sapply(X=url,FUN=str_replace,pattern=time_indicator,replacement=times)
			url <- unique(url)
			
			
		}
		
		
	}
	
	
	file <- sapply(X=url,FUN=function(urlit) {rev(str_split(urlit,"/")[[1]])[1]})
	names(url) <- file
	
	sep="/"
	if (str_sub(dest,-1)=="/") {
		
		sep=""
		
	} 
	destfile <- paste(dest,file,sep=sep)
	names(destfile) <- file
	
	for (it in file) {
		
		
		if ((file.exists(destfile[it])==FALSE)| (force.download==TRUE)) {
			
		
			message <- sprintf("Downloading %s from %s to %s",it,url[it],destfile[it])
		
			message(message)
		
			utils::download.file(url=url[it],destfile=destfile[it],method=method,...)
		}
	}

	
	
	 checked <- sapply(X=destfile,FUN=function(x) {
				
				ll <- readLines(x)
				cond <- (length(ll)<=1)  
				cond <- ("<!DOCTYPE HTML>" %in% ll) | cond
				cond <- ll[1]=="" | cond 
				
				if (cond==TRUE) {
					
					msg <- sprintf("Missing data in %s (FILE REMOVED)!!",x)
					warning(msg)
					out <- FALSE
				} else {
					
					out <- TRUE
				}
			
				
				
				return(out)
			}) ###,sep=";",header=TRUE)
	
	destfile <- destfile[which(checked)]
	
	out <- lapply(X=destfile,FUN=readLines)
	#######   ("HHH")
	out <- lapply(X=out,FUN=function(x){
			
				x <- str_split(x,";")
				
				header <- x[[1]]
			
				x[-1] <- lapply(X=x[-1],FUN=function(x,header) {
							
							x[which(header=="")] <- ""
							return(x)
						},header=header)
				
				ncol <- length(header)
				
			
				x <- unlist(x[-1])
				x <- as.data.frame(matrix(x,byrow=TRUE,ncol=ncol),stringsAsFactors=FALSE)
				names(x) <- header
				x <-x[,names(x)!=""]
			
				
				return(x)
				
				
			})
		
		header <- unique(unlist(lapply(X=out,FUN=names)))
		#######   "LLLLLL"

		out <- lapply(X=out,FUN=function(x,header) {
					
					xn <- header[!(header %in% names(x))]	
					
					if (length(xn)>0) {
						
						x[,xn] <- NA
					}
					x <- x[,header]
					
					return(x)
					
				},header=header)
		

		out <- do.call("rbind",out)
		
		rownames(out) <- NULL
	
		if (spoutput==TRUE) {
			
			
			
		
			url_stationcoord <- str_replace_all(url_stationcoord,"\n","")
			
			
			coordfile <- MeteoDataFrance::DownloadFiles(url=url_stationcoord,dest=dest,force.download=force.download,method=method,...)
			
			icsv <-   which(str_detect(coordfile,".csv"))
			ijson <-  which(str_detect(coordfile,".json"))
			names(coordfile)[icsv] <- "csv"
			names(coordfile)[ijson] <- "json"
			
			coordcsv <- readLines(coordfile["csv"])[2]
			sepc <- c(",",":",";")
			sep <- sepc[which(str_detect(coordcsv,sepc))][1]
			if (!(sep %in% sepc)) {
				
				coordfile <- MeteoDataFrance::DownloadFiles(url=url_stationcoord,dest=dest,force.download=TRUE,method=method,...)
				icsv <-   which(str_detect(coordfile,".csv"))
				ijson <-  which(str_detect(coordfile,".json"))
				names(coordfile)[icsv] <- "csv"
				names(coordfile)[ijson] <- "json"
		
				coordcsv <- readLines(coordfile["csv"])[2]
				sepc <- c(",",":",";")
				sep <- sepc[which(str_detect(coordcsv,sepc))][1]
				
			}
			
			coordcsv <- read.table(coordfile["csv"],sep=sep,stringsAsFactors=FALSE,header=FALSE)
			names(coordcsv) <- coordcsv[1,]
			
		
			coordcsv <- coordcsv[-1,]
		##	str(coordcsv)
		##	print(nrow(coordcsv))
			coordcsv <- coordcsv[which(!duplicated(coordcsv$ID)),]
		
		
			
			
			rownames(coordcsv) <- coordcsv$ID 
			coordcsv <- coordcsv
			
			coordjson <- jsonlite::fromJSON(coordfile["json"], flatten=FALSE)$features$properties ##### fromJSON(paste(readLines(coordfile["json"], collapse="")))
			
			coordjson <- coordjson[,names(coordcsv)]
			coordjson <- coordjson[which(!duplicated(coordjson$ID)),]
			rownames(coordjson) <- coordjson$ID 
			
			### see http://stackoverflow.com/questions/2617600/importing-data-from-a-json-file-into-r
			coordcsv <- coordcsv
			coordtable <- rbind(coordcsv,coordjson)
			
	
			coordtable <- coordtable[out$numer_sta,]
			
			out <- cbind(coordtable,out)
			
			
			if (any(out$ID==out$numer_sta)==FALSE) {
			
			   stop("ID or coordinate do not match with data; retry with no coords (spoutpt=FALSE)")
			
			
			}
		}
		
		if (!is.null(metaparam)) { 
		
			
				param <- as.character(metaparam$IDparam)
				type <-  as.character(metaparam$type)
				names(type) <- param
				
				param <- param[param %in% names(out)]
				param <- unique(param) ## ec 20160218
				
				for (it in param) {
				
					if (type[it]=="real") {
						
						out[,it] <- str_replace_all(out[,it],"[a-z/]","")
						
						
					
						
						out[,it] <- as.numeric(out[,it])
						
					} else if (type[it]=="int") {
						
					
						
					
						out[,it] <- str_replace_all(out[,it],"[a-z/]","")
						
						
						
						
					    out[,it] <- as.integer(out[,it])
					}
					
					
				}
		 
		        
		
		
		
		}
		
		if ((smet.output==TRUE) & (!is.null(metaparam))) {
			
			smet_ids <- metaparam$SMET_ID
			 
			smet_ids <- paste("MFR",metaparam$IDparam,sep="_")
			
			smet_ids[metaparam$SMET_ID!=""] <- metaparam$SMET_ID[metaparam$SMET_ID!=""] 
			
			names(smet_ids) <- metaparam$IDparam
			metaparam$SMET_ID <- smet_ids
			metaparam$SMET_UNIT[is.na(metaparam$SMET_UNIT)] <- 1
			metaparam$SMET_UNIT_OFFSET <- 0
			metaparam$SMET_UNIT_MULTIPLIER <- metaparam$SMET_UNIT
			
	#		print(smet_ids)
	#		print(names(out))
			names(out) <- smet_ids[names(out)]
			
			####
			
			out$timestamp <- as.POSIXlt(out$timestamp,format="%Y%m%d%H%M%S",tz="Etc/GMT+0")
		
			
			
			attr(out,"metaparam") <- metaparam[metaparam$SMET_ID %in% names(out),]
			
			
						
		}
		
		
		return(out)
		
		
		
	
	
	
}

			