# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL
#' Donlowad MetaInfo and MetaData (for MeteoFrance Snow Open Data)
#' 
#' 
#' 
#' @param url URLs. See the linked files in \url{https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=94&id_rubrique=32}
#' @param dest destination directory where the downladed csv file is saved  
#' @param method method applied in \code{\link{download.file}} 
#' @param force.download logical value. If \code{TRUE} files are forced to be downloaded and then overwritten, if it is \code{FALSE} (default) and the files already exist in \code{destdir} directory are not downloaded.
#' @param ... further argument for \code{download.file} 
#' 
#' 
#' @export 
#' 
#' @examples 
#' 
#' files <- DownloadFiles()
#' DownloadFiles(destdir='/home/ecor/Dropbox/R-packages/MeteoDataFrance/inst/dest')
#' 
#' 
#' 



DownloadFiles <- function(url=c(
				
				"https://donneespubliques.meteofrance.fr/client/document/doc_parametres_nivo_161.pdf",
				"https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Nivo/postesNivo.csv",
				"https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Nivo/postesNivo.json",
				"http://wiki.data.gouv.fr/images/9/9d/Licence_Ouverte.pdf"		
				),
				dest=system.file("dest",package="MeteoDataFrance"),
				method=c("wget","curl"),
				force.download=FALSE,...
				
				
				) {
					
				url  <- str_replace_all(url,"\n","")	
				out <- 0
				method <- method[1]
				
				
			
				
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
						
						out <- utils::download.file(url=url[[it]],destfile=destfile[[it]],method=method,...)
					}
				}
				
				
				out <- destfile
				return(out)
					
					
					
				}