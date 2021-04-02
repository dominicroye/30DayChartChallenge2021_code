##################################
##################################
# Download of lightning data     #
# from Meteogalicia              #
# Autor: Dominic Royé            #
# Email: dominic.roye@gmail.com  #
##################################
##################################

#required packages: XML

lightning_download <- function(date_from,date_to){
  
  if((as.Date(date_to,format="%d/%m/%Y")-as.Date(date_from,format="%d/%m/%Y"))>7){
    
    temp1 <- NULL
    
    if(((as.numeric(as.Date(date_to,format="%d/%m/%Y")-as.Date(date_from,format="%d/%m/%Y")))%%7)==0){
      temp_seq <- seq(as.Date(date_from,format="%d/%m/%Y"),as.Date(date_to,format="%d/%m/%Y"),7)
      temp_seq <- as.character(format(temp_seq,format="%d/%m/%Y"))
    }else{
      
      temp_seq <- seq(as.Date(date_from,format="%d/%m/%Y"),as.Date(date_to,format="%d/%m/%Y"),7)
      temp_seq <- c(as.character(format(temp_seq,format="%d/%m/%Y")),date_to)
    }
    
    
    for(i in 1:(length(temp_seq)-1)){
      url_base <- "http://servizos.meteogalicia.es/rss/observacion/rssRaios.action?request_locale=gl&dataIni="
      url_end <- "&dataFin="
      
      url <- paste(url_base,temp_seq[i],url_end,temp_seq[i+1],sep="")
      
      cfaincidents <- XML::xmlInternalTreeParse(url)
      
      strikes <- as.numeric(sapply(XML::getNodeSet(cfaincidents, "//Raios:total"),XML::xmlValue))
      
      if(all(strikes==0)) next
      
      if(any(strikes==0)){
        strikes_data <- sapply(XML::getNodeSet(cfaincidents, "//Raios:data"),XML::xmlValue)
        strikes_data <- strikes_data[-which(strikes==0)]
      }else{
        
        strikes_data <- sapply(XML::getNodeSet(cfaincidents, "//Raios:data"),XML::xmlValue)
      }
      
      temp <- as.data.frame(t(sapply(XML::getNodeSet(cfaincidents, "//Raios:raio"),XML::xmlAttrs)))
      
      temp[,"day"] <- rep(1:length(strikes),strikes)
      
      temp <- split(temp,temp$day)
      
      names(temp) <- strikes_data
      
      temp <- as.data.frame(do.call(rbind,temp),stringsAsFactors=FALSE)
      temp[,"date"] <- as.Date(row.names(temp),format="%d/%m/%Y")
      row.names(temp) <- NULL
      
      temp1 <- rbind(temp1,temp)
      
    }
    
    return(temp1)
    
  }else{
    
    
    url_base <- "http://servizos.meteogalicia.es/rss/observacion/rssRaios.action?request_locale=gl&dataIni="
    url_end <- "&dataFin="
    
    url <- paste(url_base,date_from,url_end,date_to,sep="")
    
    cfaincidents <- XML::xmlInternalTreeParse(url)
    
    strikes <- as.numeric(sapply(XML::getNodeSet(cfaincidents, "//Raios:total"),XML::xmlValue))
    
    if(any(strikes==0)){
      which(strikes==0)
      strikes_data <- sapply(XML::getNodeSet(cfaincidents, "//Raios:data"),XML::xmlValue)
      strikes_data <- strikes_data[-which(strikes==0)]
    }else{
      
      strikes_data <- sapply(XML::getNodeSet(cfaincidents, "//Raios:data"),XML::xmlValue)
    }
    
    temp <- as.data.frame(t(sapply(XML::getNodeSet(cfaincidents, "//Raios:raio"),XML::xmlAttrs)))
    
    temp[,"day"] <- rep(1:length(strikes),strikes)
    
    temp <- split(temp,temp$day)
    
    names(temp) <- strikes_data
    
    temp <- as.data.frame(do.call(rbind,temp),stringsAsFactors=FALSE)
    temp[,"Date"] <- as.Date(row.names(temp),format="%d/%m/%Y")
    row.names(temp) <- NULL
    temp[,c(1,3:6)] <- as.data.frame(apply(temp[,c(1,3:6)],2,as.numeric),stringsAsFactors=FALSE)
    temp <- temp[,-6]
    return(temp)
  }
}

