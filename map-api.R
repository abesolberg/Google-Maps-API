## Google Maps API ##
# Modified from Jose Gonzalez's GoogleMapsAndR.md https://gist.github.com/josecarlosgonz/6417633
# August 22, 2019

library(RCurl)
library(RJSONIO)
library(plyr)
library(tidyverse)

url <- function(address, return.call = "json", sensor = "false" , key) {
  
  root <- "https://maps.google.com/maps/api/geocode/"
  
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, "&key=" , key , sep = "")
  
  return(URLencode(u))
}

# This function will return the lat, long, location_type , and formatted address

geocode <- function(address, api.key , verbose=FALSE) {
  
  if(verbose) cat(address,"\n")
 
   u <- url(address , key = api.key)
  
   doc <- getURL(u)
  
   x <- fromJSON(doc,simplify = FALSE)
 
    if(x$status=="OK") {
    
      lat <- x$results[[1]]$geometry$location$lat
    
      lng <- x$results[[1]]$geometry$location$lng
    
      location_type  <- x$results[[1]]$geometry$location_type
    
      formatted_address  <- x$results[[1]]$formatted_address
    
      return(c(lat, lng, location_type, formatted_address))
    
      Sys.sleep(0.5)
    
  } else {
    
    return(c(NA,NA,NA, NA))

  }
}

# This function will return a json response from the api call

geocall <- function(address, api.key , verbose=FALSE) {
 
   if(verbose) cat(address,"\n")
  
  u <- url(address , key = api.key)
  
  doc <- getURL(u)
  
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status=="OK") {
    
    return(x)
    
    Sys.sleep(0.5)
    
  } else {
    
    return(x$error_message)
    
  }
}

# To geocode a vector of addresses

locations <- c("Wrigley Field" , "Yankee Stadium" , "Fenway Park")

geocoded  <- ldply(locations, .progress = "text" , function(x) geocode(x , api.key = key))

names(geocoded) <- c("lat","lon","location_type", "formatted")