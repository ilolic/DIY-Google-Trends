# This R code downloads data from Google Trends.
# INPUT: Csv file with the following columns
#    (*) keyword	
#    (*) topic (if topic=1, code retrieves topic data)	
#    (*) category number

# TOPIC search
# If you want to perform topic search, then you need to go to trends.google.com 
# and retrieve URL
# E.g. searching for topic unemployment
# The address is 
# https://trends.google.com/trends/explore?date=all&geo=US&q=%2Fm%2F07s_c&hl=en
# We extract the part after the q
# q=%2Fm%2F07s_c and put that string in keyword column
# This needs to be done prior and included in the csv file

# CATEGORY SEARCH
# For obtaining category search volume index: keyword="", topic=0, category=number

# KEYWORD search
# For obtaining keyword search volume index: keyword="term", topic=0, category=number or 0 (no category)


library(gtrendsR)
library(lubridate)
library(readxl)
library(BBmisc)

# We used this code for download, but unfortunately, Google corrupts data 
# (puts blocks of zeros), slows down, or even disables downloading data through gtrendsR.
# This code implements multiple attempts to obtain the data, but at the end you will 
# maybe need to manually download.

longest_consecutive_zeros <- function(x) {
  max_length <- 0
  current_length <- 0
  
  for (i in 1:length(x)) {
    if (x[i] == 0) {
      current_length <- current_length + 1
    } else {
      if (current_length > max_length) {
        max_length <- current_length
      }
      current_length <- 0
    }
  }
  
  return(max(max_length, current_length))
}


# Set your Google account credentials - it sometimes helps with download
setHandleParameters( user = "@gmail.com", password = "X", domain = NULL, proxyhost = NULL, proxyport = 8080, proxyauth = 15, extra_curl_opts = list() )


kwlist <- read.table("keys.txt",header=T)

# Define the global parameters for function gTrends
location = "US"
search_source = "web"
#category_log = 0
period = "all"

# If period = "all", then n_time is defined 
# Note: Check if the value for n_time is as expected. 
#       Google produces monthly value for the last month with small delay.

n_time <- (year(Sys.Date())-2004)*12+month(Sys.Date())
n_keyw <- nrow(kwlist)



GTdata <- array(NA, dim = c(n_time,n_keyw))

# ---- Google Trends ----
# Interest over time



# When Google detects scraping (multiple attempts to download) it returns data with
# blocks of zeros. This code implements checkpoint for this. 
# In case of zeros, it initiates multiple downloads with pauses. 
# Sometimes it is necessary to pause this code for several hours (by yourself, not implemented).



block_zeros <- 0

zeros_i <- 1

while (block_zeros == 0){

      
      #
      k_stop=1
      
      while ((sum(is.na(GTdata[1,])) > 0) && (k_stop < 4)){
        error_seq <- which(is.na(GTdata[1,]))
        
        for(i in error_seq){
          
          if (nchar(kwlist$keyword[i])==0){
            dataGT <- try(gtrends(keyword = NA,
                                  geo = location,
                                  time = period,
                                  gprop = search_source,
                                  category = kwlist$category[i],
                                  hl = "en-US",
                                  compared_breakdown = FALSE,
                                  low_search_volume = FALSE,
                                  tz = 0,
                                  cookie_url = "http://trends.google.com/Cookies/NID",
                                  onlyInterest = TRUE),
                          silent=TRUE)
            
            
            
            if (!is.error(dataGT))
              if (length(dataGT$interest_over_time$hits) >0)
                GTdata[,i] <- try(dataGT$interest_over_time$hits,
                                    silent=TRUE)
          }
          
          if (nchar(kwlist$keyword[i])>0){
        
                    dataGT <- try(gtrends(keyword = kwlist$keyword[i],
                                      geo = location,
                                      time = period,
                                      gprop = search_source,
                                      category = kwlist$category[i],
                                      hl = "en-US",
                                      compared_breakdown = FALSE,
                                      low_search_volume = FALSE,
                                      tz = 0,
                                      cookie_url = "http://trends.google.com/Cookies/NID",
                                      onlyInterest = TRUE),
                                   silent=TRUE)
                  
                  if (!is.error(dataGT))
                    if (length(dataGT$interest_over_time$hits) >0)
                        GTdata[,i] <- try(dataGT$interest_over_time$hits,
                                       silent=TRUE)

          }
        }
       
        k_stop <- k_stop + 1
        
        if (k_stop == 4)
          if (sum(is.na(GTdata[1,])) > 0) {
            k_stop=1
            print(paste("Break within for",i, " - ", Sys.time()))
            Sys.sleep(20)
            setHandleParameters( user = "X@gmail.com", password = "X", domain = NULL, proxyhost = NULL, proxyport = 8080, proxyauth = 15, extra_curl_opts = list() )
            
          }
      }#while end
      
      block_zeros <- 1
      
      for (i in 1:n_keyw){
        if ((!is.na(GTdata[1,i]))&&(longest_consecutive_zeros(GTdata[,i])>3)){
          GTdata[,i] <- GTdata[,i]*NA
          block_zeros <- block_zeros*0 
        }
        
      }
      if (block_zeros == 0){
          print(paste("Break",zeros_i, " - ", Sys.time()))
          Sys.sleep(20)
          zeros_i <- zeros_i + 1
          setHandleParameters( user = "X@gmail.com", password = "X", domain = NULL, proxyhost = NULL, proxyport = 8080, proxyauth = 15, extra_curl_opts = list() )
      }
}#while block zeros end


write.csv(GTdata, "data_GTrends.csv")



