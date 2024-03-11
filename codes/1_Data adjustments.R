#rm()
library(lubridate)
library(readxl)
library(BBmisc)
library(mFilter)
library(fpp)
library(rugarch)
library(forecast)
library(readxl)
library(lubridate)
library(dfms)
library(dplyr)
library(robustHD)
library(xgboost)
library(inTrees)
library(randomForest)
library(randomForestExplainer) 
library(pscl)
library(lmtest)
library(forecast)
library(lmtest)
library(sandwich)


# Adjustment steps 
# (1) Adjusting for breakpoints
# (2) Calculating log
# (3) HP filter
# (4) Extracting (adjusted) common trend 
# (5) Calculating m-o-m difference 


# Data beginning
start_y <- 2004
start_m <- 1


breakpoint <- function(GTdata,h) {
  n_time <- nrow(GTdata)
  n_keyw <- ncol(GTdata)
  
  
  # ---- Addressing Breaks ----
  # GT data has three breaks
  # 1) 01/01/2011 
  # 2) 01/01/2016 
  # 3) 01/01/2022
  # Approach: scale data by multiplying the post-break observations 
  # by the ratio with the 12-month averages
  
  start_y <- 2004
  start_m <- 1
  
  point1 <- 1+ (2011-start_y)*h -start_m + 1
  point2 <- 1 + (2016-start_y)*h -start_m + 1
  point3 <- 1 + (2022-start_y)*h -start_m + 1
  
  for (i in 1:n_keyw){
    ratio1 <- sum(GTdata[(point1-h):(point1-1),i])/sum(GTdata[(point1):(point1+h-1),i])
    ratio2 <- sum(GTdata[(point2-h):(point2-1),i])/sum(GTdata[(point2):(point2+h-1),i])
    ratio3 <- sum(GTdata[(point3-h):(point3-1),i])/sum(GTdata[(point3):(point3+h-1),i])
    
    GTdata[point1:n_time,i] <- GTdata[point1:n_time,i]*ratio1
    GTdata[point2:n_time,i] <- GTdata[point2:n_time,i]*ratio2
    GTdata[point3:n_time,i] <- GTdata[point3:n_time,i]*ratio3
  }
  return(GTdata)
}

logGT <- function(GTdata) {
  # ---- Calculating log ----
  
  GTdata[GTdata == 0] <- 1
  
  GTdata <- log(GTdata)
  return(GTdata)
}

filterGT <- function(GTdata,h) {
  n_time <- nrow(GTdata)
  n_keyw <- ncol(GTdata)
  
  # ---- HP filter ----
  
  GTdata.ts <- ts(GTdata,start=c(start_y, start_m),frequency = h)
  GTdata.trend <- NA*GTdata.ts
  
    for (i in 1:n_keyw){
        GTdata.trend[,i] <- hpfilter(GTdata.ts[,i], freq=h,type="frequency")$trend
  }
  return(GTdata.trend)
}


common_trend <- function(GTdata, trend,h){
  
  n_time <- nrow(GTdata)
  n_keyw <- ncol(GTdata)
  
  GTdata.adj <- NA*GTdata
  
  trend <- scale(trend)
  
  for (i in 1:n_keyw){
        mu <- mean(GTdata[,i])
        sig <- sd(GTdata[,i]) 
        GTdata.adj[,i] <- GTdata[,i] - (trend*sig+mu)
  }
  return(GTdata.adj)
}

diffGT <- function(GTdata, h){
  n_time <- nrow(GTdata)
  n_keyw <- ncol(GTdata)
  
  GTdata[(h+1):n_time,] <- GTdata[(h+1):n_time,]-GTdata[1:(n_time-h),]
  GTdata[1:h,] <- NA
  return(GTdata)
}

# ----     MAIN PART      ----

# Load downloaded (unadjusted) GT data

# CSV with first order Google-categories
# Can be omitted
#GT_cat1 <- read.csv("cat1.csv",header = TRUE)


GT_key <- read.csv("data_GTrends.csv",header = TRUE)

#monthly data
h <- 12


# --- CALCULATE COMMON TREND ON FIRST ORDER CATEGORIES --- #
# ---- PCA analysis to extract common trend ----
# comment lines if you want to skip some steps #

# GT_cat1.adj <- GT_cat1
# 
# GT_cat1.adj <- breakpoint(GT_cat1.adj,h)
# 
# GT_cat1.adj <- logGT(GT_cat1.adj)
# 
# GT_cat1.adj <- filterGT(GT_cat1.adj,h)
# 
# c_trend <- ts(prcomp(GT_cat1.adj)$x[,1],start=c(start_y, start_m),frequency = h)



# --- ADJUST KEYWORDS DATABASE --- #
# comment lines if you want to skip some steps #

GT_key.adj <- GT_key

GT_key.adj <- breakpoint(GT_key.adj,h)

GT_key.adj <- logGT(GT_key.adj)

GT_key.adj <- filterGT(GT_key.adj,h)

#GT_key.adj <- common_trend(GT_key.adj, c_trend,h)

GT_key.adj <- diffGT(GT_key.adj,h)


data_key<-GT_key.adj[-(1:12),]

