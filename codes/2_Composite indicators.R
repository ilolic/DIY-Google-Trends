
# This R code constructs several composite Google Trends indicators with 
# - Principal Components Analysis (PCA), 
# - Dynamic Factor Model (DFM) and 
# - xgboost (XGB).

# INPUT:
# (1) CSV with adjusted GT monthly data 
# (2) CSV with target variable

# OUTPUT:
# (1) CSV with composite indicators

#Data: data_key

# rm(list = ls())

start_y <- 2004+1
# +1 due to differencing
start_m <- 1

target <- ts(read.csv("target.csv"),start=c(start_y,start_m),frequency = h)

n <- nrow(data_key)
nVar <- ncol(data_key)

#### TRAINING vs. TEST DATASET

#test_sample:  last 5 years, approx 28% of the data
#2005-2016
train_sample = seq(1,156)

#2017-2023
test_sample = seq(157,n)


# Frequency of GT data
h <- 12

# Standardizing data
# Omitting standardizing helps improving composite indicators construction

target0 <- scale(target,center = mean(target[train_sample]), 
                 scale = sd(target[train_sample]))

data_key0 <- data_key
for (i in 1:nVar){
  data_key0[,i] <- scale(data_key[,i],center = mean(data_key[train_sample,i]), 
                         scale = sd(data_key[train_sample,i]))
}


#Data: data_key0

# ---- (1) PCA COMPOSITE INDICATOR  ---- #

pca_model <- prcomp(data_key0[train_sample,])
X_pca <- prcomp(data_key0[train_sample,])$x[,1]

X_pca_test <- predict(pca_model, newdata = data_key0[test_sample,])[,1]



#------ (2) Dynamic Factor Model COMPOSITE INDICATOR ---#
# ic=ICr(data_key0[train_sample,])
# screeplot(ic)

# VARselect(ic$F_pca[, 1:4])
# Experiment with different r and p

r=1
p=1

dfm_model <- DFM(data_key0[train_sample,], r = r, p=p)
X_dfm <- dfm_model$F_pca[,1]

X_dfm_test <- predict(dfm_model, h=length(test_sample))$F_fcst[,1]




#----- (3) XGBOOST COMPOSITE INDICATOR --- #

# Experiment with parameters such as learning rate, nrounds, etc.

eta <- 0.1

nrounds <- 100

max_depth <- 6

xgb <- xgboost(data = data_key0[train_sample,],
               label = target0[train_sample], 
               eta = eta,
               verbose=0, #omit info about performance
               booster = "gblinear",
               nrounds = nrounds,
               max_depth = 6,
               objective = "reg:squarederror")


#estimate
X_XGB = predict(xgb, data_key0[train_sample,]) 
X_XGB_test = predict(xgb, data_key0[test_sample,])



data_train <- ts(cbind(data_key0[train_sample,],X_pca,X_dfm,X_XGB)*sd(target[train_sample])+mean(target[train_sample]),
                 start=c(start_y,start_m),frequency = h)
data_test <- ts(cbind(data_key0[test_sample,],X_pca_test,X_dfm_test,X_XGB_test)*sd(target[train_sample])+mean(target[train_sample]),
                start=c(2018,1),frequency = h)

# --- ESTIMATE BENCHMARK  MODEL --- #
ar_best <- auto.arima(target[train_sample], 
                      max.p = 5,
                      max.q = 5)
summary(ar_best)

#Select optimal parameters according to the above results
p <- 5
d <- 0
q <- 0

ar_model <- arima(target[train_sample], order = c(p,d,q))


data_train <- cbind(data_train,ts(ar_best$fitted,start=c(start_y,start_m),frequency = h) )
data_test <- cbind(data_test,ts(predict(ar_model, n.ahead = length(test_sample))$pred,
                                start=c(2018,1),frequency = h))


write.csv(data_train,"data_train.csv",row.names = F)
write.csv(data_test,"data_test.csv",row.names = F)




