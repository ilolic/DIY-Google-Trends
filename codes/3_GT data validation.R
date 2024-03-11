

# This R code compares composite indicators with benchmark 
# - Principal Components Analysis (PCA), 
# - Dynamic Factor Model (DFM) 
# - XGBoost (XGB) and
# - AR benchmark.

# Comparison criteria:
#1     coincident correlation with qoq growth rate, period 2005Q1-2017Q4 insample
#2     coincident correlation with qoq growth rate, period 2018Q1-2022Q4 out-of-sample
#3     RMSE (insample-GDP)^2, period 2005Q1-2017Q4 insample
#4     RMSE (outsample-GDP)^2, period 2018Q1-2022Q4 out-of-sample

# INPUT:
# (1) CSV with composite indicators estimates insample 
# (2) CSV with composite indicators estimates out-of-sample
# (3) CSV with target variable

# OUTPUT:
# (1) CSV with calculated criteria
# (2) CSV with Diebold Mariano test results

target <- ts(read.csv("target.csv"),start=c(start_y,start_m),frequency = h)

# Logical indicator for non-extreme values
mu <- mean(target)
sig <- sd(target)
extreme1 = abs(scale(target[train_sample],center = mu, scale = sig)) <= 3
extreme2 = abs(scale(target[test_sample],center = mu, scale = sig)) <= 3

n_est <- ncol(data_test)

validationTable <- matrix(NA, ncol=14, nrow=n_est)

for (i in 1:n_est){
  
  # Coincident Correlation
  validationTable[i,1] <- cor(data_train[,i],target[train_sample])
  validationTable[i,2] <- cor(data_test[,i],target[test_sample])
  
  
  # 1-period ahead Leading Correlation
  validationTable[i,3] <- cor(data_train[-nrow(data_train),i],target[train_sample[-1]])
  validationTable[i,4] <- cor(data_test[-nrow(data_test),i],target[test_sample[-1]])
  
  # RMSE
  validationTable[i,5] <- sqrt(sum((data_train[,i]-target[train_sample])^2/length(train_sample)))
  validationTable[i,6] <- sqrt(sum((data_test[,i]-target[test_sample])^2/length(test_sample)))
  
  # RMSE without outlier
  validationTable[i,7] <-  sqrt(sum((data_train[extreme1,i]-target[train_sample[extreme1]])^2/length(extreme1)))
  validationTable[i,8] <- sqrt(sum((data_test[extreme2,i]-target[test_sample[extreme2]])^2/length(extreme2)))
  
  # # 1-period ahead Leading RMSE
  validationTable[i,9] <- sqrt(sum((data_train[-nrow(data_train),i]-target[train_sample[-1]])^2/length(train_sample[-1])))
  validationTable[i,10] <- sqrt(sum((data_test[-nrow(data_test),i]-target[test_sample[-1]])^2/length(test_sample[-1])))

  # DACT test
  t1<- try(DACTest(data_train[,i],target[train_sample],test="PT",conf.level=0.95))
  t2<- try(DACTest(data_test[,i],target[test_sample],test="PT",conf.level=0.95))
  
  validationTable[i,11] <- validationTable[i,12] <- -1
  
  if(!("try-error" %in% class(t1))) validationTable[i,11] <-t1$p.value
  if(!("try-error" %in% class(t2))) validationTable[i,12] <-t2$p.value
}

#

for (i in 1:(n_est-1)){
  
  # OLS: target(t) = b0 + b1*ARIMA(p,d,q) + b2 GT_i(t)
  model1 <- lm(target[test_sample]~data_test[,n_est]+data_test[,i])
  rez1 <- coeftest(model1, df = Inf, vcov = vcovHAC)
  validationTable[i,13] <- rez1[3,4]
  
  # OLS: target(t+1) = b0 + b1*ARIMA(p,d,q) + b2 GT_i(t)
  model2 <- lm(target[test_sample[-1]]~data_test[-nrow(data_test),n_est]+data_test[-nrow(data_test),i])
  rez2 <- coeftest(model2, df = Inf, vcov = vcovHAC)
  validationTable[i,14] <- rez2[3,4]
  
}

DMtest_results <- matrix(NA,n_est,n_est)

for (i in 1:(n_est-1))
  for (j in (i+1):n_est){
    DMtest_results[i,j]<- dm.test(data_test[,i],data_test[,j], alternative = "two.sided",h=1)$p.value
}

colnames(validationTable) <- c("Corr_train", "Corr_test", "Corr_lead_train", "Corr_lead_test",
                               "RMSE_train", "RMSE_test", "RMSE_noextreme_train", "RMSE_noextreme_test","RMSE_lead_train", "RMSE_lead_test",
                               "DACT_train", "DACT_test",
                               "AR_pvalue_test", "AR_pvalue_lead_test")
    
write.csv(validationTable, "ValidationTable.csv",row.names = T)

write.csv(DMtest_results, "DMtest.csv",row.names = T)


