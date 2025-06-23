#Code for the final project of undergraduate degree in Economics at Sacramento
#State University

#"The Impact of the U.S. Federal Funds Rate on Quarterly S&P 500 Returns 
# from 1993-2024"

#Install and load packages
install.packages("tseries")
install.packages("ARDL")
install.packages("dynlm")
install.packages("lmtest")
install.packages("skedastic")

library(readxl)
library(tseries)
library(ARDL)
library(dynlm)
library(car)
library(lmtest)
library(skedastic)

#Load data
Stock_market <- read_excel("~/ECON 145/Data.xlsx", 
                   sheet = "Data")
save(Stock_market, file = "StockMarket.RData")
load("StockMarket.RData")
#A)Data preparation

# This analysis will require that the variables are in the logarithmic scale.
# Given the existence of negative values for the variables S&P500 and yield
# curve, a constant was added to each variable.

log_stock_market <- Stock_market #copy to work on
for (col in names(log_stock_market)[-1]) { 
  min_val <- min(log_stock_market[[col]], na.rm = TRUE)
  constant <- abs(min_val) + 1
  log_stock_market[[col]] <- log(log_stock_market[[col]] + constant)
}
# Explanation:
# 12. "for" sets a loop, so R does this for each variable. The "-1" just excludes 
# the first column of the dataset, which is the date. "Names" just means columns
# 13. This calculates the minimum value. na.rm = TRUE means that it removes any 
# missing values.
# 14. This just takes the absolute value of the minimum, and adds 1. By doing so, 
# all the values will be positive
# 15. This adds the constant to every value of the column. On this line, the 
# log transformation was made. 

# B) Check for stationarity

# Dickey-Fuller Test: a p-value lower than 0.05 rejects the null of
# non-stationarity. 

adf.test(log_stock_market$SP500, alternative = "stationary") #Stationary
adf.test(log_stock_market$FEDFUNDS, alternative = "stationary") #non-stationary
adf.test(log_stock_market$INF, alternative = "stationary") #non-stationary
adf.test(log_stock_market$UNRATE, alternative = "stationary") #non-stationary
adf.test(log_stock_market$RGDP_GR, alternative = "stationary") #stationary
adf.test(log_stock_market$UMCSENT, alternative = "stationary") #non-stationary
adf.test(log_stock_market$MICH, alternative = "stationary") #stationary
adf.test(log_stock_market$M2, alternative = "stationary") #non-stationary
adf.test(log_stock_market$TREG, alternative = "stationary") #non-stationary
adf.test(log_stock_market$T10Y2Y, alternative = "stationary") #non-stationary


# To fix the stationarity issue, just take the first difference
non_stationary <- c('FEDFUNDS', "INF", "UNRATE", "UMCSENT", "M2", "TREG", 
                    "T10Y2Y")
for (col in non_stationary) {
  log_stock_market[[col]] <- c(NA, diff(log_stock_market[[col]],differences = 1))
}
# After the previous loop, there are NA values for the variables that were
# differentiated. Remove them
log_stock_market <- log_stock_market[-1,]
# Check again for stationarity
adf.test(log_stock_market$SP500, alternative = "stationary") #Stationary
adf.test(log_stock_market$FEDFUNDS, alternative = "stationary") #stationary
adf.test(log_stock_market$INF, alternative = "stationary") #stationary
adf.test(log_stock_market$UNRATE, alternative = "stationary") #stationary
adf.test(log_stock_market$RGDP_GR, alternative = "stationary") #stationary
adf.test(log_stock_market$UMCSENT, alternative = "stationary") #stationary
adf.test(log_stock_market$MICH, alternative = "stationary") #stationary
adf.test(log_stock_market$M2, alternative = "stationary") #stationary
adf.test(log_stock_market$TREG, alternative = "stationary") #stationary
adf.test(log_stock_market$T10Y2Y, alternative = "stationary") #stationary
adf.test(log_stock_market$TB3MS, alternative = "stationary") #stationary
# After taking the first differences, now all the variables are stationary


#Run the ARDL

model <- auto_ardl(SP500 ~ FEDFUNDS + INF + UNRATE + RGDP_GR +
                     UMCSENT + MICH + M2 + TREG + T10Y2Y, 
                   data = log_stock_market, 
                   max_order = 5, 
                   ic = "AIC")
summary(model)
best_model <- model$best_model
summary(best_model)

# Now run diagnostics

# Check for multicollinearity
# Extract a model that does not include the lags
non_lagged_model <- lm(SP500 ~ FEDFUNDS + INF + UNRATE + RGDP_GR + UMCSENT +
                         MICH + M2 + TREG + T10Y2Y, 
                       data = log_stock_market)
vif_values <- vif(non_lagged_model)
print(vif_values)
cor(log_stock_market[, c("FEDFUNDS", "INF", "UNRATE", "RGDP_GR", "UMCSENT", 
                         "MICH", "M2", "TREG", "T10Y2Y", "TB3MS")])



#All the variables presented VIFs lower than 10, so no problem. 

#Durbin-Watson Test for autocorrelation

durbinWatsonTest(model$best_model)
#This test states that values a p-value lower than 0.05 indicate that there is 
#autocorrelation. Since the p-value for this model is above 0.05, we fail to 
#reject the null, meaning that there is no statistically significant evidence
#of autocorrelation. 

#White's Test for heteroskedasticity
residuals<-residuals(model$best_model)
white_test <- bptest(residuals ~ fitted(model$best_model) +
                       I(fitted(model$best_model)^2))
print(white_test)

#fitted(model$best_model): Uses the fitted (predicted) values from the ARDL
#model as a regressor.
#I(fitted(model$best_model)^2): Adds the squared fitted values as an additional
#regressor, which is the key component for White's test. By including both the 
#fitted values and their squares, this specification allows for testing 
#non-linear forms of heteroscedasticity
#The results of this tests indicate that there is heteroskedasticity in my 
#model. To address this, use robust standard errors. 
robust_se <- coeftest(model$best_model, vcov = vcovHC(model$best_model,
                                                      type = "HC"))
print(robust_se)
#Now this is the corrected model
#Done!
