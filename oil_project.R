

install.packages("auto_ardl")
install.packages("dynlm")
install.packages("urca")
install.packages('ARDL')
install.packages('msm')
install.packages('tseries')
install.packages('forecast')
install.packages("car")


library(car)
library(tidyverse)
library(conflicted)
library(ggplot2)
library(dplyr)
library(auto_ardl)
library(dynlm)
library(urca)
library(ARDL)
library(msm)
library(lmtest)       
library(tseries)
library(zoo)
library(forecast)


df = read.csv('metric_data.csv')
df <- as_tibble(df)
df$Quarter <- as.yearqtr(df$Quarter, format = "%YQ%q")


last_7_obs <- (nrow(df) - 6):nrow(df)

df$log_OPEC85 <- df$log_OPEC 
df$log_OPEC85[last_7_obs] <- log(df$OPEC[last_7_obs] * 0.85)


df$log_OPEC70 <- df$log_OPEC 
df$log_OPEC70[last_7_obs] <- log(df$OPEC[last_7_obs] * 0.70)


df$L1log_OPEC85 <- lag(df$log_OPEC85, 1)    
df$dlog_OPEC85 <- c(NA, diff(df$log_OPEC85, lag = 1))             
df$dL1log_OPEC85 <- lag(df$dlog_OPEC85, 1)   
df$dL2log_OPEC85 <- lag(df$dlog_OPEC85, 2) 
df$dL3log_OPEC85 <- lag(df$dlog_OPEC85, 3) 
df$dL4log_OPEC85 <- lag(df$dlog_OPEC85, 4)


df$L1log_OPEC70 <- lag(df$log_OPEC70, 1)    
df$dlog_OPEC70 <- c(NA, diff(df$log_OPEC70, lag = 1))             
df$dL1log_OPEC70 <- lag(df$dlog_OPEC70, 1)   
df$dL2log_OPEC70 <- lag(df$dlog_OPEC70, 2) 
df$dL3log_OPEC70 <- lag(df$dlog_OPEC70, 3) 
df$dL4log_OPEC70 <- lag(df$dlog_OPEC70, 4)


df2 <- df[-c(1:4), ]
dfe <- ts(df, start = c(2015, 1), frequency = 4)
dfe2 <- ts(df2, start = c(2016, 1), frequency = 4)

# Just plotting the different values of OPEC being used
ggplot(df2, aes(x = Quarter)) +
  geom_line(aes(y = OPEC, color = "Baseline")) +
  geom_line(aes(y = exp(log_OPEC85), color = "15% Oil Price Reduction")) +
  geom_line(aes(y = exp(log_OPEC70), color = "30% Oil Price Reduction")) +
  scale_color_manual(name = "OPEC",
                     values = c("Baseline" = "black", 
                                "15% Oil Price Reduction" = "blue", 
                                "30% Oil Price Reduction" = "red")) +
  theme_minimal()


df_corr <- df2 %>%
  select(-Quarter)
cor(df_corr)

#############################################################################################################################
# 1. GDP 

# ARDL with autoselect 
gdp_model_auto <- auto_ardl(log_GDP ~ log_OPEC + log_CPI + log_REER, data = dfe, max_order = c(4, 4, 4, 4), selection = 'BIC')
summary(gdp_model_auto$best_model)
gdp_model_auto$top_orders
#ARDL(3,0,0,2) 
gdp_model_auto$best_model$order

gdp_ardl <- gdp_model_auto$best_model

gdp_uecm <- uecm(gdp_ardl)
summary(gdp_uecm)

gdp_test <- bounds_f_test(gdp_ardl, case =3)
gdp_test


#################################################
model1 <- ardl(log_GDP ~ log_OPEC + log_CPI + log_REER, data = dfe, order = c(4, 1, 1, 2))
summary(model1)
test1 <- bounds_f_test(model1, case=3)
test1
model2 <- uecm(model1, case=3)
summary(model2)

model2_multsr <- multipliers(model2, type = 8, se=TRUE)

plot_delay(model2_multsr, facets_ncol = 2, interval = 0.95)


# Showing the cointegration 
plot_lr2 <- data.frame(
  Time = df$Quarter[-c(1:4)],
  log_GDP = df$log_GDP[-c(1:4)],
  Cointegration_Term = coint_eq(model2, case=2)[-c(1:4)]
)
ggplot(plot_lr2, aes(x = Time)) +
  geom_line(aes(y = log_GDP, color = "log_GDP")) +  
  geom_line(aes(y = Cointegration_Term, color = "Coint. Eq.")) +
  labs(title = "log_GDP and Cointegration Term Over Time",
       x = "Time", y = "Log GDP") +
  scale_color_manual(name = "Legend",
                     values = c("log_GDP" = "black", 
                                "Coint. Eq." = "red")) +
  theme_minimal()

#################################################
#running diagnostic tests 
bgtest(model2)
# No autocorrelation 

jarque.bera.test(residuals(model2))
# normally distributed

bptest(model2)
# No heteroscedasticity

predict(model2)
forecast(model2, h=2)

###########################################################################

## GDP scenario analysis 

model2_lm <- to_lm(model2)
model2$model


gdp_newdata85 <- model2$model 
gdp_newdata85$`log_OPEC` <- df2$log_OPEC85  
gdp_newdata85$`L(log_OPEC, 1)` <- df2$L1log_OPEC85  
gdp_newdata85$`d(log_OPEC)` <- df2$dlog_OPEC85  
gdp_newdata85$`d(L(log_OPEC, 1))` <- df2$dL1log_OPEC85  


gdp_newdata70 <- model2$model  
gdp_newdata70$`log_OPEC` <- df2$log_OPEC70 
gdp_newdata70$`L(log_OPEC, 1)` <- df2$L1log_OPEC70  
gdp_newdata70$`d(log_OPEC)` <- df2$dlog_OPEC70 
gdp_newdata70$`d(L(log_OPEC, 1))` <- df2$dL1log_OPEC70  


gdp_15 <- predict(model2_lm, newdata = gdp_newdata85)
gdp_30 <- predict(model2_lm, newdata = gdp_newdata70)

# Plot dlog_GDP scenarios ie percentage change in GDP
plot_data <- data.frame(
  Time = df2$Quarter,
  dlog_GDP = df2$dlog_GDP,    # Actual observed log_GDP
  Forecast_15 = gdp_15,    # Simulated GDP for 15% oil price reduction
  Forecast_30 = gdp_30     # Simulated GDP for 30% oil price reduction
)

ggplot(plot_data, aes(x = Time)) +
  geom_line(aes(y = dlog_GDP*100, color = "Baseline")) +
  geom_line(aes(y = Forecast_15*100, color = "15% Oil Price Reduction")) +
  geom_line(aes(y = Forecast_30*100, color = "30% Oil Price Reduction")) +
  geom_vline(xintercept = as.numeric(as.yearqtr("2021 Q4")), linetype = "dashed", color = "grey", size = 0.5) +
  labs(title = "Scenario Analysis: Oil Price Sensitivities on GDP (Period to Period)",
       x = "Time", y = "% Change in GDP") +
  scale_color_manual(name = "Scenario",
                     values = c("Baseline" = "black", 
                                "15% Oil Price Reduction" = "blue", 
                                "30% Oil Price Reduction" = "red")) +
  theme_minimal()

# Plot log_GDP scenarios (log-level GDP)

# Cumulative sum to get log-level GDP from differences
log_GDP_15 <- df2$log_GDP[1] + cumsum(gdp_15)  
log_GDP_30 <- df2$log_GDP[1] + cumsum(gdp_30)  
fitted_GDP <- df2$log_GDP[1] + cumsum(fitted(model2))


plot_data <- data.frame(
  Time = df2$Quarter,
  Baseline_GDP = fitted_GDP,   
  Forecast_15 = log_GDP_15,   
  Forecast_30 = log_GDP_30   
)

# Not a fan of this plot 
ggplot(plot_data, aes(x = Time)) +
  geom_line(aes(y = Baseline_GDP, color = "Baseline")) +
  geom_line(aes(y = Forecast_15, color = "15% Oil Price Reduction")) +
  geom_line(aes(y = Forecast_30, color = "30% Oil Price Reduction")) +
  geom_vline(xintercept = as.numeric(as.yearqtr("2021 Q4")), linetype = "dashed", color = "grey", size = 1) +
  labs(title = "Scenario Analysis: Oil Price Reductions and Log-Level GDP",
       x = "Time", y = "log_GDP") +
  scale_color_manual(name = "Scenario",
                     values = c("Baseline" = "black", 
                                "15% Oil Price Reduction" = "blue", 
                                "30% Oil Price Reduction" = "red")) +
  theme_minimal()


#########################################################################################################
###### 2. REER                ###################################
#########################################################################################################



reer_model_auto <- auto_ardl(log_REER ~ log_OPEC + log_CPI + log_Total_Exports, data = df2, max_order = c(4, 4, 4, 4), selection = 'BIC')
summary(reer_model_auto$best_model)
reer_model_auto$top_orders
#ARDL(1,0,1,0) 
reer_model_auto$best_model$order

reer_model <- ardl(log_REER ~ log_OPEC + log_CPI + log_Total_Exports, data = df2, order = c(2, 1, 1, 1))
summary(reer_model)

test2 <- bounds_f_test(reer_model, case=3)
test2
###### No cointegration to be found, need to use 1st differences 

acf(df2$log_REER)

reer_model_auto <- auto_ardl(dlog_REER ~ dlog_OPEC + dlog_CPI + dlog_Total_Exports, data = dfe, max_order = c(4, 4, 4, 4), selection = 'AIC')
summary(reer_model_auto$best_model)
reer_model_auto$top_orders
#ARDL(1,0,1,0) 
reer_model_auto$best_model$order

reer_model <- ardl(dlog_REER ~ dlog_OPEC + dlog_CPI + dlog_Total_Exports, data = dfe, order = c(2, 0, 2, 0))
summary(reer_model)
#running diagnostic tests 
bgtest(reer_model)
# No autocorrelation 

jarque.bera.test(residuals(reer_model))
# normally distributed

bptest(reer_model)
# No heteroscedasticity
plot(residuals(reer_model))

# Use the auto_ardl summary
# Shows oil price shocks have no noticeable impact on REER
# Positive relationship between CPI and REER


#######################################################################################
########## 3. Exports #################################################################
#######################################################################################

exports_model_auto <- auto_ardl(log_Total_Exports ~ log_OPEC + log_CPI , data = dfe, max_order = c(4, 4, 4), selection = 'AIC')
summary(exports_model_auto$best_model)
exports_model_auto$top_orders
#ARDL(1,0,0,0) 
exports_model_auto$best_model$order

bounds_f_test(exports_model_auto$best_model, case=3)

exports_ardl <- ardl(log_Total_Exports ~ log_OPEC + log_CPI, data = dfe, order = c(2, 1, 1))
summary(exports_ardl)


exports_ecm <- uecm(exports_ardl, case=3)
summary(exports_ecm)

#### Need to finish this 


####################################################################################
############## 4. Non-Oil Sector 
####################################################################################

non_oil_model_auto <- auto_ardl(log_Non_oil_GDP ~ log_OPEC + log_CPI + log_REER , data = dfe, max_order = c(4, 4, 4, 4), selection = 'AIC')
summary(non_oil_model_auto$best_model)
non_oil_model_auto$top_orders
#ARDL(1,0,0,0) 
non_oil_model_auto$best_model$order





