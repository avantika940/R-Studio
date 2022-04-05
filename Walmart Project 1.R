install.packages("ggplot2")
install.packages("xts")
install.packages("gbm")
install.packages("dplyr")
install.packages("broom")
install.packages("forecast")
install.packages("readr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("glmnet")

library(dplyr)
library(broom)
library(forecast)
library(readr)
library(tidyr)
library(lubridate)
library(glmnet)
library(gbm)
library(xts)
library(ggplot2)
setwd=c("Downloads")
store_raw = read.csv("stores.csv")
feature_raw <- read.csv("features.csv")
train <- read.csv("train.csv")
test_final <- read.csv("test.csv")
store_raw$Store <- as.factor(store_raw$Store)
train$Store <- as.factor(train$Store)
train$Dept <- as.factor(train$Dept)
test_final$Store <- as.factor(test_final$Store)
test_final$Dept <- as.factor(test_final$Dept)
feature_raw$Store <- as.factor(feature_raw$Store)
train %>% summarize(Total_stores = n_distinct(Store))
train %>% summarize(Total_Dept = n_distinct(Dept))
train %>% summarize(min_date = min(Date), max_date = max(Date), total_weeks = difftime(min_date,max_date, unit = "weeks"))
train %>% group_by(Store, Dept) %>% summarize(count_wk = n_distinct(Date)) %>% ggplot(aes(x = count_wk)) + geom_histogram()
ggplot(store_raw, aes(x = Size)) + geom_histogram(binwidth = 15000) + facet_grid(Type~.)
train %>% left_join(store_raw, by = "Store") %>% ggplot(aes(x = Weekly_Sales)) + geom_histogram() + facet_grid(Type~.) + scale_x_log10()

train_all_factors <- train %>% left_join(store_raw, by = "Store") %>% left_join(feature_raw, by = c("Store", "Date")) %>% mutate(first_wk = ifelse(mday(Date) <= 7, TRUE, FALSE))

train_all_factors %>% group_by(Type, Date) %>% summarize(sales = sum(Weekly_Sales)) %>% ggplot(aes(x = Date, y = sales, col = Type)) + geom_line() + scale_y_log10() + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 weeks") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


train_all_factors %>% group_by(Date,first_wk) %>% summarize(sales = sum(Weekly_Sales)) %>% ggplot(aes(x = first_wk, y = sales)) + geom_boxplot()



train_all_factors %>% ggplot(aes(x = CPI, y = Weekly_Sales)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales~CPI, data = train_all_factors))), colour = "red")
train_all_factors %>% ggplot(aes(x = Temperature, y = Weekly_Sales)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales~Temperature, data = train_all_factors))), colour = "red")


feature_raw %>% filter(Store == c(1,25,33,41)) %>% ggplot(aes(x = Date, y = CPI, col = Store)) + geom_line()





#Creating Exhaustive table with all weeks and store combination and joining feature to get CPI and Unemployement exhaustive data set to implement time series forecast for missing 2013 data
CPI_Unemp_exh <- feature_raw %>% select(Store, Date) %>% complete(Store, Date) %>% left_join(feature_raw, by = c("Store","Date")) %>% select(Store, Date, CPI, Unemployment)
# Store list to put in loop
store_lst <- distinct(CPI_Unemp_exh,Store)
# Dates to be forecasted
date_fore <- CPI_Unemp_exh %>% filter(is.na(CPI))  %>% select(Date) %>% distinct() %>% arrange()
# Function to time series forecast CPI by store
forecast <- NULL
for (i in unique(CPI_Unemp_exh$Store)) {
  CPI_ts <- CPI_Unemp_exh %>% filter(Date<= "2013-04-26", Store == i) %>% select(CPI) %>% ts(start = c(2010,2,5), end = c(2013,4,26), frequency = 52)
  fit <- auto.arima(CPI_ts)
  frcst <- as.data.frame(forecast(fit,13)) %>% select(`Point Forecast`) %>% rename(CPI = `Point Forecast`)
  fore <- merge(i,date_fore, all = T) %>% cbind(frcst)
  forecast <- rbind(forecast, fore)
}

plot(forecast(fit,13))





feature_raw %>% filter(Store == c(1,39,33,44)) %>% ggplot(aes(x = Date, y = Unemployment, col = Store)) + geom_line()






# Function to forecast Unemployment
forecast_Un <- NULL

for (i in unique(CPI_Unemp_exh$Store)) {
  Unemp_ts <- CPI_Unemp_exh %>% filter(Date<= "2013-04-26", Store == i) %>% select(Unemployment) %>% ts(start = c(2010,2,5), end = c(2013,4,26), frequency = 52)
  fit <- auto.arima(Unemp_ts)
  frcst <- as.data.frame(forecast(fit,13)) %>% select(`Point Forecast`) %>% rename(Unemployment = `Point Forecast`)
  fore <- merge(i,date_fore, all = T) %>% cbind(frcst)
  forecast_Un <- rbind(forecast_Un, fore)
}

plot(forecast(fit,13))






# Combining both the forecast
forecast_CPI_Un <- inner_join(forecast, forecast_Un, by = c("x", "Date")) %>% rename(Store = x)

# Joining it with feature_raw and substituting the missing values with forecast
feature_raw1 <- feature_raw %>% left_join(forecast_CPI_Un, by = c("Store","Date")) %>% mutate(CPI = ifelse(is.na(CPI.x),CPI.y,CPI.x),Unemployment = ifelse(is.na(Unemployment.x),Unemployment.y,Unemployment.x)) %>%
  select(-Unemployment.x, -Unemployment.y, -CPI.x, -CPI.y)






# Function to forecast Unemployment
forecast_Un <- NULL

for (i in unique(CPI_Unemp_exh$Store)) {
  Unemp_ts <- CPI_Unemp_exh %>% filter(Date<= "2013-04-26", Store == i) %>% select(Unemployment) %>% ts(start = c(2010,2,5), end = c(2013,4,26), frequency = 52)
  fit <- auto.arima(Unemp_ts)
  frcst <- as.data.frame(forecast(fit,13)) %>% select(`Point Forecast`) %>% rename(Unemployment = `Point Forecast`)
  fore <- merge(i,date_fore, all = T) %>% cbind(frcst)
  forecast_Un <- rbind(forecast_Un, fore)
}

plot(forecast(fit,13))






# Combining both the forecast
forecast_CPI_Un <- inner_join(forecast, forecast_Un, by = c("x", "Date")) %>% rename(Store = x)

# Joining it with feature_raw and substituting the missing values with forecast
feature_raw1 <- feature_raw %>% left_join(forecast_CPI_Un, by = c("Store","Date")) %>% mutate(CPI = ifelse(is.na(CPI.x),CPI.y,CPI.x),Unemployment = ifelse(is.na(Unemployment.x),Unemployment.y,Unemployment.x)) %>%
  select(-Unemployment.x, -Unemployment.y, -CPI.x, -CPI.y)







feature_fi <- feature_raw1 %>% mutate_at(c("MarkDown1","MarkDown2","MarkDown3","MarkDown4","MarkDown5"),funs(ifelse(.<0 |                          is.na(.),0,.))) %>% mutate(mkdn_flag = (MarkDown1 !=0 | MarkDown2 !=0 | MarkDown3 !=0 | MarkDown4 !=0 | MarkDown5 !=0                ), mkdn_hol = IsHoliday | mkdn_flag)
train_all_fac <- train %>% left_join(feature_fi, by = c("Store","Date"))
# Treating outliers outside 1.5*IQR range at store-dept level whenever its not holiday or no markdown given
train_outlier_treated <- train_all_fac %>% group_by(Store, Dept) %>% mutate(perc_25 = quantile(Weekly_Sales,0.25), perc_75 = quantile(Weekly_Sales,0.75), iqr_sales = IQR(Weekly_Sales), Wkly_sales_treated = ifelse(Weekly_Sales<perc_25 - 1.5* iqr_sales & !mkdn_hol, perc_25 - 1.5* iqr_sales, ifelse(Weekly_Sales > perc_75 + 1.5* iqr_sales & !mkdn_hol, perc_75 + 1.5* iqr_sales,Weekly_Sales)))

# Percentage of outliers 
paste(round(mean(train_outlier_treated$Weekly_Sales != train_outlier_treated$Wkly_sales_treated) * 100,2),"%")



# Creating exhaustive set of store-dept-date combination
store_dept_date_exh <- train %>% select(Store, Dept, Date, IsHoliday) %>% rbind(test_final) %>% select(-IsHoliday) %>% complete(nesting(Store,Dept),Date)

# Checking number of data points for each store-dept combinations
cnt_obs_store_dept <- train %>% group_by(Store,Dept) %>% summarize(n())

# Filtering store-Dept combinations that will be forecasted using Time Series i.e. n_obs >= 104
time_series_store_dept <- cnt_obs_store_dept %>% filter(`n()` >= 104) %>% inner_join(store_dept_date_exh, by = c("Store", "Dept")) %>% select(-`n()`) %>% left_join(train, by = c("Store","Dept","Date")) %>% select(-IsHoliday) %>% mutate(id = paste(Store,Dept, sep = "_")) %>% ungroup()

# Treating missing weeks data in between normal weeks by taking avg of previous and next week
time_series_store_dept_mistreat <- time_series_store_dept %>% arrange(Store, Dept, Date) %>% group_by(Store, Dept) %>% 
  mutate(lead1 = lead(Weekly_Sales), lag1 = lag(Weekly_Sales)) %>% ungroup() %>%
  rowwise() %>% mutate(wkly_sales_treated = ifelse(is.na(Weekly_Sales) & Date < "2012-11-02",                                          mean(c(lead1,lag1),na.rm = T),Weekly_Sales))

date_train <-  seq(as.Date("2010-02-05"), as.Date("2012-10-26"), by = 7)
sales_ts <-  time_series_store_dept_mistreat %>% filter(Date < "2012-11-02", id == "45_98") %>% mutate(wkly_sales_treated = ifelse(is.na(wkly_sales_treated)|is.nan(wkly_sales_treated),0,wkly_sales_treated)) %>% select(wkly_sales_treated) %>% xts(order.by = date_train)








sales.ts.df1 <- diff(sales_ts,1,1)
sales.ts.df1_v1 <- ifelse(is.na(sales.ts.df1),0,sales.ts.df1)
#adf.test(sales.ts.df1_v1, alternative = "stationary", k = 0)
plot.ts(sales.ts.df1_v1)








acf(sales.ts.df1_v1, lag.max = 10)

pacf(sales.ts.df1_v1, lag.max = 10)

hol_flag <- tibble(Date = c("2010-02-12","2011-02-11", "2012-02-10","2013-02-08","2010-09-10","2011-09-09","2012-09-07","2013-09-06","2010-11-26","2011-11-25","2012-11-23","2013-11-29","2010-12-31","2011-12-30","2012-12-28","2013-12-27"), hol = c(rep("SB",4), rep("LD",4), rep("TG",4), rep("CH",4)))
hol_flag$Date <- as.Date(hol_flag$Date)

train_reg_data <- train_outlier_treated %>% select(-4,-5,-13,-c(16:20)) %>% left_join(hol_flag, by = "Date") %>% mutate(hol = ifelse(is.na(hol), "Other", hol)) %>% select(-3)
train_reg_data$hol <- as.factor(train_reg_data$hol)

x <- model.matrix(Wkly_sales_treated~.,train_reg_data)
y <- train_reg_data$Wkly_sales_treated

# Dividing data into test and train
train = sample(1:nrow(x),2*nrow(x)/3)
test = (-train)
y.test = y[test]




cv.out = cv.glmnet(x[train,], y[train],alpha = 0)
bestlam = cv.out$lambda.min
plot(cv.out)



ridge.mod = glmnet(x,y,alpha = 0, lambda = bestlam, thresh = 1e-12)









sales.boost <- gbm(Wkly_sales_treated~., data = train_reg_data, distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)

summary(sales.boost)

