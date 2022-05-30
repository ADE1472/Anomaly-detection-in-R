#PART 1

#1. Install the necessary package...
#install.packages("devtools")
#install.packages("wikipediatrend")
#install.packages("timetk")
#install.packages("Rcpp")
#install.packages("tidyverse")
#install_github("twitter/AnomalyDetection")
#install.packages("tibbletime")
#install.packages("anomalize")
#install.packages("reprex")
#install.packages('naniar')
#install.packages("furrr", type="binary")



#2. Load the libraries....
library(devtools)
library(tibbletime)
library(Rcpp)
library(timetk)
library(tidyverse)
library(anomalize)
library(wikipediatrend)
library(ggplot2)
library(reprex)
library(AnomalyDetection)
library(lubridate)
library(naniar)
library(furrr)


#PART 2
#1. Import the data
df<- read.csv('household_electricity_2005-2020.csv', header = TRUE, sep = ",")
str(df)

#2. Find the missing values
is.na(df)

#3. Plot the missing value(Result shows no missing value in the dataset)
visdat::vis_miss(df)


#4. Check the descriptive analysis of the data
summary(df)

#PART 3
#Data Preprocessing

#1. Convert the 'month' column from character to a date format
df$month<-paste(df$month, "01", sep="-")
df$month <- as.Date(df$month,format="%Y-%m-%d")
head(df)

#2. Remove unnecessary columns and leave just the month and overall columns
used_col=c("month","overall")
new_df= df[,used_col]
head(new_df) #To view the new data

#3. convert the new_df to a tibble object that follows a time series shape.

new_df<- as_tbl_time(new_df, index = month)
head(new_df)


#PART 4
#Anomaly Detection in the Singapore household 
#electricity consumption by dwelling type (in GWh) annually.

#1. summary view of the anomalize library
new_df <- new_df %>%
  time_decompose(overall, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
new_df %>% glimpse()

#2. Plot showing the Anomaly in the data (red spot)
new_df %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)


#3. Plot showing the anomaly trend with a higher consumption rate in 2020  
ggplot(new_df, aes(x=month, y=overall, color=overall)) + geom_line()

#4. Plot that showed the time series data decomposing into seasonal, 
#trend, and remainder component.
new_df %>%
  time_decompose(overall, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.75, max_anoms = 0.2) %>%
  plot_anomaly_decomposition()

#5. plot showing the anomalies after time recomposed
new_df %>% 
  time_decompose(overall) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
#the grey part of the plot shows the expected trend while the red spot
#shows anomaly i.e the high energy consumption in year 2020


#7. Extracting the actual datapoint which are anomalies
new_df %>% 
  time_decompose(overall) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')

#PART 5 (Conclusion)
#The anomalies dataset consists of the data points which were 
#identified as anomalies by the algorithm.


