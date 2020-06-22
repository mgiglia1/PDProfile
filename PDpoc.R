library(tidyverse)
library(ggplot2)
library("zoo")

headers <- read_csv("~/Desktop/03/30718_1541263021_3635.csv", 
                    col_names = FALSE, n_max = 3)
pd_1 <- read_csv("~/Desktop/03/30718_1541263021_3635.csv", 
                          col_types = cols_only(
                            MetabolicPower = col_guess(), 
                            SmoothedVelocity = col_guess(), 
                            TimeStamp = col_guess()), 
                          skip = 3)
# get centisec time from headers
CS_time = headers[2,1]
CS_time = CS_time[[1]]
CS_time = strsplit(CS_time, " ")[[1]]
CS_time = as.numeric(CS_time[2])

# downsample data from 100Hz to 10Hz
pd_1 <- na.omit(pd_1)

# get data from df, convert timestamp to centisec
pd_1 <- pd_1 %>% mutate(TimeStamp = 100*as.numeric(TimeStamp)+ CS_time)
pd_1 <- pd_1 %>% mutate(SmoothedVelocity = as.numeric(SmoothedVelocity))
pd_1 <- pd_1 %>% mutate(MetabolicPower = as.numeric(MetabolicPower))

# timeseries plot
ggplot(pd_1, aes(x=TimeStamp, y=SmoothedVelocity)) + geom_point()

#data <- data.frame("t" = c(1, 5, 10, 15))

data <- data.frame("t" = c(10, 20, 30, 40, 50, 60, 90, 120, 150, 180))
v <- numeric()
p <- numeric()
# rolling avg for loop
for (sec in data$t){
  pd_1s <- pd_1 %>% mutate(SmoothedVelocity = rollmean(SmoothedVelocity, sec*10, fill = NA), MetabolicPower = rollmean(MetabolicPower, sec*10, fill = NA))
  v <- append(v, max(pd_1s$SmoothedVelocity, na.rm = TRUE))
  p <- append(p, max(pd_1s$MetabolicPower, na.rm = TRUE))
}
data <- data %>% mutate(vel = v, pwr = p, inv_t = 1/t)
ggplot(data, aes(x=t, y=vel)) + geom_point()
ggplot(data, aes(x=t, y=pwr)) + geom_point()

ggplot(data, aes(x=inv_t, y=vel)) + geom_point()
ggplot(data, aes(x=inv_t, y=pwr)) + geom_point()

