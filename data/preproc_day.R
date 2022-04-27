#library(OpenML)
#bike = as.data.frame(getOMLDataSet(1414))
bike = read.csv("data/day.csv", stringsAsFactors = FALSE)
bike$weekday = factor(bike$weekday, levels = 0:6, labels = c('SUN', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT'))
bike$holiday = factor(bike$holiday, levels = c(0,1), labels = c('NO', 'YES'))
bike$workingday = factor(bike$workingday, levels = c(0,1), labels = c('NO', 'YES'))
bike$season = factor(bike$season, levels = 1:4, labels = c('WINTER', 'SPRING', 'SUMMER', 'FALL'))
bike$weathersit = factor(bike$weathersit, levels = 1:3, labels = c('CLEAR', 'MISTY/CLOUDY', 'SNOW/RAIN+STORM'))
bike$mnth = factor(bike$mnth, levels = 1:12, labels = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'))
#bike$yr[bike$yr == 0] = 2011
#bike$yr[bike$yr == 1] = 2012
bike$yr = factor(bike$yr, levels = 0:1, labels = c('2011', "2012"))
# denormalize weather features:
# temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
bike$temp = bike$temp * (39 - (-8)) + (-8)
# atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
bike$atemp = bike$atemp * (50 - (16)) + (16)
#windspeed: Normalized wind speed. The values are divided to 67 (max)
bike$windspeed = 67 * bike$windspeed
#hum: Normalized humidity. The values are divided to 100 (max)
bike$hum = 100 * bike$hum
# Account for trend
bike$days_since_2011 = as.numeric(as.Date(bike$dteday)-min(as.Date(bike$dteday)))
# remove features
bike$instant = bike$atemp = bike$dteday = bike$casual = bike$registered = NULL

# library(ggplot2)
# ggplot(data = bike, aes(x = as.factor(hr), y = cnt)) + geom_boxplot(varwidth = T)
save(bike, file = "data/bike.RData")
