
library(dplyr)
library(MASS)
library(car)
library(dynlm)
library(lubridate)


datanew2 <- read.csv("datanew2.csv", stringsAsFactors = FALSE)

datanew2$Month_num <- match(datanew2$Month, month.name)


datanew2 <- datanew2 %>% arrange(Year, Month_num)


datanew2 <- datanew2 %>%
  mutate(
    AQI_lag1 = lag(AQI, 1),
    AQI_lag2 = lag(AQI, 2),
    AQI_lag3 = lag(AQI, 3),
    PM25_lag1 = lag(PM25, 1),
    PM25_lag2 = lag(PM25, 2),
    PM25_lag3 = lag(PM25, 3)
  )

datanew2 <- datanew2 %>%
  mutate(
    winter = ifelse(Month_num %in% c(11,12,1), 1, 0)
  )


head(datanew2)

lm_pm25 <- lm(cases ~ PM25_lag1 + PM25_lag2 + PM25_lag3 + temp + winter, data = datanew2)
vif(lm_pm25)

nb_pm25 <- glm.nb(cases ~ PM25_lag1 + PM25_lag2 + PM25_lag3 + temp + winter, data = datanew2)
summary(nb_pm25)


nb_aqi <- glm.nb(cases ~ AQI_lag1 + AQI_lag2 + AQI_lag3 + temp + winter, data = datanew2)
summary(nb_aqi)

nb_pm25_simple <- glm.nb(cases ~ PM25_lag1 + temp + winter, data = datanew2)
summary(nb_pm25_simple)


library(dynlm)   
library(zoo)     


datanew2 <- datanew2[order(datanew2$Year, datanew2$Month), ]


ts_cases <- ts(datanew2$cases, start = c(2022, 1), frequency = 12)
ts_pm25  <- ts(datanew2$PM25, start = c(2022, 1), frequency = 12)

pdl_model <- dynlm(ts_cases ~ L(ts_pm25, 1:3))
summary(pdl_model)


