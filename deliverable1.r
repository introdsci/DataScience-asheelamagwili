## ------------------------------------------------------------------------
library("tidyverse")
library("ggplot2")


## ------------------------------------------------------------------------
pollution <- read_csv("uspollution.csv")


## ------------------------------------------------------------------------
cat(colnames(pollution),sep = "\n")


## ------------------------------------------------------------------------
pollution$`NO2 Units` <- NULL
pollution$`O3 Units` <- NULL
pollution$`SO2 Units` <- NULL
pollution$`CO Units` <- NULL


## ------------------------------------------------------------------------
pollution$X1 <- NULL


## ------------------------------------------------------------------------
pollution$`State Code` <- NULL
pollution$`County Code` <- NULL
pollution$Address <- NULL


## ------------------------------------------------------------------------
colnames(pollution)[colnames(pollution) == "Site Num"] <- "site_id"
colnames(pollution)[colnames(pollution) == "State"] <- "state"
colnames(pollution)[colnames(pollution) == "County"] <- "county"
colnames(pollution)[colnames(pollution) == "City"] <- "city"
colnames(pollution)[colnames(pollution) == "Date Local"] <- "date"
colnames(pollution)[colnames(pollution) == "NO2 Mean"] <- "NO2_mean"
colnames(pollution)[colnames(pollution) == "NO2 1st Max Value"] <- "NO2_max_value"
colnames(pollution)[colnames(pollution) == "NO2 1st Max Hour"] <- "NO2_max_hour"
colnames(pollution)[colnames(pollution) == "NO2 AQI"] <- "NO2_aqi"
colnames(pollution)[colnames(pollution) == "O3 Mean"] <- "O3_mean"
colnames(pollution)[colnames(pollution) == "O3 1st Max Value"] <- "O3_max_value"
colnames(pollution)[colnames(pollution) == "O3 1st Max Hour"] <- "O3_max_hour"
colnames(pollution)[colnames(pollution) == "O3 AQI"] <- "O3_aqi"
colnames(pollution)[colnames(pollution) == "SO2 Mean"] <- "SO2_mean"
colnames(pollution)[colnames(pollution) == "SO2 1st Max Value"] <- "SO2_max_value"
colnames(pollution)[colnames(pollution) == "SO2 1st Max Hour"] <- "SO2_max_hour"
colnames(pollution)[colnames(pollution) == "SO2 AQI"] <- "SO2_aqi"
colnames(pollution)[colnames(pollution) == "CO Mean"] <- "CO_mean"
colnames(pollution)[colnames(pollution) == "CO 1st Max Value"] <- "CO_max_value"
colnames(pollution)[colnames(pollution) == "CO 1st Max Hour"] <- "CO_max_hour"
colnames(pollution)[colnames(pollution) == "CO AQI"] <- "CO_aqi"


## ------------------------------------------------------------------------
pollution$date <- as.Date(pollution$date, "%m/%d/%y")


## ------------------------------------------------------------------------
Location <- tibble(site_id = pollution$site_id, state = pollution$state, county = pollution$county, city = pollution$city, date = pollution$date)

NO2 <- tibble(site_id = pollution$site_id,date = pollution$date, mean = pollution$NO2_mean, max_value = pollution$NO2_max_value, max_hour = pollution$NO2_max_hour, aqi = pollution$NO2_aqi)

O3 <- tibble(site_id = pollution$site_id,date = pollution$date, mean = pollution$O3_mean, max_value = pollution$O3_max_value, max_hour = pollution$O3_max_hour, aqi = pollution$O3_aqi)

SO2 <- tibble(site_id = pollution$site_id,date = pollution$date, mean = pollution$SO2_mean, max_value = pollution$SO2_max_value, max_hour = pollution$SO2_max_hour, aqi = pollution$SO2_aqi)

CO <- tibble(site_id = pollution$site_id,date = pollution$date, mean = pollution$CO_mean, max_value = pollution$CO_max_value, max_hour = pollution$CO_max_hour, aqi = pollution$CO_aqi)


## ------------------------------------------------------------------------
ggplot(data=NO2,aes(aqi)) + geom_density(kernel = "gaussian", color="blue") + labs(title="Distribution of Air Quality in Nitrogen Oxides",x="Air Quality Index",y="Density")


## ------------------------------------------------------------------------
ggplot(data=O3,aes(aqi)) + geom_density(kernel = "gaussian", color="blue") + labs(title="Distribution of Air Quality in the Ozone",x="Air Quality Index",y="Density")


## ------------------------------------------------------------------------
ggplot(data=SO2,aes(aqi)) + geom_density(kernel = "gaussian", color="blue") + labs(title="Distribution of Air Quality in Sulfur Dioxides",x="Air Quality Index",y="Density")


## ------------------------------------------------------------------------
ggplot(data=CO,aes(aqi)) + geom_density(kernel = "gaussian", color="blue") + labs(title="Distribution of Air Quality in Carbon Dioxide",x="Air Quality Index",y="Density")


## ------------------------------------------------------------------------
ggplot(data=NO2,aes(mean)) + geom_density(kernel = "gaussian", color="red") + labs(title="Distribution of Mean in Nitrogen Oxides",x="Mean",y="Density")


## ------------------------------------------------------------------------
ggplot(data=O3,aes(mean)) + geom_density(kernel = "gaussian", color="red") + labs(title="Distribution of Mean in Ozone",x="Mean",y="Density")


## ------------------------------------------------------------------------
ggplot(data=SO2,aes(mean)) + geom_density(kernel = "gaussian", color="red") + labs(title="Distribution of Mean in Sulfur Oxides",x="Mean",y="Density")


## ------------------------------------------------------------------------
ggplot(data=CO,aes(mean)) + geom_density(kernel = "gaussian", color="red") + labs(title="Distribution of Mean in Carbon Dioxides",x="Mean",y="Density")

