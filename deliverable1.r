## ----echo=FALSE, message=FALSE, error=FALSE, warning=FALSE, results='hide'----
# include(library_name) installs necessary packages for a given library name
include <- function(library_name){
  if( !(library_name %in% installed.packages()) )
    install.packages(library_name) 
  library(library_name, character.only=TRUE)
}

include("tidyverse")
include("ggplot2")


## ------------------------------------------------------------------------
pollution <- read_csv("uspollution.csv")
head(pollution)


## ------------------------------------------------------------------------
cat(colnames(pollution),sep = "\n")


## ------------------------------------------------------------------------
# Removes the variables by setting them to NULL
pollution$`NO2 Units` <- NULL
pollution$`O3 Units` <- NULL
pollution$`SO2 Units` <- NULL
pollution$`CO Units` <- NULL
head(pollution)


## ------------------------------------------------------------------------
# Removes the variables by setting them to NULL
pollution$X1 <- NULL
head(pollution)


## ------------------------------------------------------------------------
# Removes the variables by setting them to NULL
pollution$`State Code` <- NULL
pollution$`County Code` <- NULL
pollution$Address <- NULL
head(pollution)


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
head(pollution)


## ------------------------------------------------------------------------
# Changes the type of date from list to a "date" & makes sure the format is consistently Month/Day/Year
pollution$date <- as.Date(pollution$date, "%m/%d/%y")


## ------------------------------------------------------------------------
# Create Location table
Location <- tibble(site_id = pollution$site_id, state = pollution$state, county = pollution$county, city = pollution$city, date = pollution$date)
head(Location)

# Create NO2 table
NO2 <- tibble(site_id = pollution$site_id,date = pollution$date, mean = pollution$NO2_mean, max_value = pollution$NO2_max_value, max_hour = pollution$NO2_max_hour, aqi = pollution$NO2_aqi)
head(NO2)

# Create O2 table
O3 <- tibble(site_id = pollution$site_id,date = pollution$date, mean = pollution$O3_mean, max_value = pollution$O3_max_value, max_hour = pollution$O3_max_hour, aqi = pollution$O3_aqi)
head(O3)

# Create O2 table
SO2 <- tibble(site_id = pollution$site_id,date = pollution$date, mean = pollution$SO2_mean, max_value = pollution$SO2_max_value, max_hour = pollution$SO2_max_hour, aqi = pollution$SO2_aqi)
head(SO2)

# Create CO table
CO <- tibble(site_id = pollution$site_id,date = pollution$date, mean = pollution$CO_mean, max_value = pollution$CO_max_value, max_hour = pollution$CO_max_hour, aqi = pollution$CO_aqi)
head(CO)


## ------------------------------------------------------------------------
# Create a density plot using geom_density()
NO2_distribution <- ggplot(data=NO2,aes(aqi)) + geom_density(kernel = "gaussian", color="blue")

# Beautify the graph
NO2_distribution <- NO2_distribution + labs(title="Distribution of Air Quality in Nitrogen Oxides",x="Air Quality Index",y="Density")
NO2_distribution


## ------------------------------------------------------------------------
# Create a density plot using geom_density()
O3_distribution <- ggplot(data=O3,aes(aqi)) + geom_density(kernel = "gaussian", color="blue")

# Beautify graph
O3_distribution <- O3_distribution + labs(title="Distribution of Air Quality in the Ozone",x="Air Quality Index",y="Density")
O3_distribution


## ------------------------------------------------------------------------
# Create a density plot using geom_density()
SO2_distribution <- ggplot(data=SO2,aes(aqi)) + geom_density(kernel = "gaussian", color="blue")

# Beatify graph
SO2_distribution <- SO2_distribution + labs(title="Distribution of Air Quality in Sulfur Dioxides",x="Air Quality Index",y="Density")
SO2_distribution


## ------------------------------------------------------------------------
# Create a density plot using geom_density()
CO_distribution <- ggplot(data=CO,aes(aqi)) + geom_density(kernel = "gaussian", color="blue")

# Beautify graph
CO_distribution <- CO_distribution + labs(title="Distribution of Air Quality in Carbon Dioxide",x="Air Quality Index",y="Density")
CO_distribution


## ------------------------------------------------------------------------
# Create a density plot using geom_density()
NO2_distribution <- ggplot(data=NO2,aes(mean)) + geom_density(kernel = "gaussian", color="red")

# Beautify graph
NO2_distribution <- NO2_distribution + labs(title="Distribution of Mean in Nitrogen Oxides",x="Mean",y="Density")
NO2_distribution


## ------------------------------------------------------------------------
O3_distribution <- ggplot(data=O3,aes(mean)) + geom_density(kernel = "gaussian", color="red")
O3_distribution <- O3_distribution + labs(title="Distribution of Mean in Ozone",x="Mean",y="Density")


## ------------------------------------------------------------------------
# Create a density plot using geom_density()
SO2_distribution <- ggplot(data=SO2,aes(mean)) + geom_density(kernel = "gaussian", color="red")

# Beautify graph
SO2_distribution <- SO2_distribution + labs(title="Distribution of Mean in Sulfur Oxides",x="Mean",y="Density")
SO2_distribution


## ------------------------------------------------------------------------
# Create a density plot using geom_density()
CO_distribution <- ggplot(data=CO,aes(mean)) + geom_density(kernel = "gaussian", color="red")

# Beautify Graph
CO_distribution <- CO_distribution + labs(title="Distribution of Mean in Carbon Dioxides",x="Mean",y="Density")
CO_distribution

