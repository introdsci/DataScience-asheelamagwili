## ----echo=FALSE, message=FALSE, error=FALSE, warning=FALSE, results='hide'----
include <- function(library_name){
  if( !(library_name %in% installed.packages()) )
    install.packages(library_name) 
  library(library_name, character.only=TRUE)
}
include("rvest")
include("tidyr")
include("tidyverse")
include("ggplot2")
include("modelr")
include("knitr")
library(caret)
purl("deliverable1.Rmd", output = "deliverable1.r") # produces r source from rmd

# Since the first deliverable requires user to download a file (See note below) this part will be commented out - once file is downloaded this line should be uncommented.
#source("deliverable1.r") # executes the source


## ------------------------------------------------------------------------
inventory_sector <- read_csv("InventorySector.csv")
# Priview of dataset
head(inventory_sector)


## ------------------------------------------------------------------------
# Create a new dataframe called Sector and transpose the original
Sector <- as.data.frame(t(inventory_sector))
Sector


## ------------------------------------------------------------------------
# Clean up the sector names
colnames(Sector)[colnames(Sector) == "V1"] <- "Energy"
colnames(Sector)[colnames(Sector) == "V2"] <- "Agriculture"
colnames(Sector)[colnames(Sector) == "V3"] <- "Industrial"
colnames(Sector)[colnames(Sector) == "V4"] <- "Waste"
colnames(Sector)[colnames(Sector) == "V5"] <- "Forestry"
colnames(Sector)[colnames(Sector) == "V6"] <- "Gross"
colnames(Sector)[colnames(Sector) == "V7"] <- "Net"

# Remove first row
Sector <- Sector[-1,]
Sector


## ------------------------------------------------------------------------
year <- rownames(Sector)
Sector$Year <- year

Sector


## ------------------------------------------------------------------------
new_sector <- as_tibble(Sector) %>% subset(!is.na(as.numeric(Energy)) & !is.na(as.numeric(Forestry)))
set.seed(385)


## ------------------------------------------------------------------------
sample_selection <- createDataPartition(as.numeric(Sector$Energy), p = 0.70, list = FALSE)
train <- new_sector[sample_selection,]
test <- new_sector[-sample_selection,]


## ------------------------------------------------------------------------
train_model <- lm(Sector,formula = as.numeric(Energy) ~ as.numeric(Forestry) + as.numeric(Waste) + as.numeric(Agriculture) + as.numeric(Industrial))


## ------------------------------------------------------------------------
predictions <- train_model %>% predict(test)

# The square of correlation - (0 - no correlation & 1 - directly correlated)
R2(predictions, as.numeric(test$Energy))

# Average errors of all our predictions
MAE(predictions, as.numeric(test$Energy))


## ------------------------------------------------------------------------
emissions_by_sector <- gather(Sector,Sector,Emissions,"Energy":"Forestry")
# We won't need the Net or Gross, let's remove them
emissions_by_sector$Net = NULL
emissions_by_sector$Gross = NULL

# Change the type of Emissions from character to a numeric
emissions_by_sector$Emissions <- as.numeric(emissions_by_sector$Emissions)

emissions_by_sector


## ------------------------------------------------------------------------
emissions_model <- ggplot(emissions_by_sector, aes(x=Year,y=Emissions)) + geom_line(aes(group = Sector, color = Sector))

# Fix the x-axis so it is readable
emissions_model <- emissions_model + theme(axis.text.x = element_text(angle=45), plot.title = element_text(size = 20, face = "bold"))

# Add more detail to the labels
emissions_model <- emissions_model + labs(title = "Emissions by Inventory Sector", subtitle = "Measured in Million Metric Tons over Time (1990-2017)")

emissions_model


## ------------------------------------------------------------------------
# Changes the types to a numeric
Sector$Energy <- as.numeric(Sector$Energy)
Sector$Agriculture <- as.numeric(Sector$Agriculture)
Sector$Industrial <- as.numeric(Sector$Industrial)
Sector$Waste <- as.numeric(Sector$Waste)
Sector$Forestry <- as.numeric(Sector$Forestry)

# Tests the correlation between Energy
cor.test(Sector$Energy,Sector$Industrial)
cor.test(Sector$Energy,Sector$Agriculture)
cor.test(Sector$Energy,Sector$Waste)
cor.test(Sector$Energy,Sector$Forestry)


## ------------------------------------------------------------------------
# Create a scatter plot
forestry_vs_energy <- ggplot(Sector,aes(x=as.numeric(Sector$Forestry),y=as.numeric(Sector$Energy))) + geom_point() 

# Beautify the graph
forestry_vs_energy <- forestry_vs_energy + labs(title = "Forestry vs. Energy") + xlab("Forestry") + ylab("Energy") + theme(plot.title = element_text(size = 20, face = "bold"))

forestry_vs_energy


## ------------------------------------------------------------------------
simple_model <- lm(Sector,formula=Energy~Forestry + Waste + Agriculture + Industrial)
summary(simple_model)

