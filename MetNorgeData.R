# meteorological data Iskoras
#install.packages("jsonlite")
# Libraries needed (jsonlite and tidyr must be installed in R)
library(jsonlite)
library(tidyverse)

# Insert your own client ID here
client_id = "7e556d1a-8394-442c-9d7f-8d5c43cbce6e"

# data is retrieved from Frost using the fromJSON function. First we build the correct URL to a Frost endpoint using parameters for sources, elements and dates.
# Define andpoint and parameters
endpoint <- paste0("https://", "7e556d1a-8394-442c-9d7f-8d5c43cbce6e", "@frost.met.no/observations/v0.jsonld")
sources <- 'SN97710,SN97251'
elements <- 'mean(air_temperature P1D),sum(precipitation_amount P1D)'
referenceTime <- '2017-01-01/2022-12-31'
# Build the URL to Frost
url <- paste0(
  endpoint, "?",
  "sources=", sources,
  "&referencetime=", referenceTime,
  "&elements=", elements
)
# Issue an HTTP GET request and extract JSON data
xs <- try(fromJSON(URLencode(url),flatten=T))

# Check if the request worked, print out any errors
if (class(xs) != 'try-error') {
  df <- unnest(xs$data)
  print("Data retrieved from frost.met.no!")
} else {
  print("Error: the data retrieval was not successful!")
}

#If the above block printed out a success, then you can run the line below to get a preview of the data frame table.

head(df)

# These additional columns will be kept
columns <- c("sourceId","referenceTime","elementId","value","unit","timeOffset")
df2 <- df[columns]
# Convert the time value to something R understands
df2$referenceTime <- as.Date(df2$referenceTime)

# Preview the result
head(df2)

library(lubridate)
StudyPeriod_climate<- df2%>%
  mutate(Year = year(referenceTime))%>%
  group_by(sourceId, Year, elementId, unit)%>%
  summarise(mean = mean(value),
            sum = sum(value))
# data does not match seklima output?
