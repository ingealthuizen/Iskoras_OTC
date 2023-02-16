# meteorological data Iskoras
install.packages(jsonlite)
# Libraries needed (jsonlite and tidyr must be installed in R)
library(jsonlite)
library(tidyr)

# Insert your own client ID here
client_id = '<INSERT CLIENT ID HERE>'

# data is retrieved from Frost using the fromJSON function. First we build the correct URL to a Frost endpoint using parameters for sources, elements and dates.
# Define andpoint and parameters
endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")
sources <- 'SN18700,SN90450'
elements <- 'mean(air_temperature P1D),sum(precipitation_amount P1D),mean(wind_speed P1D)'
referenceTime <- '2010-04-01/2010-04-03'
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


R example script

This tutorial shows you how to get started using Frost in R. The snippets of code on this page can be put together to make your own script for extracting data.

First and most importantly, the client ID must be defined to be able to authenticate the requests.

# Libraries needed (jsonlite and tidyr must be installed in R)
library(jsonlite)
library(tidyr)

# Insert your own client ID here
client_id = '<INSERT CLIENT ID HERE>'

In the next block of code, data is retrieved from Frost using the fromJSON function. First we build the correct URL to a Frost endpoint using parameters for sources, elements and dates.

# Define andpoint and parameters
endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")
sources <- 'SN18700,SN90450'
elements <- 'mean(air_temperature P1D),sum(precipitation_amount P1D),mean(wind_speed P1D)'
referenceTime <- '2010-04-01/2010-04-03'
# Build the URL to Frost
url <- paste0(
  endpoint, "?",
  "sources=", sources,
  "&referencetime=", referenceTime,
  "&elements=", elements
)
# Issue an HTTP GET request and extract JSON data
xs <- try(fromJSON(URLencode(url),flatten=T))

The above code will give an error output if something went wrong with the request. The block below will also make sure that there is valid data in the request.

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

