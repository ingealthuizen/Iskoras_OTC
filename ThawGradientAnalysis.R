#Thaw Gradient Analysis

#Environmental data
# Ibutton
###### Read in all Ibutton data 2019-2020
Ibuttonfiles<- list.files(path = ".", 
                          pattern =  ".*.csv",  # ELLERS BRUGE GENEREL "\\.csv$"
                          full.names = TRUE, 
                          recursive = TRUE)

#funtion for reading Ibutton files, skipping logger info and merging temperature columns
importIButtonFiles <- function(fileLoc) {
  do.call(rbind, lapply(X = fileLoc, FUN = function(curFile) {
    inText <- readLines(curFile)
    divIndex <- which(grepl("^\\s*$", inText, perl = TRUE))[1] #skip everything until empty line
    wholeText <- paste(sapply(X = inText[(divIndex + 1):length(inText)], FUN = function(curLine) {
      # Reorganise the output vector so that the last entry has the correct decimalisation
      outVec <- strsplit(curLine, ",", fixed = TRUE)[[1]]
      outVec <- c(outVec[1:2], paste(outVec[3:length(outVec)], collapse = "."))
      paste(outVec, collapse = ",")
    }), collapse = "\n")
    outOb <- read.table(text = wholeText, sep = ",", dec = ".", header = TRUE)
    cbind(outOb, data.frame(file = rep(curFile, nrow(outOb))))
  }))
}

setwd("C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\2020\\Ibutton")
IbuttonData_2020 <- importIButtonFiles(Ibuttonfiles)%>%
  separate(file, into = c("PlotID", "Treatment", "Soildepth"), sep = "_")%>%
  mutate(Soiltemp = Value,
         Transect = as.factor(substring(PlotID, 3, 3)),
         PlotID = sub("./", "", PlotID),
         Habitat = as.factor(substring(PlotID, 2, 4)),
         Soildepth = as.factor(sub("\\cm.csv*", "", Soildepth)),
         DateTime = dmy_hms(Date.Time, tz = "Europe/Oslo"),
         Time = format(DateTime, "%H:%M:%S"),
         Date = as.Date(DateTime, format="%d/%m/%y"))%>%
  select(-Unit, -Value, -Date.Time)%>%
  mutate(Habitat = recode(Habitat, "WGA" = "WG"), # recode WGA and WGB to WG 
         Habitat = recode(Habitat, "WGB" = "WG"))%>%
  filter(Date >= '2019-06-07' & Date <= '2020-06-11')


# Remove malfunctioning Ibuttons 
IbuttonData_2020$Soiltemp[IbuttonData_2020$PlotID == "4S" & IbuttonData_2020$Treatment == "C" & IbuttonData_2020$Soildepth == "10"] <- NA # extreme outlier
IbuttonData_2020$Soiltemp[IbuttonData_2020$PlotID == "4P" & IbuttonData_2020$Treatment == "C" & IbuttonData_2020$Soildepth == "10"] <- NA
IbuttonData_2020$Soiltemp[IbuttonData_2020$PlotID == "4W" & IbuttonData_2020$Treatment == "C" & IbuttonData_2020$Soildepth == "20"] <- NA
IbuttonData_2020$Soiltemp[IbuttonData_2020$PlotID == "3P" & IbuttonData_2020$Treatment == "OTC" & IbuttonData_2020$Soildepth == "10"] <- NA

#TOMST data mid june 2020- sept 2023
TOMSTdata<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\TOMSTdata_SMcalculated2023.csv")
