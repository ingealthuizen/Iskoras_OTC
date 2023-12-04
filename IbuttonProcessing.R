# Soiltemp Database
#Ibutton Data Processing
library(tidyverse)
library(lubridate)
library(ggplot2)

Ibutton_1718<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\iButton\\4hour_data_iButton_all_2017-2018.csv")%>%
  mutate(Treatment = recode(Treatment, CTRL="C"),
         Treatment = recode(Treatment, CONTROL = "C"),
         Plotcode= paste(Transect, Plot, Treatment, sep = ""),
         DateTime= paste(Date, Time),
         DateTime2= as.POSIXct(DateTime, format ="%Y-%m-%d %H:%M:%S"))

Ibutton_1819<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\iButton\\4hour_data_iButton_all_2018-2019.csv")%>%
  mutate(Treatment = recode(Treatment, CTRL="C"),
         Plotcode= paste(Transect, Plot, Treatment, sep = ""),
         Time = substring(DateTime, 12, 19),
         DateTime= paste(Date, Time),
         DateTime2= as.POSIXct(DateTime, format ="%Y-%m-%d %H:%M:%S"))

#check data
#ggplot(Ibutton_1819, aes(DateTime2, Value, col= as.factor( Soil_depth_cm)))+
#  geom_point()+
#  facet_wrap(~Plotcode)

Ibutton_1718<-Ibutton_1718%>%
    select(Plotcode, Transect, Plot, Treatment, Soil_depth_cm, Date, Time, Value)

Ibutton_1819<-Ibutton_1819%>%
  select(Plotcode, Transect, Plot, Treatment, Soil_depth_cm, Date, Time, Value)

Ibutton_171819<- rbind(Ibutton_1718, Ibutton_1819)


###########################################################################
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

###### Read in all Ibutton data 2019-2020
Ibuttonfiles2020<- list.files(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\iButton\\Ibutton_2020", 
                          pattern =  ".*.csv",  # ELLERS BRUGE GENEREL "\\.csv$"
                          full.names = TRUE, 
                          recursive = TRUE)
IbuttonData_2020 <- importIButtonFiles(Ibuttonfiles2020)


##### Read in all Ibutton data 2020-2021
Ibuttonfiles2021<- list.files(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\iButton\\Ibutton_2021", 
                              pattern =  ".*.csv",  # ELLERS BRUGE GENEREL "\\.csv$"
                              full.names = TRUE, 
                              recursive = TRUE)
IbuttonData_2021 <- importIButtonFiles(Ibuttonfiles2021)

# reformat Ibutton data from years 2020-2021
Ibutton_2021<- rbind(IbuttonData_2020, IbuttonData_2021)%>%
    mutate(DateTime= as.POSIXct(Date.Time, format ="%d.%m.%y %H:%M:%S"),
         Date = date(DateTime),
         Time = substring(DateTime, 12, 19),
         Plotcode = sub('.*/', '', file),
         Plotcode = sub('cm.csv.*', '', Plotcode),
         Plotcode2 = Plotcode)%>%
  separate(Plotcode2, into = c("PlotID", "Treatment", "Soildepth"), sep = "_")%>%
  mutate(Transect = substring(PlotID, 1,1),
         Plot = substring(PlotID, 2),
         Soil_depth_cm = sub('.*_', '', Plotcode),
         Treatment = ifelse(grepl("OTC",Treatment),"OTC","C"),
         Plotcode = paste(Transect, Plot, Treatment, sep = ""))

# Remove malfunctioning Ibuttons 
Ibutton_2021$Value[Ibutton_2021$Plotcode == "4SC" & Ibutton_2021$Soil_depth_cm == "10"] <- NA # extreme outlier
Ibutton_2021$Value[Ibutton_2021$Plotcode == "4PC"  & Ibutton_2021$Soil_depth_cm == "10"] <- NA
Ibutton_2021$Value[Ibutton_2021$Plotcode== "4WC"  & Ibutton_2021$Soil_depth_cm == "20"] <- NA
Ibutton_2021$Value[Ibutton_2021$Plotcode == "3POTC"  & Ibutton_2021$Soil_depth_cm == "10"] <- NA

Ibutton_2021<-Ibutton_2021%>%
  filter(Date >= '2019-06-07' & Date <= '2020-06-11' | Date >= '2020-07-19' & Date <= '2021-07-01'  )%>%
  filter(Value < 35) # remove faulty data

#check data
ggplot(Ibutton_2021, aes(DateTime, Value, col= as.factor( Soil_depth_cm)))+
  geom_point()+
  facet_wrap(~Plotcode)


#Reformat data to uniform dataframes
Ibutton_171819<- Ibutton_171819%>%
  mutate(Temperature = Value,
         Year = year(Date),
         Month = month(Date),
         Day = day(Date),
         Date = as.POSIXct(Date, format ="%Y-%m-%d"))%>%
  select(Plotcode, Transect, Plot, Treatment, Soil_depth_cm, Year, Month, Day, Time, Temperature, Date)

Ibutton_2021<- Ibutton_2021%>%
  mutate(Temperature = Value,
         Year = year(Date),
         Month = month(Date),
         Day = day(Date),
         Date = as.POSIXct(Date, format ="%Y-%m-%d"))%>%
  select(Plotcode, Transect, Plot, Treatment, Soil_depth_cm, Year, Month, Day, Time, Temperature, Date)

## Join all Ibutton data together
Ibutton_2017_2021<- rbind(Ibutton_171819, Ibutton_2021)%>%
  mutate(Habitat = recode(Plot, "WGA" = "WG", "WGB" = "WG"))

Ibutton_2017_2021$Transect <- as.factor(Ibutton_2017_2021$Transect)

#saveRDS(Ibutton_2017_2021, file = "C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\iButton\\4hour_iButton_2017-2021.rds")
#write.csv(Ibutton_2017_2021, file= "C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\iButton\\4hour_iButton_2017-2021.csv")


DailyMeanTemperature<-Ibutton_2017_2021%>%
  group_by(Date, Habitat, Treatment, Soil_depth_cm) %>% 
  summarise(Soiltemp_mean = mean(Temperature, na.rm = TRUE))%>%
  ungroup()

DailyMeanTemperature%>%
  group_by()

library(viridis)
library(RColorBrewer)

DailyMeanTemperature$Habitat <- factor(DailyMeanTemperature$Habitat, levels = c("S", "P", "M", "W", "WG"))

DailyMeanTemperature%>%
  #filter(Soil_depth_cm =="10")%>%
  #filter(Date >= '2019-01-01' & Date <= '2019-12-31')%>%
  ggplot(aes(Date, Soiltemp_mean, col = Habitat))+
  geom_line(linewidth =0.5)+
  scale_color_manual(values = c("#E6AB02", "#66A61E", "#666666","#7570B3", "#1B9E77" ))+
  #scale_color_viridis(discrete = TRUE, direction = -1)+
  #scale_fill_viridis(discrete = TRUE, direction = -1)+
  labs(x = "Date", y= "Temperature 0C")+
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13), panel.background = element_rect(fill = "white", colour = "black"))+
  facet_wrap(~Soil_depth_cm)


iButtonData%>%
  filter(Date > "2018-01-01" & Date <"2019-12-31" )%>%
  ggplot(aes(Date, Soil_temp_10cm, col = Type))+
  geom_rect(aes(xmin=ymd('2018-01-01'), 
                xmax = ymd('2018-05-01'),
                ymin = -Inf,
                ymax = Inf), fill = 'grey', color= "white", alpha = 0.01) +
  geom_rect(aes(xmin=ymd('2018-12-01'), 
                xmax = ymd('2019-05-01'),
                ymin = -Inf,
                ymax = Inf), fill = 'grey', color= "white", alpha = 0.01) +
  geom_rect(aes(xmin=ymd('2019-11-01'), 
                xmax = ymd('2020-01-01'),
                ymin = -Inf,
                ymax = Inf), fill = 'grey', color= "white", alpha = 0.01) +
  #geom_rect(aes(xmin=ymd('2018-06-21'), #Highlight missing data
  #              xmax = ymd('2018-07-13'),
  #              ymin = -Inf,
  #              ymax = Inf), fill = 'pink', alpha = 0.01) +
  geom_line(size =0.5)+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  scale_fill_viridis(discrete = TRUE, direction = -1)+
  labs(x = "Date", y= "Temperature 0C")+
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13), panel.background = element_rect(fill = "white", colour = "black"))



################################# SOiltemp Database ###########################################################################
# formatting for soiltemp database
Iskoras_Ibutton_2017_2021<- rbind(Ibutton_171819, Ibutton_2021)%>%
  mutate(Country = "NO",
         Datamanager = "IA",
         Plotcode = paste(Country, "_", Datamanager, "_", Plotcode, "_", Soil_depth_cm, sep = ""))%>%
  select(Plotcode, Soil_depth_cm, Year, Month, Day, Time, Temperature, Country, Datamanager, Date)

write.csv(Iskoras_Ibutton_2017_2021, "C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\Iskoras_SoiltempDatabase_corrected.csv")

metadata_Iskoras<- Iskoras_Ibutton_2017_2021%>%
  group_by(Plotcode)%>%
  mutate(startDate = min(Date),
         endDate = max(Date),
         startYear = year(startDate),
         startMonth = month(startDate),
         startDay = day(startDate),
         endYear = year(endDate),
         endMonth = month(endDate),
         endDay = day(endDate))%>%
  slice(1)

write.csv(metadata_Iskoras, "C:\\Users\\ial008\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\Iskoras_SoiltempDatabase_metadata_corrected.csv")
