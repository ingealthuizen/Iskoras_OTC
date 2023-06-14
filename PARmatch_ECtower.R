# match ECtower PAR to fluxdata
library(readxl)
# Load 2019 data
flux2019_HMR<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2020\\LiCOR850\\HMRfiles\\HMR_NEEflux2019.csv")%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(Cover = recode(Cover, ER = "RECO"))%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  select(Date, PlotID, Cover, Transect, Habitat, Treatment, f0:Method, Comment, LR.f0:LR.f0.up95)

# 2019 metadata
metadata2019<-read_xlsx("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2019NEEdata\\Chamber_fluxes_metaIA.xlsx")%>%  
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)%>%
  mutate(Date = as.Date(Date))%>%
  select(Date, hour, PlotID, Cover, PAR1, PAR2, PAR3, Soiltemp1, Soiltemp2, SoilMoist1, SoilMoist2, SoilMoist3, Airtemp, Redo)

flux2019_HMRenv<- left_join(flux2019_HMR, metadata2019, by= c("Date", "PlotID",  "Cover"))%>%
  group_by(Date, Transect, Habitat, Treatment, PlotID, Cover, Redo, Comment, f0)%>%
  gather(key = par, value = value, PAR1, PAR2, PAR3)%>%
  mutate(PAR = mean(value, na.rm = TRUE))%>%
  gather(key = soilT, value = temp, Soiltemp1 , Soiltemp2)%>%
  mutate(SoilTemp = mean(temp, na.rm = TRUE))%>%
  gather(key = soilM, value = moisture, SoilMoist1 , SoilMoist2, SoilMoist3)%>%
  mutate(SoilMoist = mean(moisture, na.rm = TRUE))%>%
  distinct(Date, Transect, Habitat, Treatment, PlotID, Cover, f0, .keep_all = TRUE)%>%
  select(-par, -value, -soilT, -temp, -soilM, -moisture)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))%>% # remove duplicate measurements by taking mean
  ungroup()%>%
  filter((Comment != "delete") %>% replace_na(TRUE))#%>%
  #select(-Comment, -Method)

# replace NaN for PAR, soil with PAR from EC tower
########################################
#2019 data missing a lot of data !!!!!

#bind 2019 and 2020 NEE chamber data together
#NEEflux_20192020<- rbind(flux2019_HMRenv, flux2020_HMR)

ECdata<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Climate\\Mobileflux1_level1_30min_forCasper_update2022.csv")%>%
  separate(index, into = c("Date", "Time"), sep = " " )%>%
  mutate(hour = as.integer(substring(Time, 1, 2)),
         Date = as.Date(Date),
         PAR = shortwave_incoming * 2.114)%>%
  mutate(PAR = ifelse(PAR< 0, 0, PAR))%>% # set PAR to zero if negative
  group_by(Date, hour)%>%
  summarise_all(mean)%>%
  select(Date, hour, PAR)

##### 
HMRflux_2019_ECpar<- left_join(flux2019_HMRenv, ECdata, by= c("Date", "hour"))%>%
  rename("PAR_EC" =PAR.y, "PAR_measured"= PAR.x)%>%
  mutate(PAR_measured = ifelse(Cover == "RECO", 0, PAR_measured),
         PAR.match = ifelse(PAR_measured == "NaN", PAR_EC, PAR_measured))

write.csv(HMRflux_2019_ECpar, "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2019\\2019HMRflux_PARcorrection.csv")

# including EC tower PAR data for May and July
HMR_parCorrection<-read.csv("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\2019NEEdata\\HMRflux_PARcorrection.csv")%>%
  separate(PlotID, c("PlotID", "Treatment"))%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"),
        Flux_type = Cover)%>%
  select(Date, PlotID, Flux_type, PAR.match) 

load("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\Thawgradient\\GPP\\2019_GPP_w_PAR0_ER_data_for_modelling_UPDATE_2022_10_17.Rdata")
flux2019 <- GPP_reduced_w_PAR0 %>% 
  rename(Flux_type = Flux)

flux2019_May<- flux2019%>%
  filter(Date == "2019-05-22")

flux2019_PARcorrect_may<-left_join(flux2019_May, HMR_parCorrection, by= c("Date", "PlotID", "Flux_type"))%>%
  group_by(PlotID_Date, PlotID, Month, Flux_type, Type, Date, PAR.match)%>%
  distinct(GPP_flux_umol_CO2_m2_s, .keep_all = TRUE)%>%
  ungroup()%>%
  mutate(PAR.match = coalesce(PAR,PAR.match))%>%
  select(PlotID_Date, GPP_flux_umol_CO2_m2_s, PAR.match)

flux2019_July<- flux2019%>%
  filter(Date == "2019-07-05")%>%
  group_by(Date, PlotID, Flux_type)

flux2019_PARcorrect_July<-left_join(flux2019_July, HMR_parCorrection, by= c("Date", "PlotID", "Flux_type"))%>%
  group_by(PlotID_Date, PlotID, Month, Flux_type, Type, Date, PAR.match)%>%
  distinct(GPP_flux_umol_CO2_m2_s, .keep_all = TRUE)%>%
  ungroup()%>%
  mutate(PAR.match = coalesce(PAR,PAR.match))%>%
  distinct(GPP_flux_umol_CO2_m2_s,  .keep_all= TRUE)%>%
  select(PlotID_Date, GPP_flux_umol_CO2_m2_s, PAR.match)

flux2019_PARcorrect<- rbind(flux2019_PARcorrect_may, flux2019_PARcorrect_July)
flux2019_PAR<- left_join(flux2019, flux2019_PARcorrect, by = c("PlotID_Date", "GPP_flux_umol_CO2_m2_s"))%>%
  mutate(PAR = coalesce(PAR, PAR.match))%>%
  select(-PAR.match)

#write.csv(flux2019_PAR, "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\AnalysisR\\2019HMRflux_PARcorrection.csv")
