# C vs OTC Iskoras
setwd("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\")
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(vegan)

# VegetationData
VegComposition<-read.csv2("VegetationData\\Vegetation_plot_visual.csv")

# Moss species in different plots would be better!
VegComp2021<- VegComposition%>%
  filter(Year==2021)%>%
  filter(Habitat != "S")

VegMatrix<- VegComp2021%>%
  select(PlotID:moss)%>%
  column_to_rownames(var="PlotID")

VegMatrix[is.na(VegMatrix)] <- 0 # replace NA with 0
VegMatrix<-VegMatrix%>%
  filter_all(any_vars(. != 0))
VegMatrix<-as.matrix(VegMatrix)

Iskoras_NMDS<-metaMDS(VegMatrix, distance="bray")
stressplot(Iskoras_NMDS)

#plot NMDS
plot_NMDS <- scores(Iskoras_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  mutate(PlotID = site)%>%
  select(-site)%>%
  full_join(VegComp2021, by = "PlotID")%>%
  mutate(Habitat = as.factor(Habitat))

plot_nmds <- ggplot(plot_NMDS, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(fill= Treatment, shape = Habitat), size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill= Treatment, shape = Habitat, linetype = Treatment, col = Treatment), size = 1) +
  scale_color_manual(values= c("#5ab4ac", "#d8b365"), name = "Treatment", labels = c("C", "OTC"))+ 
  scale_fill_manual(values= c("#5ab4ac", "#d8b365"), name = "Treatment", labels = c("C", "OTC"))+ 
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  scale_shape_manual(values= c(24, 22, 21), name = "Habitat", labels = c("Palsa", "Thawslump", "Vegetated Pond"))+
  labs(title = "NMDS")+
  theme_classic()
plot_nmds


#### TRAIT DATA
### !!! for relatively small leaves 3 leaf samples were taken

Traitdata_raw<-read.csv2("VegetationData\\LeafTraits\\LeafTraits_Iskoras_leaves.csv")
Traitdata<- Traitdata_raw%>%
  mutate(LDMC1 = Dry_weight1/Wetweight_1,
         LDMC2 = Dry_weight1/Wetweight_2,
         LDMC3 = Dry_weight1/Wetweight_3,
         SLA = LA/Dry_weight1)%>% # use Dryweight 1 as that is total dry weight of multiple leaflets used for Leaf area scan
  gather(measurement, LDMC, LDMC1:LDMC3)%>%
  group_by(SampleID, Species, Sample, Treatment, SampleLocation)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)%>% # calculate average LDMC per SampleID
  gather(measurement, LT, LT1:LT3)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)%>% #calculate average leaf thickness per SampleID
  ungroup()

Traitdata%>%
  select(Species, Treatment, VH, LA, LDMC, SLA, LT)%>%
  gather(Trait, value, VH:LT)%>%
  ggplot(aes(Treatment, value, fill=Treatment))+
  geom_boxplot()+
  facet_wrap(Trait~Species, scales="free")

SpeciesTraits<- Traitdata%>%
  group_by(Species, Treatment)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)%>% #calculate average traits per species and treatment
  select(Species, Treatment, VH, LA, LDMC, SLA, LT)%>%
  ungroup()

TraitMatrix<- SpeciesTraits%>%
  unite(Species_Treatment, c("Species", "Treatment"))%>%
  column_to_rownames(var="Species_Treatment")
TraitMatrix[is.na(TraitMatrix)] <- 0 # replace NA with 0
TraitMatrix<-as.matrix(TraitMatrix)

SpeciesTreatmentMatrix<- VegComp2021%>%
  gather(Species, cover, And.pol:Eri.vag)%>%
  unite(Species_Treatment, c("Species", "Treatment"))%>%
  select(PlotID, Species_Treatment, cover)%>%
  spread(Species_Treatment, cover)%>%
  column_to_rownames(var="PlotID")

SpeciesTreatmentMatrix[is.na(SpeciesTreatmentMatrix)] <- 0 # replace NA with 0
SpeciesTreatmentMatrix<-as.matrix(SpeciesTreatmentMatrix[, colSums(SpeciesTreatmentMatrix) != 0])

# calculate CWM traits 
library(FD)
FD_Traits<-dbFD(TraitMatrix, SpeciesTreatmentMatrix, w.abun= TRUE, calc.CWM = TRUE)
Iskoras_CWM<-FD_Traits$CWM%>%
  rownames_to_column(var = "PlotID_Treatment")%>%
  rename(PlotID = PlotID_Treatment)%>%
  mutate(Treatment = str_extract(PlotID, "[^_]+$"),
         Transect = substring(PlotID_Treatment, 1,1),
         Habitat = substring(PlotID_Treatment, 2,3),
         Habitat = sub("_", "", Habitat))

Iskoras_CWM%>%
  gather(Trait, value, VH:LT)%>%
  ggplot(aes(Habitat, value, fill = Treatment))+
  geom_boxplot()+
  facet_wrap(~Trait, scales = "free")

VegComp2021_Traits<- left_join(VegComp2021, Iskoras_CWM, by = "PlotID")

#PCA
library(ggfortify)
library(devtools)
library(factoextra)

TraitPCA<- VegComp2021_Traits%>%
  select(VH, LA, SLA, LT, LDMC)
TraitPCA <- princomp(TraitPCA, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

PCAplot<- autoplot(TraitPCA, data = VegComp2021_Traits, fill= "Treatment", shape = "Habitat", size = 4,
                   loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                   loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  stat_ellipse(aes(linetype = Treatment, col = Treatment), size = 1) +
  scale_fill_manual(values= c("#5ab4ac", "#d8b365"), name = "Treatment", labels = c("C", "OTC"))+ 
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  scale_shape_manual(values= c(24, 22, 21), name = "Habitat", labels = c("Palsa", "Thawslump", "Vegetated Pond"))+
  theme_classic()
PCAplot
  

# NDVI data
NDVIdata<-read.csv2("VegetationData\\NDVI_Greenseeker.csv")%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  filter(!grepl("water", Comment))%>% #filter out plots that were fully or partially under water
  gather(Measurement, NDVI, Value1:Value2)%>%
  group_by(Date, Transect, Habitat, Treatment, PlotID)%>%
  summarise(NDVI = mean(NDVI, na.rm=TRUE))%>%
  ungroup()

NDVImean<- NDVIdata%>%
  group_by(Date, Habitat, Treatment)%>%
  summarise(NDVI.sd = sd(NDVI, na.rm = TRUE),
            NDVI.mean = mean(NDVI, na.rm=TRUE))%>%
  ungroup()

ggplot(NDVImean, aes(as.factor(Date), NDVI.mean, color= Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin=NDVI.mean-NDVI.sd, ymax=NDVI.mean+NDVI.sd), width=.2)+
  facet_wrap(~Habitat)


#####################################################################################################################################
####### Cflux data
setwd("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\")
# SR 2021 data
library(tidyverse)
library(ggplot2)

## NEED TO CHECK whether I need to do conversion still
# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp)) 
# (0.08205*273.15) equals 22.4 L/mol, which is the standard molar volume at standard conditions (temp = 0 and 1 atm pressure)
# think about using FluxCalR of gasfluxes for processing data

#Soil Respiration
# ### HMR output based on V  in L, A in m2, CH4 in ppb, C02 in ppm

# load HMR output 2020; collar Area () volume taken into account
SR2020_CO2<-read.csv("SR2020_CO2_HMRoutput.csv")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "comment"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3))%>%
  unite(PlotID, Plot:Treatment, remove =FALSE )%>%
  select(PlotID, Date, f0, LR.f0, Method)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)%>%
  rowid_to_column(var='FluxID')

## Environmental data 2020
SRenvdata04102020<-read.csv2("2020\\SRmetadata_04102020.csv")
SRenvdata08092020<-read.csv2("2020\\SRmetadata_08092020.csv")
SRenvdata11082020<-read.csv2("2020\\SRmetadata_11082020.csv")
SRenvdata1507020<-read.csv2("2020\\SRmetadata_15072020.csv")
SRenvdata17072020<-read.csv2("2020\\SRmetadata_17072020.csv")

SRenvdata2020<-rbind(SRenvdata1507020, SRenvdata17072020, SRenvdata11082020, SRenvdata08092020, SRenvdata04102020 )%>%
  mutate(Habitat = recode(Habitat, WGA = "WG", WGB = "WG", "WG "= "WG"))
#write.csv(SRenvdata2020, "SRenvdata2020.csv")

# match fluxdata and envdata
SR_FluxEnv2020<- left_join(SRenvdata2020, SR2020_CO2, by= c("PlotID", "Date"))%>%
  group_by(Date, PlotID)%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))


# load HMR output, collar volume taken into account 
SR2021_CO2<-read.csv("SR2021_CO2_HMRoutput.csv")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "info"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3))%>%
  unite(PlotID, Plot:Treatment, remove =FALSE )%>%
  select(PlotID, Date, f0, LR.f0, Method)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)%>%
  rowid_to_column(var='FluxID')

SR2021_CH4<-read.csv("SR2021_CH4_HMRoutput.csv", sep = ";", dec = ".")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "H2O"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3))%>%
  unite(PlotID, Plot:Treatment, remove =FALSE )%>%
  select(PlotID, Date, f0, LR.f0, Method)%>%
  rename(CH4.f0 = f0, CH4.LR = LR.f0, Method.CH4 = Method)%>%
  rowid_to_column(var='FluxID')

SR2021_CO2CH4<- inner_join(SR2021_CO2, SR2021_CH4, by= c("Date","PlotID", "FluxID"))%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

### environmental metadata SR ####
SRenvdata04062021<-read.csv2("2021\\SRmetadata_04062021.csv")
SRenvdata19082021<-read.csv2("2021\\SRmetadata_19082021.csv")
SRenvdata19082021_2<-read.csv2("2021\\SRmetadata_19082021_2.csv")
SRenvdata12092021<-read.csv2("2021\\SRmetadata_12092021.csv")
SRenvdata21082021<-read.csv2("2021\\SRmetadata_21082021.csv")
SRenvdata22072021<-read.csv2("2021\\SRmetadata_22072021.csv")
SRenvdata30062021<-read.csv2("2021\\SRmetadata_30062021.csv")

SRenvdata2021<- rbind(SRenvdata04062021, SRenvdata30062021, SRenvdata22072021, SRenvdata19082021, SRenvdata19082021_2, SRenvdata21082021, SRenvdata12092021)%>%
  mutate(Date = recode(Date, "19.08.2021" = "18.08.2021", "21.08.2021" = "18.08.2021"))%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat = recode(Habitat, WGA = "WG", WGB = "WG", "WG "= "WG"))
#write.csv(SRenvdata2021, "SRenvdata2021.csv")

# match fluxdata and envdata
SR_FluxEnv<- left_join(SRenvdata2021, SR2021_CO2CH4, by= c("PlotID", "Date"))%>%
  group_by(Date, PlotID)%>%
  distinct(CO2.f0, .keep_all = TRUE)%>%
  filter(CH4.f0<1000) ## clean data SR CH4   filter(f0<1000)

SR_FluxEnv%>%
  gather(GHG, flux, c("CO2.f0", "CH4.f0"))%>%
  ggplot(aes(as.factor(Date), flux, fill=Treatment))+
  geom_boxplot()+
  facet_grid(GHG~Habitat, scales = "free")
  
SR_FluxEnv%>%
  gather(GHG, flux, c("CO2.f0", "CH4.f0"))%>%
  ggplot(aes(SoilTemp1, flux, color=Treatment))+
  geom_point()+
  stat_smooth(method = "lm")+
  facet_grid(GHG~Habitat, scales = "free")

### Use environmental data to predict fluxes, see Konsta paper on tundra
### V = 2.5 L, A = 0.0625 m2, CH4 in ppb, C02 in ppm

##### NET ECOSYSTEM EXCHANGE 

# NEE chamber
NEE2021_CO2<-read.csv("2021\\HMRoutput_NEE2021_CO2.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  select(PlotID, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)

NEE2021_CH4<-read.csv("2021\\HMRoutput_NEE2021_CH4.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  select(PlotID, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CH4.f0 = f0, CH4.LR = LR.f0, Method.CH4 = Method)


NEE2021_CO2CH4<- inner_join(NEE2021_CO2, NEE2021_CH4, by= c("Date","PlotID", "FluxID", "Cover"))

## read in metadata
NEEenvdata03062021<-read.csv2("2021\\NEEmetadata_03062021.csv")
NEEenvdata04062021<-read.csv2("2021\\NEEmetadata_04062021.csv")
NEEenvdata02072021<-read.csv2("2021\\NEEmetadata_02072021.csv")%>%
  select(-X)# Check PAR ECtower
NEEenvdata03072021<-read.csv2("2021\\NEEmetadata_03072021.csv")
NEEenvdata20072021<-read.csv2("2021\\NEEmetadata_20072021.csv")
NEEenvdata21072021<-read.csv2("2021\\NEEmetadata_21072021.csv")
NEEenvdata23072021<-read.csv2("2021\\NEEmetadata_23072021.csv")
NEEenvdata17082021<-read.csv2("2021\\NEEmetadata_17082021.csv")
NEEenvdata18082021<-read.csv2("2021\\NEEmetadata_18082021.csv")
NEEenvdata21082021<-read.csv2("2021\\NEEmetadata_21082021.csv")
NEEenvdata11092021<-read.csv2("2021\\NEEmetadata_11092021.csv")
NEEenvdata12092021<-read.csv2("2021\\NEEmetadata_12092021.csv")

NEEenvdata2021<- rbind(NEEenvdata03062021, NEEenvdata04062021, NEEenvdata02072021,NEEenvdata03072021, NEEenvdata20072021, NEEenvdata21072021, NEEenvdata23072021, NEEenvdata17082021, NEEenvdata18082021, NEEenvdata21082021, NEEenvdata11092021, NEEenvdata12092021)

#!!!! CHECK PAR for NEE measurements 02072021, PAR sensor wrong 

# link Environmental data and fluxdata
NEE_FluxEnv2021<- left_join(NEEenvdata2021, NEE2021_CO2CH4, by= c("PlotID", "Date", "Cover"))%>%
  group_by(Date, PlotID, Cover)%>%
  distinct(CO2.f0, .keep_all = TRUE)%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(month = lubridate::month(Date))%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Habitat %in% c("S", "P", "M", "WG"))%>%
  filter(CH4.f0< 1000)%>%
  filter(CH4.f0>-1000)

# RECO measurements
NEE_FluxEnv2021%>%
  filter(Cover == "RECO")%>%
  gather(GHG, flux, c("CO2.f0", "CH4.f0"))%>%
  ggplot(aes(as.factor(month), flux, fill=Treatment))+
  geom_boxplot()+
  facet_grid(GHG~Habitat, scales = "free")

# NEE measurements
NEE_FluxEnv2021%>%
  filter(Cover != "RECO")%>%
  gather(GHG, flux, c("CO2.f0", "CH4.f0"))%>%
  ggplot(aes(as.factor(month), flux, fill=Treatment))+
  geom_boxplot()+
  facet_grid(GHG~Habitat, scales = "free")
