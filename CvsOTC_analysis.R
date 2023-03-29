# C vs OTC Iskoras
setwd("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\")
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
#library(readxl)
library(vegan)

# Vegetation Composition Data
VegComposition<-read.csv2("VegetationData\\Vegetation_plot_visual.csv")

# Select 2021 vegetation data as this is most recent
VegComp2021<- VegComposition%>%
  filter(Year==2021)%>%
  filter(Habitat != "S") # Remove Soil palsas from vegetation analysis no vascular species

# Create a vegetation cover matrix
VegMatrix<- VegComp2021%>%
  select(PlotID:Sphagnum.sp)%>%
  column_to_rownames(var="PlotID")

VegMatrix[is.na(VegMatrix)] <- 0 # replace NA with 0 cover value
VegMatrix<-as.matrix(VegMatrix)

# NMDS analysis with Bray-Curtis distance which is not affected by number of null values between samples like Euclidean distance
Iskoras_NMDS<-metaMDS(VegMatrix, distance="bray")
stressplot(Iskoras_NMDS)

#extact NMDS scores for plots and species
data.scores <- scores(Iskoras_NMDS) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores.sites <- as.data.frame(data.scores$sites)
data.scores.sites$sitename <- rownames(data.scores.sites)  # create a column of site names, from the rownames of data.scores
data.scores.sites$Treatment <- VegComp2021$Treatment#  add the treatment variable 
data.scores.sites$Habitat <- VegComp2021$Habitat #  add Habitat variable
data.scores.sites<-data.scores.sites%>%
  mutate(Habitat =recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", WG= "Vegetated Pond")) # recode Habitat
data.scores.sites$Habitat <- factor(data.scores.sites$Habitat, levels = c("Vegetated Palsa", "Thawslump", "Vegetated Pond"))

species.scores <- as.data.frame(data.scores$species)
species.scores$species <- rownames(species.scores)

ggplot() + 
  geom_point(data=data.scores.sites, aes(x=NMDS1, y=NMDS2, shape=Treatment, fill= Habitat), size=4) + # add the point markers
  stat_ellipse(data=data.scores.sites, aes(x=NMDS1, y=NMDS2, linetype = Treatment, col = Habitat), linewidth = 1) +
  geom_text(data=species.scores, aes(x=NMDS1, y=NMDS2, label= species), alpha=0.9) +  # add the species labels
  scale_color_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat")+
  scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat")+ 
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  scale_linetype_manual(values= c("solid", "dashed"), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  labs(title = "NMDS")+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

Iskoras_env <-VegComp2021%>%
  select(Habitat, Treatment, Transect)

### analysis of dissimilarites a.k.a. non-parametric
### permutational anova
adonis2(VegMatrix ~ Habitat * Treatment, strata = Iskoras_env$Transect, data=Iskoras_env, perm=999 )
#Significant differences between Habitat, minor significance for Treatment  

###########################################################################################################################################################
#### TRAIT DATA
### !!! for relatively small leaves 3 leaf samples were taken, only 1st leave used in analysis

Traitdata_raw<-read.csv2("VegetationData\\LeafTraits\\LeafTraits_Iskoras_leaves.csv")
Traitdata<- Traitdata_raw%>%
  mutate(LDMC1 = Dry_weight1/Wetweight_1,
         LDMC2 = Dry_weight2/Wetweight_2,
         LDMC3 = Dry_weight3/Wetweight_3,
         SLA = LA/Dry_weight1)%>% # use Dryweight 1 as that is leaf used for Leaf area scan
  #gather(measurement, LDMC, LDMC1:LDMC3)%>%
  #group_by(SampleID, Species, Sample, Treatment, Habitat)%>%
  #summarise_if(is.numeric, mean, na.rm = TRUE)%>% # calculate average LDMC per SampleID
  #group_by(SampleID, Species, Sample, Treatment, Habitat)%>%
  #gather(measurement, LT, LT1:LT3)%>%
  #summarise_if(is.numeric, mean, na.rm = TRUE)%>% #calculate average leaf thickness per SampleID
  ungroup()

Traitdata%>%
  select(Species,Treatment, Habitat, VH, LA, LDMC1, SLA, LT1)%>%
  gather(Trait, value, VH:LT1)%>%
  ggplot(aes(Species, value, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Trait~Habitat, scales="free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

Traitdata%>%
  select(Species, Habitat, VH, LA, LDMC1, SLA, LT1)%>%
  gather(Trait, value, VH:LT1)%>%
  ggplot(aes(Habitat, value, fill=Habitat))+
  geom_boxplot()+
  facet_grid(Trait~Species, scales="free")


library(glmmTMB)
library(DHARMa)

# What distribution to use?
Traitmodel<- Traitdata%>%
  filter(!Species %in% c("Arc.alp", "Eri.vag"))%>%
  mutate(VHlog = log(VH),
         LAlog = log(SLA),
         LDMC1log = log(LDMC1),
         SLAlog = log(SLA),
         LTlog = log(LT))%>%
  filter(Habitat != "WG")


hist(log(Traitmodel$VH))
hist(log(Traitmodel$LA))
hist(log(Traitmodel$LDMC1))
hist(log(Traitmodel$SLA))
hist(log(Traitmodel$LT))

# Not sure how to parameterize model to test OTC treatment effect on Traits across 
m1 <- glmmTMB(VHlog ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)

m2 <- glmmTMB(VHlog ~ Treatment + Species:Treatment + (1|Habitat ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m2)
plotQQunif(m2)
plotResiduals(m2)

m1 <- glmmTMB(LAlog ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)

m1 <- glmmTMB(LDMC1log ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)

m1 <- glmmTMB(LTlog ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)

m1 <- glmmTMB(SLAlog ~ Habitat * Treatment + (1|Species ), family= gaussian(link = "identity"), data=Traitmodel)
summary(m1)
plotQQunif(m1)
plotResiduals(m1)


#### Community weighted Trait Values
# Mean trait data per species, Habitat and  Treatment

# function to calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

# Plot mean + sd of trait values for each species in each habitat and treatment
Traitdata%>%
  select(SampleID, Species, Habitat, Treatment, VH, LA, LT1, LA, SLA, LDMC1)%>%
  gather(Trait, value, VH:LDMC1)%>%
  group_by(Species, Treatment, Habitat, Trait)%>%
  summarise(mean=mean(value), se=se(value))%>% #calculate mean, sd traits per species, treatment, habitat
  ggplot(aes(Habitat, mean, color=Treatment))+ 
  geom_point(position=position_dodge(width = 0.5), stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width = 0.5), width=.2)+
  facet_grid(Trait~Species, scales = "free")


# create traitmatrix for calculating community weighted trait values based on species, habitat and treatment specific values
SpeciesTrait<-  Traitdata%>%
  select(SampleID, Species, Habitat, Treatment, VH, LA, LT1, LA, SLA, LDMC1)%>%
  mutate(Habitat = recode(Habitat, WG ="M"))%>% #recode WG to M as trait values do not vary between these habitats
  group_by(Species, Treatment, Habitat)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

SpeciesCover<- VegComp2021%>%
  gather(Species, cover, And.pol:Eri.vag)%>%
  mutate(Trait_Habitat = recode(Habitat, WG = "M"))

CWMtraits <- left_join(SpeciesCover, SpeciesTrait, by= c("Species", "Treatment", "Trait_Habitat"="Habitat"))%>%
  drop_na(cover)%>%
  gather(Trait, value, VH:LDMC1)%>%
  group_by(PlotID, Habitat, Treatment, Trait)%>%
  summarise(CWM = weighted.mean(value, cover, na.rm = TRUE))

# function to calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

CWMtraits%>%
  group_by(Treatment, Habitat, Trait)%>%
  summarise(mean=mean(CWM), se=se(CWM))%>% #calculate mean, se traits per species, treatment, habitat
  ggplot(aes(Habitat, mean, color=Treatment))+ 
  geom_point(position=position_dodge(width = 0.5), stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width = 0.5), width=.2)+
  facet_wrap(~Trait, scales = "free")

#%>%
#  unite(Species_Habitat_Treatment, c("Species", "Habitat", "Treatment"))%>%
#  filter(!Species_Habitat_Treatment == "Bet.nan_P_C")%>% # remove Bet.nan control P values as not in vegetation data
#  column_to_rownames(var="Species_Habitat_Treatment")


VegComp2021_Traits<- left_join(VegComp2021, CWMtraits, by = c("PlotID", "Habitat", "Treatment"))%>%
  spread(Trait, CWM)%>%
  rename(LT = "LT1", LDMC = "LDMC1")%>%
  mutate(Habitat =recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", WG= "Vegetated Pond")) # recode Habitat
VegComp2021_Traits$Habitat <- factor(VegComp2021_Traits$Habitat, levels = c("Vegetated Palsa", "Thawslump", "Vegetated Pond"))

#PCA
library(ggfortify)
library(devtools)
library(factoextra)

# Not sure about LT measurments
TraitPCA<- VegComp2021_Traits%>%
  select(VH, LA, SLA, LDMC)
TraitPCA <- princomp(TraitPCA, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

PCAplot<- autoplot(TraitPCA, data = VegComp2021_Traits,  size = 4, fill= "Habitat", shape = "Treatment",
                   loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                   loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat")+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )
PCAplot

Vegetation_P<- VegComp2021_Traits%>%
  filter(Habitat == "P")

Trait_P<- VegComp2021_Traits%>%
  filter(Habitat == "P")%>%
  select(VH, LA, SLA, LDMC)

TraitPCA_P <- princomp(Trait_P, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

palsaPCA<- autoplot(TraitPCA_P, data = Vegetation_P,  size = 4, fill= "Habitat", shape = "Treatment",
                   loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                   loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#fc8d62"), name = "Habitat", labels = c("Palsa"))+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()


Vegetation_M<- VegComp2021_Traits%>%
  filter(Habitat == "M")

Trait_M<- VegComp2021_Traits%>%
  filter(Habitat == "M")%>%
  select(VH, LA, SLA, LDMC)

TraitPCA_M <- princomp(Trait_M, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

thawslumpPCA<- autoplot(TraitPCA_M, data = Vegetation_M,  size = 4, fill= "Habitat", shape = "Treatment",
                    loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                    loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#66c2a5"), name = "Habitat", labels = c("Thawslump"))+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()


Vegetation_WG<- VegComp2021_Traits%>%
  filter(Habitat == "WG")

Trait_WG<- VegComp2021_Traits%>%
  filter(Habitat == "WG")%>%
  select(VH, LA, SLA, LDMC)

TraitPCA_WG <- princomp(Trait_WG, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

VegetatedpondPCA<- autoplot(TraitPCA_WG, data = Vegetation_WG,  size = 4, fill= "Habitat", shape = "Treatment",
                        loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                        loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#8da0cb"), name = "Habitat", labels = c("Vegetated Pond"))+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()


library(cowplot)
plot_grid(PCAplot, palsaPCA, thawslumpPCA, VegetatedpondPCA, labels= c("A", "B", "C", "D"))

# leaf thicknes in same direction as VH?
  

# NDVI data
se <- function(x) sd(x)/sqrt(length(x))
# group by month and year!
NDVIdata<-read.csv2("VegetationData\\NDVI_Greenseeker.csv")%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"),
         Month = format(as.Date(Date, format="%d/%m/%Y"),"%m"),
         Year = format(as.Date(Date, format="%d/%m/%Y"),"%Y"))%>%
  filter(!grepl("water", Comment))%>% #filter out plots that were fully or partially under water
  gather(Measurement, NDVI, Value1:Value2)%>%
  group_by(Year, Month, Transect, Habitat, Treatment, PlotID)%>%
  summarise(NDVI = mean(NDVI, na.rm=TRUE))%>%
  ungroup()

NDVImean<- NDVIdata%>%
  group_by(Month, Habitat, Treatment)%>%
  summarise(NDVI.se = se(NDVI),
            NDVI.mean = mean(NDVI))%>%
  ungroup()%>%
  mutate(Habitat =recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", S = "Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
NDVImean$Habitat <- factor(NDVImean$Habitat, levels = c("Vegetated Palsa", "Soil Palsa", "Thawslump", "Vegetated Pond"))


ggplot(NDVImean, aes(as.factor(Month), NDVI.mean, color= Habitat, shape= Treatment)) +
  geom_point(position = position_dodge(0.8), size=4) +
  geom_errorbar(aes(ymin=NDVI.mean-NDVI.se, ymax=NDVI.mean+NDVI.se), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  labs(x = "Month of the year", y= "NDVI")+
  facet_grid()+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

####################################################################################################################################
####### Abiotic conditions (TOMST loggers)
#! NEED TO CHECK LOGGERID and LOCATION!!!
library(lubridate)
TomstData<-read.csv("AnalysisR\\TOMSTdata_SMcalculated.csv")
se <- function(x) sd(x)/sqrt(length(x))

### calculate mean temp for habitat and treatments
TomstData<-TomstData%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  select(PlotID:LoggerID, Date, Date_Time, SoilTemperature:RawSoilmoisture, Soilmoisture_Volumetric)%>%
  mutate(Date = as.Date(Date),
         DateTime = as.POSIXct(strptime(Date_Time, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")),
         Hour = hour(DateTime))

TomstData_HourlyPlotID<- TomstData%>%
  group_by(PlotID, Transect, Habitat, Treatment, Date, Hour)%>%
  summarise(SoilTemperature = mean(SoilTemperature, na.rm = TRUE), 
            GroundTemperature = mean(GroundTemperature, na.rm = TRUE),
            AirTemperature = mean(AirTemperature, na.rm = TRUE),
            Soilmoisture_Volumetric = mean(Soilmoisture_Volumetric, na.rm = TRUE))%>%
            ungroup()

# calculate min, max and mean for each Climatic variable measured by TOMST
ma <- function(x, n = 7){  stats::filter(x, rep(1 / n, n), sides = 2)} # moving average for 7 days 

DailyVariablity<- TomstData%>%
  group_by(PlotID, Transect, Habitat, Treatment, Date)%>%
  gather(Climatevariable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  select(PlotID, Transect, Habitat, Treatment, Date, Climatevariable, value)%>%
  group_by(Habitat, Treatment, Date, Climatevariable)%>%
  summarise(min = min(value),
            max = max(value),
            mean = mean(value))%>%
  mutate(Treatment = factor(Treatment, c("OTC", "C"), ordered = T)) %>% 
  group_by(Habitat, Date, Climatevariable) %>% 
  mutate(diff_min = min - lag(min, 1),
         diff_max = max - lag(max, 1),
         diff_mean = mean - lag(mean, 1))%>%
  filter(Treatment != "C")

library(zoo)
DailyVariablity%>%
  gather(Difference, value, diff_min:diff_mean)%>%
  group_by(Habitat, Climatevariable)%>%
  mutate(moving_avg7 = rollmean(value, k=7, fill=NA, align='right'))%>%
  filter(Climatevariable != "RawSoilmoisture")%>%
  filter(Date > "2021-06-01" & Date <"2021-09-01" )%>%
  ggplot(aes(Date, value, col= Difference))+
  geom_point()+
  geom_line(aes(y=moving_avg7))+
  facet_grid(Climatevariable~Habitat, scales = "free")


# summary Hourly per Transect and Habitat
TomstData_MeanHourlyTransect<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat, Treatment, Transect, Date, Hour, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, se = se))

# Plot SoilTemperature 
TomstData_MeanHourlyTransect%>%
  filter(Date == "2021-07-01")%>%
  filter(Climate_variable %in% c("SoilTemperature"))%>%
  ggplot(aes(Hour, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")

# Plot AirTemperature 
TomstData_MeanHourlyTransect%>%
  filter(Date == "2021-07-01")%>%
  filter(Climate_variable %in% c("AirTemperature"))%>%
  ggplot(aes(Hour, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")

# Plot Soilmoisture
TomstData_MeanHourlyTransect%>%
  filter(Date == "2021-07-01")%>%
  filter(Climate_variable %in% c("Soilmoisture_Volumetric"))%>%
  ggplot(aes(Hour, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")


# summary Hourly per Habitat
TomstData_MeanHourlyHabitat<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  filter(Date > "2021-06-01" & Date <"2021-09-01")%>%
  group_by(Habitat, Treatment, Hour, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, se =se))

# plot summer season hourly based on June-August data in 2021
TomstData_MeanHourlyHabitat%>%
  filter(Climate_variable %in% c("AirTemperature", "SoilTemperature"))%>%
  ggplot(aes(Hour, Mean, col= Habitat, linetype =Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Habitat), alpha=0.3) +
  facet_grid(Climate_variable~Habitat, scales="free")

##### DAILY
# Summary Daily Per Transect and Habitat
TomstData_MeanDailyTransect<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat, Treatment, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, se =se))

# plot summer period
TomstData_MeanDailyTransect%>%
  filter(Date > "2021-06-01" & Date <"2021-09-01")%>%
  filter(Climate_variable %in% c("SoilTemperature"))%>%
  ggplot(aes(Date, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Treatment), alpha=0.3) +
  facet_grid(Climate_variable~Habitat, scales="free")

 TomstData_MeanDailyTransect%>%
  filter(Date > "2022-06-01" & Date <"2022-09-01")%>%
  filter(Climate_variable %in% c("AirTemperature"))%>%
  ggplot(aes(Date, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Transect~Habitat, scales="free")

TomstData_MeanDailyTransect%>%
  filter(Date > "2022-06-01" & Date <"2022-09-01")%>%
  filter(Climate_variable %in% c("Soilmoisture_Volumetric"))%>%
  ggplot(aes(Date, Mean, col= Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Treatment), alpha=0.3) +
  facet_grid(~Habitat, scales="free")

# Summary per Habitat
TomstData_MeanDailyHabitat<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat,Treatment, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd))

# plot summer period
TomstData_MeanDailyHabitat%>%
  #filter(Date > "2022-06-01" & Date <"2022-09-01")%>%
  filter(Climate_variable %in% c("AirTemperature", "SoilTemperature", "Soilmoisture_Volumetric"))%>%
  ggplot(aes(Date, Mean, col= Treatment))+
  geom_line()+
  #geom_ribbon(aes(ymin = Mean-Sd, ymax = Mean+Sd, fill = Treatment), alpha=0.3) +
  facet_grid(Climate_variable~Habitat, scales="free")


#####################################################################################################################################
####### Cflux data
# SR 2021 data
library(here)
library(tidyverse)
library(ggplot2)


#Soil Respiration
# ### HMR output based on V  in L, A in m2, CH4 in ppb, C02 in ppm
# load HMR output 2020; collar Area and volume taken into account
SR2020_CO2<-read.csv("Cflux\\SR2020_CO2_HMRoutput.csv")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "comment"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3),
         Date = as.Date(Date, "%Y-%m-%d"))%>%
  unite(PlotID, Plot:Treatment, remove = FALSE)%>%
  #select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)%>%
  rowid_to_column(var='FluxID')%>%
  select(-comment)

## Environmental data 2020 and 2021
SRenvdata04102020<-read.csv2("Cflux\\2020\\SRmetadata_04102020.csv")
SRenvdata08092020<-read.csv2("Cflux\\2020\\SRmetadata_08092020.csv")
SRenvdata11082020<-read.csv2("Cflux\\2020\\SRmetadata_11082020.csv")
SRenvdata1507020<-read.csv2("Cflux\\2020\\SRmetadata_15072020.csv")
SRenvdata17072020<-read.csv2("Cflux\\2020\\SRmetadata_17072020.csv")
SRenvdata2020<-rbind(SRenvdata1507020, SRenvdata17072020, SRenvdata11082020, SRenvdata08092020, SRenvdata04102020 )%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat = dplyr::recode(Habitat, WGA = "WG", WGB = "WG", "WG "= "WG"))

SR2020_CO2_env<- left_join(SR2020_CO2, SRenvdata2020, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment"))%>%
  distinct(FluxID, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))

## Add airtemp based on TOMSTloggerData for measurement hour
SR2020_CO2_env<-left_join(SR2020_CO2_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
# (0.08205*273.15) equals 22.4 L/mol, which is the standard molar volume at standard conditions (temp = 0 and 1 atm pressure)
#!! for now recalculate flux with soiltemp as loggerID missing for some TOMSTloggers
SR2020_CO2_env<- SR2020_CO2_env%>%
  mutate(CO2flux = CO2.f0/(0.08205*(273.15+SoilTemp1)),
         CO2flux.LR = CO2.LR/(0.08205*(273.15+SoilTemp1)))

# load HMR output, collar volume taken into account 
SR2021_CO2<-read.csv("Cflux\\2021\\HMRoutput_SR2021_CO2.csv")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "H2O"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3),
         Date = as.Date(Date, "%Y-%m-%d"))%>%
  unite(PlotID, Plot:Treatment, remove =FALSE )%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)%>%
  rowid_to_column(var='FluxID')%>%
  select(-H2O)

# SR cH4 data
SR2021_CH4<-read.csv2("Cflux\\2021\\HMRoutput_SR2021_CH4.csv", dec = ".")%>%
  separate(Series, sep = "_", into = c("Plot", "Treatment", "Date", "H2O"))%>%
  mutate(Transect = substring(Plot,1,1),
         Habitat = substring(Plot, 2,3),
         Date = as.Date(Date, "%Y-%m-%d"))%>%
  unite(PlotID, Plot:Treatment, remove =FALSE )%>%
  rename(CH4.f0 = f0, CH4.LR = LR.f0, Method.CH4 = Method)%>%
  rowid_to_column(var='FluxID')%>%
  select(-H2O)
  

## Environmental data 2021
SRenvdata04062021<-read.csv2("Cflux\\2021\\SRmetadata_04062021.csv")
SRenvdata19082021<-read.csv2("Cflux\\2021\\SRmetadata_19082021.csv")
SRenvdata19082021_2<-read.csv2("Cflux\\2021\\SRmetadata_19082021_2.csv")
SRenvdata12092021<-read.csv2("Cflux\\2021\\SRmetadata_12092021.csv")
SRenvdata21082021<-read.csv2("Cflux\\2021\\SRmetadata_21082021.csv")
SRenvdata22072021<-read.csv2("Cflux\\2021\\SRmetadata_22072021.csv")
SRenvdata30062021<-read.csv2("Cflux\\2021\\SRmetadata_30062021.csv")
SRenvdata2021<- rbind(SRenvdata04062021, SRenvdata30062021, SRenvdata22072021, SRenvdata19082021, SRenvdata19082021_2, SRenvdata21082021, SRenvdata12092021)%>%
  mutate(Date = dplyr::recode(Date, "19.08.2021" = "18.08.2021", "21.08.2021" = "18.08.2021"))%>% #match envdata to flux data
  mutate(Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat = dplyr::recode(Habitat, WGA = "WG", WGB = "WG", "WG "= "WG"))

# combine SR2021data with environmental data
SR2021_CO2_env<- left_join(SR2021_CO2, SRenvdata2021, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment"))%>%
  distinct(FluxID, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))

## Add airtemp based on TOMSTloggerData for measurement hour
SR2021_CO2_env<-left_join(SR2021_CO2_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
SR2021_CO2_env<- SR2021_CO2_env%>%
  mutate(CO2flux = CO2.f0/(0.08205*(273.15+SoilTemp1)),
         CO2flux.LR = CO2.LR/(0.08205*(273.15+SoilTemp1)))

# bind together 2020 and 2021 CO2 flux data
SR20202021_CO2_env<- rbind(SR2020_CO2_env, SR2021_CO2_env)

# combine SR2021data CH4 with environmental data
SR2021_CH4_env<- left_join(SR2021_CH4, SRenvdata2021, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment"))%>%
  distinct(FluxID, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))

## Add airtemp based on TOMSTloggerData for measurement hour
SR2021_CH4_env<-left_join(SR2021_CH4_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
SR2021_CH4_env<- SR2021_CH4_env%>%
  filter(Method.CH4 != "No flux")%>%
  mutate(CH4flux = CH4.f0/(0.08205*(273.15+SoilTemp1)),
         CH4flux.LR = CH4.LR/(0.08205*(273.15+SoilTemp1)))


########################################################################################################################################
### DATA CLEANING & visualitions
SR20202021_CO2_env_clean<-SR20202021_CO2_env%>%
  mutate(Month = as.factor(month(Date)),
         Year = as.factor(year(Date)))%>%
  filter(Comment != "redo")%>%
  filter(Habitat != "W")%>%
  filter(CO2flux > 0)%>% # remove negative values
  #filter(CO2flux< 2.1)%>% #remove 3 outliers
  mutate(Habitat =dplyr::recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", S = "Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
SR20202021_CO2_env_clean$Habitat <- factor(SR20202021_CO2_env_clean$Habitat, levels = c("Vegetated Palsa", "Soil Palsa", "Thawslump", "Vegetated Pond"))

# compute summary statistics
SR20202021_CO2_env_clean%>%
  group_by(Habitat, Treatment) %>%
  summarise(
    count = n(),
    mean = round(mean(CO2flux, na.rm = TRUE), 2),
    median = round(median(CO2flux, na.rm = TRUE), 2),
    sd = round(sd(CO2flux, na.rm = TRUE), 2),
    cv = round(sd/mean, 2),
  ) %>%
  ungroup()

# ANOVA to test Treatment and Habitat effect 
aov.org <- aov( CO2flux ~ Habitat * Treatment, data = SR20202021_CO2_env_clean,
  contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.org, type = 'III')

aov.log <- aov( log(CO2flux) ~ Habitat * Treatment, data = SR20202021_CO2_env_clean,
                contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.log, type = 'III')

aov.rank <- aov( rank(CO2flux) ~ Habitat * Treatment, data = SR20202021_CO2_env_clean,
                contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.rank, type = 'III')

res.org = aov.org$resid
res.log = aov.log$resid
res.rnk = aov.rank$resid

qqnorm(  res.org, pch = 20, main = "Original Data",
  cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.org)
plot(aov.org, 1, main = "Original Data")

qqnorm(  res.log, pch = 20, main = "Log-Transformed",
  cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.log)
plot(aov.log, 1, main = "Log-Transformed")

qqnorm(  res.rnk, pch = 20, main = "Rank-Transformed",
  cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.rnk)
plot(aov.rank, 1, main = "Rank-Transformed")

library(emmeans)
emmeans(aov.log, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(aov.log,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)

# SR for each habitat and treatment over summer seasons 2020 and 2021 
SR20202021_CO2_env_clean%>%
  ggplot(aes(Habitat, CO2flux, fill=Treatment))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=TRUE)+
  theme_classic()

SR20202021_CO2_env_clean%>%
  group_by(Habitat, Treatment)%>%
  summarise(CO2flux.se = se(CO2flux),
            CO2flux.sd = sd(CO2flux, na.rm=TRUE),
            CO2flux.mean= mean(CO2flux, na.rm=TRUE))%>%
  ggplot(aes(Habitat, CO2flux.mean, color= Habitat, shape= Treatment))+
  geom_point(position = position_dodge(0.8), size=4 )+
  geom_errorbar(aes(ymin=CO2flux.mean-CO2flux.sd, ymax=CO2flux.mean+CO2flux.sd), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

# supplement include seasonal pattern
SR20202021_CO2_env_clean%>%
  ggplot(aes(Month, CO2flux, fill=Treatment))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  facet_grid(~Habitat)+
  theme_classic()

SR20202021_CO2_env_clean%>%
  group_by(Habitat, Treatment, Month)%>%
  summarise(CO2flux.se = se(CO2flux),
            CO2flux.sd = sd(CO2flux, na.rm=TRUE),
            CO2flux.mean= mean(CO2flux, na.rm=TRUE))%>%
  ggplot(aes(Month, CO2flux.mean, color= Habitat, shape= Treatment))+
  geom_point(position = position_dodge(0.8), size=4 )+
  geom_errorbar(aes(ymin=CO2flux.mean-CO2flux.sd, ymax=CO2flux.mean+CO2flux.sd), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  facet_grid(~Habitat)+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


SR2021_CH4_env_clean<-SR2021_CH4_env%>%
  mutate(Month = as.factor(month(Date)),
         Year = as.factor(year(Date)))%>%
  filter(Habitat != "W")%>%
  filter(CH4flux < 45)%>%# remove extreme values
  mutate(Habitat = dplyr::recode(Habitat, M = "Thawslump", P= "Vegetated Palsa", S = "Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
SR2021_CH4_env_clean$Habitat <- factor(SR2021_CH4_env_clean$Habitat, levels = c("Vegetated Palsa", "Soil Palsa", "Thawslump", "Vegetated Pond"))

SR2021_CH4_env_clean%>%
  group_by(Habitat, Treatment)%>%
  summarise(CH4flux.se = se(CH4flux),
            CH4flux.sd = sd(CH4flux, na.rm=TRUE),
            CH4flux.mean= mean(CH4flux, na.rm=TRUE))%>%
  ggplot(aes(Habitat, CH4flux.mean, color= Habitat, shape= Treatment))+
  geom_point(position = position_dodge(0.8), size=4 )+
  geom_errorbar(aes(ymin=CH4flux.mean-CH4flux.sd, ymax=CH4flux.mean+CH4flux.sd), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  geom_hline(yintercept =0, color= "grey")+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

SR2021_CH4_env_clean%>%
  ggplot(aes(Habitat, CH4flux, fill=Treatment))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=TRUE)

# ANOVA to test Treatment and Habitat effect 
aov.org <- aov( CH4flux ~ Habitat * Treatment, data = SR2021_CH4_env_clean,
                contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.org, type = 'III')

aov.rank <- aov( rank(CH4flux) ~ Habitat * Treatment, data = SR2021_CH4_env_clean,
                 contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.rank, type = 'III')

res.org = aov.org$resid
res.rnk = aov.rank$resid

qqnorm(  res.org, pch = 20, main = "Original Data",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.org)
plot(aov.org, 1, main = "Original Data")

qqnorm(  res.rnk, pch = 20, main = "Rank-Transformed",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.rnk)
plot(aov.rank, 1, main = "Rank-Transformed")

emmeans(aov.rank, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(aov.rank,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)



######################################################################################################################################################
##### NET ECOSYSTEM EXCHANGE 
# NEE chamber  V = 2.5 L, A = 0.0625 m2, CH4 in ppb, C02 in ppm
NEE2020_CO2<- read.csv("Cflux\\2020\\NEE2020_CO2_HMRoutput.csv", sep= ",")%>%
  rename(FluxID = X, CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)%>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

# load environmental metadata
meta.data1<- read.csv2("2020\\LiCOR850\\NEEmetadata_14072020.csv")
meta.data2<- read.csv2("2020\\LiCOR850\\NEEmetadata2_14072020.csv")
meta.data3<- read.csv2("2020\\LiCOR850\\NEEmetadata_15072020.csv")
meta.data4<- read.csv2("2020\\LiCOR850\\NEEmetadata2_15072020.csv")
meta.data5<- read.csv2("2020\\LiCOR850\\NEEmetadata_14082020.csv")
meta.data6<- read.csv2("2020\\LiCOR850\\NEEmetadata_15082020.csv")
meta.data7<- read.csv2("2020\\LiCOR850\\NEEmetadata_07092020.csv")
meta.data8<- read.csv2("2020\\LiCOR850\\NEEmetadata2_07092020.csv")
meta.data9<- read.csv2("2020\\LiCOR850\\NEEmetadata_08092020.csv")
meta.data10<- read.csv2("2020\\LiCOR850\\NEEmetadata_09092020.csv")
meta.data11<- read.csv2("2020\\LiCOR850\\NEEmetadata2_09092020.csv")
meta.data12<- read.csv2("2020\\LiCOR850\\NEEmetadata_04102020.csv")
meta.data13<- read.csv2("2020\\LiCOR850\\NEEmetadata_05102020.csv")
#Li7810
meta.data14<- read.csv2("2020\\LiCOR7810\\NEEmetadata_20062020.csv")
meta.data15<- read.csv2("2020\\LiCOR7810\\NEEmetadata_21062020.csv")

NEEenvdata2020<- rbind(meta.data1, meta.data2,meta.data3, meta.data4, meta.data5, meta.data6, meta.data7, meta.data8, meta.data9,  meta.data10, meta.data11, meta.data12, meta.data13, meta.data14, meta.data15)%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"))

# combine SR2021data with environmental data
NEE2020_CO2_env<- left_join(NEE2020_CO2, NEEenvdata2020, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "Cover"))%>%
  distinct(FluxID, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)


########## 2021 
NEE2021_CO2<-read.csv("2021\\Cfluxdata\\HMRoutput_NEE2021_CO2.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  #dplyr::select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)

NEE2021_CH4<-read.csv("2021\\Cfluxdata\\HMRoutput_NEE2021_CH4.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  #dplyr::select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CH4.f0 = f0, CH4.LR = LR.f0, Method.CH4 = Method)

## read in metadata
NEEenvdata03062021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_03062021.csv")
NEEenvdata04062021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_04062021.csv")
NEEenvdata02072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_02072021.csv")%>%
  dplyr::select(-X)# Check PAR ECtower
NEEenvdata03072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_03072021.csv")
NEEenvdata20072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_20072021.csv")
NEEenvdata21072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_21072021.csv")
NEEenvdata23072021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_23072021.csv")
NEEenvdata17082021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_17082021.csv")
NEEenvdata18082021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_18082021.csv")
NEEenvdata21082021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_21082021.csv")
NEEenvdata11092021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_11092021.csv")
NEEenvdata12092021<-read.csv2("2021\\Cfluxdata\\NEEmetadata_12092021.csv")

NEEenvdata2021<- rbind(NEEenvdata03062021, NEEenvdata04062021, NEEenvdata02072021,NEEenvdata03072021, NEEenvdata20072021, NEEenvdata21072021, NEEenvdata23072021, NEEenvdata17082021, NEEenvdata18082021, NEEenvdata21082021, NEEenvdata11092021, NEEenvdata12092021)%>%
  mutate(FluxID = as.character(FluxID))

# link Environmental data and CO2fluxdata
NEE2021_CO2_env<- left_join(NEE2021_CO2, NEEenvdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)

# bind together 2020 and 2021 NEE CO2 data
NEE20202021_CO2_env<- rbind(NEE2020_CO2_env, NEE2021_CO2_env)%>%
  mutate(Month = lubridate::month(Date),
         Year = lubridate::year(Date))

NEE2021_CH4_env<- left_join(NEE2021_CH4, NEEenvdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)

## Add airtemp based on TOMSTloggerData for measurement hour
#NEE2020_CO2_env<-left_join(NEE2020_CO2_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID", "Transect" , "Habitat", "Treatment"))

# NEED TO CORRECT FLUXES WITH IBUTTON TEMPERATURE TOMSTDATA not available for all dates !!!
#NEELi7810_notHMR<-read.csv("2020\\LiCOR7810\\NEEflux_2020_Li7810.csv")%>%
#  dplyr::select(Date, Transect, Habitat, Treatment, PlotID, Cover, Airtemp)%>%
#  mutate(Date = as.Date(Date, "%d.%m.%Y"))

#NEELi850_notHMR<-read.csv("2020\\LiCOR850\\NEEflux_2020_Li850.csv")%>%
#  dplyr::select(Date, Transect, Habitat, Treatment, PlotID, Cover, Airtemp)%>%
#  mutate(Date = as.Date(Date, "%d.%m.%Y"))

#NEE_ibuttonTemp<- rbind(NEELi7810_notHMR, NEELi850_notHMR)%>%
#  dplyr::rename(ChamberAirtemp= Airtemp)%>%
#  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))

# match chamber airtemp to NEE data
#NEE2020_CO2_env<- left_join(NEE2020_CO2_env, NEE_ibuttonTemp, by= c("Date", "Transect", "Habitat", "Treatment", "PlotID", "Cover"))%>%
#  distinct(FluxID, .keep_all = TRUE)

# match EC tower airtemp with fluxes
#ECtower<-read.csv("Climate\\Mobileflux1_level1_30min.csv")%>%
#  mutate(Date = as.Date(index, "%Y-%m-%d"),
#         Hour = as.integer(substr(index, 12,13)))%>%
#  group_by(Date, Hour)%>%
#  summarise(ECairtemp = mean(air_temperature))%>%
 # dplyr::select(Date, Hour, ECairtemp)

#NEE2020_CO2_env<- left_join(NEE2020_CO2_env, ECtower, by= c("Date", "Hour"))



##############
# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
# (0.08205*273.15) equals 22.4 L/mol, which is the standard molar volume at standard conditions (temp = 0 and 1 atm pressure)
# for now using soilTemp1 but better with either chamber ibutton data/ TOMST logger data or EC airtemp data
NEE20202021_CO2_env<-NEE20202021_CO2_env%>%
  mutate(CO2flux = CO2.f0/(0.08205*(273.15+SoilTemp1)),
         CO2flux.LR = CO2.LR/(0.08205*(273.15+SoilTemp1)))

NEE2021_CH4_env<- NEE2021_CH4_env%>%
  mutate(CH4flux = CH4.f0/(0.08205*(273.15+SoilTemp1)),
         CH4flux.LR = CH4.LR/(0.08205*(273.15+SoilTemp1)))

# RECO
NEE20202021_CO2_env%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Habitat %in% c("S", "P", "M", "WG"))%>%
  filter(Cover == "RECO")%>%
  filter(CO2flux > 0)%>%
  ggplot(aes(Habitat, CO2flux, col=Treatment))+ 
  geom_boxplot()

NEE2021_CH4_env%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Habitat %in% c("S", "P", "M", "WG"))%>%
  filter(Cover == "RECO")%>%
  filter(CH4flux < 25)%>%
  ggplot(aes(Habitat, CH4flux, col=Treatment))+ 
  geom_boxplot()+
  geom_hline(yintercept = 0)+
  facet_wrap(~Habitat, scales="free")






#!!!! CHECK PAR for NEE measurements 02072021, PAR sensor wrong 
### process fluxes with fluxcalc function to get airtemp from chamber!


#
#  mutate(plotDate = recode(Date, "03.06.2021" = "04.06.2021", 
#                           "02.07.2021" = "03.07.2021", 
#                           "20.07.2021" = "23.07.2021", "21.07.2021" = "23.07.2021", 
#                           "17.08.2021" = "21.08.2021", "18.08.2021" = "21.08.2021", 
#                           "11.09.2021" = "12.09.2021"))%>%
#  mutate(plotDate = as.Date(plotDate, "%d.%m.%Y"),


%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  filter(Habitat %in% c("S", "P", "M", "WG"))%>%
  filter(CH4.f0< 1000)%>%
  filter(CH4.f0>-1000)



