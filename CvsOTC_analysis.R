# C vs OTC Iskoras
setwd("C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\")
library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
#library(readxl)
library(vegan)

# function to calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

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
  mutate(Habitat =recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", WG= "Vegetated Pond")) # recode Habitat
data.scores.sites$Habitat <- factor(data.scores.sites$Habitat, levels = c("Vegetated Palsa", "Thaw slump", "Vegetated Pond"))

species.scores <- as.data.frame(data.scores$species)
species.scores$species <- rownames(species.scores)

ggplot() + 
  geom_point(data=data.scores.sites, aes(x=NMDS1, y=NMDS2, shape=Treatment, fill= Habitat), size=4) + # add the point markers
  stat_ellipse(data=data.scores.sites, aes(x=NMDS1, y=NMDS2, linetype = Treatment, col = Habitat), level=0.95,  linewidth = 1) +
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

#### Community weighted Trait Values
# Mean trait data per species, Habitat and  Treatment
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
  mutate(Habitat = dplyr::recode(Habitat, WG ="M"))%>% #recode WG to M as trait values do not vary between these habitats
  group_by(Species, Treatment, Habitat)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

SpeciesCover<- VegComp2021%>%
  gather(Species, cover, And.pol:Eri.vag)%>%
  mutate(Trait_Habitat = dplyr::recode(Habitat, WG = "M"))

CWMtraits <- left_join(SpeciesCover, SpeciesTrait, by= c("Species", "Treatment", "Trait_Habitat"="Habitat"))%>%
  drop_na(cover)%>%
  rename(LT = "LT1", LDMC = "LDMC1")%>%
  gather(Trait, value, VH:LDMC)%>%
  group_by(PlotID, Habitat, Treatment, Trait)%>%
  summarise(CWM = weighted.mean(value, cover, na.rm = TRUE))%>%
  filter(Trait != "LT")

CWMplot<- CWMtraits%>%
  group_by(Treatment, Habitat, Trait)%>%
  summarise(mean=mean(CWM), se=sd(CWM))%>% #calculate mean, se traits per species, treatment, habitat
  ggplot(aes(Habitat, mean, color=Treatment, shape= Treatment))+ 
  geom_point(position=position_dodge(width = 0.5), stat="identity", size= 4)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width = 0.5), width=.2)+
  scale_color_manual(values= c("grey70", "grey30"), name = "Treatment", labels = c("C", "OTC"))+
  scale_shape_manual(values= c(19, 17), name = "Treatment", labels = c("C", "OTC"))+
  facet_grid(Trait~., scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom")

# test differences between community weighted traitsB(
# non-parametric test for Treatment in each separate Habitat
library(purrr)
Outcomes <- c("LA", "LDMC", "SLA", "VH") # create vector with test subjects

# separate datasets for different habitats
PalsaCWM<-CWMtraits%>%
  filter(Habitat == "P")%>%
  spread(Trait, CWM)%>%
  ungroup()

ThawslumpCWM<-CWMtraits%>%
  filter(Habitat == "M")%>%
  spread(Trait, CWM)%>%
  ungroup()

ThawpondCWM<-CWMtraits%>%
  filter(Habitat == "WG")%>%
  spread(Trait, CWM)%>%
  ungroup()


map_dfr(setNames(Outcomes, Outcomes), function(my) {
  f <- as.formula(paste(my, "~Treatment", sep=""))
  broom::tidy(kruskal.test(f, data=PalsaCWM))
}, .id = "Outcomes")

map_dfr(setNames(Outcomes, Outcomes), function(my) {
  f <- as.formula(paste(my, "~Treatment", sep=""))
  broom::tidy(kruskal.test(f, data=ThawslumpCWM))
}, .id = "Outcomes")

map_dfr(setNames(Outcomes, Outcomes), function(my) {
  f <- as.formula(paste(my, "~Treatment", sep=""))
  broom::tidy(kruskal.test(f, data=ThawpondCWM))
}, .id = "Outcomes")


#%>%
#  unite(Species_Habitat_Treatment, c("Species", "Habitat", "Treatment"))%>%
#  filter(!Species_Habitat_Treatment == "Bet.nan_P_C")%>% # remove Bet.nan control P values as not in vegetation data
#  column_to_rownames(var="Species_Habitat_Treatment")

VegComp2021_Traits<- left_join(VegComp2021, CWMtraits, by = c("PlotID", "Habitat", "Treatment"))%>%
  spread(Trait, CWM)


#PCA
library(ggfortify)
library(devtools)
library(factoextra)
library(cowplot)

# Not sure about LT measurments
TraitPCA<- VegComp2021_Traits%>%
  dplyr::select(VH, LA, SLA, LDMC)
TraitPCA <- princomp(TraitPCA, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

PCAplot<- autoplot(TraitPCA, data = VegComp2021_Traits,  size = 4, fill= "Habitat", shape = "Treatment",
                   loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                   loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  #stat_ellipse(aes( col = Habitat.x), size = 1) +
  scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), name = "Habitat", labels = c("Veg. Palsa", "Thaw slump", "Veg. Pond"))+
  scale_shape_manual(values= c(21, 24), guide= "none")+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )
PCAplot

plot_grid(PCAplot, CWMplot, labels= c("A", "B" ))
#14, 22 and 24 same scores, 15, 23 and 27 same scores

Vegetation_P<- VegComp2021_Traits%>%
  filter(Habitat == "P")

Trait_P<- VegComp2021_Traits%>%
  filter(Habitat == "P")%>%
  select(VH, LA, SLA, LDMC)

TraitPCA_P <- princomp(Trait_P, cor= TRUE, scores=TRUE) 

palsaPCA<- autoplot(TraitPCA_P, data = Vegetation_P,  size = 4, fill= "Habitat", shape = "Treatment",
                    loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black", 
                    loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  scale_fill_manual(values= c("#fc8d62"), name = "Habitat", labels = c("Veg. Palsa"))+
                    guides(fill=guide_legend(override.aes=list(shape=21)))+
                    theme_classic()
palsaPCA
                                          
Vegetation_M<- VegComp2021_Traits%>%
                filter(Habitat == "M")

Trait_M<- VegComp2021_Traits%>%
          filter(Habitat == "M")%>%
          select(VH, LA, SLA, LDMC)
                                          
TraitPCA_M <- princomp(Trait_M, cor= TRUE, scores=TRUE) 

thawslumpPCA<- autoplot(TraitPCA_M, data = Vegetation_M,  size = 4, fill= "Habitat", shape = "Treatment",
                        loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black",
                        loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  scale_fill_manual(values= c( "#66c2a5"), name = "Habitat", labels = c("Thaw slump"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()
thawslumpPCA


Vegetation_WG<- VegComp2021_Traits%>%
  filter(Habitat == "WG")

Trait_WG<- VegComp2021_Traits%>%
  filter(Habitat == "WG")%>%
  select(VH, LA, SLA, LDMC)

TraitPCA_WG <- princomp(Trait_WG, cor= TRUE, scores=TRUE) #, Temperature = T_summer_longterm, Precipitation = P_annual_longterm

VegetatedpondPCA<- autoplot(TraitPCA_WG, data = Vegetation_WG,  size = 4, fill= "Habitat", shape = "Treatment",
                            loadings = TRUE, loadings.colour = 'black', loadings.label.colour = "black",
                            loadings.label = TRUE, loadings.label.size = 5, loadings.label.vjust = -.6, loadings.label.hjust = 0.9)+
  scale_shape_manual(values= c(21, 24), name = "Treatment", labels = c("Control", "OTC"))+
  scale_fill_manual(values= c("#8da0cb"), name = "Habitat", labels = c("Veg. Pond"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  theme_classic()
VegetatedpondPCA


library(cowplot)
plot_grid(palsaPCA, thawslumpPCA, VegetatedpondPCA, labels= c("A", "B", "C"), nrow = 1)
  

#NDVI data
# group by month and year!
NDVIdata<-read.csv2("VegetationData\\NDVI_Greenseeker.csv")%>%
  mutate(Date = as.Date(Date, "%d.%m.%Y"),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date))%>%
  filter(!grepl("water", Comment))%>% #filter out plots that were fully or partially under water
  gather(Measurement, NDVI, Value1:Value2)%>%
  group_by(Year, Month, Transect, Habitat, Treatment, PlotID)%>%
  summarise(NDVI = mean(NDVI, na.rm=TRUE))%>%
  ungroup()

NDVImean<- NDVIdata%>%
  filter(Habitat != "S")%>%
  group_by(Month, Habitat, Treatment)%>%
  summarise(NDVI.se = se(NDVI),
            NDVI.mean = mean(NDVI))%>%
  ungroup()%>%
  mutate(Habitat =recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa",  WG= "Vegetated Pond")) # recode Habitat
NDVImean$Habitat <- factor(NDVImean$Habitat, levels = c("Vegetated Palsa", "Thaw slump", "Vegetated Pond"))


ggplot(NDVImean, aes(as.factor(Month), NDVI.mean, color= Habitat, shape= Treatment)) +
  geom_point(position = position_dodge(0.8), size=4) +
  geom_errorbar(aes(ymin=NDVI.mean-NDVI.se, ymax=NDVI.mean+NDVI.se), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  labs(x = "Month of the year", y= "NDVI")+
  theme_classic()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


####################################################################################################################################
####### Abiotic conditions (TOMST loggers)
#! NEED TO CHECK LOGGERID and LOCATION!!!
library(lubridate)
TomstData<-read.csv("AnalysisR\\TOMSTdata_SMcalculated.csv")

TomstData<-TomstData%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  select(PlotID:LoggerID, Date, Date_Time, SoilTemperature:RawSoilmoisture, Soilmoisture_Volumetric)%>%
  mutate(Date = as.Date(Date),
         DateTime_UTC = as.POSIXct(strptime(Date_Time, tz="UTC", "%Y-%m-%dT%H:%M:%SZ")),
         DateTime = format(DateTime_UTC, tz="Europe/Berlin"),
         Hour = hour(DateTime),
         Soilmoisture_Volumetric= Soilmoisture_Volumetric*100)

# summary of microclimate across summer season June-August
Summersummary<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  filter(Date > "2021-07-01" & Date <"2021-09-01")%>%
  group_by(PlotID, Habitat, Treatment, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Mean = mean, Sd = sd, se =se, Max = max, Min = min ))

# hourly climate data per plot
TomstData_HourlyPlotID<- TomstData%>%
  group_by(PlotID, Transect, Habitat, Treatment, Date, Hour)%>%
  summarise(SoilTemperature = mean(SoilTemperature, na.rm = TRUE), 
            GroundTemperature = mean(GroundTemperature, na.rm = TRUE),
            AirTemperature = mean(AirTemperature, na.rm = TRUE),
            Soilmoisture_Volumetric = mean(Soilmoisture_Volumetric, na.rm = TRUE))%>%
            ungroup()

##### DAILY
# Summary Daily Per Habitat and Treatment
TomstData_MeanDailyHabitat<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  group_by(Habitat, Treatment, Date, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, se =se))%>%
  mutate(Habitat =recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
TomstData_MeanDailyHabitat$Habitat <- factor(TomstData_MeanDailyHabitat$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Vegetated Pond"))

# plot summer season hourly based on June-August data in 2021
TomstData_MeanDailyHabitat%>%
  filter(Habitat != "Bare Soil Palsa")%>%
  filter(Date > "2021-05-01" & Date <"2021-11-01")%>%
  filter(Climate_variable %in% c("SoilTemperature", "Soilmoisture_Volumetric"))%>%
  ggplot(aes(Date, Mean, col= Habitat, linetype =Treatment))+
  geom_line()+
  #geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Habitat), alpha=0.3) +
  scale_color_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  #scale_fill_manual(values= c("#fc8d62", "#e5c494","#66c2a5", "#8da0cb"), 
  #                  name = "Habitat")+
  facet_grid(Climate_variable~Habitat, scales="free")+
  theme_bw()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

# Dates thaw out for OTC vs Control

# Calculate difference in soilmoisture and plot
Soilmoist_diff<-TomstData%>%
  filter(Habitat != "S")%>%
  filter(Date > "2021-05-01" & Date <"2021-11-01")%>%
  select(Habitat, Treatment, Date, Soilmoisture_Volumetric)%>%
  group_by(Habitat, Treatment, Date)%>%
  drop_na(Soilmoisture_Volumetric)%>%
  summarise_at(vars(Soilmoisture_Volumetric), list(Soilmoisture_Mean = mean))%>%
  group_by(Date)%>%
  pivot_wider(names_from = Treatment, values_from = Soilmoisture_Mean)%>%
  mutate(diff = OTC - C)

ggplot(Soilmoist_diff, aes(Date, diff, col= Habitat))+
  geom_line(linewidth = 1)+
  #geom_ribbon(aes(ymin = Soilmoisture_Mean-se, ymax = Soilmoisture_Mean+se, fill = Habitat), alpha=0.3) +
  scale_color_manual(values= c("#66c2a5","#fc8d62",  "#8da0cb"), 
                     name = "Habitat",
                     labels = c("Thaw slump","Vegetated Palsa","Bare Soil Palsa","Vegetated Pond" ))+
  theme_bw()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


# summary Hourly per Habitat
TomstData_MeanHourlyHabitat<-TomstData%>%
  gather(Climate_variable, value, SoilTemperature:Soilmoisture_Volumetric)%>%
  filter(Date > "2021-05-31" & Date <"2021-09-01")%>%
  group_by(Habitat, Treatment, Hour, Climate_variable)%>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, se =se))%>%
  mutate(Habitat =recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
TomstData_MeanHourlyHabitat$Habitat <- factor(TomstData_MeanHourlyHabitat$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Vegetated Pond"))

# plot summer season hourly based on June-August data in 2021
TomstData_MeanHourlyHabitat%>%
  filter(Habitat != "Bare Soil Palsa")%>%
  filter(Climate_variable %in% c("AirTemperature", "SoilTemperature"))%>%
  ggplot(aes(Hour, Mean, col= Habitat, linetype =Treatment))+
  geom_line()+
  geom_ribbon(aes(ymin = Mean-se, ymax = Mean+se, fill = Habitat), alpha=0.3) +
  scale_color_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_fill_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  facet_grid(Climate_variable~Habitat, scales="free")+
  theme_bw()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

TomstData_MeanHourlyHabitat%>%
  group_by(Habitat, Treatment)%>%
  filter(Climate_variable %in% c("AirTemperature"))%>%
  summarise(max(Mean))

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





#####################################################################################################################################
####### Cflux data
# SR 2021 data

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
  mutate(CO2flux = CO2.f0/(0.08205*(273.15+AirTemperature)),
         CO2flux.LR = CO2.LR/(0.08205*(273.15+AirTemperature)))

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
  mutate(CO2flux = CO2.f0/(0.08205*(273.15+AirTemperature)),
         CO2flux.LR = CO2.LR/(0.08205*(273.15+AirTemperature)))

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
  mutate(CH4flux = CH4.f0/(0.08205*(273.15+AirTemperature)),
         CH4flux.LR = CH4.LR/(0.08205*(273.15+AirTemperature)))


########################################################################################################################################
### DATA CLEANING & visualizations
SR20202021_CO2_env_clean<-SR20202021_CO2_env%>%
  mutate(Month = as.factor(lubridate::month(Date)),
         Year = as.factor(lubridate::year(Date)))%>%
  filter(Comment != "redo")%>%
  filter(Habitat != "W")%>%
  #filter(Habitat != "S")%>%
  filter(CO2flux > 0)%>% # remove negative values
  mutate(Habitat =dplyr::recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
SR20202021_CO2_env_clean$Habitat <- factor(SR20202021_CO2_env_clean$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Vegetated Pond"))

# compute summary statistics
SR20202021_CO2_env_clean%>%
  group_by(Habitat, Treatment) %>%
  summarise(
    count = n(),
    mean = round(mean(CO2flux, na.rm = TRUE), 2),
    median = round(median(CO2flux, na.rm = TRUE), 2),
    sd = round(sd(CO2flux, na.rm = TRUE), 2),
    cv = round(sd/mean, 2)) %>%
  ungroup()

# histogram of response variable
# log transformation
ggplot(SR20202021_CO2_env_clean, aes(x=log(CO2flux)))+
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 10)

ggplot(SR20202021_CO2_env_clean, aes(x=log(CO2flux), fill=Treatment))+
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 10) +
  facet_grid(. ~ Habitat)

# ANOVA to test Treatment and Habitat effect 
library(car)
library(emmeans)

aov.log <- aov( log(CO2flux) ~ Habitat * Treatment, data = SR20202021_CO2_env_clean,
                 contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.log, type = 'III')

res.log = aov.log$resid
qqnorm(  res.log, pch = 20, main = "Log-Transformed",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.log)
plot(aov.log, 1, main = "Log-Transformed")

emmeans(aov.log, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(aov.log,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)

# SR for each habitat and treatment over summer seasons 2020 and 2021 
SR20202021_CO2_env_clean%>%
  filter(Habitat != "Bare Soil Palsa")%>%
  ggplot(aes(Habitat, CO2flux, fill=Treatment))+
  scale_fill_manual(values= c("grey70", "grey30"), name = "Treatment", labels = c("C", "OTC"))+
  geom_boxplot()+
  ylab(expression(Soil~Respiration~CO[2]~(micromol/m^{2}/s))) + 
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


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


##### CH4 
SR2021_CH4_env_clean<-SR2021_CH4_env%>%
  mutate(Month = as.factor(lubridate::month(Date)),
         Year = as.factor(lubridate::year(Date)))%>%
  filter(Habitat != "W")%>%
  #filter(Habitat != "S")%>%
  mutate(Habitat = dplyr::recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
SR2021_CH4_env_clean$Habitat <- factor(SR2021_CH4_env_clean$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Vegetated Pond"))

# compute summary statistics
SR2021_CH4_env_clean%>%
  group_by(Habitat, Treatment) %>%
  summarise(
    count = n(),
    mean = round(mean(CH4flux, na.rm = TRUE), 2),
    median = round(median(CH4flux, na.rm = TRUE), 2),
    sd = round(sd(CH4flux, na.rm = TRUE), 2),
    cv = round(sd/mean, 2)) %>%
  ungroup()

# histogram of response variable
# rank transformation
ggplot(SR2021_CH4_env_clean, aes(x=rank(CH4flux)))+
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 6)

# ANOVA to test Treatment and Habitat effect 
aov.rank <- aov( rank(CH4flux) ~ Habitat * Treatment, data = SR2021_CH4_env_clean,
                 contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.rank, type = 'III')

res.rnk = aov.rank$resid
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

SR2021_CH4_env_clean%>%
  filter(Habitat != "Bare Soil Palsa")%>%
  #filter(CH4flux < 10)%>% # 33 measurements not plotted
  ggplot(aes(Habitat, CH4flux/1000, fill=Treatment))+
  scale_fill_manual(values= c("grey70", "grey30"), name = "Treatment", labels = c("C", "OTC"))+
  geom_boxplot()+
  ylab(expression(Soil~Respiration~CH[4]~(micromol/m^{2}/s))) + 
  #facet_wrap(~Habitat, scales="free")+
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


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

# Soiltemp SR CO2flux relationship
SR20202021_CO2_env_clean%>%
  ggplot(aes(x=SoilTemp1+273.15, y=CO2flux, col=Habitat, shape= Treatment, linetype=Treatment))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "nls", formula= y~A*exp(-308.56/I(x-227.13)), method.args = list(start=c(A=0)), se=FALSE, na.rm= TRUE)+
  facet_grid(~Habitat)+
  theme_bw()


###########################################################################################################################################
##### NET ECOSYSTEM EXCHANGE 
# NEE chamber  V = 2.5 L, A = 0.0625 m2, CH4 in ppb, C02 in ppm
CO2files_NEE_2020 <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2020", 
                         pattern = "^HMR - HMRinput.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to read in data
NEE_CO2data_2020 <- map_df(set_names(CO2files_NEE_2020), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ",", dec = "."))
}, .id = "File")

NEE2020_CO2<- NEE_CO2data_2020%>%
  separate(Series, sep = "_", c("PlotID", "Treatment", "Cover", "Date", "H2O"))%>%
  mutate(Transect = str_sub(PlotID, 1, 1),
         Habitat = str_sub(PlotID, 2,3),
         Date =ymd(Date))%>%
  unite(PlotID, c(PlotID, Treatment), sep = "_", remove = FALSE)


# load environmental metadata
metafiles_NEE2020 <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2020\\", 
                         pattern = "^NEEmetadata.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Function to combine metadata files into one dataframe
NEE_envdata2020 <- map_df(set_names(metafiles_NEE2020), function(file) {
  file %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ";", dec = ",", fill = T) %>% 
             mutate(Transect = as.character(Transect),
                    Habitat = dplyr::recode(Habitat, WGA = "WG", WGB = "WG"), 
                    Date = as.Date(Date, "%d.%m.%Y")))
}, .id = "File")

# combine SR2021data with environmental data
NEE2020_CO2_env<- left_join(NEE2020_CO2, NEE_envdata2020, by= c("Date", "PlotID", "Transect" , "Habitat", "Treatment", "Cover"))%>%
  distinct(f0, .keep_all = TRUE)%>% # remove duplicated rows
  mutate(Hour = as.integer(substr(Starttime, 1,2)))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  select(-File.x, -H2O, -File.y)%>%
  #dplyr::select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)


########## 2021 
NEE2021_CO2<-read.csv("Cflux\\2021\\HMR - HMRinput_NEE_2021_CO2.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  #dplyr::select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CO2.f0 = f0, CO2.LR = LR.f0, Method.CO2 = Method)

NEE2021_CH4<-read.csv("Cflux\\2021\\HMR - HMRinput_NEE_2021_CH4.csv")%>%
  separate(Series, sep = "_", into = c("PlotID", "Treatment", "Cover", "Date", "FluxID"))%>%
  mutate(Transect = substring(PlotID,1,1),
         Habitat = substring(PlotID, 2,3))%>%
  unite(PlotID, PlotID:Treatment, remove =FALSE )%>%
  #dplyr::select(PlotID, Transect, Habitat, Treatment, Date, f0, LR.f0, Method, FluxID, Cover)%>%
  rename(CH4.f0 = f0, CH4.LR = LR.f0, Method.CH4 = Method)

## read in metadata
metafiles_NEE2021 <- dir(path = "C:\\Users\\ialt\\OneDrive - NORCE\\Iskoras\\Data\\Cflux\\2021\\", 
                         pattern = "^NEEmetadata.*\\.csv$", full.names = TRUE, recursive = TRUE)

NEE_envdata2021 <- map_df(set_names(metafiles_NEE2021), function(file) {
  file %>% 
    map_df(~ read.csv(file = file, header = TRUE, sep = ";", dec = ",", fill = T) %>% 
             mutate(Habitat = dplyr::recode(Habitat, "WGA" = "WG", "WGB"="WG"))%>%
             mutate(Transect = as.character(Transect),
                    FluxID = as.character(FluxID),
                    SoilTemp2 = dplyr::recode(SoilTemp2, '100.6' = 10.6L)))#correct typo on data
}, .id = "File")


# link Environmental data and CO2fluxdata
NEE2021_CO2_env<- left_join(NEE2021_CO2, NEE_envdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID, -X, -File)

# bind together 2020 and 2021 NEE CO2 data
NEE20202021_CO2_env<- rbind(NEE2020_CO2_env, NEE2021_CO2_env)%>%
  mutate(Month = lubridate::month(Date),
         Year = lubridate::year(Date))

NEE2021_CH4_env<- left_join(NEE2021_CH4, NEE_envdata2021, by= c("FluxID", "Date", "PlotID", "Transect", "Habitat", "Treatment", "Cover"))%>%
  mutate(Hour = as.integer(substr(Starttime, 1,2)),
         Date = as.Date(Date, "%d.%m.%Y"))%>%
  mutate(Habitat= dplyr::recode(Habitat, WGA = "WG", WGB = "WG"))%>%
  dplyr::select(-FluxID)


## Add airtemp based on TOMSTloggerData for measurement hour
NEE20202021_CO2_env_TOMST<-left_join(NEE20202021_CO2_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID","Transect", "Habitat", "Treatment"))%>%
  mutate(Habitat = dplyr::recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
NEE20202021_CO2_env_TOMST$Habitat <- factor(NEE20202021_CO2_env_TOMST$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Vegetated Pond"))

NEE2021_CH4_env_TOMST<-left_join(NEE2021_CH4_env, TomstData_HourlyPlotID, by= c("Date", "Hour", "PlotID","Transect", "Habitat", "Treatment"))%>%
  mutate(Habitat = dplyr::recode(Habitat, M = "Thaw slump", P= "Vegetated Palsa", S = "Bare Soil Palsa", WG= "Vegetated Pond")) # recode Habitat
NEE2021_CH4_env_TOMST$Habitat <- factor(NEE2021_CH4_env_TOMST$Habitat, levels = c("Vegetated Palsa", "Bare Soil Palsa", "Thaw slump", "Vegetated Pond"))

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
ECtower<-read.csv("Climate\\Mobileflux1_level1_30min_forInge.csv")%>%
  mutate(Date = as.Date(index, "%Y-%m-%d"),
         Hour = as.integer(substr(index, 12,13)))%>%
  group_by(Date, Hour)%>%
  summarise(ECairtemp = mean(air_temperature))%>%
 dplyr::select(Date, Hour, ECairtemp)

NEE20202021_CO2_env_TOMST_EC<- left_join(NEE20202021_CO2_env_TOMST, ECtower, by= c("Date", "Hour"))
NEE2021_CH4_env_TOMST_EC<- left_join(NEE2021_CH4_env_TOMST, ECtower, by= c("Date", "Hour"))


##############
# Flux conversion HMR microL/m2/s > micromol/m2/s HMRoutput/(0.08205*(273.15+Air_temp))
# (0.08205*273.15) equals 22.4 L/mol, which is the standard molar volume at standard conditions (temp = 0 and 1 atm pressure)
# for now using soilTemp1 but better with either chamber ibutton data/ TOMST logger data or EC airtemp data
NEE20202021_CO2_env_TOMST_EC<-NEE20202021_CO2_env_TOMST_EC%>%
  mutate(CO2flux = CO2.f0/(0.08205*(273.15+AirTemperature)),
         CO2flux.LR = CO2.LR/(0.08205*(273.15+AirTemperature)),
         CO2flux_EC = CO2.f0/(0.08205*(273.15+ECairtemp)))%>%
  mutate(CO2flux_final = ifelse(is.na(CO2flux) == TRUE, CO2flux_EC, CO2flux))

NEE2021_CH4_env_TOMST_EC<- NEE2021_CH4_env_TOMST_EC%>%
  mutate(CH4flux = CH4.f0/(0.08205*(273.15+AirTemperature)),
         CH4flux.LR = CH4.LR/(0.08205*(273.15+AirTemperature)),
         CH4flux_EC = CH4.f0/(0.08205*(273.15+ECairtemp)))%>%
  mutate(CH4flux_final = ifelse(is.na(CH4flux) == TRUE, CH4flux_EC, CH4flux))


# calculate GPP 
RECO_CO2<-NEE20202021_CO2_env_TOMST_EC%>%
  filter(!grepl("R", PlotID))%>%
  filter(Cover== "RECO")%>%
  filter(Comment != "redo")%>%
  filter(CO2flux > 0)%>%
  rename( CO2flux_RECO = CO2flux_final)%>%
  dplyr::select(PlotID, Habitat, Treatment, Date, Year, Month, CO2flux_RECO)

NEE_CO2<-NEE20202021_CO2_env_TOMST_EC%>%
  filter(!grepl("R", PlotID))%>%
  filter(Treatment %in% c("C",  "OTC"))%>%
  filter(Cover != "RECO")%>%
  filter(Method.CO2 != "No flux") # 11 measurements dropped

# Recalculate PAR based on shading cover, 
# need to check shading effect of covers
GPP_CO2<-left_join(NEE_CO2, RECO_CO2, by=c("PlotID", "Habitat", "Treatment", "Date", "Year", "Month"))%>%
  mutate(GPPflux = (CO2flux_final- CO2flux_RECO))%>%
  drop_na(GPPflux)%>%
  group_by(PlotID, Transect, Habitat, Treatment, Cover, Date, Hour, Month, Year)%>%
  gather(PAR, value, PAR1:PAR3)%>%
  mutate(PAR.mean = mean(value, na.rm=TRUE))%>%
  distinct(GPPflux, .keep_all = TRUE)%>%
  mutate(PAR.mean = ifelse(Cover == 'NEE2', PAR.mean*0.33,
                         ifelse(Cover == 'NEE1', PAR.mean*0.67, PAR.mean)))%>%
  mutate(SoilTemp.mean = (SoilTemp1+ SoilTemp2)/2,
         SoilMoist.mean =(SoilMoist1+ SoilMoist2 +SoilMoist3)/3)%>%
  filter(GPPflux < 0)%>%
  ungroup()


# ANOVA test RECO, NEE, GPP
hist(NEE_CO2$CO2flux) # normally distributed

NEE_CO2%>%
  ggplot(aes(Habitat, CO2flux, fill=Treatment))+
  scale_fill_manual(values= c("grey70", "grey30"), name = "Treatment", labels = c("C", "OTC"))+
  geom_boxplot()+
  ylab(expression(Net~Ecosystem~Exchange~CO[2]~(micromol/m^{2}/s))) + 
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

NEE_summary<-NEE_CO2%>%
  group_by(Habitat, Treatment) %>%
  summarise(
    count = n(),
    mean = round(mean(CO2flux, na.rm = TRUE), 2),
    median = round(median(CO2flux, na.rm = TRUE), 2),
    sd = round(sd(CO2flux, na.rm = TRUE), 2),
    cv = round(sd/mean, 2)) %>%
  ungroup()

# ANOVA to tests Treatment and Habitat effect 
library(car)
library(emmeans)

#NEE
aov.org<- aov( CO2flux ~ Habitat * Treatment, data = NEE_CO2,
                contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(aov.org, type = 'III')

res.org = aov.org$resid
qqnorm(  res.org, pch = 20, main = "Original",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.org)
plot(aov.org, 1, main = "Original Data")

emmeans(aov.org, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(aov.org,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)

#RECO
# Take out S plots
RECO_CO2_clean<-RECO_CO2%>% 
  filter(Habitat != "Bare Soil Palsa")

RECO_summary<-RECO_CO2_clean%>%
  group_by(Habitat, Treatment) %>%
  summarise(
    count = n(),
    mean = round(mean(CO2flux_RECO, na.rm = TRUE), 2),
    median = round(median(CO2flux_RECO, na.rm = TRUE), 2),
    sd = round(sd(CO2flux_RECO, na.rm = TRUE), 2),
    cv = round(sd/mean, 2)) %>%
  mutate(flux = "RECO",
         GHG = "CO2")%>%
  ungroup()

RECO_CO2_clean%>%
  ggplot(aes(Habitat, CO2flux_RECO, fill=Treatment))+
  scale_fill_manual(values= c("grey70", "grey30"), name = "Treatment", labels = c("C", "OTC"))+
  geom_boxplot()+
  ylab(expression(Ecosystem~Respiration~CO[2]~(micromol/m^{2}/s))) + 
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )



# hist(log(RECO_CO2_clean$CO2flux_RECO)) # normal distribution

RECO.log<- aov( log(CO2flux_RECO)~ Habitat * Treatment, data = RECO_CO2_clean,
               contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(RECO.log, type = 'III')

res.RECO.log = RECO.log$resid
qqnorm(  res.RECO.log, pch = 20, main = "Log Reco Data",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.RECO.log)
plot(RECO.log, 1, main = "Log Reco Data")

emmeans(RECO.log, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(RECO.log,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)


### GPP
hist(log(GPP_CO2$GPPflux*-1))

GPP_CO2%>%
  ggplot(aes(Habitat, GPPflux, fill=Treatment))+
  scale_fill_manual(values= c("grey70", "grey30"), name = "Treatment", labels = c("C", "OTC"))+
  geom_boxplot()+
  ylab(expression(Gross~Primary~Production~CO[2]~(micromol/m^{2}/s))) + 
  theme_classic()+
  theme(legend.position = "bottom", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


# how are PAR values distributed for GPP measurements for C and OTC across habitats
GPP_CO2%>%
  ggplot(aes(x=PAR.mean, fill=Treatment))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values= c("grey70", "grey30"), name = "Treatment", labels = c("C", "OTC"))+
  #geom_histogram(alpha=0.5, binwidth = 100)+
  facet_grid(~Habitat)+
  theme_classic()

GPP.log<- aov( log(-1*GPPflux)~ Habitat * Treatment, data = GPP_CO2,
                contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(GPP.log, type = 'III')

res.GPP.log = GPP.log$resid
qqnorm(  res.GPP.log, pch = 20, main = "Log GPP Data",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.GPP.log)
plot(GPP.log, 1, main = "Log GPP Data")

emmeans(GPP.log, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(GPP.log,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)


#### CH4 NEE
# !!! NEED TO FIGURE OUT WHICH DISTRIBUTION TO USE
NEE_CH4_clean<-NEE2021_CH4_env_TOMST_EC%>%
  filter(Habitat %in% c("Thaw slump", "Vegetated Palsa", "Vegetated Pond"))%>%
  filter(Treatment %in% c("C", "OTC"))%>%
  drop_na(CH4flux_final)%>%
  filter(CH4flux_final<900)%>%
  filter(CH4flux_final>-5)
  

ggplot(NEE_CH4_clean, aes(Treatment, CH4flux_final, fill=Treatment))+
  geom_boxplot()+
  ylab(expression(Ecosystem~Exchange~CH[4]~(nanomol/m^{2}/s))) + 
  scale_fill_manual(values= c("grey70", "grey30"), name = "Treatment", labels = c("C", "OTC"))+
  facet_wrap(~Habitat, scales = "free")+
  theme_classic()

ggplot(NEE_CH4_clean, aes(x=rank(CH4flux_final)))+
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 10)

CH4.rank<- aov( rank(CH4flux_final)~ Habitat * Treatment, data = NEE_CH4_clean,
               contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(CH4.rank, type = 'III')

res.CH4.rank = CH4.rank$resid
qqnorm(  res.CH4.rank, pch = 20, main = "Log GPP Data",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.CH4.rank)
plot(CH4.rank, 1, main = "Log GPP Data")

emmeans(CH4.rank, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(CH4.rank,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)




# summary on CH4 flux recalculated from nanomol to micromol (/1000) and multiplied by GWP of 100 years (84x) to compare to CO2
CH4_summary<-NEE_CH4_clean%>%
  group_by(Habitat, Treatment) %>%
  summarise(
    count = n(),
    mean = round(mean(CH4flux/1000*84, na.rm = TRUE), 2),
    median = round(median(CH4flux/1000*84, na.rm = TRUE), 2),
    sd = round(sd(CH4flux/1000*84, na.rm = TRUE), 2),
    cv = round(sd/mean, 2)) %>%
  mutate(flux = "NEE",
  GHG = "CH4")%>%
  ungroup()
#unbalanced might be problem

fluxsummary<-rbind(GPP_summary, RECO_summary, CH4_summary)

ggplot(fluxsummary, aes(Habitat, mean, color= Treatment, shape= GHG)) +
  geom_point(position = position_dodge(0.8), size=4) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.8), width=.4)+
  scale_color_manual(values= c("grey70", "grey30"), name = "Treatment", labels = c("C", "OTC"))+
  #scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  labs(x = "Habitat", y= "GHG flux")+
  geom_hline(yintercept = 0)+
  theme_classic()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

ggplot(fluxsummary, aes(fill=GHG, y=mean, x=Treatment)) + 
  geom_bar(position="stack", stat="identity")+
  #geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.3, position = "identity")+
  facet_grid(~Habitat)+
  geom_hline(yintercept = 0)+
  theme_classic()+
  theme(legend.position = "right", axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )


CH4.rank<- aov( rank(CH4flux)~ Habitat * Treatment, data = NEE_CH4_clean,
               contrasts = list(Habitat = 'contr.sum', Treatment = 'contr.sum' ))
Anova(CH4.rank, type = 'III')

res.CH4.rank = CH4.rank$resid
qqnorm(  res.CH4.rank, pch = 20, main = "Ranked CH4 Data",
         cex.lab = 1, cex.axis = 0.7, cex.main = 1)
qqline(res.CH4.rank)
plot(CH4.rank, 1, main = "Ranked CH4 Data")

emmeans(CH4.rank, pairwise ~ Habitat | Treatment)
em_out_category<-emmeans(CH4.rank,  ~ Treatment | Habitat) 
em_out_category %>% 
  pairs() %>% 
  test(joint = TRUE)
pairs(em_out_category)





#!!!! CHECK PAR for NEE measurements 02072021, PAR sensor wrong 
### process fluxes with fluxcalc function to get airtemp from chamber!

#### MODELLING GPP
# Link NDVI data to GPP 
GPP_NDVI<- left_join(GPP_CO2, NDVIdata, by=c("PlotID", "Treatment", "Transect", "Year", "Month"))%>%
  dplyr::select(-Habitat.y)

CWMtraits_wide<- CWMtraits%>%
  spread(Trait, CWM)%>%
  ungroup()%>%
  select(-Habitat)

GPP_NDVI_CWM<-left_join(GPP_NDVI, CWMtraits_wide, by= c("PlotID", "Treatment"))%>%
  drop_na(NDVI)

ggplot(GPP_NDVI_CWM, aes(NDVI, GPPflux))+
  geom_point(aes(col=Habitat.x, shape = Treatment))+
  scale_color_manual(values= c("#fc8d62", "#66c2a5", "#8da0cb"), 
                     name = "Habitat")+
  scale_shape_manual(values= c(19,17), name = "Treatment", labels = c("Control", "OTC"))+
  geom_smooth(method = "lm", col="black", se=FALSE)+
  theme_classic()+
  theme( axis.title = element_text(size = 14), axis.text = element_text(size =12), legend.text = element_text(size =11) )

ggplot(GPP_NDVI_CWM, aes(VH, GPPflux))+
  geom_point(aes(col=Habitat.x))+
  geom_smooth(method = "lm")

ggplot(GPP_NDVI_CWM, aes(PAR.mean, GPPflux))+
  geom_point(aes(col=Habitat.x))+
  geom_smooth(method = "lm")

ggplot(GPP_NDVI_CWM, aes(SoilTemp.mean, GPPflux))+
  geom_point(aes(col=Habitat.x))+
  geom_smooth(method = "lm")

library(glmmTMB)
library(lme4)
library(lmerTest)
library(DHARMa)
library(MuMIn)

# first correction of GPP bayesian hierarchical model?
fitGPP <- lmer(GPPflux ~ NDVI + VH + LA + SLA + LDMC + (1|PlotID) ,  data = GPP_NDVI_CWM)
summary(fitGPP)
anova(fitGPP)
MuMIn::r.squaredGLMM(fitGPP)
simulationOutput <- simulateResiduals(fittedModel = fitGPP, plot = F)
plot(simulationOutput)

fitReco <- lmer(CO2flux_RECO ~ NDVI + VH + LA + SLA + LDMC + (1|PlotID) ,  data = GPP_NDVI_CWM)
summary(fitReco)
anova(fitReco)
MuMIn::r.squaredGLMM(fitReco)
simulationOutput <- simulateResiduals(fittedModel = fitReco, plot = F)
plot(simulationOutput)

fitNEE<-lmer(CO2flux_final ~ NDVI + VH + LA + SLA + LDMC + (1|PlotID) ,  data = GPP_NDVI_CWM)
summary(fitNEE)
anova(fitNEE)
MuMIn::r.squaredGLMM(fitNEE)
simulationOutput <- simulateResiduals(fittedModel = fitNEE, plot = F)
plot(simulationOutput)


#CH4
library(lubridate)
NEE_CH4<-NEE_CH4_clean%>%
  mutate(Month = lubridate::month(Date),
         Year = lubridate::year(Date))

CH4_NDVI<- left_join(NEE_CH4, NDVIdata, by=c("PlotID", "Treatment", "Transect", "Year", "Month"))%>%
  dplyr::select(-Habitat.y)

CH4_NDVI_CWM<-left_join(CH4_NDVI, CWMtraits_wide, by= c("PlotID", "Treatment"))%>%
  drop_na(NDVI)


fitCH4<-lmer(CH4flux_final ~ NDVI + VH + LA + SLA + LDMC + (1|PlotID) ,  data = CH4_NDVI_CWM)
summary(fitCH4)
anova(fitCH4)
MuMIn::r.squaredGLMM(fitCH4)
simulationOutput <- simulateResiduals(fittedModel = fitCH4, plot = F)
plot(simulationOutput)


# https://www.nature.com/articles/s42003-023-04626-3 
# BC<rkner, P.-C. Advanced bayesian multilevel modeling with the R Package brms. R J. 10, 395b