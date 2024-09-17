
### To-do:
# This could use more notation

### SAS veg data ###
#library(readxl)
library(tidyverse)
library(reshape2)

rm(list=ls())

not_all_na <- function(x) any(!is.na(x))

######## 4 parks #########
### 4 parks sas veg ###
# abundance category where 
# 100 = very rare (1-4 inidividuals, or 1 well developed perennial individual); 
# 200 = rare (5-19 individuals, or 2-9 well developed perennial individuals); 
# 300 = rare to scattered; 
# 400 = scattered (20-49 individuals, or 10-29 well developed perennial individuals); 
# 500 = scattered to common; 
# 600 = common (more than above, but less than 50% cover); 
# 700 = common to dominant; 
# 800 = dominant (more than 50% cover, many of these species never reach this abundance class)
sasveg1 <- read_csv("./InputData/vegetation/NPS_IMD_GLORIA_Vegetation_2287428_DataPackage/NPS_IMD_GLORIA_Vegetation_2287428_SASVegetation-dataset.csv") %>% 
  select(park=Park, summit=Summit, year=Year,plot=Plot, aspect=PlotAspect,abundance=Abundance, GLORIA_SciName) %>%
  separate(GLORIA_SciName,into = c("genus","species")) %>%
  mutate(species = ifelse(species=="in","sp",species),
         sciname=paste(genus,species)) %>%
  select(-c(genus,species)) %>%
  select(sciname,abundance,park,summit,year,plot,aspect) %>% 
  unique() %>% 
  filter(park!="PEC")

pecsas <- read_csv("./InputData/vegetation/PECO_Summary_SASVegetation_AllSchema_20220506_Revised.csv") %>% 
  select(park=Park, summit=Summit,year=Year,plot=Plot, aspect=PlotAspect,abundance=Abundance, sciname=species_name) %>%
  separate(sciname,into = c("genus","species")) %>% 
  mutate(species = ifelse(species=="in","sp",species),
         sciname=paste(genus,species)) %>%
  select(-c(genus,species)) %>%
  select(sciname,abundance,park,summit,year,plot,aspect) %>% 
  unique()

sasveg <- rbind(sasveg1,pecsas)
sasveg$abundance <- as.factor(sasveg$abundance)
# Change the 05 and 10 plot elevation indicator to upper and lower, respectively. 5 = 5 meters from highest point. 
sasveg$plot <- ifelse(str_detect(sasveg$plot,"05"),"upper","lower")

############
##### GNP #####
### GLAC sas veg ###

# r! (very rare): One or a few small individuals;
# r (rare): Some individuals at several locations that can hardly be overlooked in a careful observation;
# s (scattered): Widespread within the section, species can hardly be overlooked, but the presence is not obvious at first glance; individuals are not necessarily evenly dispersed over the entire summit area section;
# c (common): Occurring frequently and widespread within the section – presence is obvious at first glance, it covers, however, less than 50% of the SAS’s area;
# d (dominant): Very abundant, making up a high proportion of the phytomass, often forming more or less patchy or dense vegetation layers; species covers more than 50% of the area of the SAS (this is the only abundance category which is entirely related to cover).

# SAS veg crosswalk table from Erin
sasvegcw = read_csv("./InputData/vegetation/SAS_AbundanceDefinitions_v2.csv",skip=1) %>% select(1,3)
colnames(sasvegcw) = c("abun","number")

# GLAC species code to species
glacspec <- read_csv("./InputData/vegetation/GLAC_GLORIA_Data_For_CU/GLAC_speccodetospec.csv") %>% 
  select(genus=Plants_Genus,species=Plants_Species,speccode=ROMN_Code) %>% 
  mutate(spp=paste(genus,species)) %>% 
  select(spp,speccode) %>% 
  unique()

# Glacier NP sas veg 2003-2014
gnpsasveg03 <- read_csv("./InputData/vegetation/GLAC_GLORIA_Data_For_CU/glac_0314_SAScover.csv")

# Wide to long
gnpsasveg03L = melt(gnpsasveg03,id.vars=1:3,variable.name = "speccode",value.name="abun") %>% 
  filter(!is.na(abun))

gnpsasveg03L2 <- merge(gnpsasveg03L,glacspec,by="speccode", all.x=T) %>% 
  select(sciname=spp,speccode,summit=Summit,year=Year,SAS=`Summit Area Section`,abun)
gnpsasveg03L2$speccode <- as.character(gnpsasveg03L2$speccode)


# Glacier NP sas veg 2019
gnpsasveg19 <- read_csv("./InputData/vegetation/GLAC_GLORIA_Data_For_CU/glac_19_SAScover.csv")
# Wide to long
gnpsasveg19L = melt(gnpsasveg19,id.vars=1:3,variable.name = "speccode",value.name="abun") %>% 
  filter(!is.na(abun))

gnpsasveg19L2 <- merge(gnpsasveg19L,glacspec,by="speccode", all.x=T) %>% 
  select(sciname=spp,speccode,summit=Summit,year=Year,SAS=`Summit Area Section`,abun)
gnpsasveg19L2$speccode <- as.character(gnpsasveg19L2$speccode)

gnpsas=rbind(gnpsasveg03L2,gnpsasveg19L2)
gnpsas = merge(gnpsas,sasvegcw,by="abun",all.x = T) %>% 
  filter(!is.na(number)) %>% 
  select(-abun) %>% 
  mutate(park="GNP") %>% 
  select(sciname,abundance=number,park,summit,year,sas="SAS") %>% 
  mutate(plot = ifelse(str_detect(sas,"10"),"lower","upper"),
         aspect = str_sub(sas,1,1)) %>% 
  select(-sas)

sasveg$abundance <- as.numeric(as.character(sasveg$abundance))

allsas = rbind(sasveg,gnpsas)
write_csv(allsas,paste0("./InputData/byClaire/SASabundance-",Sys.Date(),".csv"))

sas_summary = allsas %>% filter(year!=2010) %>%  group_by(year,park,summit) %>% 
  summarise(mean.abun = mean(abundance,na.rm=T))

ggplot(sas_summary,aes(x=year,y=mean.abun,fill=summit))+
  geom_col(position="dodge")+
  facet_wrap(~park,scales="free_x")+
  theme_bw()

gnpvals = gnpsas %>% 
  group_by(abun,year) %>% 
  tally()
