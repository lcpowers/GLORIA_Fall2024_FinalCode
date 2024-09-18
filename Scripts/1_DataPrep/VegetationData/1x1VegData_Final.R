### Preparing 1x1m veg data ###

# Clean environment
rm(list=ls())

# Packages 
library(tidyverse)
library(reshape2)
library(data.table)
library(readxl)

# Files are referenced with the current working directory set to the level of the directory containing "GLORIA_Fall2024_FinalCode.Rproj"
setwd("~/Desktop/CurrentProjects/GLORIA_Fall2024_FinalCode")

# Function to remove columns with all NAs
not_all_na <- function(x) any(!is.na(x))

###### Bring in combined veg data from YELL, ROMO, and GRSD. PECO read in later next #####
veg_3parks <- read_csv("./Data/vegetation/NPS_IMD_GLORIA_Vegetation_2287428_DataPackage/NPS_IMD_GLORIA_Vegetation_2287428_Vegetation1x1-dataset.csv") %>% 
  select(park=Park,summit=Summit,year=Year, 
         plot=Plot,aspect=PlotAspect,
         GLORIA_SciName=GLORIA_SciName,cover=VisualCover) %>% 
  separate(GLORIA_SciName,into = c("genus","species")) %>% 
  mutate(species = ifelse(species=="in","sp",species),
         sciname=paste(genus,species)) %>% 
  select(-c(genus,species)) %>% 
  # mutate(plot2 = str_sub(plot,2,3)) %>% 
  select(park,summit,aspect,plot,year,sciname,cover) %>% 
  filter(park!="PEC")

pec <- read_csv("./Data/vegetation/PECO_Summary_1x1Vegetation_AllSchema_20220506_Revised.csv") %>% 
  select(park=Park,summit=Summit,year=Year, 
         plot=Plot,aspect=PlotAspect,
         sciname=species_name,cover=VisualCover) %>% 
  separate(sciname,into = c("genus","species")) %>% 
  mutate(sciname=paste(genus,species)) %>% 
  select(-c(genus,species)) %>% 
  # mutate(plot2 = str_sub(plot,2,3)) %>% 
  select(park,summit,aspect,plot,year,sciname,cover) %>% 
  ungroup()

# combine into one dataset
veg_4parks = rbind(veg_3parks,pec) 

# Change 2018 GSD-HUK data to 2015 (presumably a data entry error as there are a very small number of instances of 2018 (17) and surveys not done in 2018)
veg_4parks$year[veg_4parks$summit=="HUK"&veg_4parks$year==2018]=2015

veg_4parks = veg_4parks %>% 
  filter(!(summit=="GLA"&year==2009)) %>% # Remove 2009 data for summit GLA
  filter(!(summit%in%c("VQS","PIK","JSM")&year%in%c(2009,2011))) # Remove 2009 and 2011 data for 3 RMN summits, retain 2010 data.
rm(pec,veg_3parks)
###### End reading in data for GSD, RMN, PEC, and YNP #####

###### Bring in Glacier NP 1x1 veg Data ######

## 2003-2014 data
veg_glac1 <- read_csv("./Data/vegetation/GLAC_GLORIA_Data_For_CU/glac_0314_cover.csv") %>%
  select(where(not_all_na)) %>% 
  select(-Comments) %>% 
  reshape2::melt(., id.vars = c("Summit","Year","plot"), 
                 variable.name = "speccode", 
                 value.name = "cover") %>% 
  filter(!str_detect(Summit,"Band"))
#####

## 2019 data
veg_glac2 <- read_csv("./Data/vegetation/GLAC_GLORIA_Data_For_CU/glac_19_cover.csv") %>%
  select(where(not_all_na)) %>% 
  select(-Comments) %>% 
  reshape2::melt(., id.vars = c("Summit","Year","plot"), 
                 variable.name = "speccode", 
                 value.name = "cover")

##### Combine GLAC data
veg_glac3 <- rbind(veg_glac1,veg_glac2) %>% 
  select(summit=Summit,year=Year,plot,speccode,cover) %>% 
  filter(!is.na(cover)) %>% 
  mutate(aspect= str_sub(plot,1,1)) 

veg_glac3$speccode <- as.character(veg_glac3$speccode)
veg_glac = veg_glac3
rm(veg_glac1,veg_glac2,veg_glac3)
######

###### Match GLAC species code to spec names using crosswalk table from Erin ######
glac_crosswalk <- read_csv("./Data/vegetation/GLAC_GLORIA_Data_For_CU/GLAC_GLORIAnames_2003_2014v2_crosswalk_complete.csv") %>% 
  select(-USDA_Code)
colnames(glac_crosswalk)[1] <- "speccode"
glac_crosswalk$speccode = as.character(glac_crosswalk$speccode)

veg_glac <- veg_glac %>% 
  merge(.,glac_crosswalk) %>% 
  separate(USDA_ScientificName,into=c("Genus","Species")) %>% 
  mutate(sciname = paste(Genus,Species),
         park="GNP") %>%
  dplyr::select(names(veg_4parks))

veg_all = rbind(veg_glac,veg_4parks) %>% 
  filter(!str_detect(sciname,"Dummy")) %>% # remove dummy rows
  filter(!is.na(cover))

# Fixing what seem like clear mis-IDs
veg_all$sciname[veg_all$park=="YNP"&veg_all$sciname=="Carex filifolia"] = "Carex elynoides"
veg_all$sciname[veg_all$park=="GSD"&veg_all$sciname=="Artemisia campestris"] = "Artemisia scopulorum"

# write output dataset that does not include zeros
write_csv(veg_all,paste0("./Data/IntermediateData/all_1x1veg_no0s_final.csv"))

# #### Create species list from 1x1 veg data ######
# spec_list <- veg_all %>%
#   select(sciname) %>%
#   unique()
# 
# glor_speclist <- read_excel("./Data/vegetation/SpeciesList_YELL_ROMO_GRSA_PEC.xlsx") %>%
#   select(GLORIA_SciName,family=Plants_Family) %>%
#   separate(GLORIA_SciName,into = c("genus","species")) %>%
#   mutate(sciname=paste(genus,species)) %>%
#   select(sciname,family) %>%
#   unique()
# 
# spec_list <- read_csv("./Data/speciestraits/allparks_speclist_20220124 traits.csv")
# 

# Expand the vegetation data to include meaningful zeros. 
# Criteria to add in 0-values:
# - Species occurs on summit 's' at some point in the study period, but was not recorded in quadrat.
summits = unique(veg_all$summit)
all_1x1veg = NULL

for(s in summits){
  
  # Filter full dataset for the current summit
  tmp.s = filter(veg_all,summit==s)
  
  # aspects surveyed across the study period on this summit
  aspects = unique(tmp.s$aspect)
  
  # species found across the study period on this summit
  spp = unique(tmp.s$sciname)
  
  for(a in aspects){
    
    tmp.s.a = filter(tmp.s,aspect==a) %>% # filter summit data to summit.aspect
      group_by(park,summit,aspect,plot,sciname,year) %>% 
      summarise(cover = sum(cover), .groups = "drop") # This deals with instances of when a species was recorded more than once in the same plot
    
    years = unique(tmp.s.a$year)
    plots = unique(tmp.s.a$plot)
    
    tmp.expand = expand.grid(park=unique(tmp.s$park),
                             summit=s,
                             year=years,
                             aspect=a,
                             plot=plots,
                             sciname=spp)
    
    tmp.s.a.out = merge(tmp.s.a,tmp.expand,by=c("park","summit","year","aspect","sciname","plot"),all=T)
    tmp.s.a.out$cover = replace_na(tmp.s.a.out$cover,0)
    
    all_1x1veg = rbind(all_1x1veg,tmp.s.a.out)
    
    } # End aspect loop
} # End summit loop

# Write output data set that includes meaningful zeros
write_csv(all_1x1veg,paste0("./Data/IntermediateData/all_1x1veg_w0s_final.csv"))
