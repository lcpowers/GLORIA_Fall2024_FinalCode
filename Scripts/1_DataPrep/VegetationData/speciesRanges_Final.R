rm(list=ls())

library(tidyverse)
library(sf)
speclist <- read_csv("./InputData/speciesTraits/allparks_speclist_20220124 traits.csv")

not_all_na <- function(x) any(!is.na(x))

# Read in data downloaded from idigbio. Most recent download on Feb 2
# idigbio <- read_csv("./InputData/speciesRanges/idigbio_2022-02-02/occurrence_raw.csv")%>%
#   select(where(not_all_na))
# 
# idigbiofltr <- idigbio %>%
#   select(sciname=`dwc:scientificName`,
#          lat=`dwc:decimalLatitude`,
#          long=`dwc:decimalLongitude`,
#          country=`dwc:country`,
#          continent=`dwc:continent`,
#          countrycode = `dwc:countryCode`)
# 
# write_csv(idigbiofltr, "./../InputData/speciesRanges/filtered_specRanges.csv")
#
# After writing the above CSV, the points were read into QGIS and clipped using a shapefile of North America (downloaded from Princeton)
# 
# tetra = read_csv("./InputData/speciesRanges/tetraneuris.csv")%>%
#   select(sciname=`dwc:scientificName`,
#          lat=`dwc:decimalLatitude`,
#          long=`dwc:decimalLongitude`,
#          country=`dwc:country`,
#          continent=`dwc:continent`,
#          countrycod = `dwc:countryCode`) %>%
#   unique()
# 
# tetra$sciname = "Tetraneuris brandegeei"
# 
# spp_pts <- read_sf("./InputData/speciesRanges/shapefiles/NA_spp_pts.shp") %>% 
#   as.data.frame() %>% 
#   dplyr::select(-geometry) %>% 
#   filter(sciname!="Caloplaca sp"&sciname!="NA") %>% 
#   rbind(.,tetra)
# 
# spp_pts$sciname <- tolower(spp_pts$sciname)
# species <- unique(spp_pts$sciname)

# First find 10th and 90th percentile lat values for each species
latqvals <- spp_pts %>% 
  dplyr::group_by(sciname) %>% 
  dplyr::summarize(p90 = quantile(lat,0.90),
                   p10 = quantile(lat,0.10),
                   n_obs=n())

### Now find min/max/mean/median/range lats for each spec ###
# Initiate dataframe
spp_range_summ <- data.frame(sciname = species,
                             latmin=NA,
                             latmin1090=NA,
                             latmax=NA,
                             latmax1090=NA,
                             latrange=NA,
                             latrange1090=NA,
                             latmean=NA,
                             latmean1090=NA,
                             latmedian=NA,
                             latmedian1090=NA,
                             n_obs=NA,
                             n_obs1090=NA)


# Find summary stats for each species
for(sp in species){
  
  df <- filter(spp_pts,sciname==sp)
  if(nrow(df)>0){
    spp_range_summ$latmin[spp_range_summ$sciname==sp] = min(df$lat,na.rm = T)
    spp_range_summ$latmax[spp_range_summ$sciname==sp] = max(df$lat,na.rm = T)
    spp_range_summ$latrange[spp_range_summ$sciname==sp] = max(df$lat,na.rm = T)-min(df$lat,na.rm = T)
    spp_range_summ$latmean[spp_range_summ$sciname==sp] = mean(df$lat,na.rm = T)
    spp_range_summ$latmedian[spp_range_summ$sciname==sp] = median(df$lat,na.rm = T)
    spp_range_summ$n_obs[spp_range_summ$sciname==sp] = nrow(df)
  }
  
  ### Fill in 10-90th percentile latitude range summary stats
  df1090 <- filter(spp_pts,sciname==sp) %>% 
    filter(lat>=latqvals$p10[latqvals$sciname==sp] & # lat greater than 10th% thresh
             lat<=latqvals$p90[latqvals$sciname==sp])   # and less than 90th% thresh
  
  if(nrow(df1090)>0){
    spp_range_summ$latmin1090[spp_range_summ$sciname==sp] = min(df1090$lat,na.rm = T)
    spp_range_summ$latmax1090[spp_range_summ$sciname==sp] = max(df1090$lat,na.rm = T)
    spp_range_summ$latrange1090[spp_range_summ$sciname==sp] = max(df1090$lat,na.rm = T)-min(df$lat,na.rm = T)
    spp_range_summ$latmean1090[spp_range_summ$sciname==sp] = mean(df1090$lat,na.rm = T)
    spp_range_summ$latmedian1090[spp_range_summ$sciname==sp] = median(df1090$lat,na.rm = T)
    spp_range_summ$n_obs1090[spp_range_summ$sciname==sp] = nrow(df1090)
    
  }
  
  rm(df,df1090)
  
}

write_csv(spp_range_summ,"./InputData/IntermediateData/species_range_summary.csv")s