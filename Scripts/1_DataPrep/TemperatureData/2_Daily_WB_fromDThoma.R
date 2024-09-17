#### Claire's version of the Daily WB model ####

####### Water Balance code ##########

library(WaterBalance) # Will need to access a zip file provided by D. Thoma to install this package
library(geosphere)
library(raster)
library(lubridate)
library(tidyverse)
library(readxl)

## Two pound signs are lines from the original script that I commented out to replace with my own lines (CP)

############################################################# USER INPUTS ##################################################################### 
rm(list=ls())

##### Read in prism data #####
prism_files <- list.files("./Data/climate/prism/", pattern = "1990.csv",full.names = T)

Historical <- NULL

for(i in prism_files){
  
  summiti <- str_sub(string = basename(i),start = 1,end = 3)
  df <- read_csv(i,skip=10) %>%
    mutate(summit=summiti) %>%
    select(summit,date=Date,ppt="ppt (mm)",
           tmean="tmean (degrees C)",tmax="tmax (degrees C)",tmin="tmin (degrees C)") 
  
  Historical <- rbind(Historical,df)
  
  rm(df,summiti)
}
rm(prism_files,i)

# Site characteristics 
# sites = read.csv("C:/Users/adillon/Documents/RSS/CONG/WB/CONG_site_characteristics.csv") #CSV file containing properties for all sites

### Read in David's site info csv
siteinfo = read_csv("./Data/siteInfo/summitcoordinates.csv") %>% select(summit,elev,startdate,enddate)
wb_sites = read_xlsx("./Data/climate/daymet/sites.xlsx") %>% 
  mutate(park=str_sub(site,1,3),
         summit=str_sub(site,5,7),
         aspectDeg=aspect,
         aspect=str_sub(site,9,9),
         WB_site=paste(summit,aspect,sep="_")) 

wb_sites = merge(wb_sites,siteinfo,by="summit")

n<-nrow(wb_sites)
#Threshold temperature (deg C) for growing degree-days calculation
T.Base = 0 

#Method for PET calculation 
Method = "Oudin"  #Oudin is default method for daily PRISM and MACA data (containing only Tmax, Tmin, and Date). 

#Date format
DateFormat = "%m/%d/%Y"

############################################################ END USER INPUTS ###################################################################

######################################################### CALCULATE WB VARIABLES ################################################################
AllDailyWB<-list()

  for(i in 1:nrow(wb_sites)){

    ID = wb_sites$WB_site[i]
    
    DailyWB<-filter(Historical,summit==str_sub(ID,1,3))
    
    park = wb_sites$park[wb_sites$WB_site==ID]
    Lat = wb_sites$latitude[i] 
    Lon = wb_sites$longitude[i] 
    Elev = wb_sites$elev[i] ## Elevation -> elev
    Aspect = wb_sites$aspectDeg[i] ## Aspect -> aspectDeg
    Slope = wb_sites$slope[i] ## Could bring this in but maybe don't need it
    SWC.Max = 50 ## From excel sheet
    Snowpack.Init = 0 ## optional in get_melt() function with default = 0
    Soil.Init = 0 ## wb_sites$Soil.Init[i] ## Soil initial conditions. Not sure what this is or how David determines this
    Shade.Coeff = 1 ## wb_sites$Shade.Coeff[i] No shade -- alpine
    #Calculate daily water balance variables 
    
    DailyWB$park = park
    DailyWB$ID = ID
    DailyWB$doy <- yday(DailyWB$date) ## Date to date
    DailyWB$daylength = get_daylength(DailyWB$date, Lat)
    
    DailyWB$jtemp = as.numeric(get_jtemp(Lon, Lat))
    # print(mean(DailyWB$jtemp))
    
    DailyWB$F = get_freeze(DailyWB$jtemp, DailyWB$tmean)
    
    DailyWB$RAIN = get_rain(DailyWB$ppt, DailyWB$F)
    DailyWB$SNOW = get_snow(DailyWB$ppt, DailyWB$F)
    DailyWB$MELT = get_melt(tmean = DailyWB$tmean, jtemp = DailyWB$jtemp, 
                            hock=4, snow = DailyWB$SNOW, sp.0 = Snowpack.Init) ## Snowpack init?? ##
    
    # print(c(mean(DailyWB$jtemp),mean(DailyWB$SNOW),mean(DailyWB$MELT)))
    
    DailyWB$PACK = get_snowpack(DailyWB$jtemp, DailyWB$SNOW, DailyWB$MELT) # Daily snowpack depth
    DailyWB$W = DailyWB$MELT + DailyWB$RAIN # Daily soil water inputs
    
    if(Method == "Hamon"){
      DailyWB$PET = ET_Hamon_daily(DailyWB)
    } else {
      if(Method == "Penman-Monteith"){
        DailyWB$PET = ET_PenmanMonteith_daily(DailyWB)
      } else {
        if(Method == "Oudin"){
          DailyWB$PET = get_OudinPET(DailyWB$doy, Lat, DailyWB$PACK, DailyWB$tmean, Slope, Aspect, Shade.Coeff)
        } else {
          print("Error - PET method not found")
        }
      }
    }
    
    DailyWB$PET = modify_PET(DailyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
    DailyWB$W_PET = DailyWB$W - DailyWB$PET
    DailyWB$SOIL = get_soil(DailyWB$W, Soil.Init, DailyWB$PET, DailyWB$W_PET, SWC.Max) # Soil water content
    DailyWB$DSOIL = diff(c(Soil.Init, DailyWB$SOIL)) # Starting soil conditions - soil water content??
    DailyWB$AET = get_AET(DailyWB$W, DailyWB$PET, DailyWB$SOIL, Soil.Init) 
    DailyWB$W_ET_DSOIL = DailyWB$W - DailyWB$AET - DailyWB$DSOIL
    DailyWB$D = DailyWB$PET - DailyWB$AET
    DailyWB$GDD = get_GDD(DailyWB$tmean, T.Base)
    AllDailyWB[[i]] = DailyWB
    rm(DailyWB)
  }
WBData<-do.call(rbind,AllDailyWB)
write_csv(WBData,"./Data/IntermediateData/dailyWBdata.csv")
######################################################### END WB VARIABLE CALCULATIONS ################################################################





