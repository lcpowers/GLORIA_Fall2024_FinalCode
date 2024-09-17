library(tidyverse)
library(lubridate)
library(zoo)

##### Raw soil T and PRISM data #####
# Read in raw soil T data and remove first column
rawclimate <- read_csv("./Data/IntermediateData/rawTemp_all.csv") %>% filter(aspect %in% c("N","S","E","W"))

# Read in prism data
prism_files <- list.files("./Data/climate/prism/", pattern = ".csv",full.names = T)
prism <- NULL

for(i in prism_files){
  
  summiti <- str_sub(string = basename(i),start = 1,end = 3)
  df <- read_csv(i,skip=10) %>%
    mutate(summit=summiti) %>%
    dplyr::select(summit,date=Date,ppt="ppt (mm)",
                  tmean="tmean (degrees C)",tmax="tmax (degrees C)",tmin="tmin (degrees C)") %>%
    mutate(trange=tmax-tmin)
  
  prism <- rbind(prism,df)
  
  rm(df,i,summiti)
}

rm(prism_files)


##### WBM data #####
# Get unique park and aspect codes
aspects <- unique(rawclimate$aspect)
parks <- unique(rawclimate$park[rawclimate$park!="PEC"])

# Initiate WB Model data frame
wbm_data <- read_csv("Data/IntermediateData/dailyWBdata.csv")


##### Read in compiled raw climate data (Includes raw data for all parks) #####
###### This generates new daily data. Commented out unless need to re-do for some reason #######
# rawclimate$park <- as.factor(rawclimate$park)
# rawclimate$summit <- as.factor(rawclimate$summit)
# rawclimate$plot <- as.factor(rawclimate$plot)
# rawclimate$aspect <- as.factor(rawclimate$aspect)
# rawclimate$date <- as.Date(rawclimate$date)
# 
# summits <- unique(as.character(rawclimate$summit))
# 
# dailyT <- NULL
# 
# for(i in summits){
#   # find mean T by day for individual aspects on summit i.
#   # Use all columns that we want to keep as grouping variable
#   tmp_summit <- filter(rawclimate,summit == i) %>%
#     group_by(park,summit,aspect,date,year,month,day) %>%
#     dplyr::summarise(tmean=(max(tempC)+min(tempC))/2, # Find daily mean based on daily min and max values so that these means are determined the same way as PRISM daily means
#               tmax=max(tempC,na.rm=T),
#               tmin=min(tempC,na.rm=T),
#               trange=(tmax-tmin),
#               q25tval=quantile(tempC,0.25),
#               q25tmean=mean(tempC[tempC<=q25tval],na.rm=T),
#               n_obs = dplyr::n(),
#               .groups = "drop")
# 
#   # Find unique aspects for current summit. This only matters for the 2 summits with missing aspect data in Pe
#   aspects <- unique(tmp_summit$aspect) %>% as.character()
# 
#   for(j in aspects){
# 
#     # Filter summit data for aspect j
#     tmp_aspect <- tmp_summit %>%
#       filter(aspect==j)
# 
#     # Create dataframe of complete date range by day
#     tmp_date_range = data.frame(missing_dates=seq.Date(from = min(tmp_aspect$date,na.rm=T),
#                                                        to = max(tmp_aspect$date,na.rm=T),
#                                                        by = 1),
#                                 park=tmp_aspect$park[1],summit=i,aspect=j) %>%
#       mutate(year=year(missing_dates),
#              month=month(missing_dates),
#              day=day(missing_dates))
# 
#     # Merge full date range with tmp_aspect dataframe, keeping all possible dates
#     tmp_aspect <- merge(tmp_aspect,tmp_date_range,
#                       by.x=c('date','park','summit','aspect','year','month','day'),
#                       by.y=c('missing_dates','park','summit','aspect','year','month','day'),
#                       all.y = T)
# 
#     # Put zero values into nobs column where tmean = NA (which is where dates were added)
#     tmp_aspect$n_obs[is.na(tmp_aspect$tmean)]=0
# 
#     # Bind with full data frame
#     dailyT = rbind(dailyT,tmp_aspect)
#   }
# 
#   # assign(paste0(i,"_alldates"),tmp_output)
# 
#   rm(tmp_summit,tmp_date_range,tmp_aspect)
# 
# }
# 
# # Add growing season column
# grwszndates <- seq(as.Date('2020-07-25'),as.Date('2020-09-25'),by='days')
# grwszndates <-  format(grwszndates,format="%m-%d")
# dailyT$grwszn <- ifelse(format(dailyT$date,format="%m-%d") %in% grwszndates,1,0)
# 
# rm(grwszndates)
# 
# write_csv(dailyT,paste0("./Data/IntermediateData/allparks_dailysoilTs_",Sys.Date(),".csv"))
#This generates new daily data. Commented out unless need to re-do for some reason 

# remember to change to most recent data if needed. Current most recent data is Nov 1
dailyT <- read_csv("./Data/IntermediateData/dailyT_unfilled.csv")

# Separate daily T and separate into parks (and read in if top of chunk not run)
RMN <- filter(dailyT,park=="RMN"&aspect!="C") # 63335 rows (65941 rows with aspect=="C")
YNP <- filter(dailyT,park=="YNP") # 52547 rows
GSD <- filter(dailyT,park=="GSD") # 61413 rows
GNP <- filter(dailyT,park=="GNP"&aspect!="HSP"&!aspect%in%c("NW","SW","SE")) # 72946 rows (77278 rows with NW, SW, SE) (83821 rows with aspect=="HSP")
PEC <- filter(dailyT,park=="PEC")


##### Create df/csv to use in models that predict missing mean t values #####
# Create data.frames for each pack which will be used to interpolated climate. Columns should be park, summit, date, then temperature for each park-summit combo, and prism temps

parks <- c("GSD","RMN","YNP","GNP") # No pecos because no missing T data @ pecos

for(parki in parks){
  
  # Grab climate data for park i
  parkdf <- eval(as.name(parki))
  
  # Create summit*aspect column
  parkdf$summitaspect <- paste(parkdf$summit,parkdf$aspect,sep = "_")
  
  # Convert to wide format using date as ID var
  parkdfwide <- pivot_wider(data = parkdf,id_cols = c(date),names_from = summitaspect,values_from = c(meanT),names_sep = ".")
  
  # get prism data for current park
  parkiprism <- prism %>% 
    filter(summit%in%parkdf$summit)
  
  # Convert prism data to wide format using date as ID var. This only keeps tmean values from prism temp. Keep diff, min, and max for variance
  prismwide <- pivot_wider(parkiprism,id_cols=c(date),names_from=summit,values_from=c(tmean,tmin),names_sep = ".")
  for(coli in 2:ncol(prismwide)) { colnames(prismwide)[coli] = paste('PR',colnames(prismwide)[coli],sep = ".") }
  
  # merge soil T and prism data
  parkdf <- merge(parkdfwide,prismwide)
  
  # Create a DOY column
  parkdf$doy = yday(parkdf$date)
  parkdf$doysq = parkdf$doy^2
  
  # give df unique name
  assign(paste0(parki,"_allT"),parkdf)
  
  write_csv(eval(as.name(paste0(parki,"_allT"))),paste0("./Data/IntermediateData/",parki,"_dailyT_moddata.csv"))
  
  # keep environment clean
  rm(parkdf,parkdfwide,parkiprism,prismwide, parki)
  
}
