#### Compile soil temperature data then find and visualize gaps in the data ####
#### Claire Powers ####
#### Created 11/11/2021 ####

rm(list=ls())

library(tidyverse)
library(lubridate)
library(viridis)

####### Compile T Data ########
# First compile all T data and save
# Read in GNP soil temp data
gnp_temp_files <- list.files("./InputData/climate/GlacierNational-SoilTemp/",pattern = ".csv",recursive = T)
gnp_temp_data <- NULL

for(i in gnp_temp_files){

  print(which(gnp_temp_files==i))
  tmp <- read_csv(paste0("./InputData/climate/GlacierNational-SoilTemp/",i))
  tmp$Date <- as.Date(x = tmp$Date,format = "%m/%d/%Y")
  tmp <- tmp %>%
    separate(Site,c("summit","aspect"),remove=F) %>%
    separate(Date,c("year","month","day"),remove=F) %>%
    mutate(park="GNP") %>%
    select(park,summit,aspect,date=Date,year,month,day,time=Time,tempC=`Temp C`)

  gnp_temp_data <- rbind(gnp_temp_data,tmp)
  rm(tmp)
}
rm(i)
# If using the same data set, this df should be 1766756 lines long

# Write compiled GNP temp data rather than looping to compile every time
write_csv(gnp_temp_data,"./InputData/byClaire/GNP_rawT.csv")

gnp_temp_data <- read_csv("./InputData/byClaire/GNP_rawT.csv")

# Convert GNP temp data date
# gnp_temp_data$date <- as.Date(gnp_temp_data$date,"%m/%d/%Y")

# Create plot column -- identical to aspect but makes GNP column names match other temp data.frame
gnp_temp_data$plot <- gnp_temp_data$aspect
###### End Glacier data ######

# Read in temp data for GRSD, RM, Yell, and Pecos
temp_data <- read_csv("./InputData/climate/NPS_IMD_GLORIA_SoilTemperature_2288176_DataPackage_UPDATED_Pecos/NPS_IMD_GLORIA_SoilTemperature_2288176_Raw-dataset.csv") %>% 
  mutate(aspect=str_sub(Plot,1,1)) %>%      # Create an aspect column using substring of the plot column
  select(park=Park,summit=Summit,plot=Plot,aspect,datetime=DateTime,tempC=Value) %>% # rename columns to be all lower case
  separate(datetime,c('date','time'),sep=" ") # separate components (date and time) of datetime column
temp_data$date <- as.Date(temp_data$date) # Convert date column to date format

# Separate Y, M, D columns
temp_data <- separate(temp_data,date,c("year","month","day"),remove=F)

# Change temp data park codes to match veg data park codes
temp_data$park[temp_data$park=="GRSA"] <- "GSD"
temp_data$park[temp_data$park=="ROMO"] <- "RMN"
temp_data$park[temp_data$park=="YELL"] <- "YNP"
temp_data$park[temp_data$park=="USFS_PEC"] <- "PEC"
# unique(temp_data$park) # check park codes


# Check for same column names
# setdiff(colnames(gnp_temp_data), colnames(temp_data))

all_raw_temp_data <- rbind(temp_data,gnp_temp_data)
all_raw_temp_data$day=as.numeric(all_raw_temp_data$day)
all_raw_temp_data$month=as.numeric(all_raw_temp_data$month)
all_raw_temp_data$year=as.numeric(all_raw_temp_data$year)

# Get rid of extra dfs to keep env clean
rm(temp_data,gnp_temp_data,gnp_temp_files)

# Write csv of compiled raw temp data
# write_csv(all_raw_temp_data,"./InputData/byClaire/all_raw_temp.csv")

####### Find temperature gaps ######
# all_raw_temp_data <- read_csv("./InputData/byClaire/all_raw_temp.csv") 
summary(all_raw_temp_data)
head(all_raw_temp_data)

all_raw_temp_data$park <- as.factor(all_raw_temp_data$park)
all_raw_temp_data$summit <- as.factor(all_raw_temp_data$summit)
all_raw_temp_data$plot <- as.factor(all_raw_temp_data$plot)
all_raw_temp_data$aspect <- as.factor(all_raw_temp_data$aspect)
all_raw_temp_data$date <- as.Date(all_raw_temp_data$date)

# Get list of unique summits
summits <- unique(as.character(all_raw_temp_data$summit))

# Initiate dataframe for all dates
dailyT_alldates <- NULL

for(i in summits){
  # find mean T by day for individual aspects on summit i.
  # Use all columns that we want to keep as grouping variable
  tmp_summit <- filter(all_raw_temp_data,summit == i) %>% 
    group_by(park,summit,aspect,date,year,month,day,) %>% 
    dplyr::summarise(meanT=mean(tempC,na.rm=T),
              n_obs = n(),
              .groups = "drop")
  
  # Get all unique aspects. Should be N,S,W,E
  aspects = unique(tmp_summit$aspect)
  
  for(j in aspects){
    
    # Filter summit data for aspect j
    tmp_aspect <- tmp_summit %>% 
      filter(aspect==j)
    
    # Create dataframe of complete date range by day
    tmp_date_range = data.frame(missing_dates=seq.Date(from = min(tmp_aspect$date),
                                                       to = max(tmp_aspect$date),
                                                       by = 1),
                                park=tmp_aspect$park[1],summit=i,aspect=j) %>% 
      mutate(year=year(missing_dates),
             month=month(missing_dates),
             day=day(missing_dates))
    
    # Merge full date range with main dataframe, keeping all possible dates 
    tmp_full <- merge(tmp_aspect,tmp_date_range,
                      by.x=c('date','park','summit','aspect','year','month','day'),
                      by.y=c('missing_dates','park','summit','aspect','year','month','day'),
                      all.y = T)
    tmp_full$n_obs[is.na(tmp_full$meanT)]=0
    
    dailyT_alldates = rbind(dailyT_alldates,tmp_full)  
  }
  
  # assign(paste0(i,"_alldates"),tmp_output)
  
  rm(tmp_summit,tmp_date_range,tmp_aspect,tmp_full)

}

write_csv(dailyT_alldates,"InputData/byClaire/dailyT_alldates.csv")

romo <- filter(dailyT_alldates,park=="RMN"&aspect!="C") # 63335 rows (65941 rows with aspect=="C")
yell <- filter(dailyT_alldates,park=="YNP") # 52547 rows
grsd <- filter(dailyT_alldates,park=="GSD") # 61413 rows
glac <- filter(dailyT_alldates,park=="GNP"&aspect!="HSP"&!aspect%in%c("NW","SW","SE")) # 72946 rows (77278 rows with NW, SW, SE) (83821 rows with aspect=="HSP")
peco <- filter(dailyT_alldates,park=="PEC")

# vis_miss(romo)
# vis_miss(yell)
# vis_miss(grsd)
# vis_miss(glac)
# vis_miss(peco)

### Rocky Mountain ###
# Number of missing days per year
romo_table <- table(romo$year, as.character(romo$summit),as.character(romo$aspect),romo$n_obs) %>% as.data.frame() %>% 
  filter(Var4==0) %>% 
  select(-Var4)
colnames(romo_table) <- c("year","summit","aspect","n_missing")
romo_wide_table <- pivot_wider(data = romo_table,
                               id_cols = c(year,aspect),
                               names_from = summit,
                               values_from = n_missing,
                               values_fill = 0)

ggplot(subset(romo,summit=="VQS"),aes(x=date,y=n_obs,color=n_obs))+
  geom_point(alpha=1,size=0.01)+
  facet_wrap(~summit+aspect)+
  theme_classic(base_size = 14)+
  ggtitle("Rocky Mountain")
# ggsave("missingTdata_plots/romo1.png",w=6,h=5)

ggplot(romo,aes(x=date,y=meanT,color=aspect))+
  geom_point(alpha=0.25,size=1)+
  facet_wrap(~summit)+
  theme_classic(base_size = 7)+
  ggtitle("Rocky Mountain")
# ggsave("missingTdata_plots/romo2.png",w=5,h=6)

ggplot(subset(romo,summit=="VQS"),aes(x=date,y=meanT,color=aspect))+
  geom_point(size=2)+
  facet_wrap(~aspect,nrow=1)+
  theme_classic(base_size = 14)+
  ggtitle("Rocky Mountain")+
  labs(x="year",y="mean daily T")
# ggsave("missingTdata_plots/romo3.png",w=4,h=4)
  
##### Yellowstone #####
yell_table <- table(yell$year,as.character(yell$summit),as.character(yell$aspect),yell$n_obs) %>% as.data.frame() %>% 
  filter(Var4==0) %>% 
  select(-Var4)
colnames(yell_table) <- c("year","summit","aspect","n_missing")
yell_wide_table <- pivot_wider(data = yell_table,
                               id_cols = c(year,aspect),
                               names_from = summit,
                               values_from = n_missing,
                               values_fill = 0)

ggplot(yell, aes(x=date,y=n_obs,color=n_obs))+
  geom_point(alpha=0.75)+
  facet_wrap(~summit+aspect)+
  theme_classic(base_size = 7)+
  ggtitle("Yellowstone")+
  scale_color_viridis(direction=-1)
ggsave("missingTdata_plots/yell1.png",w=6,h=5)

ggplot(yell,aes(x=date,y=meanT,color=aspect))+
  geom_point(alpha=0.25,size=1)+
  facet_wrap(~summit)+
  theme_classic(base_size = 9)+
  ggtitle("Yellowstone")
ggsave("missingTdata_plots/yell2.png",w=6,h=6)

ggplot(yell,aes(x=date,y=meanT,color=aspect))+
  geom_point(alpha=0.25,size=1)+
  facet_wrap(~summit+aspect)+
  theme_classic(base_size = 7)+
  ggtitle("Yellowstone")
ggsave("missingTdata_plots/yell3.png",w=5,h=5)


##### Great Sand Dunes #####
grsd_table <- table(grsd$year,as.character(grsd$summit),as.character(grsd$aspect),grsd$n_obs) %>% as.data.frame() %>% 
  filter(Var4==0) %>% 
  select(-Var4) %>% 
  filter(!Var3%in%c("HSP","NW","SW","SE","C"))
colnames(grsd_table) <- c("year","summit","aspect","n_missing")
grsd_wide_table <- pivot_wider(data = grsd_table,
            id_cols = c(year,aspect),
            names_from = summit,
            values_from = n_missing,
            values_fill = 0)

ggplot(grsd, aes(x=date,y=n_obs,color=n_obs))+
  geom_point(alpha=0.75)+
  facet_wrap(~aspect+summit)+
  theme_classic(base_size = 14)+
  ggtitle("Great Sand Dunes")+
  scale_color_viridis(direction=-1)
# ggsave("missingTdata_plots/grsd1.png",w=6,h=5)

ggplot(grsd,aes(x=date,y=meanT,color=aspect))+
  geom_point(alpha=0.75,size=2)+
  facet_wrap(~summit,nco=1)+
  theme_classic(base_size = 14)+
  # ggtitle("Great Sand Dunes")+
  scale_color_viridis_d()+
  labs(x = "Date", y = "Mean daily T", color = "")+
  theme(legend.position = "none")
# ggsave("grsdmissingT.png",w=5,h=8)

ggplot(grsd,aes(x=date,y=meanT,color=aspect))+
  geom_point(alpha=0.25,size=1)+
  facet_wrap(~summit+aspect)+
  theme_classic(base_size = 7)+
  ggtitle("Great Sand Dunes")
# ggsave("missingTdata_plots/grsd3.png",w=5,h=5)

###### Glacier #######
glac_table <- table(glac$year,as.character(glac$summit),as.character(glac$aspect),glac$n_obs) %>% as.data.frame() %>% 
  filter(Var4==0) %>% 
  select(-Var4)
colnames(glac_table) <- c("year","summit","aspect","n_missing")
glac_wide_table <- pivot_wider(data =glac_table,
                               id_cols = c(year,summit),
                               names_from = aspect,
                               values_from = n_missing,
                               values_fill = NULL)

ggplot(glac, aes(x=date,y=n_obs,color=n_obs))+
  geom_point(alpha=0.75)+
  facet_wrap(~summit+aspect)+
  theme_classic(base_size = 7)+
  ggtitle("Glacier")+
  scale_color_viridis(direction=-1)
ggsave("missingTdata_plots/glac1.png",w=6,h=5)

ggplot(glac,aes(x=date,y=meanT,color=aspect))+
  geom_point(alpha=0.25,size=1)+
  facet_wrap(~summit)+
  theme_classic(base_size = 9)+
  ggtitle("Glacier")
ggsave("missingTdata_plots/glac2.png",w=6,h=6)

ggplot(glac,aes(x=date,y=meanT,color=aspect))+
  geom_point(alpha=0.25,size=1)+
  facet_wrap(~summit+aspect)+
  theme_classic(base_size = 7)+
  ggtitle("Glacier")
ggsave("missingTdata_plots/glac3.png",w=5,h=5)

###### Pecos ######
peco_table <- table(peco$year,as.character(peco$summit),as.character(peco$aspect),peco$n_obs) %>% as.data.frame() %>% 
  filter(Var4==0) %>% 
  select(-Var4)
colnames(peco_table) <- c("year","summit","aspect","n_missing")
peco_wide_table <- pivot_wider(data = peco_table,
                               id_cols = c(year,summit),
                               names_from = aspect,
                               values_from = n_missing,
                               values_fill = NULL)

ggplot(peco, aes(x=date,y=n_obs,color=n_obs))+
  geom_point(alpha=0.75)+
  facet_wrap(~summit+aspect)+
  theme_classic(base_size = 7)+
  ggtitle("pecos")+
  scale_color_viridis(direction=-1)
ggsave("missingTdata_plots/peco1.png",w=6,h=5)

ggplot(peco,aes(x=date,y=meanT,color=aspect))+
  geom_point(alpha=0.25,size=1)+
  facet_wrap(~summit)+
  theme_classic(base_size = 9)+
  ggtitle("Pecos")
ggsave("missingTdata_plots/pecos2.png",w=6,h=6)

ggplot(peco,aes(x=date,y=meanT,color=aspect))+
  geom_point(alpha=0.25,size=1)+
  facet_wrap(~summit+aspect)+
  theme_classic(base_size = 7)+
  ggtitle("Pecos")
ggsave("missingTdata_plots/pecos3.png",w=5,h=5)

#######

temp_data <- read.csv("./InputData/climate/NPS_IMD_GLORIA_SoilTemperature_2288176_DataPackage_UPDATED_Pecos/NPS_IMD_GLORIA_SoilTemperature_2288176_Raw-dataset.csv")
temp_data <- temp_data %>% filter(Park=="ROMO")
  
##### Tables of missing days #####

parks <- c("romo","glac","grsd","yell")

for(i in parks){
  
  tmp_df <- eval(as.name(paste0(i,"_table"))) 
  summits <- unique(as.character(tmp_df$summit))
  tmp_df2 <- tmp_df %>% 
    group_by(year) %>% 
    filter(sum(n_missing)>=0)
  
  tmp_wide_table <- pivot_wider(data =tmp_df2,
                                 id_cols = c(year,summit),
                                 names_from = aspect,
                                 values_from = n_missing,
                                 values_fill = 0) %>% 
    arrange(year)
  
  assign(paste0(i,"_wide_table"),tmp_wide_table)
  rm(tmp_df,tmp_df2,tmp_wide_table)
  
}


##### Find number of dates missing in snowy/wintery months #####
missing_winter_dates <- dailyT_alldates %>% 
  filter(aspect%in%c("N","S","E","W")) %>% 
  filter(is.na(meanT)|n_obs<20) %>% 
  group_by(park,summit,aspect,month) %>% 
  dplyr::summarise(n_missing=n()) 


ggplot(subset(missing_winter_dates,park=="RMN"), aes(x=as.factor(month),y=n_missing))+
  geom_col()+
  facet_wrap(~summit+aspect)+
  labs(title="RMN")+
  theme_bw()

ggplot(subset(missing_winter_dates,park=="YNP"), aes(x=as.factor(month),y=n_missing))+
  geom_col()+
  facet_wrap(~summit+aspect)+
  labs(title="YNP")+
  theme_bw()

ggplot(subset(missing_winter_dates,park=="GSD"), aes(x=as.factor(month),y=n_missing))+
  geom_col()+
  facet_wrap(~summit+aspect)+
  labs(title="GSD")+
  theme_bw()

ggplot(subset(missing_winter_dates,park=="GNP"), aes(x=as.factor(month),y=n_missing))+
  geom_col()+
  facet_wrap(~summit+aspect)+
  labs(title="GNP")+
  theme_bw()

ggplot(subset(glac,summit=="BSN"),aes(x=date,y=meanT))+
  geom_point()+
  facet_wrap(~aspect)

propmissing = dailyT_alldates %>% 
  filter(aspect%in%c("N","S","E","W")) %>% 
  mutate(temp = ifelse(n_obs<20,NA,meanT)) %>% 
  group_by(park) %>% 
  dplyr::summarise(propmissing = sum(is.na(meanT))/n()*100)

ggplot(propmissing,aes(x=park,y=propmissing))+
  geom_col(position="dodge")

##### Plot % missing data ###
missingDays = dailyT_alldates %>% 
  group_by(park) %>% 
  summarise(days = n(),
            missingDays = sum(is.na(meanT))) %>% 
  mutate(pMissing=missingDays/days*100) %>% 
  arrange(-pMissing) %>% 
  mutate(park = factor(park, park))

ggplot(missingDays,aes(x=park,y=pMissing,fill=park))+
  geom_col()+
  theme_classic(base_size = 14)+
  scale_fill_viridis_d()+
  labs(x="Region",y="% days with missing temperature data",fill="")
  
ggsave("reportFigs/missingDays.png",w=8,h=5)




