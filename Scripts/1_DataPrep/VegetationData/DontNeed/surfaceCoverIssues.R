
### Cleaning surface cover data ###
library(tidyverse)
library(reshape2)
rm(list=ls())

### Generating surface cover data from species-level % cover ###
traits = read_csv("./InputData/speciestraits/allparks_speclist_20220124 traits.csv")

# colpal = c(gnp_col,ynp_col,rmn_col,gsd_col,pec_col)
###### Bring in combined veg data from YELL, ROMO, GRSD and PECOS #####
veg_3parks <- read_csv("./InputData/vegetation/NPS_IMD_GLORIA_Vegetation_2287428_DataPackage/NPS_IMD_GLORIA_Vegetation_2287428_Vegetation1x1-dataset.csv") %>% 
  select(park=Park,summit=Summit,year=Year, 
         plot=Plot,aspect=PlotAspect,
         GLORIA_SciName=GLORIA_SciName,cover=VisualCover,hits=NumberHits) %>% 
  separate(GLORIA_SciName,into = c("genus","species")) %>% 
  mutate(species = ifelse(species=="in","sp",species),
         sciname=paste(genus,species)) %>% 
  select(-c(genus,species)) %>% 
  # mutate(plot2 = str_sub(plot,2,3)) %>% 
  select(park,summit,aspect,plot,year,sciname,cover,hits) %>% 
  filter(park!="PEC")

veg_3parks$year[veg_3parks$park=="GSD"&veg_3parks$year==2018]=2015

pec <- read_csv("./InputData/vegetation/PECO_Summary_1x1Vegetation_AllSchema_20220506_Revised.csv") %>% 
  select(park=Park,summit=Summit,year=Year, 
         plot=Plot,aspect=PlotAspect,
         sciname=species_name,cover=VisualCover,hits=NumberHits) %>% 
  separate(sciname,into = c("genus","species")) %>% 
  mutate(sciname=paste(genus,species)) %>% 
  select(-c(genus,species)) %>% 
  # mutate(plot2 = str_sub(plot,2,3)) %>% 
  select(park,summit,aspect,plot,year,sciname,cover,hits) %>% 
  ungroup() %>% 
  filter(!is.na(hits))

cover_df = rbind(veg_3parks,pec)  
######################

##### Start plotting #####
cover_compare = cover_df %>% 
  group_by(park,summit,aspect,plot,year,sciname) %>% 
  summarise(totalCover=sum(cover,na.rm = T),
            totalHits=sum(hits,na.rm=T),.groups="drop") %>% 
  filter(!(park=="GSD"&year<2015)) %>% 
  filter(!(park=="RMN"&year<2014)) %>% 
  na.omit()

ggplot(cover_compare,aes(x=totalCover,y=totalHits,color=park))+
  geom_abline(slope=1)+
  geom_smooth(method="lm",se=F)+
  geom_point(alpha=0.75)+
  theme_bw(base_size=14)+
  scale_color_manual(values=park.pal)+
  facet_wrap(~park,scales="free")+
  theme(legend.title = element_blank())+
  labs(x="Total % cover",y="Total hits",title="Total hits vs total cover - spp level")
ggsave("Results/CoverPlots/totalHitsCover_sppLevel.jpeg",h=8,w=10,dpi=300)

cover_corr = cover_compare %>% 
  group_by(park,year) %>% 
  summarise(cover_corr = cor(totalCover,totalHits))

parks = unique(cover_corr$park)
change_cover = NULL

for(i in parks){
  
  tmp_df = filter(cover_compare,park==i) %>% 
    pivot_wider(id_cols=c(park,summit,aspect,plot,sciname),names_from = year,values_from = c(totalCover,totalHits),values_fill = 0)
  
  assign(paste0(i,"_change"),tmp_df)
  rm(tmp_df)
}

#### GSD #####
GSD_change =GSD_change %>% 
  mutate(change.cover = totalCover_2020-totalCover_2015,
         change.hits = totalHits_2020-totalHits_2015)

gsd_cor = round(cor(GSD_change$change.cover,GSD_change$change.hits),2)

ggplot()+
  geom_abline(slope=1,color="grey",linewidth=1.5)+
  geom_point(data=GSD_change,aes(x=change.cover,y=change.hits)) +
  geom_smooth(method="lm")+
  theme_bw(base_size=12)+
  geom_text(aes(label=paste("Correlation = ",gsd_cor),x=max(GSD_change$change.cover)-5,y=min(GSD_change$change.hits)),
            size=4,color="red")+
  labs(x="Change in cover (%)",y="Change in hits",title="GSD")+
  theme(plot.title = element_text(hjust=0.5,size=16,face="bold"))
ggsave("Results/HitsIssues/GSD_hitsvcover.png",h=6,w=7,dpi=300)
######

#### RMN #####
RMN_change =RMN_change %>% 
  mutate(change.cover = totalCover_2019-totalCover_2014,
         change.hits = totalHits_2019-totalHits_2014)

rmn_cor = round(cor(RMN_change$change.cover,RMN_change$change.hits),2)

ggplot()+
  geom_abline(slope=1,color="grey",linewidth=1.5)+
  geom_point(data=RMN_change,aes(x=change.cover,y=change.hits)) +
  geom_smooth(method="lm")+
  theme_bw(base_size=12)+
  geom_text(aes(label=paste("Correlation = ",rmn_cor),x=max(RMN_change$change.cover)-4,y=min(RMN_change$change.hits)),
            size=4,color="red")+
  labs(x="Change in cover (%)",y="Change in hits",title="RMN")+
  theme(plot.title = element_text(hjust=0.5,size=16,face="bold"))
ggsave("Results/HitsIssues/RMN_hitsvcover.png",h=6,w=7,dpi=300)
######

#### pec #####
PEC_change =PEC_change %>% 
  mutate(change.cover = totalCover_2021-totalCover_2016,
         change.hits = totalHits_2021-totalHits_2016)

pec_cor = round(cor(PEC_change$change.cover,PEC_change$change.hits),2)

ggplot()+
  geom_abline(slope=1,color="grey",linewidth=1.5)+
  geom_point(data=PEC_change,aes(x=change.cover,y=change.hits)) +
  geom_smooth(method="lm")+
  theme_bw(base_size=12)+
  geom_text(aes(label=paste("Correlation = ",pec_cor),x=max(PEC_change$change.cover)-4,y=min(PEC_change$change.hits)),
            size=4,color="red")+
  labs(x="Change in cover (%)",y="Change in hits",title="PEC")+
  theme(plot.title = element_text(hjust=0.5,size=16,face="bold"))
# ggsave("Results/HitsIssues/PEC_hitsvcover.png",h=6,w=7,dpi=300)
######


#### YNP #####
YNP_change = YNP_change %>% 
  mutate(change.cover = totalCover_2016-totalCover_2011,
         change.hits = totalHits_2016-totalHits_2011)

ynp_cor = round(cor(YNP_change$change.cover,YNP_change$change.hits),2)

ggplot()+
  geom_abline(slope=1,color="grey",linewidth=1.5)+
  geom_point(data=YNP_change,aes(x=change.cover,y=change.hits)) +
  geom_smooth(method="lm")+
  theme_bw(base_size=12)+
  geom_text(aes(label=paste("Correlation = ",ynp_cor),x=max(YNP_change$change.cover)-8,y=min(YNP_change$change.hits)),
            size=4,color="red")+
  labs(x="Change in cover (%)",y="Change in hits",title="YNP")+
  theme(plot.title = element_text(hjust=0.5,size=16,face="bold"))
# ggsave("Results/HitsIssues/YNP_hitsvcover.png",h=6,w=7,dpi=300)
######




all_change = NULL
for(i in parks){
  
  
  df = eval(as.name(paste0(i,"_change"))) %>% 
    select(park,change.cover,change.hits) 
  all_change =  rbind(all_change,df)
  
}

ggplot(all_change,aes(x=change.cover,y=change.hits,color=park))+
  geom_abline(slope=1)+
  geom_smooth(method="lm",se=F)+
  geom_point(alpha=0.5)+
  scale_color_manual(values=park.pal)+
  theme_bw(base_size=14)+
  facet_wrap(~park,scales="free")+
  theme(legend.title = element_blank())+
  labs(x="Change in % cover",y="Change in hits",title="Change hits vs Change cover - spp level")

ggsave("Results/CoverPlots/changeHitsCover_sppLevel.jpeg",h=8,w=10,dpi=300)
######## end spp level plots #######

##### Plot level #####
cover_compare2 = cover_df %>% 
  group_by(park,summit,aspect,plot,year) %>% 
  summarise(totalCover=sum(cover,na.rm = T),
            totalHits=sum(hits,na.rm=T),.groups="drop") %>% 
  filter(!(park=="GSD"&year<2015)) %>% 
  filter(!(park=="RMN"&year<2014)) %>% 
  na.omit()

ggplot(cover_compare2,aes(x=totalCover,y=totalHits,color=park))+
  geom_abline(slope=1)+
  geom_smooth(method="lm",se=F)+
  geom_point(alpha=0.75)+
  theme_bw(base_size=14)+
  scale_color_manual(values=park.pal)+
  facet_wrap(~park,scales="free")+
  theme(legend.title = element_blank())+
  labs(x="Total % cover",y="Total hits",title="Total hits vs total cover - 1x1-m plot level")
ggsave("Results/CoverPlots/totalHitsCover_1x1mplotLevel.jpeg",h=8,w=10,dpi=300)


change_cover2 = NULL
for(i in parks){
  
  tmp_df = filter(cover_compare2,park==i) %>% 
    pivot_wider(id_cols=c(park,summit,aspect,plot),names_from = year,values_from = c(totalCover,totalHits),values_fill = 0)
  
  assign(paste0(i,"_change2"),tmp_df)
  rm(tmp_df)
}

#### GSD #####
GSD_change2 =GSD_change2 %>% 
  mutate(change.cover = totalCover_2020-totalCover_2015,
         change.hits = totalHits_2020-totalHits_2015)

gsd_cor2 = round(cor(GSD_change2$change.cover,GSD_change2$change.hits),2)

RMN_change2 =RMN_change2 %>% 
  mutate(change.cover = totalCover_2019-totalCover_2014,
         change.hits = totalHits_2019-totalHits_2014)

rmn_cor2 = round(cor(RMN_change2$change.cover,RMN_change2$change.hits),2)

######

#### pec #####
PEC_change2 = PEC_change2 %>% 
  mutate(change.cover = totalCover_2021-totalCover_2016,
         change.hits = totalHits_2021-totalHits_2016)

pec_cor2 = round(cor(PEC_change2$change.cover,PEC_change2$change.hits),2)
######


#### YNP #####
YNP_change2 = YNP_change2 %>% 
  mutate(change.cover = totalCover_2016-totalCover_2011,
         change.hits = totalHits_2016-totalHits_2011)

YNP_cor2 = round(cor(YNP_change2$change.cover,YNP_change2$change.hits),2)
######


all_change2 = NULL
for(i in parks){
  
  
  df = eval(as.name(paste0(i,"_change2"))) %>% 
    select(park,change.cover,change.hits) 
  all_change2 =  rbind(all_change2,df)
  
}

ggplot(all_change2,aes(x=change.cover,y=change.hits,color=park))+
  geom_abline(slope=1)+
  geom_smooth(method="lm",se=F)+
  geom_point(alpha=0.5)+
  scale_color_manual(values=park.pal)+
  theme_bw(base_size=14)+
  facet_wrap(~park,scales="free")+
  theme(legend.title = element_blank())+
  labs(x="Change in % cover",y="Change in hits",title="Change hits vs Change cover - 1x1-m plot level")

ggsave("Results/CoverPlots/changeHitsCover_1x1mplotLevel.jpeg",h=8,w=10,dpi=300)



























#######
## Surface cover from total vascular plant cover files ##
sc4parks.raw = read_csv("InputData/vegetation/NPS_GLORIA_Veg_2297334_DataPackage/NPS_IMD_GLORIA_Vegetation_2297334_SurfaceCover1x1.csv")

sc4parks = sc4parks.raw  %>% 
  select(park = Park, summit = Summit, year = Year, plot = Plot, aspect = PlotAspect, 
         plants = `SurfaceVascPlant`,hits.plants = HitsTopCoverVasc) %>% 
  filter(!(park=="RMN"&year%in%c(2009,2011))) %>% 
  na.omit()

sc4parks$year[sc4parks$year==2018&sc4parks$summit=="HUK"]=2015

gsd.hits.cover = sc4parks %>% 
  filter(park=="GSD") %>% 
  group_by(park,summit,aspect,year) %>% 
  summarize(hits = sum(hits.plants),
            cover = sum(plants)) %>% 
  pivot_wider(id_cols = c(park,summit,aspect),names_from = year,values_from = c(hits,cover)) %>% 
  na.omit() %>% 
  mutate(perc.change.hits = (hits_2020-hits_2015)/hits_2015,
         perc.chanve.cover = (cover_2020-cover_2015)/cover_2015)

rmn.hits.cover = sc4parks %>% 
  filter(park=="RMN") %>% 
  group_by(park,summit,aspect,year) %>% 
  summarize(hits = sum(hits.plants),
            cover = sum(plants)) %>% 
  pivot_wider(id_cols = c(park,summit,aspect),names_from = year,values_from = c(hits,cover))%>% 
  mutate(change.hits = hits_2019-hits_2014,
         change.cover = cover_2019-cover_2014)
cor(rmn.hits.cover$change.hits,rmn.hits.cover$change.cover)

ynp.hits.cover = sc4parks %>% 
  filter(park=="YNP") %>% 
  group_by(park,summit,aspect,year) %>% 
  summarize(hits = sum(hits.plants),
            cover = sum(plants)) %>% 
  pivot_wider(id_cols = c(park,summit,aspect),names_from = year,values_from = c(hits,cover)) %>% 
  mutate(change.hits = hits_2016-hits_2011,
         change.cover = cover_2016-cover_2011)
cor(ynp.hits.cover$change.hits,ynp.hits.cover$chanve.cover)


corr.df = sc4parks %>% 
  na.omit() %>% 
  group_by(park,year) %>% 
  summarise(plant.corr = cor(plants,hits.plants)) # %>% filter(!is.na(plant.corr))


### GSD did not do hits in 2009 and did a small subset in 2015
### PEC did not do hits in 2016, just 2020

#######