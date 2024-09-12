
### Generating surface cover data from species-level % cover ###
library(tidyverse)
library(reshape2)
rm(list=ls())

traits = read_csv("./InputData/speciestraits/allparks_speclist_20220124 traits.csv")
parks_ns = c("GNP","YNP","RMN","GSD","PEC")

specCover = read_csv("InputData/byClaire/all_1x1veg_no0s_2023-06-28.csv") %>% 
  merge(.,traits,by="sciname") 
genus_data = filter(specCover,species=="sp")
table(genus_data$park)

vascCover = specCover %>% 
  filter(tax_unit!='lichen'&habit!="clubmoss"&habit!="fern") %>% 
  # filter(species!="sp") %>% 
  group_by(park,summit,aspect,year,plot) %>% 
  summarise(plotCover=sum(cover),.groups="drop") %>% 
  group_by(park,summit,aspect,year) %>% 
  summarise(totalCover=sum(plotCover),
         meanCover=mean(plotCover),.groups="drop") 
vascCover$park <- factor(vascCover$park,levels = parks_ns)
vascCover$aspect <- factor(vascCover$aspect,levels = c("N","S","E","W"))

# 
# ggplot(vascCover,aes(x=meanCover,y=totalCover,color=as.factor(year)))+
#   geom_abline(slope=1)+
#   geom_point()+
#   facet_wrap(~park+year)
#

allcover.wide = NULL
summits = unique(vascCover$summit)

for(s in summits){
  
  tmp1 = filter(vascCover,summit==s) 
  park.yrs = unique(vascCover$year[vascCover$park==tmp1$park[1]])
  summit.yrs = unique(tmp1$year)
  transitions = length(summit.yrs)-1
  
  for(i in 1:transitions){
    
    tmp2 = filter(tmp1,year==summit.yrs[i]) %>% 
      rename(startyear = year,
             start.plantCover=totalCover)
    
    tmp3 = filter(tmp1,year==summit.yrs[i+1])%>% 
      rename(endyear = year,
             end.plantCover=totalCover)
    
    tmp.out = merge(tmp2,tmp3,all = T) %>% 
      mutate(transition = paste0(s,i))
    
    allcover.wide = rbind(allcover.wide,tmp.out)
    
  }
}

allcover.wide = allcover.wide %>% 
  mutate(change.cover = end.plantCover - start.plantCover) %>% 
  mutate(transNum=substr(transition,4,4))

# write_csv(allcover.wide,paste0("InputData/byClaire/vascplantcover_",Sys.Date(),".csv"))
write_csv(allcover.wide,paste0("InputData/byClaire/vascplantcover_final.csv"))

ggplot(allcover.wide,aes(x=transNum,y=change.cover,color=change.cover>0))+
  geom_hline(yintercept = 0,linetype=3)+
  geom_point()+
  scale_color_manual(values=c("red","forestgreen"))+
  facet_grid(park~aspect,scales="free",switch = "y")+
  theme_bw(base_size = 16)

ggplot(allcover.wide,aes(x=transNum,y=change.cover,color=change.cover>0))+
  geom_hline(yintercept = 0,linetype=3)+
  geom_point()+
  facet_grid(aspect~park,scales = "free",switch="y")+
  theme_bw(base_size = 16)+
  scale_color_manual(values=c("red","forestgreen"),labels=c("decreasing","increasing"))+
  labs(x="transition",y="change in total plant cover",color="Direction of change")
