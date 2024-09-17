
### Generating surface cover data from species-level % cover ###
library(tidyverse)
library(reshape2)
rm(list=ls())

traits = read_csv("./Data/speciestraits/allparks_speclist_20220124 traits.csv")
parks_ns = c("GNP","YNP","RMN","GSD","PEC")

specCover = read_csv("Data/IntermediateData/all_1x1veg_no0s_final.csv") %>% 
  merge(.,traits,by="sciname") 

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
             start.plantCover=totalCover,
             start.meanCover=meanCover)
    
    tmp3 = filter(tmp1,year==summit.yrs[i+1])%>% 
      rename(endyear = year,
             end.plantCover=totalCover,
             end.meanCover=meanCover)
    
    tmp.out = merge(tmp2,tmp3,all = T) %>% 
      mutate(transition = paste0(s,i))
    
    allcover.wide = rbind(allcover.wide,tmp.out)
    
  }
}

allcover.wide = allcover.wide %>% 
  mutate(change.cover = end.plantCover - start.plantCover) %>% 
  mutate(transNum=substr(transition,4,4))

write_csv(allcover.wide,paste0("Data/IntermediateData/vascplantcover_final.csv"))
