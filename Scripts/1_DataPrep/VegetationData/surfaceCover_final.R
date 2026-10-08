
### Generating total plant cover data from GLORIA top cover ###
### (replaces surfaceCoverfromSpecCover_final.R's summed-species-cover approach:
###  top cover is bounded at 100%, summed species cover is not, because plants overlap)
### Kept at the quadrat (plot) level, not averaged up to summit-aspect.
library(tidyverse)
library(reshape2)
rm(list=ls())

traits = read_csv("./Data/speciestraits/allparks_speclist_20220124 traits.csv")
parks_ns = c("GNP","YNP","RMN","GSD","PEC")

## GNP top cover ##
surfCover_gnp <- read_csv("Data/vegetation/GLAC_GLORIA_Data_For_CU/GLAC_coverType_allYears.csv") |>
  mutate(park="GNP",
         aspect=str_sub(plot,1,1)) |>
  select(park,summit,year,plot,aspect,cover=vascPlant)

## Top cover for the other 4 parks ##
surfCover_others <- read_csv("Data/vegetation/NPS_GLORIA_Veg_2297334_DataPackage/NPS_IMD_GLORIA_Vegetation_2297334_SurfaceCover1x1.csv") |>
  select(park=Park,summit=Summit,year=Year,plot=Plot,aspect=PlotAspect,cover=SurfaceVascPlant) |>
  filter((park=="RMN" & year %in% c(2010,2014,2019)) |
         park%in%c("GSD","PEC","YNP"))

# GSD-HUK 2018 data entry fix -> 2015 (same fix applied to the species-level data)
surfCover_others$year[surfCover_others$park=="GSD" & surfCover_others$year==2018] <- 2015

vascCover <- rbind(surfCover_gnp,surfCover_others)
vascCover$park <- factor(vascCover$park,levels = parks_ns)
vascCover$aspect <- factor(vascCover$aspect,levels = c("N","S","E","W"))
rm(surfCover_gnp,surfCover_others)

## RMN quadrats surveyed per summit-year (16 = complete) ##
#       GLA JSM PIK VQS
# 2010   0  16  16  16
# 2014  16  16  16  16
# 2019  16  16  16  16

## GSD quadrats surveyed per summit-year (16 = complete) ##
#      HUK MUD PAD SIX
# 2009   8   8   8  12
# 2015  16  16  16  16
# 2020  16  16  16  16

## Matched quadrats only: pair each quadrat with its own next survey ##
## One row per quadrat (plot) per transition ##
allcover.wide = vascCover |>
  arrange(park, summit, plot, year) |>
  group_by(park, summit, plot) |>
  mutate(endyear = lead(year),
         end.plantCover = lead(cover)) |>
  ungroup() |>
  filter(!is.na(endyear)) |>
  rename(startyear = year, start.plantCover = cover) |>
  mutate(change.cover = end.plantCover - start.plantCover) |>
  group_by(summit) |>
  mutate(transition = paste0(summit,dense_rank(startyear))) |>
  ungroup() |>
  mutate(transNum=substr(transition,nchar(summit)+1,nchar(summit)+1))

table(allcover.wide$park,allcover.wide$summit)

write_csv(allcover.wide,"Data/IntermediateData/vascplantcover_final.csv")
