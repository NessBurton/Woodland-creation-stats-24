
# date: 17-01-24
# author: VB
# purpose: Explore data from FoE on opportunities for woodland creation by England LAs

### working dirs --------------------------------------------------------------------------------------

#wd <- "~/Documents/Woodland-Trust/Data-Analysis/Project-SOWT2" # MacBook path
wd <- "C:/Users/vbu/OneDrive - the Woodland Trust/Projects/CO&E - SoWT2/Project-SoWT2" # WT laptop path
dirData <- paste0(wd,"/data-raw/")
dirScratch <- paste0(wd,"/data-scratch/")
dirOut <- paste0(wd,"data-out")

### libraries -----------------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stringr)
library(tidyr)

### read in data -------------------------------------------------------------------------------

# downloaded from https://friendsoftheearth.uk/nature/woodland-opportunity-local-authority-full-dataset 
# on 17-01-2024
# converted to csv format

df_opportunity <- read.csv(paste0(dirData,"Woodland_opportunity_by_local_authority_full_data.csv"))
head(df_opportunity)
summary(df_opportunity)

# add WT regions and/or counties to the dataframe based on local authority?
# then can facet by region/county instead

### plot ---------------------------------------------------------------------------------------

df_opportunity %>% 
  ggplot()+
  geom_(aes(area.ha,woodland.opportunity.ha))+
  facet_wrap(~local.authority)
