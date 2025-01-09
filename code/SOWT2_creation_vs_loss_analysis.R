
# date: 15-01-24
# author: VB
# purpose: Explore options for presenting creation data.

### working dirs ---------------------------------------------------------------

#wd <- "~/Documents/Woodland-Trust/Data-Analysis/Project-SOWT2" # MacBook path
wd <- "C:/Users/vbu/OneDrive - the Woodland Trust/Projects/CO&E - SoWT2/Project-SoWT2" # WT laptop path
dirData <- paste0(wd,"/data-raw/")
dirScratch <- paste0(wd,"/data-scratch/")
dirOut <- paste0(wd,"data-out")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stringr)
library(extrafont)
extrafont::loadfonts()

### read in data ---------------------------------------------------------------

# downloaded from https://www.forestresearch.gov.uk/tools-and-resources/statistics/data-downloads/
# on 15-01-2024
# converted selected spreadsheets to csv format

# data on woodland area
# data_area <- read.csv(paste0(dirData,"area-timeseries-UK.csv"))
# 
# # planting & restocking data
# data_creation <- read.csv(paste0(dirData,"new_planting_type_ownership.csv"))
# data_restock <- read.csv(paste0(dirData,"restocking_type_ownership.csv"))
# 
# # loss/change data
# # https://www.globalforestwatch.org/dashboards/country/GBR/?location=WyJjb3VudHJ5IiwiR0JSIl0%3D
# data_loss <- read.csv(paste0(dirData, "GFW_UK_tree_cover_loss_subnational.csv"))
# 
# 
# ### area first - have a look ---------------------------------------------------
# 
# head(data_area)
# summary(data_area)
# colnames(data_area)
# # change column names
# colnames(data_area) <- c("year",
#                          "private.sector.cf",
#                          "public.sector.cf",
#                          "private.sector.bf",
#                          "public.sector.bf",
#                          "uk.total")
# 
# # issue that a couple of variables are reading in as characters
# # clean - strip out ","
# test <- str_replace_all(data_area$uk.total,",","")
# # convert to numeric
# as.numeric(test)
# rm(test)
# 
# # all good, apply to data
# data_area$uk.total <- as.numeric(str_replace_all(data_area$uk.total,",",""))
# data_area$private.sector.bf <- as.numeric(str_replace_all(data_area$private.sector.bf,",",""))
# summary(data_area)
# 
# ### wrangle --------------------------------------------------------------------
# 
# # convert to long format
# data_area_long <- gather(data_area, ownership, thousand.ha, private.sector.cf:uk.total, factor_key = T)
# 
# # separate conifer/broadleaf into new var
# data_area_plot <- data_area_long %>% 
#   filter(ownership != "uk.total") %>% 
#   separate(ownership, into = c("sector","delete","woodland.type")) %>% 
#   mutate(woodland.type.full = ifelse(woodland.type == "cf", "conifer","broadleaf"),
#          delete = NULL)
# 
# ### plot it --------------------------------------------------------------------
# 
# # labels for facets
# ownership.labs <- c("Private sector - conifer", "Public sector - conifer", "Private sector - broadleaf", "Public sector - broadleaf")
# names(ownership.labs) <- c("private.sector.cf", "public.sector.cf", "private.sector.bf", "public.sector.bf")
# 
# # plot
# data_area_plot %>% 
#   #filter(ownership != 'uk.total') %>% # just the country data
#   ggplot()+
#   #geom_area(aes(x = year, y = thousand.ha, fill = ownership))
#   #geom_area(aes(x = year, y = thousand.ha), fill = "chartreuse4")+
#   geom_area(aes(x = year, y = thousand.ha, fill = woodland.type.full), na.rm = TRUE)+
#   facet_wrap(~sector, ncol = 2)+#,
#              #labeller = labeller(ownership = ownership.labs))+
#   ggtitle("Woodland area by sector, 1998 to 2023")+
#   labs(x = "Year", y = "Area (thousand ha)")+
#   #ylim(c(0,1000))+
#   #xlim(c(1998,2023))+
#   theme_grey()+
#   theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10,0,10,0), family = "Avenir"),
#         axis.title.x = element_text(vjust = 0.5),
#         axis.title.y = element_text(vjust = 0.5),
#         legend.title = element_blank())
# 
# ### now creation data ----------------------------------------------------------
# 
# head(data_creation)
# summary(data_creation)
# colnames(data_creation) <- c("year",
#                              "private.sector.cf",
#                              "private.sector.bf",
#                              "public.sector.cf",
#                              "public.sector.bf",
#                              "total",
#                              "note")
# summary(data_creation)
# 
# # clean (character to numeric)
# data_creation$private.sector.cf <- as.numeric(str_replace_all(data_creation$private.sector.cf,",",""))
# data_creation$public.sector.cf <- as.numeric(str_replace_all(data_creation$public.sector.cf,",",""))
# data_creation$public.sector.bf <- as.numeric(str_replace_all(data_creation$public.sector.bf,",",""))
# summary(data_creation)
# 
# ### wrangle --------------------------------------------------------------------
# 
# # convert to long format
# data_creation_long <- gather(data_creation, ownership, thousand.ha, private.sector.cf:total, factor_key = T)
# data_creation_long
# 
# 
# ### plot -----------------------------------------------------------------------
# # labels for facets
# ownership.labs <- c("Private sector - conifer", "Public sector - conifer", "Private sector - broadleaf", "Public sector - broadleaf")
# names(ownership.labs) <- c("private.sector.cf", "public.sector.cf", "private.sector.bf", "public.sector.bf")
# 
# # plot
# data_creation_long %>% 
#   filter(ownership != 'total') %>% # just the country data
#   ggplot()+
#   #geom_area(aes(x = year, y = thousand.ha, fill = ownership))
#   geom_area(aes(x = year, y = thousand.ha), fill = "chartreuse4")+
#   facet_wrap(~ownership, ncol = 2,
#              labeller = labeller(ownership = ownership.labs))+
#   ggtitle("Woodland creation by sector, 1998 to 2023")+
#   labs(x = "Year", y = "Area (thousand ha)")+
#   #ylim(c(0,2000))+
#   #xlim(c(1998,2023))+
#   theme_grey()+
#   theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10,0,10,0), family = "TomsHand"),
#         axis.title.x = element_text(vjust = 0.5),
#         axis.title.y = element_text(vjust = 0.5),
#         legend.title = element_blank())
# 
# ### now re-stock data ----------------------------------------------------------
# 
# head(data_restock)
# summary(data_restock)
# colnames(data_restock)
# colnames(data_restock) <- c("year",
#                              "private.sector.cf",
#                              "private.sector.bf",
#                              "public.sector.cf",
#                              "public.sector.bf",
#                              "total",
#                              "note")
# summary(data_restock)
# 
# # clean (character to numeric)
# data_restock$private.sector.cf <- as.numeric(str_replace_all(data_restock$private.sector.cf,",",""))
# data_restock$private.sector.bf <- as.numeric(str_replace_all(data_restock$private.sector.bf,",",""))
# 
# summary(data_restock)
# 
# ### wrangle --------------------------------------------------------------------
# 
# # convert to long format
# data_restock_long <- gather(data_restock, ownership, thousand.ha, private.sector.cf:total, factor_key = T)
# data_restock_long
# 
# 
# ### plot -----------------------------------------------------------------------
# 
# # plot
# data_restock_long %>% 
#   filter(ownership != 'total') %>% # just the country data
#   ggplot()+
#   #geom_area(aes(x = year, y = thousand.ha, fill = ownership))
#   geom_area(aes(x = year, y = thousand.ha), fill = "red4")+
#   facet_wrap(~ownership, ncol = 2,
#              labeller = labeller(ownership = ownership.labs))+
#   ggtitle("Woodland restock by sector, 1998 to 2023")+
#   labs(x = "Year", y = "Area (thousand ha)")+
#   #ylim(c(0,2000))+
#   #xlim(c(1998,2023))+
#   theme_grey()+
#   theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10,0,10,0), family = "Arial Narrow"),
#         axis.title.x = element_text(vjust = 0.5),
#         axis.title.y = element_text(vjust = 0.5),
#         legend.title = element_blank())



### plan -----------------------------------------------------------------------

# what do I want to show?
# essentially want to show woodland area & creation (woodland stock)
# to woodland loss (with restock taken account of)
# by country
# in thousand ha units

# read in data that i wrangled by hand from forestry stats xls

df_forestry_stats <- read.csv(paste0(dirScratch,"forestry_stats_2023_creation-restock_wrangled.csv"),
                                header = TRUE)
head(df_forestry_stats)
summary(df_forestry_stats)

### plot -----------------------------------------------------------------------

df_FS_long <- df_forestry_stats %>% 
  gather(., forest.stat, t.ha, area.t.ha:restock.t.ha) #%>% 
  #mutate(year = as.factor(year))

summary(df_FS_long)

# df_FS_long %>% 
#   filter(forest.stat == "area.t.ha" & country == "England" & sector == "Public") %>% 
#   ggplot()+
#   geom_area(aes(year,t.ha, fill = woodland.type))

png(paste0(wd,"/figures/area-by-country.png"), width = 850, height = 650)
(p1 <- df_FS_long %>% 
    filter(forest.stat == "area.t.ha") %>% 
    ggplot()+
    geom_area(aes(year,t.ha, fill = woodland.type), na.rm = T)+
    scale_fill_manual(values = c("#AFFACE","#497A5E"))+
    facet_grid(country~sector)+
    theme_light()+
    ylab("Woodland area (thousand ha)")+xlab("Year")+
    labs(fill="Woodland type")+
    theme(title = element_text(size = 22, face = "bold", family = "Calibri"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 18),
          strip.text = element_text(face="bold", size = 12)))
dev.off()

png(paste0(wd,"/figures/creation-by-country.png"), width = 850, height = 650)
(p2 <- df_FS_long %>% 
    filter(forest.stat == "creation.t.ha") %>% 
    ggplot()+
    geom_area(aes(year,t.ha, fill = woodland.type), na.rm = T)+
    scale_fill_manual(values = c("#AFFACE","#497A5E"))+
    facet_grid(country~sector)+
    theme_light()+
    ylab("Woodland creation (thousand ha)")+xlab("Year")+
    labs(fill="Woodland type")+
    theme(title = element_text(size = 22, face = "bold", family = "Calibri"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 18),
          strip.text = element_text(face="bold", size = 12)))
dev.off()

png(paste0(wd,"/figures/restock-by-country.png"), width = 850, height = 650)
(p3 <- df_FS_long %>% 
    filter(forest.stat == "restock.t.ha") %>% 
    ggplot()+
    geom_area(aes(year,t.ha, fill = woodland.type), na.rm = T)+
    scale_fill_manual(values = c("#AFFACE","#497A5E"))+
    facet_grid(country~sector)+
    theme_light()+
    ylab("Area restocked (thousand ha)")+xlab("Year")+
    labs(fill="Woodland type")+
    theme(title = element_text(size = 22, face = "bold", family = "Calibri"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 18),
          strip.text = element_text(face="bold", size = 12)))
dev.off()


### now add in loss to compare -------------------------------------------------

# loss/change data
# https://www.globalforestwatch.org/dashboards/country/GBR/?location=WyJjb3VudHJ5IiwiR0JSIl0%3D
df_loss <- read.csv(paste0(dirData, "GFW_UK_tree_cover_loss_subnational.csv"))

head(df_loss)
summary(df_loss)
colnames(df_loss)

### wrangle --------------------------------------------------------------------

# convert to long format
df_loss_long <- gather(df_loss, year, tc.loss.ha, tc_loss_ha_2001:tc_loss_ha_2022) #, factor_key = T)
df_loss_long
summary(df_loss_long)

df_loss_long <- tidyr::separate(data = df_loss_long, year, into = c("delete1","delete2","delete3", "year"))
summary(df_loss_long)

# use mutate, change year to numeric and remove un-needed vars
df_loss_long <- df_loss_long %>% 
  mutate(Country = subnational1,
         subnational1 = NULL,
         year = as.numeric(df_loss_long$year),
         delete1 = NULL,
         delete2 = NULL,
         delete3 = NULL)

### plot -----------------------------------------------------------------------

# loss is recorded for different thresholds of canopy cover, so facet by these
(p4 <- df_loss_long %>% 
   ggplot()+
   geom_area(aes(year,tc.loss.ha, fill = Country))+
   scale_fill_manual(values = c("#FA7470","#73FA70","#F170FA","#438442"))+
   ggtitle("Woodland loss over time, 2001 - 2022")+
   facet_wrap(~threshold)+
   theme_light()+
   theme(title = element_text(size = 22, face = "bold", family = "Calibri"),
         axis.title.x = element_blank(),
         axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
         axis.text.y = element_text(size = 18),
         axis.text.x = element_text(size = 18),
         axis.ticks.x = element_blank(),
         legend.title = element_text(size = 20, face = "bold"),
         legend.text = element_text(size = 18),
         strip.text = element_text(face="bold", size = 12)))

png(paste0(wd,"/figures/loss-variation-by-canopy-threshold.png"), width = 850, height = 650)
p4
dev.off()

# or, show variation by threshold
# df_loss_long %>% 
#   ggplot()+
#   geom_boxplot(aes(as.factor(year),tc.loss.ha))+
#   ggtitle("Woodland loss over time, 2001 - 2022")+
#   facet_wrap(~Country)+
#   theme_grey()

(p5 <- df_loss_long %>% 
    ggplot()+
    geom_line(aes(year,tc.loss.ha, colour = as.factor(threshold)))+
    ggtitle("Woodland loss over time, 2001 - 2022")+
    facet_wrap(~Country)+
    labs(colour = "Canopy threshold")+
    theme_light()+
    theme(title = element_text(size = 22, face = "bold", family = "Calibri"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 18),
          strip.text = element_text(face="bold", size = 12)))

png(paste0(wd,"/figures/loss-variation-by-canopy-threshold-v2.png"), width = 850, height = 650)
p5
dev.off()

# filter to just 30% canopy cover threshold
#df_loss_30 <- df_loss_long %>% 
  #filter(., threshold == 30) %>% 
  #mutate(country = NULL,
         #country = Country,
         #Country = NULL)

# loss data doesn't split by woodland.type, or sector, so need to remove these distinctions from df_FS_long before joining
df_FS_wide <- pivot_wider(df_FS_long, names_from = c(woodland.type, sector), values_from = t.ha) %>% 
  rowwise() %>% 
  mutate(tot.t.ha = sum(Conifer_Private, Broadleaf_Private, Conifer_Public,Broadleaf_Public, na.rm = TRUE))

summary(df_FS_wide)

# I want to show the range of potential loss, not just the 30% canopy cover threshold.

# just creation and restock
df_FS_filter <- df_FS_wide %>% filter(., forest.stat == 'restock.t.ha' | forest.stat == 'creation.t.ha')
summary(df_FS_filter)

# and convert tree loss to thousand ha
df_loss_long <- df_loss_long %>% mutate(loss.t.ha = tc.loss.ha/1000)

# ggplot()+
#   geom_col(data = df_FS_filter, aes(year,tot.t.ha, fill = forest.stat), na.rm = T, position = 'stack')+
#   scale_fill_manual(values=c('#999999','#E69F00'))+
#   facet_wrap(~country)+
#   geom_line(data = df_loss_long, aes(year, loss.t.ha, colour = as.factor(threshold)))+
#   facet_wrap(~Country)
# doesn't quite work, plots facet doesn't work for both dataframes at the same time... whyyy?

# join
df_FS_select <- df_FS_wide %>% select(year,forest.stat,tot.t.ha,country)
df_loss_select <- df_loss_long %>% select(year,threshold,loss.t.ha, Country) %>% mutate(country = Country, Country = NULL)
df_join <- right_join(df_FS_select, df_loss_select, by = c('year','country'), relationship = 'many-to-many') %>% #distinct()
  #cheeky workaround 
  mutate(tot.t.ha = ifelse(threshold == 0 | is.na(threshold), tot.t.ha, NA))

df_join_select <- df_join %>% 
  filter(forest.stat == 'restock.t.ha' | forest.stat == 'creation.t.ha') 

# type_manual <- c('0' = "dashed",
#                  '10' = "dashed",
#                  '15' = "dashed",
#                  '20' = "solid",
#                  '25' = "dashed",
#                  '30' = "dashed",
#                  '50' = "dashed",
#                  '75' = "dashed")

size_manual <- c('0' = 1,
                 '10' = 1,
                 '15' = 1,
                 '20' = 2,
                 '25' = 1,
                 '30' = 1,
                 '50' = 1,
                 '75' = 1)


palette_manual <- c('0' = "#D6D591",
                    '10' = "#D6D591",
                    '15' = "#D6D591",
                    '20' = "#D5D730",
                    '25' = "#D6D591",
                    '30' = "#D6D591",
                    '50' = "#D6D591",
                    '75' = "#D6D591")

(p6 <- df_join_select %>% 
    #mutate(threshold = as.factor(threshold))+
    ggplot()+
    geom_col(aes(year,tot.t.ha, fill = forest.stat), na.rm = T, position = 'stack')+
    scale_fill_manual(values=c('#1FAB91','#C85328'), labels = c("Woodland created", "Woodland restocked"))+
    geom_line(aes(year,loss.t.ha, colour = as.factor(threshold),  size = as.factor(threshold)))+
    scale_colour_manual(values = palette_manual, labels = c("0% canopy threshold",
                                                            "10% canopy threshold",
                                                            "15% canopy threshold",
                                                            "20% canopy threshold",
                                                            "25% canopy threshold",
                                                            "30% canopy threshold",
                                                            "50% canopy threshold",
                                                            "75% canopy threshold"))+
    #scale_linetype_manual(values = type_manual) +
    scale_size_manual(values = size_manual, labels = c("0% canopy threshold",
                                                       "10% canopy threshold",
                                                       "15% canopy threshold",
                                                       "20% canopy threshold",
                                                       "25% canopy threshold",
                                                       "30% canopy threshold",
                                                       "50% canopy threshold",
                                                       "75% canopy threshold"))+
    facet_wrap(~country)+
    ylab("Area (thousand ha)")+
    labs(fill = "Forestry Statistic", colour = "Woodland loss", size = "Woodland loss")+
    theme_light()+
    theme(axis.title.x = element_blank(),
          panel.grid.major = element_blank(), 
          #panel.grid.minor = element_blank(),
          axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15), family = "Rockwell" ),
          axis.text.y = element_text(size = 18,  family = "Rockwell"),
          axis.text.x = element_text(size = 18,  family = "Rockwell"),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 20, face = "bold", family = "Rockwell"),
          legend.text = element_text(size = 16,  family = "Rockwell"),
          strip.text = element_text(face="bold", size = 14,  family = "Rockwell")))

# annual values seem too high
# max(df_join_select %>% filter(country == "England" & forest.stat == "creation.t.ha") %>% select(tot.t.ha), na.rm = TRUE)
# df_join_select$tot.t.ha
# it's because Fstats are repeated for each loss threshold value - happens in the join.

png(paste0(wd,"/figures/creation_restock_vs_loss.png"), width = 850, height = 650)
p6
dev.off()

# also add in lines to illustrate annual creation target per country? or do a separate plot for that

### ewan's suggestion, plot loss against creation (and restock?)

df_join_select %>% 
  #filter(forest.stat == "creation.t.ha") %>% 
  ggplot()+
  geom_point(aes(tot.t.ha,loss.t.ha, colour = forest.stat))+
  geom_smooth(aes(tot.t.ha,loss.t.ha, colour = forest.stat))+
  theme_minimal()
