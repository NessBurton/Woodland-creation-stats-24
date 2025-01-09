
# date: 13-02-24
# author: VB
# purpose: Explore spatial method for reproducing woodland communities

### working dirs ---------------------------------------------------------------

wd <- "C:/Users/vbu/OneDrive - the Woodland Trust/Projects/CO&E - SoWT2/Project-SoWT2" # WT laptop path
dirData <- paste0(wd,"/data-raw/")
dirScratch <- paste0(wd,"/data-scratch/")
dirOut <- paste0(wd,"/data-out/")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(stars)
library(stringr)
library(magrittr)

### read in species shapefiles -------------------------------------------------

# manually first, need to check and clean

hornbeam <- st_read(paste0(dirData,"Hornbeam.shp"))
hornbeam
unique(hornbeam$Code)

# convert NAs to 0
hornbeam$Code[which(is.na(hornbeam$Code))] <- 0
hornbeam$Code <- factor(hornbeam$Code, levels = c(0,1,1.5,2,2.5,3,4))

ggplot()+
  geom_sf(hornbeam, mapping = aes(fill = Code), colour = NA)+
  scale_fill_brewer(type = "seq", palette = 2)+
  labs(fill = "Appropriateness")+
  ggtitle(i)+
  theme_minimal()+
  geom_sf(sfOpportunity, mapping = aes(colour = woodland.opportunity.ha), fill = NA)

# loop once data cleaned

lstSpecies <- c("Hornbeam", "Holly", "Hazel", "Guelder_rose")

for (i in lstSpecies){
  
  #i <- lstSpecies[1]
  
  species <- st_read(paste0(dirData,i,".shp"))
  
  species$Code <- factor(species$Code, levels = c(NA,0,1,1.5,2,2.5,3,4))
  
  print(ggplot()+
          geom_sf(species, mapping = aes(fill = Code), colour = NA)+
          scale_fill_brewer(type = "seq", palette = 2)+
          labs(fill = "Appropriateness")+
          ggtitle(i)+
          theme_minimal())
  
}


### read in England creation target data and clean -----------------------------

# woodland opportunity data from FoE https://friendsoftheearth.uk/nature/woodland-opportunity-local-authority-full-dataset 
dfOpportunity <- read.csv(paste0(dirData,"Woodland_opportunity_by_local_authority_full_data.csv"))
head(dfOpportunity)

# local authorities shapefile from https://geoportal.statistics.gov.uk/datasets/196d1a072aaa4882a50be333679d4f63/explore
sfLA <- st_read(paste0(dirData,"Local_Authority_Districts_May_2022_UK_BFE_V3_2022_3331011932393166417/LAD_MAY_2022_UK_BFE_V3.shp"))
head(sfLA)

# check codes - do these represent country?
unique(sfLA$LAD22CD)

# filter to England
sfLA <- sfLA %>% filter(grepl("E",LAD22CD))

# check whether LA names match
dfOpportunity$local.authority %in% sfLA$LAD22NM
unique(dfOpportunity$local.authority)
unique(sfLA$LAD22NM)
# need to strip "district" and "B" from most of the dfOpportunity entries
words <- c("London Boro", "District B", "District")
pat <-str_c(words, collapse = "|")
(test <- dfOpportunity$local.authority %>% str_remove_all(pat) %>% trimws())
gsub("\\(B\\)", "", test) %>% trimws("right")

dfOpportunity$local.authority <- dfOpportunity$local.authority %>% str_remove_all(pat) %>% trimws() %>% gsub("\\(B\\)", "", .) %>% trimws("right")

length(dfOpportunity$local.authority)
length(sfLA$LAD22NM) # doesn't match
dfOpportunity$local.authority %in% sfLA$LAD22NM #  some still don't match
(issues <- dfOpportunity$local.authority[which(dfOpportunity$local.authority %in% sfLA$LAD22NM == FALSE)]) # these ones

# city of causing some of the issues...
words2 <- c("City and County of the ", "City of ", "The ", "County of ", "[.]")
pat2 <-str_c(words2, collapse = "|")
(test2 <- dfOpportunity$local.authority %>% str_remove_all(pat2) %>% trimws())
dfOpportunity$local.authority <- dfOpportunity$local.authority %>% str_remove_all(pat2) %>% trimws()

# do the same for sfLA
(issues <- sfLA$LAD22NM[which(sfLA$LAD22NM %in% dfOpportunity$local.authority == FALSE)]) # these ones
words3 <- c(", City of", ", County of")
pat3 <-str_c(words3, collapse = "|")
(test3 <- sfLA$LAD22NM %>% str_remove_all(pat3) %>% trimws())
sfLA$LAD22NM <- sfLA$LAD22NM %>% str_remove_all(pat3) %>% trimws()

# check again
#(issues <- dfOpportunity$local.authority[which(dfOpportunity$local.authority %in% sfLA$LAD22NM == FALSE)]) # these ones
# erg. close enough for now

# join
sfLA <- sfLA %>% mutate(LA = LAD22NM)
dfOpportunity <- dfOpportunity %>% mutate(LA = local.authority)

# catch issues found below before merging
dfOpportunity$LA[which(dfOpportunity$local.authority == "Corby" |
                                      dfOpportunity$local.authority == "East Northamptonshire"|
                                      dfOpportunity$local.authority == "Kettering"|
                                      dfOpportunity$local.authority == "Wellingborough")] <- "North Northamptonshire"

dfOpportunity$LA[which(dfOpportunity$local.authority == "Northampton" |
                                      dfOpportunity$local.authority == "Daventry"|
                                      dfOpportunity$local.authority == "South Northamptonshire")] <- "West Northamptonshire"

dfOpportunity$woodland.opportunity.ha[which(dfOpportunity$LA == "North Northamptonshire")]

dfOpportunity$LA[which(dfOpportunity$local.authority == "St Helens")] <- "St. Helens"

# dissolve so they're the same length
dfOpportunity2 <- dfOpportunity %>%
  group_by(LA) %>% 
  summarise(area.ha = sum(area.ha),
            woodland.opportunity.ha = sum(woodland.opportunity.ha))

sfOpportunity <- merge(sfLA, dfOpportunity2, by = "LA", all.x = TRUE)

# plot LAs
ggplot()+
  geom_sf(sfOpportunity, mapping = aes(fill = woodland.opportunity.ha))+
  #scale_fill_brewer(type = "seq", palette = 2)+
  #labs(fill = "Appropriateness")+
  theme_minimal()

# track down issues (fixed now)
#sfOpportunity %>% filter(is.na(woodland.opportunity.ha))
# City of London
# North Northamptonshire
# St. Helens
# West Northamptonshire

# write as shapefile
st_write(sfOpportunity, paste0(dirOut,"LA_woodland_opportunity_FoE.shp"))



### plot with spp appropriateness for lols ----

ggplot()+
  geom_sf(hornbeam, mapping = aes(fill = Code), colour = NA)+
  scale_fill_brewer(type = "seq", palette = 2)+
  labs(fill = "Appropriateness")+
  theme_minimal()+
  geom_sf(sfOpportunity, mapping = aes(colour = woodland.opportunity.ha), fill = NA)+
  #scale_fill_brewer(type = "seq", palette = "BuPu")+
  labs(colour = "Woodland opportunity (ha)")


### assumption time!!! ---------------------------------------------------------

### is an LA appropriate for the species in question - make this y/n, depending on whether over 50% of the area is made up by an appropriateness code

# species dataset needs to be a raster for this?
rstHornbeam <- st_rasterize(hornbeam %>% dplyr::select(Code, geometry))

plot(rstHornbeam)

### how much of the woodland opportunity area do we assume we use for creation?
# could do a range e.g. 25%, 50%, 75%, 100%

### per species, estimate the proportion of any creation scheme it would make up - e.g. is it a major or minor component?
# e.g. hornbeam = minor component of groves, major in OWH and glades

### where the species is appropriate to the LA, estimate number of trees per ha this might equate to
# think through an example
# 100 ha opportunity area in an LA with max appropriateness for hornbeam
# lets go with 25% this having actual creation - 25 ha
# need to decide on a split between grove, OWH, glades
# lets say 10ha grove, 10ha OWH, 5ha glades
# decide on a density per each of these too
# lets say 1100/ha grove, 100/ha OWH, 50 glades
# and this won't be the only spp.



