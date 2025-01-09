
# date: 17-01-24
# author: VB
# purpose: Create metadata for SoWT2 creation data
# link: https://annakrystalli.me/rrresearchACCE20/dataspice.html

### working dirs --------------------------------------------------------------------------------------

wd <- "C:/Users/vbu/OneDrive - the Woodland Trust/Projects/CO&E - SoWT2/Project-SoWT2" # WT laptop path
dirData <- paste0(wd,"/data-raw/")
dirScratch <- paste0(wd,"/data-scratch/")
dirOut <- paste0(wd,"data-out")

### libraries -----------------------------------------------------------------------------------------

library(tidyverse)
library(dataspice)

# create metadata folder within data-raw
setwd(wd)
create_spice(dir = "data-raw")

# record metadata
edit_creators(metadata_dir = file.path("data-raw","metadata"))

prep_access(data_path = "data-raw", access_path = "data-raw/metadata/access.csv")
edit_access(metadata_dir = file.path("data-raw","metadata"))             

prep_attributes(data_path = "data-raw", attributes_path = "data-raw/metadata/attributes.csv")
edit_attributes(metadata_dir = file.path("data-raw","metadata"))
