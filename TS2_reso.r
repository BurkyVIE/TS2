# LIBRARIES ----
library(tidyverse)

# DATA ----

## raw data ----
TS2_reso_raw <- "Good,Patch,Component,Quantity
coal,NA,NA,NA
iron ore,NA,NA,NA
steel,NA,NA,NA
copper ore,NA,NA,NA
wood,NA,NA,NA
timber,NA,NA,NA
crude oil,NA,NA,NA
gasoline,NA,NA,NA
grain,NA,NA,NA
livestock,NA,NA,NA
flour,NA,NA,NA
(steel,40,iron ore,10
(steel,40,coal,30
copper,30,copper ore,40
copper,30,coal,40
iron powder,30,iron ore,30
nails,40,steel,40
saw blade,70,steel,40
saw blade,70,iron powder,30
copper wire,110,copper,80
copper wire,110,copper ore,30
sheet steel,70,steel,40
sheet steel,70,iron ore,30
(timber,40,wood,40
barrel,210,wood,100
barrel,210,copper wire,110
plywood,160,glue,100
plywood,160,wood,60
pallet,120,timber,80
pallet,120,nails,40
chair,120,timber,80
chair,120,nails,40
table,150,timber,80
table,150,saw blade,70
closet,200,plywood,160
closet,200,nails,40
glue,100,crude oil,50
glue,100,wood,50
milk,80,livestock,80
plastics,100,crude oil,100
wool,120,livestock,120
"

# TIDY ----

## import ----
TS2_reso <- read_delim(TS2_reso_raw, col_types = "cici", lazy = FALSE) %>% 
  arrange(Good)

# CLEAN UP ----
rm(TS2_reso_raw)
