# LIBRARIES ----
library(tidyverse)

# DATA ----

## search for ----
sf <- "copper wire"

## raw data ----
res_raw <- "Good,Patch,Component,Quantity
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
res_dat <- read_delim(res_raw, col_types = "cici", lazy = FALSE)

## expand ----
res_full <- left_join(
  left_join(
    res_dat, select(res_dat, -Patch), by = c("Component" = "Good"), suffix = c("", ".1")),
  select(res_dat, -Patch), by = c("Component.1" = "Good"), suffix = c("", ".2"))

# CALCULATE ----
bind_rows(
  select(res_full, c(Good, Patch, Component = Component.2, Quantity = Quantity.2)) %>% filter(!is.na(Component)),
  select(filter(res_full, is.na(Component.2)), c(Good, Patch, Component = Component.1, Quantity = Quantity.1)) %>% filter(!is.na(Component)),
  select(filter(res_full, is.na(Component.1)), c(Good, Patch, Component = Component, Quantity = Quantity)) %>% filter(!is.na(Component))
) %>% filter(Good == sf) %>%
  group_by(Good, Patch, Component) %>%
  summarise(Quantity = sum(Quantity), .groups = "drop") %>% 
  print()

filter(res_full, Good == sf) %>% 
  print()

# CLEAN UP ----
rm(res_raw, res_dat, sf)
