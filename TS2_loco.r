# LIBRARIES ----
library(tidyverse)
library(janitor)

# DATA ----

## raw data ----
TS2_loco_raw <- "Name,Tier,Rarity_raw,Power_raw,Capmax
ATSF 3000,2,violet,steam,45
(BALDWIN 60000,3,gold,steam,60
BLACK FIVE,4,violet,steam,45
BURLINGTON ZEPHYR,4,blue,diesel,30
BNR CLASS P GARRAT,4,blue,steam,30
CFV BILLARD 213,4,violet,diesel,45
CLASS A-1 BERKSHIRE,3,silver,steam,20
CN NORTHERN U-4,4,silver,steam,20
CRAMPTON,1,gold,steam,60
C&O T-1,4,silver,steam,20
DB-BAUREIHE V 100,2,silver,diesel,20
DB-BAUREIHE V 200,2,violet,diesel,45
DR-BAUREIHE 44,3,blue,steam,30
DR-BAUREIHE 86,3,silver,steam,20
DR-BAUREIHE E 60,3,silver,electric,20
DR-BAUREIHE E 93,3,violet,electric,45
DR 18 201,2,blue,steam,30
(DRG 877 HAMBURG FLYER,4,violet,diesel,45
DRG CLASS E18,4,blue,electric,30
DRG CLASS E 19,4,violet,electric,45
DRG V 140 001,4,silver,diesel,20
DUCHESS OF HAMILTON,4,violet,steam,45
EASTER MIREO,2,violet,electric,60
EMC E3,4,blue,diesel,30
EMD DD35,4,violet,diesel,45
(EMD DDA40X,3,gold,diesel,60
EP-2 BIPOLAR,2,gold,electric,60
ERIE K-5A,3,blue,steam,30
ERIEL L-1,1,gold,steam,60
ÉTAT 141,4,blue,steam,30
FLYEUROPE CARAVAGGIO,3,blue,electric,40
(FLYING SCOTSMAN,4,gold,steam,60
FM H-24-66 TRAIN MASTER,4,silver,diesel,30
FS CLASS 670,2,violet,steam,45
FS CLASS 740,1,silver,steam,20
GER CLASS S69,1,silver,steam,20
GWR 3041 THE QUEEN,1,violet,steam,45
GWR 6000 CLASS KING,3,blue,steam,30
GWR CITY OF TRURO,3,blue,steam,30
GWR OLTON HALL,3,gold,steam,60
KRAUSS-MAFFEI ML 4000,4,blue,diesel,40
KROKODIL CE 6/8,2,violet,electric,45
LB&SCR B4,1,blue,steam,30
LMS HUGHES CRAB,3,violet,steam,45
LMS STAINER CLASS 8F,4,silver,steam,20
LNER A4 MALLARD,1,gold,steam,60
LNER K3,2,silver,steam,20
LNER V2 GREEN ARROW,4,violet,steam,45
LRZ 14,2,blue,diesel,30
MILWAUKEE ROAD,4,silver,steam,20
MILWAUKEE ROAD EF-1,2,blue,electric,30
NEW YORK CENTRAL HUDSON,3,silver,steam,20
NORD 140,1,blue,steam,30
NSB EL 4,3,blue,electric,30
PREUẞISCHE P8,1,silver,steam,20
PREUẞISCHE T 14,2,blue,steam,30
(PRR GG1,4,gold,electric,60
PRR K-4,2,blue,steam,30
SAR CLASS 8E,4,silver,electric,30
SBB AE 4/7,3,violet,electric,45
SECR N CLASS,2,silver,steam,20
SHAY CLASS C,1,violet,steam,45
SJ D,4,blue,electric,30
STAR CLASS 4000,1,silver,steam,20
SŽD EMCH 3,3,violet,diesel,45
TFR CLASS 19E,3,gold,electric,80
(UP BIG BOY,2,gold,steam,60
UP CHALLENGER,4,gold,steam,60
UP CLASS 9000,3,violet,steam,45
UP GTEL 3RD GEN,3,violet,diesel,45
UP GTEL VERANDA,3,violet,diesel,45
UP FEF-3,4,blue,steam,30
USRA HEAVY PACIFIC,3,silver,steam,20
VICTORIAN C CLASS,2,silver,steam,20
"

## factor definitions ----
fcts <- list(con = c(BRITAIN = 1L, GERMANY = 2L, USA = 3L, FRANCE = 4L),
             rat = c(COMMON = "silver", RARE = "blue", EPIC = "violet", LEGENDARY = "gold"),
             rar = c(SILVER = "silver", BLUE = "blue", VIOLET = "violet", GOLD = "gold"),
             pow = c(STEAM = "steam", DIESEL = "diesel", ELECTRIC = "electric"),
             # col = c(SILVER = "#d6d9de", BLUE = "#9cd3fc", VIOLET = "#cea6ff", GOLD = "#fbe20b"), # original
             col = c(SILVER = "#a6acb7", BLUE = "#4db0fa", VIOLET = "#a052ff", GOLD = "#ceb903") # 25 % darker
)

# TIDY ----
TS2_loco <- read_delim(TS2_loco_raw, col_types = "ciffd", lazy = FALSE) %>% 
  mutate(Country = factor(Tier, levels = fcts$con, labels = names(fcts$con)),
         Rarity = factor(Rarity_raw, levels = fcts$rar, labels = names(fcts$rar)),
         Rating = factor(Rarity_raw, levels = fcts$rat, labels = names(fcts$rat)),
         Power = factor(Power_raw, levels = fcts$pow, labels = names(fcts$pow)),
         Active = !str_starts(Name, "\\(")) %>% 
  select(-ends_with("_raw")) %>% 
  arrange(Tier, Rarity, Power, Name) %>% 
  filter(Active)

# GRAPH ----

## number of locos Rarity vs Power per Country ----
TS2_loco %>%
  mutate(Tier = paste(Tier, "-", Country)) %>% 
  group_by(Tier, Rarity, Power) %>%
  summarise(Count = n(), .groups = "drop") %>%
  ggplot(mapping = aes(x = Rarity, y = Power)) +
  geom_tile(mapping = aes(fill = Count), color = "white") +
  geom_text(mapping = aes(label = paste0("(",Count,")")), size = 3, fontface = "bold") +
  scale_fill_distiller(palette = "YlGn") +
  facet_wrap(~ Tier) +
  labs(title = "TRAINSTATION 2") +
  theme_bw() +
  theme(legend.position = "right") -> p1

## density of Capmax and Rarity per Country ----
TS2_loco %>%
  mutate(Tier = paste(Tier, "-", Country)) %>% 
  ggplot(mapping = aes(x = Capmax, color = Rarity)) +
  geom_density(mapping = aes(fill = Rarity), size = 1.5, alpha = .5) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), minor_breaks = seq(0, 100, by = 5), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = fcts$col) +
  scale_fill_manual(values = fcts$col) +
  facet_wrap(~ Tier, scales = "free_y") +
  labs(title = "TRAINSTATION 2") +
  theme_bw() +
  theme(legend.position = "right") -> p2

## count of engines per tier and capmax ----
TS2_loco |> 
  ggplot(mapping = aes(x = Tier, y = Capmax, size = ..n.., color = Rarity)) +
  stat_sum() +
  scale_color_manual(values = fcts$col) +
  labs(title = "TRAINSTATION 2") +
  theme_bw() +
  theme(legend.position = "right") -> p3

## do the plot ----
windows(16, 9)
plot(p1)

windows(16, 9)
plot(p2)

windows(11, 11)
plot(p3)

# TABULATE ----
TS2_loco %>% 
  tabyl(Power, Rarity, Tier) %>% 
  adorn_totals(c("row", "col")) %>% 
  print()

# CLEAN UP ----
rm(TS2_loco_raw, fcts, p1, p2, p3)
