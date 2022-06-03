# LIBRARIES ----
library(tidyverse)
library(janitor)

# DATA ----

## raw data ----
TS2_raw <- "Name,Tier,Rarity,Power,Capmax
ATSF 3000,2,violet,steam,45
CLASS A-1 BERKSHIRE,3,silver,steam,20
CRAMPTON,1,gold,steam,60
C&O T-1,4,silver,steam,20
DB-BAUREIHE V 100,2,silver,diesel,20
DB-BAUREIHE V 200,2,violet,diesel,45
DR-BAUREIHE 44,3,blue,steam,30
DR-BAUREIHE 86,3,silver,steam,20
DR-BAUREIHE E 60,3,silver,electric,20
DR-BAUREIHE E 93,3,violet,electric,45
DR 18 201,2,blue,steam,30
DRG V 140 001,4,silver,diesel,20
EASTER MIREO,2,violet,electric,60
EP-2 BIPOLAR,2,gold,electric,60
ERIE K-5A,3,blue,steam,30
ERIEL L-1,1,gold,steam,60
FLYEUROPE CARAVAGGIO,3,blue,electric,40
FS CLASS 670,2,violet,steam,45
FS CLASS 740,1,silver,steam,20
GER CLASS S69,1,silver,steam,20
GWR 3041 THE QUEEN,1,violet,steam,45
GWR 6000 CLASS KING,3,blue,steam,30
GWR CITY OF TRURO,3,blue,steam,30
GWRT OLTON HALL,3,gold,steam,60
KROKODIL CE 6/8,2,violet,electric,45
LB&SCR B4,1,blue,steam,30
LMS HUGHES CRAB,3,violet,steam,45
LNER A4 MALLARD,1,gold,steam,60
LNER K3,2,silver,steam,20
LRZ 14,2,blue,diesel,30
MILWAUKEE ROAD,4,silver,steam,20
MILWAUKEE ROAD EF-1,2,blue,electric,30
NEW YORK CENTRAL HUDSON,3,silver,steam,20
NORD 140,1,blue,steam,30
NSB EL 4,3,blue,electric,30
PREUẞISCHE P8,1,silver,steam,20
PREUẞISCHE T 14,2,blue,steam,30
PRR K-4,2,blue,steam,30
SAR CLASS 8E,4,silver,electric,30
SBB AE 4/7,3,violet,electric,45
SECR N CLASS,2,silver,steam,20
SHAY CLASS C,1,violet,steam,45
STAR CLASS 4000,1,silver,steam,20
SŽD EMCH 3,3,violet,diesel,45
TFR CLASS 19E,3,gold,electric,80
UP CLASS 9000,3,violet,steam,45
UP GTEL 3RD GEN,3,violet,diesel,45
UP GTEL VERANDA,3,violet,diesel,45
UP FEF-3,4,blue,steam,30
USRA HEAVY PACIFIC,3,silver,steam,20
VICTORIAN C CLASS,2,silver,steam,20
"
## factor definitions ----
fcts <- list(con = c(BRITAIN = 1L, GERMANY = 2L, USA = 3L, FRANCE = 4L),
             rat = c(COMMON = "silver", RARE = "blue", V_RARE = "violet", X_RARE = "gold"),
             rar = c(SILVER = "silver", BLUE = "blue", VIOLET = "violet", GOLD = "gold"),
             pow = c(STEAM = "steam", DIESEL = "diesel", ELECTRIC = "electric"))

# TIDY ----
TS2 <- read_delim(TS2_raw, col_types = "ciffd", lazy = FALSE) %>% 
  mutate(Country = factor(Tier, levels = fcts$con, labels = names(fcts$con)),
         Rating = factor(Rarity, levels = fcts$rat, labels = names(fcts$rat)),
         Rarity = factor(Rarity, levels = fcts$rar, labels = names(fcts$rar)),
         Power = factor(Power, levels = fcts$pow, labels = names(fcts$pow))) %>% 
  arrange(Tier, Rarity, Power, Name)

# TABULATE ----
TS2 %>% 
  tabyl(Power, Rarity, Tier) %>% 
  adorn_totals(c("row", "col"))

# GRAPH ----
TS2 %>%
  mutate(Tier = paste(Tier, "-", Country)) %>% 
  group_by(Tier, Rarity, Power) %>%
  summarise(Count = n(), .groups = "drop") %>%
  ggplot(mapping = aes(x = Rarity, y = Power)) +
  geom_tile(mapping = aes(fill = Count), color = "white") +
  geom_text(mapping = aes(label = paste0("(",Count,")")), size = 3) +
  scale_fill_distiller(palette = "YlGn") +
  facet_wrap(~ Tier) +
  labs(title = "TRAINSTATION 2") +
  theme_bw() +
  theme(legend.position = "right")

# CLEAN UP ----
rm(TS2_raw, fcts)
