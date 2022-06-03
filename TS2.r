# LIBRARIES ----
library(tidyverse)
library(janitor)

# DATA ----

## raw data ----
TS2_raw <- "Tier,Rarity,Power,Name,Capmax
2,violet,steam,ATSF 3000,45
3,silver,steam,CLASS A-1 BERKSHIRE,20
1,gold,steam,CRAMPTON,60
4,silver,steam,C&O T-1,20
2,silver,diesel,DB-BAUREIHE V 100,20
2,violet,diesel,DB-BAUREIHE V 200,45
3,blue,steam,DR-BAUREIHE 44,30
3,silver,steam,DR-BAUREIHE 86,20
3,silver,electric,DR-BAUREIHE E 60,20
3,violet,electric,DR-BAUREIHE E 93,45
2,blue,steam,DR 18 201,30
4,silver,diesel,DRG V 140 001,20
2,violet,electric,EASTER MIREO,60
2,gold,electric,EP-2 BIPOLAR,60
3,blue,steam,ERIE K-5A,30
1,gold,steam,ERIEL L-1,60
3,blue,electric,FLYEUROPE CARAVAGGIO,40
2,violet,steam,FS CLASS 670,45
1,silver,steam,FS CLASS 740,20
1,silver,steam,GER CLASS S69,20
1,violet,steam,GWR 3041 THE QUEEN,45
3,blue,steam,GWR 6000 CLASS KING,30
3,blue,steam,GWR CITY OF TRURO,30
3,gold,steam,GWRT OLTON HALL,60
2,violet,electric,KROKODIL CE 6/8,45
1,blue,steam,LB&SCR B4,30
3,violet,steam,LMS HUGHES CRAB,45
1,gold,steam,LNER A4 MALLARD,60
2,silver,steam,LNER K3,20
2,blue,diesel,LRZ 14,30
4,silver,steam,MILWAUKEE ROAD, 20
2,blue,electric,MILWAUKEE ROAD EF-1,30
3,silver,steam,NEW YORK CENTRAL HUDSON,20
1,blue,steam,NORD 140,30
3,blue,electric,NSB EL 4,30
1,silver,steam,PREUẞISCHE P8,20
2,blue,steam,PREUẞISCHE T 14,30
2,blue,steam,PRR K-4,30
4,silver,electric,SAR CLASS 8E,30
3,violet,electric,SBB AE 4/7,45
2,silver,steam,SECR N CLASS,20
1,violet,steam,SHAY CLASS C,45
1,silver,steam,STAR CLASS 4000,20
3,violet,diesel,SŽD EMCH 3,45
3,gold,electric,TFR CLASS 19E,80
3,violet,steam,UP CLASS 9000,45
3,violet,diesel,UP GTEL 3RD GEN,45
3,violet,diesel,UP GTEL VERANDA,45
4,blue,steam,UP FEF-3,30
3,silver,steam,USRA HEAVY PACIFIC,20
2,silver,steam,VICTORIAN C CLASS,20
"
## factor definitions ----
fcts <- list(con = c(BRITAIN = 1L, GERMANY = 2L, USA = 3L, FRANCE = 4L),
             rat = c(COMMON = "silver", RARE = "blue", V_RARE = "violet", X_RARE = "gold"),
             rar = c(SILVER = "silver", BLUE = "blue", VIOLET = "violet", GOLD = "gold"),
             pow = c(STEAM = "steam", DIESEL = "diesel", ELECTRIC = "electric"))

# TIDY ----
TS2 <- read_delim(TS2_raw, col_types = "iffcd", lazy = FALSE) %>% 
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
