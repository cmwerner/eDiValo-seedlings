# condense the seedling trait data into single average values

library(tidyverse)
library(GGally)
library(ggplot2)

### Biomass and Length ---------------
# Chhaya's path
seedlings.size <- read.csv("~/Dropbox/eDiValo-seedlings/Data Entry/seedling_trait_size.csv", 
                           stringsAsFactors = FALSE)[,1:7]

# Mia's path
seedlings.size <- read.csv("C:/Users/mj65tivo/Dropbox/eDiValo-seedlings/Data Entry/seedling_trait_size.csv", 
                           stringsAsFactors = FALSE)[,1:7]
# for Mia: change name of fist column to species 
colnames(seedlings.size)[1]<-"species"

View(seedlings.size)

# calculated metrics: root-shoot length and biomass, total biomass
seedlings.size$root.shoot.length <- seedlings.size$root.length / seedlings.size$shoot.length
seedlings.size$root.shoot.bm <- seedlings.size$root.bm / seedlings.size$shoot.bm
seedlings.size$total.bm <- seedlings.size$root.bm + seedlings.size$shoot.bm
seedlings.size$treatment <- factor(seedlings.size$treatment, 
                                   levels = c('none','fert','shade'))

# average for each species (rather than by individual)
species.size <- seedlings.size %>% 
  group_by(treatment, species) %>%
  dplyr::summarize(bm.sh = mean(shoot.bm),
            bm.rt = mean(root.bm),
            bm.tot = mean(total.bm),
            len.sh = mean(shoot.length),
            rt.sh.bm = mean(root.shoot.bm))

# calculated metric: fertilizer and shade responses of shoot length and biomass
species.size.2 <- species.size %>% 
  pivot_wider(id_cols = species,
              names_from = treatment, 
              values_from = c(bm.sh, len.sh, bm.rt, bm.tot, rt.sh.bm), 
              names_sep = ".") %>%
  select(species, bm.sh = bm.sh.none, bm.rt = bm.rt.none, bm.tot = bm.tot.none,
         len.sh = len.sh.none, rt.sh.bm = rt.sh.bm.none, 
         bm.sh.fert, bm.sh.shade, len.sh.fert, len.sh.shade)

species.size.2$fert.diff.bm <- species.size.2$bm.sh.fert / species.size.2$bm.sh
species.size.2$fert.diff.len <- species.size.2$len.sh.fert / species.size.2$len.sh
species.size.2$shade.diff.bm <- species.size.2$bm.sh.shade / species.size.2$bm.sh
species.size.2$shade.diff.len <- species.size.2$len.sh.shade / species.size.2$len.sh

# trimming down columns to only our final metrics
species.size.3 <- species.size.2 %>% 
  select(species, bm.tot, bm.sh, len.sh, 
         bm.rt, rt.sh.bm, 
         fert.diff.bm, fert.diff.len, shade.diff.bm, shade.diff.len)

toothpick.list <- c('plalan','crebie','galalb','medfal','diacar','daucar')
species.size.3$toothpicks <- species.size.3$species %in% toothpick.list


### SLA ----------------

# Chhaya's path
seedlings.leaf <- read.csv("~/Dropbox/eDiValo-seedlings/Data Entry/seedling_trait_SLA.csv", 
                           stringsAsFactors = FALSE)[,1:6]

# Mia's path
#seedlings.leaf <- read.csv("C:/Users/mj65tivo/Dropbox/eDiValo-seedlings/Data Entry/seedling_trait_SLA.csv", 
#                           stringsAsFactors = FALSE)[,1:6]
# For Mia: change name of first column to species
# colnames(seedlings.leaf)[1]<-"species"

View(seedlings.leaf)

seedlings.leaf$species <- tolower(seedlings.leaf$species)

## still need a lot of the species to be entered

# notes: data frame is currently sorted by individuals (10/species) and leaf
# where 1 is the first emergent leaf, 2 is the second, and 3 is the third
# in some cases, the 3rd leaf was smaller than leaf 1+2 due to collection timing
# want to check any analyses we do to make sure they're robust to using 2 or 3 leaves

seedlings.leaf$sla.mm2 <- seedlings.leaf$size.mm2/seedlings.leaf$biomass.mg
 
# note: this is a good place to check for data entry errors (extreme values)

species.sla <- seedlings.leaf %>% 
  group_by(species) %>%
  dplyr::summarize(leaf.weight = mean(biomass.mg),
            leaf.area = mean(size.mm2),
            sla = mean(sla.mm2), # using all three leaves
            sla.2 = mean(sla.mm2[leaf %in% c(1,2)])) # using only the first two leaves

## add in to main trait df
species.size.4 <- species.size.3 %>%
  left_join(species.sla, by='species')

## even more condensed to key traits
species.size.5 <- species.size.4 %>% 
  select(species, bm.tot, len.sh, rt.sh.bm,
         bm.sh, bm.rt, sla, sla.2, toothpicks)

### Visualization Plots-----------------
# simple visualization plots to see the range of variation
ggcorr(species.size.5)
# root bm, shoot bm, and total bm are strongly correlated, 
# should probably just use total
# sla and sla2 are strongly correlated, that's good

species.size.6 <- species.size.5 %>% 
  select(species, bm.tot, len.sh, rt.sh.bm, sla, toothpicks)
ggpairs(species.size.6, columns = c('bm.tot', 'len.sh', 'rt.sh.bm', 'sla'))
# shoot length is positively correlated with total bm and root:shoot bm
# SLA is also correlated with shoot length and root:shoot bm

## seeing where our toothpick species fall on these axes
ggplot(species.size.6, aes(x=bm.tot, y = rt.sh.bm)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red'))

ggplot(species.size.6, aes(x=bm.tot, y = len.sh)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red'))

# double-checking sla vs sla2
ggplot(species.size.5, aes(x=sla, y = sla.2)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red'))

# surprisingly positive correlation between fert and shade responses
# I'm not convinced on this, I think it's possible it's an artifact of 
# how the data were collected (counting germination days etc)
ggplot(species.size.3, aes(x=fert.diff.bm, y = shade.diff.bm)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red'))

# somewhat different pattern for the heights 
# some respond to shade by growing taller--this is its own trait
ggplot(species.size.3, aes(x=fert.diff.len, y = shade.diff.len)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red'))
