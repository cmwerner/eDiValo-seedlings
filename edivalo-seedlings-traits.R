# condense the seedling trait data into single average values

library(tidyverse)
library(GGally)
library(ggplot2)

### Biomass and Length ---------------
# main path
seedlings.size <- read.csv("data/seedling_traits_size.csv", 
                           stringsAsFactors = FALSE)[,1:7]

# View(seedlings.size)

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

# main path
seedlings.leaf <- read.csv("data/seedling_traits_SLA.csv", 
                           stringsAsFactors = FALSE) %>%
  select(group:size.mm2)

# View(seedlings.leaf)

seedlings.leaf$species <- tolower(seedlings.leaf$species)


# notes: data frame is currently sorted by individuals (10/species) and leaf
# where 1 is the first emergent leaf, 2 is the second, and 3 is the third
# in some cases, the 3rd leaf was smaller than leaf 1+2 due to collection timing
# want to check any analyses we do to make sure they're robust to using 2 or 3 leaves

# We only have mass (and therefore SLA) for forbs, not for the grasses

seedlings.leaf$sla.mm2 <- seedlings.leaf$size.mm2/seedlings.leaf$biomass.mg
 

species.sla <- seedlings.leaf %>% 
  group_by(group, species) %>%
  dplyr::summarize(leaf.weight = mean(biomass.mg),
            leaf.area = mean(size.mm2),
            sla = mean(sla.mm2), # using all three leaves
            sla.2 = mean(sla.mm2[leaf %in% c(1,2)])) # using only the first two leaves

## add in to main trait df
species.size.4 <- species.size.3 %>%
  left_join(species.sla, by='species')


### C:N---------------
# main path
seedlings.cn <- read.csv("data/seedling_trait_CN.csv", stringsAsFactors = FALSE,sep = "") %>% 
  select(species = sample, c.perc, n.perc) %>%  
  filter(!is.na(c.perc))

# calculate C:N ratio
seedlings.cn$c.n.ratio <- seedlings.cn$c.perc / seedlings.cn$n.perc

## add in to main trait df
species.size.5 <- species.size.4 %>%
  left_join(seedlings.cn, by='species')

### Visualization Plots-----------------
## condensed to key traits and just the forbs
species.size.6 <- species.size.5 %>% 
  filter(group=='forb') %>%
  select(species, bm.tot, len.sh, rt.sh.bm,
         bm.sh, bm.rt, sla, sla.2, c.n.ratio, toothpicks)

# simple visualization plots to see the range of variation
ggcorr(species.size.6)
# root bm, shoot bm, and total bm are strongly correlated, 
# should probably just use total

# double-checking sla vs sla2, yes they are tightly correlated
ggplot(species.size.6, aes(x=sla, y = sla.2)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red'))

# further filtering based on correlation plots
species.size.7 <- species.size.6 %>% 
  select(species, bm.tot, len.sh, rt.sh.bm, sla.2, c.n.ratio, toothpicks)
ggpairs(species.size.7, columns = c('bm.tot', 'len.sh', 'rt.sh.bm', 'sla.2','c.n.ratio'))
# total biomass is pos.correlated with shoot length, root:shoot, and neg. with SLA
# shoot length is also pos. correlated with C:N
# the only strong (R2 > .5) corrleation is between total biomass and shoot length
# UPDATE: with the new species there is no strong (R2 > .5) correlation anymore

## seeing where our toothpick species fall on these axes
ggplot(species.size.7, aes(x=bm.tot, y = rt.sh.bm)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red')) +
  geom_smooth(method = 'lm', se = FALSE)

ggplot(species.size.7, aes(x=bm.tot, y = len.sh)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red')) +
  geom_smooth(method = 'lm', se = FALSE)

ggplot(species.size.7, aes(x=sla.2, y = c.n.ratio)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red')) +
  geom_smooth(method = 'lm', se = FALSE)

# surprisingly positive correlation between fert and shade responses
# I'm not convinced on this, I think it's possible it's an artifact of 
# how the data were collected (counting germination days etc)
ggplot(species.size.3, aes(x=fert.diff.bm, y = shade.diff.bm)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red')) +
  geom_smooth(method = 'lm', se = FALSE)

# somewhat different pattern for the heights 
# some respond to shade by growing taller--this is its own trait
ggplot(species.size.3, aes(x=fert.diff.len, y = shade.diff.len)) + 
  geom_text(aes(label=species, color=toothpicks),hjust=0, vjust=0) +
  scale_color_manual(values=c('black','red')) +
  geom_smooth(method = 'lm', se = FALSE)
