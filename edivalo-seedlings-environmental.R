# looking at the environmental data for the edivalo plots
# available is light quantity, light quality, vole disturbance, litter depth, soil moisture
# may get air temp and rh later

library(tidyverse) # data structuring
library(ggplot2) # plotting
library(ggthemes) # plotting
library(lme4) # basic models
library(lmerTest) # p-values on basic models
#library(broom.mixed)

# theme for plotting
theme_cw <- function () { 
  theme_bw(base_size=12) %+replace% 
    theme(
      panel.background = element_blank(), 
      plot.background = element_blank(), 
      axis.ticks = element_line(colour = "grey70", size = rel(0.5)),
      panel.grid.minor = element_blank(), 
      panel.grid.major.x = element_blank(),
      legend.background = element_blank(), 
      legend.key = element_blank(),
      strip.background = element_blank(), 
      strip.text=element_text(size=12),
      axis.text=element_text(size=12),
      complete = TRUE
    )
}

plot.list <- read.csv("data/plot_list.csv", stringsAsFactors = FALSE)
# combine plot numbers and treatments together in plot list df
plot.list$plot <- paste(plot.list$plotid, plot.list$treat, sep="")
plot.list$block <- rep(1:10, each = 8)
# adding separate columns for exclosure, light, and fertilization treatments
plot.list$grazing <- 'sheep'
plot.list$grazing[grep('E', plot.list$plot)] <- 'exclosure'
plot.list$nutrient <- 'unfertilized'
plot.list$nutrient[grep('F', plot.list$plot)] <- 'fertilized'
plot.list$light <- 'control'
plot.list$light[grep('L', plot.list$plot)] <- 'lamps'

### light quantity-------
light <- read.csv("data/light_quantity_20b.csv", stringsAsFactors = FALSE)

plot.light <- plot.list %>% 
  left_join(light, by = c('block', 'plotid' = 'subplot'))

# making the unlighted values for the no lamp plots the same as the lighted ones
unlighted <- which(is.na(plot.light$light_quant_unlighted))
plot.light$light_quant_unlighted[unlighted] <- plot.light$light_quant_lighted[unlighted]

plot.light.summary <- plot.light %>% 
  group_by(grazing, nutrient, light) %>%
  dplyr::summarise(
    n = length(light_quant_lighted),
    light.mean = mean(light_quant_lighted),
    light.se = sd(light_quant_lighted)/sqrt(n),
    unlit.mean = mean(light_quant_unlighted),
    unlit.se = sd(light_quant_unlighted)/sqrt(n)
  )

lighted.plot <- ggplot(plot.light.summary, 
                       aes(x=nutrient, y=light.mean, 
                           ymin=light.mean-light.se,
                           ymax=light.mean+light.se,
                           color=light)) +
  facet_grid(.~grazing) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_errorbar(width=0.2, position=position_dodge(0.2)) +
  ylab('Light levels (under lamps)') +
  xlab('') +
  theme_cw() +
  theme(text = element_text(size=12)) +
  scale_color_manual(values=c('black','orange'), name='') +
  theme(legend.position = 'bottom', legend.text=element_text(size=12))
#ggsave(filename = 'light_quant_1.pdf', width=6, height=5, units='in')



unlighted.plot <- ggplot(plot.light.summary, 
                         aes(x=nutrient, y=unlit.mean, 
                             ymin=unlit.mean-unlit.se,
                             ymax=unlit.mean+unlit.se,
                             color=light)) +
  facet_grid(.~grazing) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_errorbar(width=0.2, position=position_dodge(0.2)) +
  ylab('Light levels (unlit)') +
  xlab('') +
  theme_cw() +
  theme(text = element_text(size=12)) +
  scale_color_manual(values=c('black','orange'), name='') +
  theme(legend.position = 'bottom', legend.text=element_text(size=12))

#ggsave(filename = 'light_quant_2.pdf', width=6, height=5, units='in')


### light quality----
## Note that this data was collected in late July
light.qual <- read.csv("data/light_quality_20.csv", stringsAsFactors = FALSE)[,c(2:6, 8)]

plot.light.2 <- light.qual %>% 
  pivot_wider(names_from = 'lighted', values_from = 'rfr') %>%
  dplyr::rename(light_qual_lighted = Y, light_qual_unlighted = N)

# making the unlighted values for the no lamp plots the same as the lighted ones
unlighted.2 <- which(is.na(plot.light.2$light_qual_lighted))
plot.light.2$light_qual_lighted[unlighted.2] <- plot.light.2$light_qual_unlighted[unlighted.2]

plot.light <- left_join(plot.light, plot.light.2[,c(1, 2, 5, 6)],
                        by = c('block','plotid' = 'plot'))


plot.light.summary.2 <- plot.light %>% 
  group_by(grazing, nutrient, light) %>%
  dplyr::summarise(
    n = length(light_qual_lighted),
    qual.mean = mean(light_qual_lighted),
    qual.se = sd(light_qual_lighted)/sqrt(n),
    qual.unlit.mean = mean(light_qual_unlighted),
    qual.unlit.se = sd(light_qual_unlighted)/sqrt(n)
  )

lighted.plot <- ggplot(plot.light.summary.2, 
                       aes(x=nutrient, y=qual.mean, 
                           ymin=qual.mean-qual.se,
                           ymax=qual.mean+qual.se,
                           color=light)) +
  facet_grid(.~grazing) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_errorbar(width=0.2, position=position_dodge(0.2)) +
  ylab('Light red:far red') +
  xlab('') +
  theme_cw() +
  theme(text = element_text(size=12)) +
  scale_color_manual(values=c('black','orange'), name='') +
  theme(legend.position = 'bottom', legend.text=element_text(size=12))
#ggsave(filename = 'light_qual_1.pdf', width=6, height=5, units='in')



unlighted.plot <- ggplot(plot.light.summary.2, 
                         aes(x = nutrient, y = qual.unlit.mean, 
                             ymin = qual.unlit.mean - qual.unlit.se,
                             ymax = qual.unlit.mean + qual.unlit.se,
                             color = light)) +
  facet_grid(.~grazing) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_errorbar(width=0.2, position=position_dodge(0.2)) +
  ylab('Light red:far red (unlit)') +
  xlab('') +
  theme_cw() +
  theme(text = element_text(size=12)) +
  scale_color_manual(values=c('black','orange'), name='') +
  theme(legend.position = 'bottom', legend.text=element_text(size=12))

#ggsave(filename = 'light_qual_2.pdf', width=6, height=5, units='in')


### vole disturbance--------
vole <- read.csv("data/vole_disturb.csv", stringsAsFactors = FALSE)[,c(3, 5, 12:16)]
# also reads in bare ground and litter cover, litter depth is in another file

plot.light.vole <- plot.light %>% 
  left_join(vole, by = c('plotid' = 'subplot', 'treat' = 'treatmentcode', 'block'))

plot.vole.summary <- plot.light.vole %>% 
  group_by(grazing, nutrient, light) %>%
  dplyr::summarise(
    n = length(vole_disturbance),
    vole.mean = mean(vole_disturbance),
    vole.se = sd(vole_disturbance)/sqrt(n)
  )

vole.plot <- ggplot(plot.vole.summary, 
                       aes(x=nutrient, y=vole.mean, 
                           ymin=vole.mean-vole.se,
                           ymax=vole.mean+vole.se,
                           color=light)) +
  facet_grid(.~grazing) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_errorbar(width=0.2, position=position_dodge(0.2)) +
  ylab('Vole disturbance (%)') +
  xlab('') +
  theme_cw() +
  theme(text = element_text(size=12)) +
  scale_color_manual(values=c('black','orange'), name='') +
  theme(legend.position = 'bottom', legend.text=element_text(size=12))
#ggsave(filename = 'vole_dist.pdf', width=6, height=5, units='in')

### Litter depth--------
litter <- read.csv("data/litter_depth_20.csv", stringsAsFactors = FALSE)[c(1, 2, 7)]

plot.light.vole$litter_depth <- NULL
plot.env <- plot.light.vole %>% 
  left_join(litter, by = c('block','plotid' = 'plot')) %>%
  dplyr::rename(litter_depth = depth_avg) %>%
  rename_with(., ~ tolower(gsub("_",".",.x, fixed = TRUE)))


plot.litter.summary <- plot.env %>% 
  group_by(grazing, nutrient, light) %>%
  dplyr::summarise(
    n = length(litter.depth),
    litter.mean = mean(litter.depth),
    litter.se = sd(litter.depth)/sqrt(n)
  )

litter.plot <- ggplot(plot.litter.summary, 
                    aes(x=nutrient, y=litter.mean, 
                        ymin=litter.mean-litter.se,
                        ymax=litter.mean+litter.se,
                        color=light)) +
  facet_grid(.~grazing) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_errorbar(width=0.2, position=position_dodge(0.2)) +
  ylab('Litter Depth') +
  xlab('') +
  theme_cw() +
  theme(text = element_text(size=12)) +
  scale_color_manual(values=c('black','orange'), name='') +
  theme(legend.position = 'bottom', legend.text=element_text(size=12))
#ggsave(filename = 'litter_depth.pdf', width=6, height=5, units='in')

