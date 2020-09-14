# looking at the environmental data for the edivalo plots
# available is light quantity, light quality, vole disturbance, litter depth, soil moisture
# may get air temp and rh later

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
ggsave(filename = 'light_quant_1.pdf', width=6, height=5, units='in')



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

ggsave(filename = 'light_quant_2.pdf', width=6, height=5, units='in')


### vole disturbance--------
vole <- read.csv("data/vole_disturb.csv", stringsAsFactors = FALSE)[,c(3, 5, 12:16)]
# also reads in bare ground and litter cover, litter depth is in another file

plot.vole <- plot.list %>% 
  left_join(vole, by = c('plotid' = 'subplot', 'treat' = 'treatmentcode', 'block'))

plot.vole.summary <- plot.vole %>% 
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
ggsave(filename = 'vole_dist.pdf', width=6, height=5, units='in')

