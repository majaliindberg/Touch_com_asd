library(readr)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(stringr)

source('comm_source.R')

live.pleas.data <- read_csv('data/primary/live-pleas_collated.csv', col_types = cols())

if ( !dir.exists('figures') ) { dir.create('figures') }

theme_set(theme_light()) 

quartz(width = 6.2, height = 3.6); plot(1:10)

live.pleas.data %>% 
  ggplot(aes(x = cued, y = response, colour = group)) +
  geom_hline(yintercept = 0) +
  stat_summary(geom = 'crossbar', fun.data = 'mean_cl_normal', 
               width = 0.4, fill = 'white', alpha = 0.5,
               position = position_dodge(0.5)) +
  geom_point(aes(y = response), alpha = 0.7,
              fill='white', shape = 21,
              position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0, dodge.width = 0.5)) +
  scale_x_discrete(label = str_trunc(str_to_title(orderedCues),3,'right','')) +
  scale_y_continuous(breaks = c(-10,0,10), labels = c('unpleasant','','pleasant')) +
  scale_colour_manual(values = c(colour.ASD, colour.Control)) +
  theme_x45deg + 
  labs(y = 'Pleasantness rating (VAS)', x = NULL)

ggsave('figures/live-pleas_ASD-vs-Control-ratings.svg')
ggsave('figures/live-pleas_ASD-vs-Control-ratings.pdf')
